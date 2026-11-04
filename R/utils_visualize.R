#' Parse custom threshold entries
#'
#' @description `parse_custom_thresh()` splits semicolon-separated threshold
#' values and labels into a data frame, checking that each value is numeric
#' and has a matching label.
#'
#' @param values String. Threshold values separated by semicolons.
#' @param labels String. Threshold labels separated by semicolons.
#'
#' @return A list with `thresh`, a data frame with columns `thresh` and
#' `label` (or `NULL` if both entries are blank), and `error`, a string
#' describing the problem (or `NULL` if the entries are valid).
#'
#' @noRd
parse_custom_thresh <- function(values, labels) {
  split_entries <- function(x) {
    x <- if (is.null(x)) "" else x
    if (trimws(x) == "") {
      return(character(0))
    }
    trimws(strsplit(x, ";", fixed = TRUE)[[1]])
  }

  fail <- function(msg) list(thresh = NULL, error = msg)

  vals <- split_entries(values)
  labs <- split_entries(labels)

  if (length(vals) == 0 && length(labs) == 0) {
    return(list(thresh = NULL, error = NULL))
  }

  if (any(vals == "") || any(labs == "")) {
    return(fail("Remove empty entries between semicolons."))
  }

  num <- suppressWarnings(as.numeric(vals))
  if (any(is.na(num))) {
    return(fail(paste0(
      "Values must be numbers: ",
      paste(vals[is.na(num)], collapse = ", "),
      "."
    )))
  }

  if (length(vals) != length(labs)) {
    return(fail(paste0(
      "Each value needs a label. Found ",
      length(vals), " value(s) and ", length(labs), " label(s)."
    )))
  }

  if (anyDuplicated(labs)) {
    return(fail("Each label must be unique."))
  }

  list(
    thresh = data.frame(thresh = num, label = labs),
    error = NULL
  )
}

#' Show plot errors in the app
#'
#' @description `show_plot_errors()` evaluates a plot expression and passes
#' any error on as a `shiny::safeError()`. Shiny Server sanitizes errors by
#' default, which hides informative MassWateR messages (e.g., unit
#' mismatches) behind a generic message.
#'
#' @param expr Expression that creates a plot.
#'
#' @return The value of `expr`.
#'
#' @noRd
show_plot_errors <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      # req() and validate() conditions are handled by shiny as usual
      if (inherits(e, "shiny.silent.error") || inherits(e, "validation")) {
        stop(e)
      }
      stop(shiny::safeError(e))
    }
  )
}

#' Add custom threshold lines to a plot
#'
#' @description `add_custom_thresh()` adds horizontal threshold lines to a
#' MassWateR plot. Line types alternate between dashed and dotted, starting
#' with the MassWateR default color. The color changes after every two
#' lines.
#'
#' @param p A ggplot object.
#' @param thresh Data frame with columns `thresh` and `label`, or `NULL`.
#'
#' @return The ggplot object with threshold lines drawn behind the data.
#'
#' @noRd
add_custom_thresh <- function(p, thresh) {
  if (is.null(thresh) || nrow(thresh) == 0) {
    return(p)
  }

  linetypes <- c("dashed", "dotted")
  colors <- c("tan", "#0072B2", "#D55E00", "#009E73", "#CC79A7", "black")

  n <- nrow(thresh)
  idx <- seq_len(n) - 1
  thresh$linetype <- linetypes[idx %% length(linetypes) + 1]
  thresh$color <- colors[(idx %/% length(linetypes)) %% length(colors) + 1]

  lyr <- ggplot2::geom_hline(
    data = thresh,
    ggplot2::aes(yintercept = .data$thresh, linetype = .data$label),
    linewidth = 0.75,
    color = thresh$color,
    inherit.aes = FALSE
  )

  # draw behind the data, as MassWateR does for its thresholds
  p$layers <- c(list(lyr), p$layers)

  p +
    ggplot2::scale_linetype_manual(
      values = stats::setNames(thresh$linetype, thresh$label),
      breaks = thresh$label
    ) +
    ggplot2::guides(
      linetype = ggplot2::guide_legend(
        order = 1,
        override.aes = list(color = thresh$color, shape = NA)
      )
    ) +
    ggplot2::labs(linetype = NULL)
}
