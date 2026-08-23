#' Convert a MassWateR flextable to a reactable
#'
#' @description Rebuilds a `MassWateR::tabMWR*()` flextable as a reactable.
#' Cells are rendered using the flextable's own rendered text (so number
#' formatting, "-" placeholders, and blanked-out repeat labels all match
#' exactly), while sorting uses the flextable's underlying data value rather
#' than that display text - otherwise columns sort lexicographically (e.g.
#' "100%" before "75%") instead of numerically. Per-cell background coloring
#' (used for DQO pass/fail highlighting) is reapplied the same way.
#'
#' @param ft A flextable object, or `NULL`.
#' @param group_by String. Name of a column that flextable displays as a
#' merged row-group label (blank on repeat rows), e.g. "Type" or "Parameter".
#' When set, rows are grouped by this column so the label stays attached to
#' its block when the table is sorted by another column. `NULL` for tables
#' with no such grouping column.
#'
#' @return A reactable, or `NULL` if `ft` is `NULL`.
#'
#' @noRd
flextable_to_reactable <- function(ft, group_by = NULL) {
  if (is.null(ft)) {
    return(NULL)
  }

  col_keys <- ft$col_keys
  header_labels <- as.character(unlist(ft$header$dataset[1, col_keys]))
  n <- ft$body$content$nrow
  bg <- ft$body$styles$cells$background.color$data

  # exact displayed text per cell - used only for rendering
  disp <- stats::setNames(
    as.data.frame(
      lapply(seq_along(col_keys), function(j) {
        vapply(seq_len(n), function(i) {
          paste(ft$body$content$data[[i, j]]$txt, collapse = "")
        }, character(1))
      }),
      stringsAsFactors = FALSE
    ),
    col_keys
  )

  # underlying data values - used for sorting so numeric columns sort
  # numerically instead of sorting the display text lexicographically
  dat <- ft$body$dataset[col_keys]
  dat[] <- lapply(dat, function(x) if (is.factor(x)) as.character(x) else x)
  dat[] <- lapply(dat, function(x) {
    if (!is.character(x)) {
      return(x)
    }
    nonblank <- x[!is.na(x) & !x %in% c("", "-")]
    if (length(nonblank) == 0) {
      return(x)
    }
    num <- suppressWarnings(as.numeric(gsub("\\s*%\\s*$", "", nonblank)))
    if (any(is.na(num))) {
      return(x)
    }
    suppressWarnings(as.numeric(gsub("\\s*%\\s*$", "", x)))
  })

  # flextable shows the group label on its own otherwise-blank row, then
  # blanks it on the remaining rows of the block. reactable generates its own
  # group header, so drop those label-only rows, and carry the label forward
  # onto the real data rows so reactable can group on it.
  if (!is.null(group_by) && group_by %in% col_keys) {
    other_cols <- setdiff(col_keys, group_by)
    is_label_row <- disp[[group_by]] != "" &
      apply(disp[other_cols], 1, function(r) all(r == ""))

    last <- ""
    filled <- vapply(disp[[group_by]], function(v) {
      if (v != "") last <<- v
      last
    }, character(1), USE.NAMES = FALSE)
    disp[[group_by]] <- filled
    dat[[group_by]] <- filled

    disp <- disp[!is_label_row, , drop = FALSE]
    dat <- dat[!is_label_row, , drop = FALSE]
    bg <- bg[!is_label_row, , drop = FALSE]
  }

  columns <- stats::setNames(
    lapply(seq_along(col_keys), function(j) {
      key <- col_keys[j]
      reactable::colDef(
        name = header_labels[j],
        align = if (j == 1) "left" else "center",
        cell = function(value, index) disp[[key]][index],
        style = function(value, index) {
          bgcol <- unname(bg[index, key])
          if (is.na(bgcol) || bgcol == "transparent") {
            NULL
          } else {
            list(background = bgcol)
          }
        }
      )
    }),
    col_keys
  )

  reactable::reactable(
    dat,
    columns = columns,
    groupBy = group_by,
    defaultExpanded = TRUE,
    bordered = TRUE,
    resizable = TRUE,
    defaultColDef = reactable::colDef(minWidth = 110)
  )
}

# reactable column defaults for dqo tables - centered, "-" for NA, left-aligned Parameter
dqo_col_defs <- function(dat) {
  defs <- lapply(names(dat), function(nm) {
    reactable::colDef(
      align = if (nm == "Parameter") "left" else "center",
      cell = function(value) if (is.na(value)) "-" else value
    )
  })
  stats::setNames(defs, names(dat))
}

# frecomdat table
frecomdat_reactable <- function(frecomdat) {
  dat <- dplyr::arrange(frecomdat, .data$Parameter, .locale = "en")

  freq_cols <- c(
    "Field Duplicate", "Lab Duplicate", "Field Blank", "Lab Blank",
    "Spike/Check Accuracy"
  )

  reactable::reactable(
    dat,
    columns = dqo_col_defs(dat),
    columnGroups = list(
      reactable::colGroup(name = "Frequency %", columns = freq_cols)
    ),
    bordered = TRUE,
    pagination = FALSE,
    resizable = TRUE,
    defaultColDef = reactable::colDef(minWidth = 110)
  )
}

# accdat table
accdat_reactable <- function(accdat) {
  dat <- dplyr::arrange(accdat, .data$Parameter, .locale = "en")

  reactable::reactable(
    dat,
    columns = dqo_col_defs(dat),
    bordered = TRUE,
    pagination = FALSE,
    resizable = TRUE,
    defaultColDef = reactable::colDef(minWidth = 110)
  )
}
