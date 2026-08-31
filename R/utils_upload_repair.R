#' Check for column errors
#'
#' @description `is_column_error()` checks if the validation message contains
#' any warnings for missing or misnamed columns.
#'
#' @param msg String. Validation message.
#'
#' @return Boolean. If any column name warning(s) detected, returns `TRUE`,
#' else returns `FALSE`.
#'
#' @noRd
is_column_error <- function(msg) {
  if (is.null(msg) || nchar(trimws(msg)) == 0) {
    return(FALSE)
  }
  grepl("correct the column names|Missing the following columns", msg)
}

#' Parse problem rows
#'
#' @description `parse_problem_rows()` parses the listed row indices from a
#' validation message.
#'
#' @param msg String. Validation message.
#'
#' @return List or integer containing row numbers.
#'
#' @noRd
parse_problem_rows <- function(msg) {
  if (is.null(msg) || nchar(trimws(msg)) == 0) {
    return(integer(0))
  }
  msg <- gsub("\033\\[[0-9;]*[mGKHFABCDJK]", "", msg)
  hits <- regmatches(
    msg, gregexpr("row\\(s\\)\\s+([0-9, ]+)", msg, perl = TRUE)
  )[[1]]
  if (length(hits) == 0) {
    return(integer(0))
  }
  nums <- gsub("row\\(s\\)\\s+", "", hits)
  rows <- suppressWarnings(as.integer(unlist(strsplit(nums, "[, ]+"))))
  sort(unique(rows[!is.na(rows)]))
}

# Parse column indices and a column->row cell map from a validation message.
# col_indices: 1-based positions flagged via "(column N)" — for header highlighting.
# cell_map:    named list col_name -> row_indices for cell-level highlighting.
#   Pattern A: "ColName (row(s) N, M)" explicit pairing (frecomdat/accdat style).
#   Pattern B: a known column name appears in the same line as "row(s) N".
parse_error_locations <- function(msg, col_names = NULL) {
  empty <- list(col_indices = integer(0), cell_map = list())
  if (is.null(msg) || nchar(trimws(msg)) == 0) {
    return(empty)
  }
  msg <- gsub("\033\\[[0-9;]*[mGKHFABCDJK]", "", msg)

  col_idx_hits <- regmatches(
    msg, gregexpr("\\(column (\\d+)\\)", msg, perl = TRUE)
  )[[1]]
  col_indices <- suppressWarnings(
    as.integer(gsub("[^0-9]", "", col_idx_hits))
  ) |>
    unique() |>
    sort()
  col_indices <- col_indices[!is.na(col_indices)]

  cell_map <- list()

  for (ln in strsplit(msg, "\n")[[1]]) {
    if (!grepl("row\\(s\\)", ln)) {
      next
    }

    pA <- regmatches(
      ln,
      gregexpr("([^,\n]+?)\\s+\\(row\\(s\\)\\s+[\\d, ]+\\)", ln, perl = TRUE)
    )[[1]]
    if (length(pA) > 0) {
      for (hit in pA) {
        col_nm <- trimws(sub("\\s*\\(row\\(s\\).*", "", hit))
        rows_str <- regmatches(hit, regexpr("row\\(s\\)\\s+[\\d, ]+", hit))
        rows <- sort(unique(suppressWarnings(as.integer(
          unlist(strsplit(gsub("row\\(s\\)\\s+", "", rows_str), "[, ]+"))
        ))))
        rows <- rows[!is.na(rows)]
        if (nchar(col_nm) > 0 && length(rows) > 0) {
          cell_map[[col_nm]] <- sort(unique(c(cell_map[[col_nm]], rows)))
        }
      }
      next
    }

    if (!is.null(col_names)) {
      rows_str <- regmatches(
        ln, gregexpr("row\\(s\\)\\s+[\\d, ]+", ln, perl = TRUE)
      )[[1]]
      rows <- sort(unique(suppressWarnings(as.integer(
        unlist(strsplit(gsub("row\\(s\\)\\s+", "", rows_str), "[, ]+"))
      ))))
      rows <- rows[!is.na(rows)]
      if (length(rows) > 0) {
        for (cn in col_names) {
          if (grepl(cn, ln, fixed = TRUE)) {
            cell_map[[cn]] <- sort(unique(c(cell_map[[cn]], rows)))
          }
        }
      }
    }
  }

  list(col_indices = col_indices, cell_map = cell_map)
}

# Parse repeat errors
parse_repeat_errors <- function(dat, locs) {
  target_col <- names(locs$cell_map)[1]

  col_list <- c(
    "Parameter", "Characteristic Name", "uom", "Activity Depth/Height Unit",
    "Result Unit", "Activity Type"
  )

  if (is.null(target_col) || !target_col %in% col_list) {
    return(NULL)
  }

  problem_rows <- locs$cell_map[[target_col]]

  if (length(problem_rows) < 10) {
    return(NULL)
  }

  dat <- dat[problem_rows, , drop = FALSE]
  ndat <- dplyr::count(dat, .data[[target_col]])

  if (max(ndat$n) < 5) {
    return(NULL)
  }

  new_col <- paste("Invalid", target_col)

  ndat |>
    dplyr::filter(.data$n > 1) |>
    dplyr::rename(!!new_col := !!target_col, "Row Count" = "n") |>
    dplyr::mutate("Delete Rows" = FALSE, .before = !!new_col) |>
    dplyr::mutate("Replace With" = NA, .before = "Row Count")
}

#' Update column names
#'
#' @description `update_hot_col()` parses a rhandsontable containing column
#' names and updates the column names for `raw_dat`.
#'
#' Temp note: `input$hot_headers` should be run through `hot_to_r` before
#' running this function; had trouble testing it otherwise.
#'
#' @param .data Dataframe. Hot table containing updated column names.
#' @param raw_dat Dataframe containing raw data. This is the table that will be
#' updated.
#'
#' @return Updated data frame with new column names.
#'
#' @noRd
update_hot_col <- function(.data, raw_dat) {
  new_names <- unlist(.data[1, ], use.names = FALSE)

  if (length(new_names) == ncol(raw_dat)) {
    names(raw_dat) <- new_names
  }

  raw_dat
}

#' Update Variables
#'
#' @description `update_hot_var()` parses a rhandsontable containing variable
#' name substitutions and updates the variables in `raw_dat`.
#'
#' Temp note: `input$hot_var` should be run through `hot_to_r` before
#' running this function; had trouble testing it otherwise.
#'
#' @param .data Dataframe. Hot table containing replacement variables. Must
#' include the columns "Delete Rows", "Invalid [...]", and "Replace With".
#' @param raw_dat Dataframe containing raw data. This is the table that will be
#' updated.
#'
#' @return Updated dataframe. Updated variables are either updated or removed.
#' If no changes can be made, returns `NULL`.
#'
#' @noRd
update_hot_var <- function(.data, raw_dat) {
  dat_var <- .data |>
    dplyr::filter(
      .data[["Delete Rows"]] == TRUE | .data[["Replace With"]] != " "
    )

  if (nrow(dat_var) == 0) {
    return(NULL)
  }

  var_col <- colnames(dat_var)[2]
  target_col <- gsub("Invalid ", "", var_col)

  del_list <- dat_var |>
    dplyr::filter(.data[["Delete Rows"]] == TRUE) |>
    dplyr::pull(.data[[var_col]])

  if (length(del_list) > 0) {
    raw_dat <- raw_dat |>
      dplyr::filter(!.data[[target_col]] %in% del_list)
  }

  dat_sub <- dat_var |>
    dplyr::filter(.data[["Delete Rows"]] == FALSE)

  if (nrow(dat_sub) == 0) {
    return(raw_dat)
  }

  old_var <- dat_sub[, 2]
  new_var <- dat_sub[, 3]

  wqformat::update_var(raw_dat, target_col, old_var, new_var)
}

#' Update Rows
#'
#' @description `update_hot_rows()` parses a rhandsontable containing substitute
#' rows and update `raw_dat`.
#'
#' Temp note: `input$hot_rows` should be run through `hot_to_r` before
#' running this function; had trouble testing it otherwise.
#'
#' @param .data Dataframe. Hot table containing replacement variables. Must
#' include the columns "Delete Rows", "Invalid [...]", and "Replace With".
#' @param raw_dat Dataframe containing raw data. This is the table that will be
#' updated.
#' @param show_all Boolean. Whether `.data` includes all rows or only a subset
#' of rows. Default `TRUE`.
#' @param problem_rows Numeric list. List of problem rows.
#' Default value `integer(0)`.
#'
#' @return Updated dataframe.
#'
#' @noRd
update_hot_rows <- function(
  .data, raw_dat, show_all = TRUE, problem_rows = integer(0)
) {
  edited_df <- .data

  # If filtered view was active, merge the edited subset back into the full data
  if (!show_all && length(problem_rows) > 0 && !is.null(raw_dat)) {
    names(raw_dat) <- names(edited_df)
    valid_rows <- problem_rows[
      problem_rows >= 1 & problem_rows <= nrow(raw_dat)
    ]
    raw_dat[valid_rows, ] <- edited_df[seq_along(valid_rows), ]
    edited_df <- raw_dat
  }

  # Drop blank rows
  edited_df[
    !apply(is.na(edited_df) | edited_df == "" | edited_df == FALSE, 1, all),
  ]
}


# Handle retry after user edits in handsontable
# .data -- edited_df from previous function, but pipeable
handle_retry <- function(.data, data_name, val_log, val_edit, val_dat) {
  val_log$msg <- ""
  edited_df <- .data

  # Persist edits into raw_data_states so they survive a failed retry and the
  # re-rendered table reflects the user's work on the next round of checks
  val_dat$raw_dat <- edited_df

  result <- tryCatch(
    {
      val_log$catch_msg(retry_fns[[data_name]](edited_df))
    },
    error = function(e) {
      val_log$msg <- paste0("Error in ", data_name, ": ", e$message)
      NULL
    }
  )

  val_dat$dat <- result
  val_dat$msg <- val_log$msg

  if (!is.null(result)) {
    val_edit[[data_name]] <- FALSE
    val_dat$raw_dat <- NULL
  }
}
