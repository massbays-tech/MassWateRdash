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
    msg,
    gregexpr("row\\(s\\)\\s+([0-9, ]+)", msg, perl = TRUE)
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
    msg,
    gregexpr("\\(column (\\d+)\\)", msg, perl = TRUE)
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
        ln,
        gregexpr("row\\(s\\)\\s+[\\d, ]+", ln, perl = TRUE)
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

# Handle retry after user edits in handsontable
# show_all: TRUE when the user toggled to full-table view (no row merge needed)
# problem_rows: indices that were displayed in filtered view
handle_retry <- function(
  data_name,
  val_log,
  val_edit,
  val_dat,
  hot_input,
  hot_headers_input = NULL,
  show_all = TRUE,
  problem_rows = integer(0)
) {
  val_log$msg <- ""

  if (!is.null(hot_input)) {
    edited_df <- rhandsontable::hot_to_r(hot_input)
  } else {
    req(val_dat$raw_dat)
    edited_df <- val_dat$raw_dat
  }

  # Apply any edited column names from the header editor
  if (!is.null(hot_headers_input)) {
    new_names <- unlist(
      rhandsontable::hot_to_r(hot_headers_input)[1, ],
      use.names = FALSE
    )
    if (length(new_names) == ncol(edited_df)) {
      names(edited_df) <- new_names
    }
  }

  # When filtered view was active, merge the edited subset back into the full data
  if (!show_all && length(problem_rows) > 0 && !is.null(val_dat$raw_dat)) {
    full_df <- val_dat$raw_dat
    names(full_df) <- names(edited_df)
    valid_rows <- problem_rows[
      problem_rows >= 1 & problem_rows <= nrow(full_df)
    ]
    full_df[valid_rows, ] <- edited_df[seq_along(valid_rows), ]
    edited_df <- full_df
  }

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

  if (!is.null(result)) {
    val_edit[[data_name]] <- FALSE
    val_dat$raw_dat <- NULL
  }
}
