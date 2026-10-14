editDat <- R6::R6Class(
  "editDat",
  public = list(
    missing_col = NULL,
    problem_rows = NULL,
    locs = NULL,
    df_col = NULL,
    df_var = NULL,
    df_row = NULL,
    parse_msg = function(msg, raw_dat, dat_name) {
      col_error <- is_column_error(msg)
      locs <- parse_error_locations(msg, names(raw_dat))

      self$locs <- locs

      if (col_error) {
        all_col <- colnames(raw_dat)
        target_col <- file_columns[[dat_name]]

        df_col <- data.frame(
          "Delete Column" = FALSE,
          "Invalid Column Name" = setdiff(all_col, target_col),
          "New Column Name" = NA,
          check.names = FALSE
        )

        self$missing_col <- setdiff(target_col, all_col)
        self$problem_rows <- NULL

        self$df_col <- df_col
        self$df_var <- NULL
        self$df_row <- NULL
      } else {
        bad_rows <- parse_problem_rows(msg)

        if (!is.null(raw_dat)) {
          raw_dat <- raw_dat |>
            dplyr::mutate("ID" = dplyr::row_number()) |>
            dplyr::relocate("ID") |>
            dplyr::mutate("bad_row" = FALSE)

          if (length(bad_rows) > 0) {
            raw_dat[bad_rows, "bad_row"] <- TRUE
          }
        }

        self$missing_col <- NULL
        self$problem_rows <- bad_rows

        self$df_col <- NULL
        self$df_var <- parse_repeat_errors(raw_dat, locs)
        self$df_row <- raw_dat
      }
    },
    edit_row = function(val, filter_rows) {
      row_num <- val$row

      if (filter_rows) {
        # Find equivalent row number for filtered data
        dat <- dplyr::filter(self$df_row, .data$bad_row == TRUE)
        id_num <- dat$ID[row_num]
        row_num <- which(self$df_row$ID == id_num)
      }

      self$df_row[row_num, val$column] <- val$value
    },
    initialize = function(
      missing_col = NULL, problem_rows = NULL, locs = NULL, df_col = NULL,
      df_var = NULL, df_row = NULL
    ) {
      self$missing_col <- missing_col
      self$problem_rows <- problem_rows
      self$locs <- locs
      self$df_col <- df_col
      self$df_var <- df_var
      self$df_row <- df_row
    }
  )
)
