dfRepair <- R6::R6Class(
  "dfRepair",
  public = list(
    problem_col = NULL,
    missing_col = NULL,
    problem_rows = NULL,
    repeat_errors = NULL,
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

        self$problem_col <- df_col
        self$missing_col <- setdiff(target_col, all_col)
        self$problem_rows <- NULL
        self$repeat_errors <- NULL

        self$df_col <- df_col
        self$df_var <- NULL
        self$df_row <- NULL
      } else {
        df_var <- parse_repeat_errors(raw_dat, locs)

        self$problem_col <- NULL
        self$missing_col <- NULL
        self$problem_rows <- parse_problem_rows(msg)
        self$repeat_errors <- df_var

        self$df_col <- NULL
        self$df_var <- df_var
        self$df_row <- raw_dat
      }
    },
    initialize = function(
      problem_col = NULL, missing_col = NULL, problem_rows = NULL,
      repeat_errors = NULL, locs = NULL, df_col = NULL, df_var = NULL,
      df_row = NULL
    ) {
      self$problem_col <- problem_col
      self$missing_col <- missing_col
      self$problem_rows <- problem_rows
      self$repeat_errors <- repeat_errors
      self$locs <- locs
      self$df_col <- df_col
      self$df_var <- df_var
      self$df_row <- df_row
    }
  )
)
