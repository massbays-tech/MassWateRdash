dfRepair <- R6::R6Class(
  "dfRepair",
  public = list(
    col_error = FALSE,
    problem_rows = NULL,
    repeat_errors = NULL,
    locs = NULL,
    df_col = NULL,
    df_var = NULL,
    df_row = NULL,
    parse_msg = function(msg, raw_dat) {
      col_error <- is_column_error(msg)
      locs <- parse_error_locations(msg, names(raw_dat))

      self$col_error <- col_error
      self$locs <- locs

      if (col_error) {
        self$problem_rows <- NULL
        self$repeat_errors <- NULL

        col_names <- names(raw_dat)
        self$df_col <- setNames(
          as.data.frame(as.list(col_names), stringsAsFactors = FALSE),
          as.character(seq_along(col_names))
        )

        self$df_var <- NULL
        self$df_row <- NULL
      } else {
        self$problem_rows <- parse_problem_rows(msg)
        df_var <- parse_repeat_errors(raw_dat, locs)
        self$repeat_errors <- df_var

        self$df_col <- NULL
        self$df_var <- df_var
        self$df_row <- raw_dat
      }
    },
    initialize = function(
      problem_rows = NULL, repeat_errors = NULL, locs = NULL, df_col = NULL,
      df_var = NULL, df_row = NULL
    ) {
      self$problem_rows <- problem_rows
      self$repeat_errors <- repeat_errors
      self$locs <- locs
      self$df_col <- df_col
      self$df_var <- df_var
      self$df_row <- df_row
    }
  )
)
