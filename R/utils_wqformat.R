#' Rename variables in column
#'
#' @description `try_rename()` is a helper function for `convert_results()` 
#' that renames the variables in a column. 
#'
#' @param .data Input dataframe
#' @param col_name Column name.
#' @param var_names Named vector. Vector should consist of old variable names, 
#' with vector names set to equivalent new variable name.
#'
#' @return Updated dataframe. If `var_names` is `NA` or `NULL`, the original 
#' dataframe is returned.
#'
#' @noRd
try_rename <- function(.data, col_name, var_names) {
  new_var <- names(var_names)
  old_var <- unname(var_names)
  
  chk <- shiny::isTruthy(old_var) && shiny::isTruthy(new_var)
  if (!chk) {
    return(.data)
  }
  
  .data |>
    wqformat::update_var(col_name, old_var, new_var)
}

#' Convert custom results to MassWateR format 
#'
#' @description `convert_results()` converts input data from a custom
#' format to MassWateR by renaming columns and key variables. 
#'
#' @param .data Dataframe
#' @param col_names Named list. Old and new column names.
#' @param var_activity Named list. Old and new activity type names.
#' @param var_param Named list. Old and new paramater names.
#' @param var_unit Named list. Old and new unit names.
#' @param var_qualifer Named list. Old and new qualifier names.
#'
#' @return Updated data frame that has been formatted for MassWateR
#'
#' @noRd
convert_results <- function(
    .data, col_names = NA, var_activity = NA, var_param = NA, var_unit = NA,
    var_qualifier = NA
  ) {
  if (shiny::isTruthy(col_names)) {
    dat <- .data |>
      wqformat::rename_col(col_names, names(col_names))
  }
  
  key_col <- c(
    "Monitoring Location ID", "Activity Type", "Activity Start Date",
    "Activity Start Time", "Activity Depth/Height Measure",
    "Activity Depth/Height Unit", "Activity Relative Depth Name",
    "Characteristic Name", "Result Value", "Result Unit",
    "Quantitation Limit", "Result Measure Qualifier", 
    "Sample Collection Method ID", "Project ID"
  )
  missing_col <- setdiff(all_col, colnames(dat))
  if (length(missing_col) > 1) {
    stop(
      "\tMissing ", ncol(missing_col), " mandatory columns: ", 
      paste(missing_col, sep = ", ")
    )
  }
  
  dat <- dat |>
    try_rename("Activity Type", var_activity) |>
    try_rename("Characteristic Name", var_param) |>
    try_rename("Result Unit", var_unit) |>
    try_rename("Result Measure Qualifier", var_qualifier) |>
    wqformat::format_mwr_results()
}