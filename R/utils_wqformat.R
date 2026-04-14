#' Upload custom format
#'
#' @description `custom_format()` uploads a sheet from the custom conversion 
#' excel table and produces a named list containing variables to convert. 
#'
#' @param .data Input excel file
#' @param sheet_name Sheet to read. Either a string (the name of a sheet), or an 
#' integer (the position of the sheet).
#'
#' @return Named list of variable names. List is composed of old variables, with
#' the new variables set as names. If no valid entries found, returns `NULL`.
#'
#' @noRd
custom_format <- function(.data, sheet_name) {
  dat <- readxl::read_excel(
    .data,
    sheet = sheet_name,
    col_types = "text",
    na = c("NA", "na", "")
  ) |>
    dplyr::select("MassWateR", "Custom") |>
    dplyr::filter(
      !is.na(.data$MassWateR),
      !is.na(.data$Custom)
    )
  
  if (nrow(dat) == 0) {
    return(NULL)
  }
  
  out <- dat$Custom
  names(out) <- dat$MassWateR
  
  return(out)
}


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
  if (is.null(var_names)) {
    return(.data)
  }

  new_var <- names(var_names)
  old_var <- unname(var_names)

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
  .data, col_names = NULL, var_activity = NULL, var_param = NULL,
  var_unit = NULL, var_qualifier = NULL
) {
  if (!is.null(col_names)) {
    .data <- .data |>
      wqformat::rename_col(col_names, names(col_names))
  }
  dat <- .data

  all_col <- c(
    "Monitoring Location ID", "Activity Type", "Activity Start Date",
    "Activity Start Time", "Activity Depth/Height Measure",
    "Activity Depth/Height Unit", "Activity Relative Depth Name",
    "Characteristic Name", "Result Value", "Result Unit",
    "Quantitation Limit", "QC Reference Value", "Result Measure Qualifier",
    "Result Attribute", "Sample Collection Method ID", "Project ID",
    "Local Record ID", "Result Comment"
  )

  missing_col <- setdiff(all_col, colnames(dat))
  if (length(missing_col) > 0) {
    dat[missing_col] <- NA
    message("\tAdded ", length(missing_col), " missing columns")
  }

  dat <- dat |>
    try_rename("Activity Type", var_activity) |>
    try_rename("Characteristic Name", var_param) |>
    try_rename("Result Unit", var_unit) |>
    try_rename("Result Measure Qualifier", var_qualifier) |>
    wqformat::format_mwr_results()
}
