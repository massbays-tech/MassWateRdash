test_that("try_rename works", {
  df_in <- data.frame(
    Param = c("DO", "foo", "DO sat", "DO")
  )

  df_out <- data.frame(
    Param = c(
      "Dissolved oxygen (DO)", "foo", "Dissolved oxygen saturation",
      "Dissolved oxygen (DO)"
    )
  )

  var_names <- c("DO", "DO sat")
  names(var_names) <- c("Dissolved oxygen (DO)", "Dissolved oxygen saturation")

  expect_equal(
    try_rename(df_in, "Param", var_names),
    df_out
  )

  # Test edge case
  expect_equal(
    try_rename(df_in, "Param", NULL),
    df_in
  )
})

test_that("convert_results works", {
  df_in <- data.frame(
    Site_ID = c("HBS-016", "HBS-016", NA, NA),
    Activity_Type = c(
      "Field Msr/Obs", "Sample-Routine", "Lab Duplicate", "Calibration Check"
    ),
    Date = as.Date(c("2021-06-13", "2021-08-15", "2021-05-16", "2021-09-12")),
    Time = c("8:00", "7:40", NA, NA),
    Depth = c(1, 0.75, NA, NA),
    Depth_Unit = c("ft", "ft", NA, NA),
    Parameter = c(
      "Dissolved oxygen saturation", "Total suspended solids", "Nitrate",
      "Specific conductance"
    ),
    Result = c(46.8, 5, 0.45, 980),
    Result_Unit = c("%", "mg/L", "mg/L", "uS/cm"),
    Quantitation_Limit = NA,
    QC_Reference_Value = c(7, NA, 0.46, 1000),
    Qualifier = c(NA, "Q", NA, NA),
    Result_Attribute = c(NA, NA, "K16452-MB3", NA),
    Method_ID = c(NA, "Grab-MassWateR", NA, NA),
    Project_ID = "Water Quality",
    Comment = c(NA, "River was very full", NA, NA)
  )

  df_out <- data.frame(
    "Monitoring Location ID" = c("HBS-016", "HBS-016", NA, NA),
    "Activity Type" = c(
      "Field Msr/Obs", "Sample-Routine", "Quality Control Sample-Lab Duplicate",
      "Quality Control-Calibration Check"
    ),
    "Activity Start Date" = as.Date(
      c("2021-06-13", "2021-08-15", "2021-05-16", "2021-09-12")
    ),
    "Activity Start Time" = c("8:00", "7:40", NA, NA),
    "Activity Depth/Height Measure" = c(1, 0.75, NA, NA),
    "Activity Depth/Height Unit" = c("ft", "ft", NA, NA),
    "Activity Relative Depth Name" = NA,
    "Characteristic Name" = c(
      "DO saturation", "TSS", "Nitrate", "Sp Conductance"
    ),
    "Result Value" = c(46.8, 5, 0.45, 980),
    "Result Unit" = c("%", "mg/l", "mg/l", "uS/cm"),
    "Quantitation Limit" = NA,
    "QC Reference Value" = c(7, NA, 0.46, 1000),
    "Result Measure Qualifier" = c(NA, "Q", NA, NA),
    "Result Attribute" = c(NA, NA, "K16452-MB3", NA),
    "Sample Collection Method ID" = c(NA, "Grab-MassWateR", NA, NA),
    "Project ID" = "Water Quality",
    "Local Record ID" = NA,
    "Result Comment" = c(NA, "River was very full", NA, NA),
    check.names = FALSE
  )

  col_names <- c(
    "Site_ID", "Activity_Type", "Date", "Time", "Depth", "Depth_Unit",
    "Parameter", "Result", "Result_Unit", "Quantitation_Limit",
    "QC_Reference_Value", "Qualifier", "Result_Attribute", "Method_ID",
    "Project_ID", "Comment"
  )
  names(col_names) <- c(
    "Monitoring Location ID", "Activity Type", "Activity Start Date",
    "Activity Start Time", "Activity Depth/Height Measure",
    "Activity Depth/Height Unit", "Characteristic Name", "Result Value",
    "Result Unit", "Quantitation Limit", "QC Reference Value",
    "Result Measure Qualifier", "Result Attribute",
    "Sample Collection Method ID", "Project ID", "Result Comment"
  )

  var_activity <- c("Lab Duplicate", "Calibration Check")
  names(var_activity) <- c(
    "Quality Control Sample-Lab Duplicate", "Quality Control-Calibration Check"
  )
  var_param <- c(
    "Dissolved oxygen saturation", "Total suspended solids",
    "Specific conductance"
  )
  names(var_param) <- c("DO saturation", "TSS", "Sp Conductance")
  var_unit <- "mg/L"
  names(var_unit) <- "mg/l"
  var_qualifier <- NULL

  expect_equal(
    suppressMessages(
      convert_results(
        df_in, col_names, var_activity, var_param, var_unit, var_qualifier
      )
    ),
    df_out
  )
})
