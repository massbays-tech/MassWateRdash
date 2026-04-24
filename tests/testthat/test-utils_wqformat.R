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

  var_names <- c(
    `Dissolved oxygen (DO)` = "DO", `Dissolved oxygen saturation` = "DO sat"
  )

  expect_equal(
    suppressMessages(
      try_rename(df_in, "Param", var_names)
    ),
    df_out
  )

  # Test edge case
  expect_equal(
    suppressMessages(
      try_rename(df_in, "Param", NULL)
    ),
    df_in
  )
})

test_that("format_custom_results works", {
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
  
  all_var <- list(
    col_name = c(
      `Monitoring Location ID` = "Site_ID", 
      `Activity Type` = "Activity_Type", 
      `Activity Start Date` = "Date", 
      `Activity Start Time` = "Time", 
      `Activity Depth/Height Measure` = "Depth", 
      `Activity Depth/Height Unit` = "Depth_Unit",
      `Characteristic Name` = "Parameter", 
      `Result Value` = "Result", 
      `Result Unit` = "Result_Unit", 
      `Quantitation Limit` = "Quantitation_Limit",
      `QC Reference Value` = "QC_Reference_Value", 
      `Result Measure Qualifier` = "Qualifier", 
      `Result Attribute` = "Result_Attribute", 
      `Sample Collection Method ID` = "Method_ID",
      `Project ID` = "Project_ID", 
      `Result Comment` = "Comment"
    ),
    param = c(
      `DO saturation` = "Dissolved oxygen saturation", 
      TSS = "Total suspended solids",
      `Sp Conductance` = "Specific conductance"
    ),
    param_unit = c(`mg/l` = "mg/L"),
    qualifier = NULL,
    activity = c(
      `Quality Control Sample-Lab Duplicate` = "Lab Duplicate", 
      `Quality Control-Calibration Check` = "Calibration Check"
    )
  )

  expect_equal(
    suppressMessages(
      format_custom_results(df_in, all_var)
    ),
    df_out
  )
})
