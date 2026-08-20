tst <- list(
  # MassWateR site data
  sitdat = data.frame(
    "Monitoring Location ID" = c(
      "ABT-026", "ABT-062", "ABT-077", "ABT-144", "ABT-162", "ABT-237",
      "ABT-301", "ABT-312", "DAN-013", "ELZ-004", "HOP-011", "NSH-002"
    ),
    "Monitoring Location Name" = c(
      "Rte 2, Concord", "Rte 62, Acton", "Rte 27/USGS, Maynard", "Rte 62, Stow",
      "Cox Street bridge", "Robin Hill Rd, Marlboro", "Rte 9, Westboro",
      "Mill Road, Westboro", "Danforth Br, Hudson", "Elizabeth Br, Stow",
      "Hop Br, Northboro", "Nashoba, Commonwealth, W. Concord"
    ),
    "Monitoring Location Latitude" = c(
      42.465938, 42.440765, 42.432356, 42.404519, 42.399797, 42.346645,
      42.283169, 42.269452, 42.40383, 42.426642, 42.292157, 42.4589660
    ),
    "Monitoring Location Longitude" = c(
      -71.391128, -71.429409, -71.449407, -71.526349, -71.545985, -71.614691,
      -71.638509, -71.633209, -71.574769, -71.485172, -71.657337, -71.3971660
    ),
    "Location Group" = c(
      "Assabet", "Assabet", "Assabet", "Assabet", "Assabet", "Assabet",
      "Assabet", "Assabet", "Tributaries", "Tributaries", "Tributaries",
      "Tributaries"
    ),
    check.names = FALSE
  ),
  # MassWateR result data
  resdat = data.frame(
    "Monitoring Location ID" = c("HBS-016", "HBS-016", NA, NA),
    "Activity Type" = c(
      "Field Msr/Obs", "Sample-Routine", "Quality Control Sample-Lab Duplicate",
      "Quality Control-Calibration Check"
    ),
    "Activity Start Date" = as.Date(c(
      "2021-06-13", "2021-08-15", "2021-05-16", "2021-09-12"
    )),
    "Activity Start Time" = c("8:00", "7:40", NA, NA),
    "Activity Depth/Height Measure" = c(1, 0.75, NA, NA),
    "Activity Depth/Height Unit" = c("ft", "ft", NA, NA),
    "Activity Relative Depth Name" = NA,
    "Characteristic Name" = c(
      "DO saturation",
      "TSS",
      "Nitrate",
      "Sp Conductance"
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
)
