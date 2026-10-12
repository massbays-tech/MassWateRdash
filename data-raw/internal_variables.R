# Distinct columns unique to each file type - used to detect wrong-file uploads
file_signatures <- list(
  resdat = c("Activity Type", "Characteristic Name", "Result Value"),
  accdat = c("Value Range", "MDL"),
  frecomdat = c("% Completeness"),
  sitdat = c("Monitoring Location Latitude", "Monitoring Location Longitude"),
  wqxdat = c("Sampling Method Context", "Analytical Method Context"),
  censdat = c("Parameter", "Missed and Censored Records")
)

file_labels <- c(
  resdat = "Results data",
  accdat = "DQO Accuracy data",
  frecomdat = "DQO Frequency & Completeness data",
  sitdat = "Site data",
  wqxdat = "WQX metadata",
  censdat = "Censored data"
)

file_columns <- list(
  resdat = c(
    "Monitoring Location ID", "Activity Type", "Activity Start Date",
    "Activity Start Time", "Activity Depth/Height Measure",
    "Activity Depth/Height Unit", "Activity Relative Depth Name",
    "Characteristic Name", "Result Value", "Result Unit", "Quantitation Limit",
    "QC Reference Value	Result Measure Qualifier", "Result Attribute",
    "Sample Collection Method ID", "Project ID", "Local Record ID",
    "Result Comment"
  ),
  accdat = c(
    "Parameter", "uom", "MDL", "UQL", "Value Range", "Field Duplicate",
    "Lab Duplicate", "Field Blank", "Lab Blank", "Spike/Check Accuracy"
  ),
  frecomdat = c(
    "Parameter", "Field Duplicate", "Lab Duplicate", "Field Blank", "Lab Blank",
    "Spike/Check Accuracy", "% Completeness"
  ),
  sitdat = c(
    "Monitoring Location ID", "Monitoring Location Name",
    "Monitoring Location Latitude", "Monitoring Location Longitude",
    "Location Group"
  ),
  wqxdat = c(
    "Parameter", "Sampling Method Context", "Method Speciation",
    "Result Sample Fraction", "Analytical Method", "Analytical Method Context"
  ),
  censdat = c("Parameter", "Missed and Censored Records")
)

# Accepted variables
mwr_activity <- c(
  "Field Msr/Obs", "Sample-Routine", "Quality Control Sample-Field Blank",
  "Quality Control Sample-Lab Duplicate", "Quality Control Sample-Lab Blank",
  "Quality Control Sample-Lab Spike", "Quality Control-Meter Lab Duplicate",
  "Quality Control-Meter Lab Blank", "Quality Control-Calibration Check"
)
mwr_param <- c(
  "Air Temp", "Algae, blue-green (phylum cyanophyta) density", "Ammonia",
  "Ammonium", "Chl a", "Chl a (probe)", "Chloride", "Chlorophyll a",
  "Chlorophyll a (probe)",
  "Chlorophyll a (probe) concentration, Cyanobacteria (bluegreen)",
  "Conductivity", "Cyanobacteria", "Cyanobacteria (probe)", "Depth",
  "Depth, Secchi disk depth", "Dissolved oxygen (DO)",
  "Dissolved oxygen saturation", "DO", "DO saturation", "E.coli",
  "Enterococcus", "Escherichia coli", "Fecal Coliform", "Flow", "Gage",
  "Height, gage", "Metals", "Microcystins", "Nitrate",
  "Nitrate + Nitrite", "Nitrite", "Ortho P", "Orthophosphate",
  "Particulate organic carbon", "pH", "Pheophytin", "Pheophytin a",
  "Phosphorus, Particulate Organic", "Phycocyanin",
  "Phycocyanin (probe)", "Phycoerythrin", "POC", "PON",
  "POP", "Salinity", "Secchi Depth", "Silicate", "Sp Conductance",
  "Specific conductance", "Sulfate", "Surfactants", "TDN", "TDP", "TDS",
  "Temperature, air", "Temperature, water", "TKN", "TN",
  "Total dissolved solids", "Total Kjeldahl nitrogen",
  "Total Nitrogen, mixed forms", "Total Phosphorus, mixed forms",
  "Total suspended solids", "TP", "TSS", "Turbidity", "Water Temp"
)
mwr_unit <- c(
  "#/100ml", "%", "% recovery", "AU", "BU", "cfm", "cfs", "cfu/100ml", "cm",
  "deg C", "deg F", "FAU", "FBU", "FNMU", "FNRU", "FNU", "ft", "FTU",
  "g/kg", "JTU", "l/min", "l/sec", "m", "mg/l", "mgd", "MPN/100ml", "mS/cm",
  "None", "NTMU", "NTRU", "NTU", "ppm", "ppt", "ppth", "PSS", "PSU", "RFU",
  "s.u.", "S/m", "ug/l", "umol/l", "uS/cm"
)


usethis::use_data(
  file_signatures,
  file_labels,
  file_columns,
  mwr_activity,
  mwr_param,
  mwr_unit,
  overwrite = TRUE,
  internal = TRUE
)
