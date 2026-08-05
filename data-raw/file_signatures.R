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

usethis::use_data(
  file_signatures,
  file_labels,
  overwrite = TRUE,
  internal = TRUE
)
