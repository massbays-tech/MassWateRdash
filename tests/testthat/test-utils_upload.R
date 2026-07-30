test_that("fl_upload works", {
  # Define var
  val_log <- validationLog$new()
  val_edit <- editVisible$new()
  val_dat <- sitdatClass$new()
  df_sitdat <- system.file(
    "extdata",
    "ExampleSites.xlsx",
    package = "MassWateR"
  )

  # Test
  fl_upload(
    file = df_sitdat,
    read_function = readMWRsites,
    data_name = "sitdat",
    val_log = val_log,
    val_edit = val_edit,
    val_dat = val_dat
  )

  expect_equal(
    val_log$msg,
    paste0(
      "Running checks on site metadata...\n\n\tChecking column names... OK",
      "\n\tChecking all required columns are present... OK",
      "\n\tChecking for missing latitude or longitude values... OK",
      "\n\tChecking for non-numeric values in latitude... OK",
      "\n\tChecking for non-numeric values in longitude... OK",
      "\n\tChecking for positive values in longitude... OK",
      "\n\tChecking for missing entries for Monitoring Location ID... OK",
      "\n\nAll checks passed!"
    )
  )
  expect_equal(val_log$msg, val_dat$msg)
  expect_false(val_edit$resdat)
  expect_false(val_edit$accdat)
  expect_false(val_edit$frecomdat)
  expect_false(val_edit$sitdat)
  expect_false(val_edit$wqxdat)
  expect_false(val_edit$censdat)
  expect_equal(data.frame(val_dat$dat, check.names = FALSE), tst$sitdat)
  expect_equal(val_dat$raw_dat, NULL)
})


test_that("from_format_upload works", {
  # Define var
  val_log <- validationLog$new()
  val_edit <- editVisible$new()
  val_dat <- sitdatClass$new()

  # Test
  from_format_upload(
    df = tst$sitdat,
    retry_fn = retry_fns$sitdat,
    data_name = "sitdat",
    val_log = val_log,
    val_edit = val_edit,
    val_dat = val_dat
  )

  expect_equal(
    val_log$msg,
    paste0(
      "Running checks on site metadata...\n\n\tChecking column names... OK",
      "\n\tChecking all required columns are present... OK",
      "\n\tChecking for missing latitude or longitude values... OK",
      "\n\tChecking for non-numeric values in latitude... OK",
      "\n\tChecking for non-numeric values in longitude... OK",
      "\n\tChecking for positive values in longitude... OK",
      "\n\tChecking for missing entries for Monitoring Location ID... OK",
      "\n\nAll checks passed!"
    )
  )
  expect_equal(val_log$msg, val_dat$msg)
  expect_false(val_edit$sitdat)
  expect_equal(val_dat$dat, tst$sitdat)
  expect_equal(val_dat$raw_dat, NULL)
})

test_that("detect_wrong_file works", {
  dat_results <- data.frame(
    "Activity Type" = NA,
    "Characteristic Name" = NA,
    "Result Value" = NA,
    check.names = FALSE
  )

  dat_nonsense <- data.frame(
    "foo" = NA,
    "bar" = NA
  )

  expect_null(
    detect_wrong_file(raw_df = NULL, data_name = "resdat")
  )
  expect_null(
    detect_wrong_file(raw_df = dat_results, data_name = "resdat")
  )
  expect_equal(
    detect_wrong_file(raw_df = dat_nonsense, data_name = "resdat"),
    "Error: Did you upload the wrong file? The column names do not match the expected format."
  )
  expect_equal(
    detect_wrong_file(raw_df = dat_results, data_name = "sitdat"),
    "Error: Did you upload the wrong file? This looks like it may be Results data."
  )
})

test_that("fl_status works", {
  expect_equal(
    fl_status(TRUE, NULL, NULL),
    HTML("<span style='color:#00A4CF'>Using test data</span>")
  )
  expect_equal(
    fl_status(FALSE, NULL, NULL),
    HTML("No file uploaded")
  )
  expect_equal(
    fl_status(FALSE, "foo", NULL),
    HTML("<span style='color:#f54242'>Error loading</span>")
  )
  expect_equal(
    fl_status(FALSE, NULL, "bar"),
    HTML("<span style='color:#64C147'>Loaded from format converter</span>")
  )
  expect_equal(
    fl_status(FALSE, "foo", "bar"),
    HTML("<span style='color:#64C147'>Data loaded</span>")
  )
})

test_that("format_log works", {
  expect_equal(
    format_log("foo\nbar"),
    div(HTML("foo<br>bar"))
  )

  # Check R6
  val_log <- validationLog$new()
  val_log$msg <- "foo\nbar"

  expect_equal(
    format_log(val_log$msg),
    div(HTML("foo<br>bar"))
  )
})
