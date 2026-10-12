test_that("is_column_error works", {
  expect_true(
    is_column_error("correct the column names")
  )
  expect_true(
    is_column_error("Missing the following columns")
  )
  expect_false(
    is_column_error("hello this is an error message")
  )
  expect_false(
    is_column_error(" ")
  )
})

test_that("parse_problem_rows works", {
  expect_equal(
    parse_problem_rows("row(s) 41, 42, 52, 53, 2"),
    c(41, 42, 52, 53)
  )
  expect_equal(
    parse_problem_rows("row(s): 5, 7, 4"),
    c(4, 5, 7)
  )
  expect_equal(
    parse_problem_rows("column(s) 5, 7, 4"),
    integer(0)
  )
  expect_equal(
    parse_problem_rows(" "),
    integer(0)
  )
})

test_that("parse_error_locations works", {
  # Pattern B: no column prefix pairing, "row(s): N, M" (colon variant), two
  # columns named in the same line - e.g. checkMWRresults' depth data check
  msg <- paste0(
    "\tChecking depth data present...\n",
    "\tNo data in Activity Depth/Height Measure and Activity Relative ",
    "Depth Name on row(s): 8, 9"
  )
  col_names <- c(
    "Activity Depth/Height Measure", "Activity Relative Depth Name"
  )
  locs <- parse_error_locations(msg, col_names)
  expect_equal(locs$col_indices, integer(0))
  expect_equal(locs$cell_map[["Activity Depth/Height Measure"]], c(8, 9))
  expect_equal(locs$cell_map[["Activity Relative Depth Name"]], c(8, 9))

  # Pattern B without colon
  msg <- "\tIncorrect Activity Type found: bad in row(s) 3, 4"
  locs <- parse_error_locations(msg, "Activity Type")
  expect_equal(locs$cell_map[["Activity Type"]], c(3, 4))

  # Pattern A: "ColName (row(s) N, M)" explicit pairing
  msg <- "Parameter (row(s) 1, 2)"
  locs <- parse_error_locations(msg)
  expect_equal(locs$cell_map[["Parameter"]], c(1, 2))

  # column index flagged via "(column N)"
  msg <- "bad header (column 3)"
  locs <- parse_error_locations(msg)
  expect_equal(locs$col_indices, 3)

  # no error message
  expect_equal(
    parse_error_locations(" "),
    list(col_indices = integer(0), cell_map = list())
  )
})
#
#
# test_that("handle_retry works", {
# })

test_that("parse_repeat_errors works", {
  # No errors
  locs <- list(
    col_indices = NULL,
    cell_map = NULL
  )

  expect_equal(
    parse_repeat_errors(tst$resdat, locs),
    NULL
  )

  # Some errors
  df_res <- rbind(tst$resdat, tst$resdat, tst$resdat)
  df_res[["Activity Type"]] <- c("foo", "bar", "foofy")

  locs <- list(
    col_indices = NULL,
    cell_map = list(
      "Activity Type" = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    )
  )

  expect_equal(
    parse_repeat_errors(df_res, locs),
    NULL
  )

  # Many errors
  df_res[["Activity Type"]] <- c(
    "foo", "foo", "foo", "foo", "foo", "bar", "bar", "foofy", "bar", "bar",
    "Sample-Routine", "Sample-Routine"
  )

  locs <- list(
    col_indices = NULL,
    cell_map = list(
      "Activity Type" = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    )
  )
  
  df_out <- data.frame(
    "Invalid Activity Type" = c("bar", "foo"),
    "Replace With" = NA,
    "Row Count" = c(4,5),
    check.names = FALSE
  )

  expect_equal(parse_repeat_errors(df_res, locs), df_out)
})

test_that("update_hot_col works", {
  # Set variables
  df_bad <- tst$sitdat
  colnames(df_bad) <- c(
    "Site_ID", "Site_Name", "Latitude", "Longitude", "Group"
  )
  df_bad$Foo <- "bar"

  df_react <- data.frame(
    "Delete Column" = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
    "Invalid Column Name" = c(
      "Site_ID", "Site_Name", "Latitude", "Longitude", "Group", "Foo"
    ),
    "New Column Name" = c(
      "Monitoring Location ID", "Monitoring Location Name", 
      "Monitoring Location Latitude", "Monitoring Location Longitude", 
      "Location Group", " "),
    check.names = FALSE
  )

  # Test
  expect_equal(
    update_hot_col(df_react, df_bad),
    tst$sitdat
  )
})

test_that("update_hot_var works", {
  # Set variables
  df_bad <- tst$resdat
  df_bad[["Activity Type"]] <- c("Field Msr/Obs", "foo", "bar", "foofy")

  df_hot <- data.frame(
    "Invalid Activity Type" = c("bar", "foo", "foofy"),
    "Replace With" = c(
      "Quality Control Sample-Lab Duplicate", "Sample-Routine",
      "Quality Control-Calibration Check"
    ),
    "Row Count" = 1,
    check.names = FALSE
  )

  # Test
  expect_equal(
    update_hot_var(df_hot, df_bad),
    tst$resdat
  )
})

test_that("update_hot_rows works", {
  # Set variables
  df_in <- tst$resdat
  df_in[5, ] <- NA
  df_in$ID <- c(1,2,3,4,5)
  df_in$bad_row <- c(FALSE, FALSE, FALSE, FALSE, TRUE)

  # Test
  expect_equal(
    update_hot_rows(df_in),
    tst$resdat
  )
})
