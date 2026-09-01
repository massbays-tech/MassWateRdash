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
    parse_problem_rows("row(s) 5, 7, 4"),
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

# test_that("parse_error_locations works", {
# })
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
    "Delete Rows" = FALSE,
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
    "Delete Rows" = c(FALSE, FALSE, TRUE),
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
    tst$resdat[1:3, ]
  )
})

test_that("update_hot_rows works", {
  # Set variables
  df_bad <- tst$resdat
  df_bad[["Activity Type"]] <- c("Field Msr/Obs", "foo", "bar", "foofy")

  df_hot <- tst$resdat

  # Test
  expect_equal(
    update_hot_rows(df_hot, df_bad),
    tst$resdat
  )

  # Test - only show problem rows, one row blank
  df_hot[4, ] <- NA

  expect_equal(
    update_hot_rows(df_hot[2:4, ], df_bad, FALSE, c(2,3,4)),
    tst$resdat[1:3, ]
  )
})
