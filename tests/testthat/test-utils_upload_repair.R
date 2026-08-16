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

  expect_snapshot(parse_repeat_errors(df_res, locs))
})
