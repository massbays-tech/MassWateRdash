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
