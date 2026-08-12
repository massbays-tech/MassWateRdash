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
  expect_equal(
    parse_repeat_errors(tst$resdat),
    NULL
  )
  
  # Some errors
  df_res <- rbind(tst$resdat, tst$resdat, tst$resdat)
  df_res[["Activity Type"]] <- c("foo", "bar", "foofy")
  
  expect_equal(
    parse_repeat_errors(df_res, "Activity Type", c(1,2,3,4,5,6,7,8,9,10,11,12)),
    NULL
  )
  
  # Many errors
  df_res <- rbind(tst$resdat, tst$resdat, tst$resdat)
  df_res[["Activity Type"]] <- c("foo", "foo", "bar")

  expect_equal(
    parse_repeat_errors(df_res, "Activity Type", c(1,2,3,4,5,6,7,8,9,10,11,12)),
    data.frame(
      "Delete" = FALSE,
      "Invalid Activity Type" = c("bar", "foo"),
      "Replace With" = NA,
      "Row Count" = c(4, 8),
      check.names = FALSE
    )
  )
})
