test_that("validationLog default values", {
  test_log <- validationLog$new()

  expect_equal(
    c(
      test_log$msg,
      test_log$result,
      test_log$dat,
      test_log$raw_dat,
      test_log$edit_dat
    ),
    c("", NULL, NULL, NULL, NULL, "")
  )
})


test_that("validationLog$msg works", {
  test_log <- validationLog$new()
  test_log$msg <- "foo"

  expect_equal(test_log$msg, "foo")
})


test_that("validationLog$catch_msg works", {
  # Define var
  test_log <- validationLog$new()

  test_fun <- function() {
    message("This is a message")

    "This is the result"
  }

  # Test - run catch_msg
  test_log$catch_msg(test_fun())

  expect_equal(
    c(test_log$msg, test_log$result),
    c("This is a message", "This is the result")
  )

  # Test - run catch_msg again
  test_log$catch_msg(test_fun())

  expect_equal(
    c(test_log$msg, test_log$result),
    c("This is a message\nThis is a message", "This is the result")
  )
})
