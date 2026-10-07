test_that("validationLog$parse_msg works - column error", {
  # Define var
  test_class <- dfRepair$new()

  test_dat <- tst$sitdat
  colnames(test_dat) <- c(
    "Site_ID", "Site_Name", "Latitude", "Longitude", "Group"
  )
  test_msg <- paste(
    "Checking column names... Please correct the column names or remove:",
    "Site_ID, Site_Name, Latitude, Longitude, Group"
  )

  test_class$parse_msg(test_msg, test_dat, "sitdat")

  # Test
  expect_equal(
    test_class$missing_col,
    c(
      "Monitoring Location ID", "Monitoring Location Name",
      "Monitoring Location Latitude", "Monitoring Location Longitude",
      "Location Group"
    )
  )

  expect_equal(
    test_class$locs,
    list(
      col_indices = numeric(),
      cell_map = list()
    )
  )

  expect_equal(
    test_class$df_col,
    data.frame(
      "Delete Column" = FALSE,
      "Invalid Column Name" = c(
        "Site_ID", "Site_Name", "Latitude", "Longitude", "Group"
      ),
      "New Column Name" = NA,
      check.names = FALSE
    )
  )

  expect_null(test_class$problem_rows)
  expect_null(test_class$df_var)
  expect_null(test_class$df_row)
})

test_that("validationLog$parse_msg works - repeat row error", {
  # Define var ----
  test_class <- dfRepair$new()

  test_dat <- rbind(tst$resdat, tst$resdat, tst$resdat)
  test_dat$`Activity Type`[2:11] <- c("Foo", "Bar")

  out_dat <- test_dat
  out_dat$ID <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  out_dat$bad_row <- c(
    FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE
    )

  test_msg <- paste(
    "Checking valid Activity Types... Incorrect Activity Type found:",
    "Foo, Bar in row(s) 2, 3, 4, 5, 6, 7, 8, 9, 10, 11"
  )

  test_class$parse_msg(test_msg, test_dat, "resdat")

  # Test ----
  expect_equal(
    test_class$problem_rows,
    c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
  )

  expect_equal(
    test_class$locs,
    list(
      col_indices = numeric(),
      cell_map = list(
        "Activity Type" = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
      )
    )
  )

  expect_equal(
    test_class$df_var,
    data.frame(
      "Invalid Activity Type" = c("Bar", "Foo"),
      "Replace With" = NA,
      "Row Count" = 5,
      check.names = FALSE
    )
  )

  expect_equal(
    test_class$df_row,
    out_dat
  )

  expect_null(test_class$missing_col)
  expect_null(test_class$df_col)
})

test_that("validationLog$edit_row works", {
  # Define var ----
  test_class <- dfRepair$new()

  test_dat <- tst$resdat
  test_dat$`Activity Type`[2] <- "Grab"
  test_dat$ID <- c(1,2,3,4)
  test_dat$bad_row <- c(FALSE, TRUE, FALSE, FALSE)
  
  out_dat <- tst$resdat
  out_dat$ID <- c(1,2,3,4)
  out_dat$bad_row <- c(FALSE, TRUE, FALSE, FALSE)

  # Test 1 ----
  test_class$df_row <- test_dat
  test_class$edit_row(
    val = list(
      row = 2,
      column = "Activity Type",
      value = "Sample-Routine"
    ),
    show_all = TRUE
  )

  expect_equal(
    test_class$df_row,
    out_dat
  )
  
  # Test 2 ----
  test_class$df_row <- test_dat
  test_class$edit_row(
    val = list(
      row = 1,
      column = "Activity Type",
      value = "Sample-Routine"
    ),
    show_all = FALSE
  )
  
  expect_equal(
    test_class$df_row,
    out_dat
  )

})
