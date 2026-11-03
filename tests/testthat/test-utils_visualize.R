test_that("parse_custom_thresh() parses semicolon entries", {
  out <- parse_custom_thresh("5; 8.5", " Low ;High")
  expect_null(out$error)
  expect_equal(out$thresh$thresh, c(5, 8.5))
  expect_equal(out$thresh$label, c("Low", "High"))

  # trailing semicolon is ignored
  out <- parse_custom_thresh("5;", "Low;")
  expect_null(out$error)
  expect_equal(nrow(out$thresh), 1)
})

test_that("parse_custom_thresh() returns NULL for blank entries", {
  out <- parse_custom_thresh("", "  ")
  expect_null(out$error)
  expect_null(out$thresh)

  out <- parse_custom_thresh(NULL, NULL)
  expect_null(out$error)
  expect_null(out$thresh)
})

test_that("parse_custom_thresh() catches invalid entries", {
  expect_match(parse_custom_thresh("5; 8", "Low")$error, "2 value\\(s\\) and 1 label")
  expect_match(parse_custom_thresh("5", "")$error, "1 value\\(s\\) and 0 label")
  expect_match(parse_custom_thresh("5; abc", "Low; High")$error, "abc")
  expect_match(parse_custom_thresh("5;; 8", "a; b; c")$error, "empty")
  expect_match(parse_custom_thresh("5; 8", "Low; Low")$error, "unique")
  expect_null(parse_custom_thresh("5; abc", "Low")$thresh)
})

test_that("add_custom_thresh() styles lines and keeps data on top", {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_point()

  expect_identical(add_custom_thresh(p, NULL), p)

  thresh <- data.frame(thresh = 1:5, label = letters[1:5])
  out <- add_custom_thresh(p, thresh)
  expect_s3_class(out$layers[[1]]$geom, "GeomHline")
  expect_s3_class(out$layers[[2]]$geom, "GeomPoint")

  lyr <- ggplot2::layer_data(out, 1)
  expect_equal(lyr$linetype, c("dashed", "dotted", "dashed", "dotted", "dashed"))
  expect_equal(lyr$colour, c("tan", "tan", "#0072B2", "#0072B2", "#D55E00"))
})
