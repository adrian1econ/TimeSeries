library(testthat)

test_that("Test of Output", {
  expect_error(plot_periodogram(c("a","b")), "pd must be numeric vector.")
  expect_error(plot_periodogram(NA), "pd must be numeric vector.")
  expect_error(plot_periodogram(list(1,2,3)), "pd must be numeric vector.")
})
