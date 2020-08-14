library(testthat)
test_that("Input test work", {
  expect_error(innovation(c("a","b")), "ts must be numeric vector or object of class arma!")
  expect_error(innovation(NA), "ts must be numeric vector or object of class arma!")
  expect_error(innovation(list(2,3,4)), "ts must be numeric vector or object of class arma!")
  expect_error(innovation(c(1)), "length of ts must be greater than 1")
  expect_error(innovation(c(1,1,1)), "Variance cant be zero")
  expect_error(innovation(c(1,2,6,3,5,3,2), lag.max = 1.1), "lag.max must be integer or NA!")
  expect_error(innovation(c(1,2,6,3,5,3,2), lag.max = c(1,1)), "length of lag.max must equal 1")
  expect_error(innovation(c(1,2,6,3,5,3,2), lag.max ="a"), "lag.max must be integer or NA!")
  })
test_that("Innovation works", {
  vec <- c(1,3,0.5)
  acf_vec <- acf(vec)
  v0 <- acf_vec[1]
  theta11 <- acf_vec[2]/v0
  v1 <- v0-theta11*theta11*v0
  theta22 <- (acf_vec[3])/v0
  theta21 <- (acf_vec[2]-theta11*theta22*v0)/v1
  x_next <- 0
  x_next[2] <- theta11*vec[1]
  x_next[3] <- theta21*(vec[2]-x_next[2])+theta22*vec[1]
  expect_equal(innovation(vec), x_next)
})
