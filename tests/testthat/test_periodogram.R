library(testthat)

test_that("Test of Errors", {
  expect_error(periodogram(c("a","b")), "ts must be numeric vector or object of class arma.")
  expect_error(periodogram(NA), "ts must be numeric vector or object of class arma.")
  expect_error(periodogram(list(1,2,3)), "ts must be numeric vector or object of class arma.")
  })

test_that("Periodogram works", {
  x <-  c(1,2)
  n <- 2                
  I <- double(2)
  F_n <- c(0,1)
  
  sum1 <- x[1]*exp(as.complex(-1i)*1*(2*pi*0)/n)+x[2]*exp(as.complex(-1i)*2*(2*pi*0)/n)
  sum2 <- x[1]*exp(as.complex(-1i)*1*(2*pi*1)/n)+x[2]*exp(as.complex(-1i)*2*(2*pi*1)/n)
  
  x_peri <- double(0)
  x_peri[1] <- 1/2 * abs(sum1)^2
  x_peri[2] <- 1/2 * abs(sum2)^2

  expect_equal(periodogram(x), x_peri)
})
