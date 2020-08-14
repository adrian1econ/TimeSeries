test_that("Input test work", {
        expect_error(TimeSeries::acf(c("a","b")), "ts must be numeric vector or object of class arma!")
        expect_error(TimeSeries::acf(NA), "ts must be numeric vector or object of class arma!")
        expect_error(TimeSeries::acf(NULL), "ts must be numeric vector or object of class arma!")

        expect_error(TimeSeries::acf(c(1,2,6,3,5,3,2), lag.max=1.1), "lag.max must be integer or NULL!")
        expect_error(TimeSeries::acf(c(1,2,6,3,5,3,2), lag.max=c(1,1)), "lag.max must be integer or NULL!")
        expect_error(TimeSeries::acf(c(1,2,6,3,5,3,2), lag.max=NA), "lag.max must be integer or NULL!")
        expect_error(TimeSeries::acf(c(1,2,6,3,5,3,2), lag.max="a"), "lag.max must be integer or NULL!")
        expect_error(TimeSeries::acf(c(1,2,6,3,5,3,2), lag.max=10), "lag.max must be between 0 and length\\(ts\\)-1")
        expect_error(TimeSeries::acf(c(1)), "length of ts must be greater than 1")

})

test_that("acf works", {
        vec <- arma_sim(phi=0.5, n=10, burnin = 1000)$arma
        expect_equal(acf(vec)[1], mean((vec-mean(vec))^2) )
        expect_equal(acf(vec, lag.max = 0), mean((vec-mean(vec))^2) )
        expect_equal(acf(vec),stats::acf(vec, type = "covariance")$acf[1:10,,])
})


