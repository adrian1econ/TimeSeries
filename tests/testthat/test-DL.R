test_that("Input test work", {
        expect_error(DL(c("a","b")), "ts must be numeric vector or object of class arma!")
        expect_error(DL(NA), "ts must be numeric vector or object of class arma!")
        expect_error(DL(NULL), "ts must be numeric vector or object of class arma!")

        expect_error(DL(c(1,2,6,3,5,3,2), p=1.1), "p must be NULL or an integer >= 2")
        expect_error(DL(c(1,2,6,3,5,3,2), p=c(1,1)), "p must be NULL or an integer >= 2")
        expect_error(DL(c(1,2,6,3,5,3,2), p=NA), "p must be NULL or an integer >= 2")
        expect_error(DL(c(1,2,6,3,5,3,2), p="a"), "p must be NULL or an integer >= 2")
        expect_error(DL(c(1,2,6,3,5,3,2), p=1), "p must be NULL or an integer >= 2")
})

test_that("DL works", {
        vec <- c(1,3,0.5)
        gamma <- acf(vec)
        phi_11 <- gamma[2]/gamma[1]
        v_1 <- gamma[1]*(1-phi_11^2)
        phi_22 <- (gamma[3]-phi_11*gamma[2])/v_1
        phi_21 <- phi_11-phi_22*phi_11
        res <- c(phi_22, phi_21)

        expect_equal(DL(vec), res)
})
