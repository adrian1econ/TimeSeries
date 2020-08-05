test_that("Input Tests work", {
        # Phi & Theta
        expect_error(arma_sim(phi=c("b")), "phi has to be numeric vector or NULL!")
        expect_error(arma_sim(phi=NA), "phi has to be numeric vector or NULL!")

        # n
        expect_error(arma_sim(phi=0.5), "n must be an integer!")
        expect_error(arma_sim(phi=0.5, n=20.1), "n must be an integer!")
        expect_error(arma_sim(phi=0.5, n="b"), "n must be an integer!")
        expect_error(arma_sim(phi=0.5, n=NA), "n must be an integer!")
        expect_error(arma_sim(phi=0.5, n=NULL), "n must be an integer!")
        expect_error(arma_sim(phi=0.5, n=c(1,2,3)), "n must be an integer!")
        expect_error(arma_sim(phi=0.5, n=-1L), "n must be strictly positive!")

        # Burn-in
        expect_error(arma_sim(phi=0.5, n=20, burnin = 20.1), "burnin must be an integer!")
        expect_error(arma_sim(phi=0.5, n=20, burnin = "b"), "burnin must be an integer!")
        expect_error(arma_sim(phi=0.5, n=20, burnin = NULL), "burnin must be an integer!")
        expect_error(arma_sim(phi=0.5, n=20, burnin = c(1,2,3)), "burnin must be an integer!")
        expect_error(arma_sim(phi=0.5, n=20, burnin = 0), "burnin period must be at least as long as max\\(p,q\\)")
})

test_that("AR(1)-Process", {

})
