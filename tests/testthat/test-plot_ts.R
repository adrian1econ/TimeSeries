test_that("Plot returns ggplot object", {
        plot <- plot_ts(arma_sim(phi = c(0.5,-0.1), theta = c(0.1,0.2,-0.3), n=100))
        expect_is(plot, "ggplot")
})

test_that("Plot returns ggplot object 2", {
        plot2 <- plot_ts(arma_sim(phi = c(0.5,-0.1), theta = c(0.1,0.2,-0.3), n=100)$arma)
        expect_is(plot2, "ggplot")
})
