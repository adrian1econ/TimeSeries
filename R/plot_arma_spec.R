#' Spectral Density Plot
#'
#' The function plots the theoretical spectral density of an ARMA-Series.
#'
#' Details
#'
#' @param arma A numeric vector containing a time series or an object of class
#'   "arma".
#' @return A numeric vector containing the sample autocovariance function.
#' @examples
#' plot_ts(arma_sim(phi = c(0.5,-0.1), theta = c(0.1,0.2,-0.3), n=100))
#' @export
plot_ts <- function(ts){

        # 1. Check inputs:
        # ts
        if( all(!is.numeric(ts),!(class(ts)=="arma")) ) stop("ts must be numeric vector or object of class arma!")
        if(is.numeric(ts)) series <- ts
        if(class(ts)=="arma") series <- ts$arma
        if(length(series)<=1) stop("length of ts must be greater than 1")

        # 2. Plot:
        tbl <- tibble::tibble(series=series, t=seq_along(series))
        plot <- ggplot2::ggplot(tbl, ggplot2::aes(x=t,y=series)) +
                ggplot2::geom_line()
        plot
}

