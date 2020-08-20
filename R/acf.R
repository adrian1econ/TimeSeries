#' Autocovariance Function Estimation
#'
#' The function acf computes an estimate of the autocovariance function.
#'
#' The autocovariance is a function that gives the covariance of the process
#' with itself at pairs of time points. For more detials see the long
#' dokumentation in the vignette "Dokumentation".
#'
#' @param ts A numeric vector containing a time series or an object of class
#'   "arma" ARMA(p,q) model.
#' @param lag.max An integer specifying the maximum lag, for which the
#'   autocovariance is estimated
#' @return A numeric vector containing the sample autocovariance function.
#' @examples
#' acf(arma_sim(phi = c(0.5,-0.1), theta = c(0.1,0.2,-0.3), n=100))
#' @export
acf <- function(ts, lag.max=NULL){

        # 1. Check inputs:
        # ts
        if( all(!is.numeric(ts),!(class(ts)=="arma")) ) stop("ts must be numeric vector or object of class arma!")
        if(is.numeric(ts)) x <- ts
        if(class(ts)=="arma") x <- ts$arma
        if(length(x)<=1) stop("length of ts must be greater than 1")

        # lag.max
        n <- length(x)
        if(is.null(lag.max)) lag.max <- n-1
        if(all(!is.numeric(lag.max),!is.null(lag.max))) stop("lag.max must be integer or NULL!")
        if(length(lag.max)!=1) stop("lag.max must be integer or NULL!")
        if(lag.max%%1!=0) stop("lag.max must be integer or NULL!")



        if(lag.max > n-1 | lag.max < 0) stop("lag.max must be between 0 and length(ts)-1")


        # 2. Calculation of Autocovariance Coefficients
        # Sample mean
        smpl_mean <- 1/n*sum(x)
        # lag.max=0 --> return variance
        if(lag.max==0){return( mean((x-smpl_mean)^2) )}
        # Lag matrix
        lag_mat <- cbind(x, sapply(1:lag.max, function(k) c(x[-c(1:k)], rep(NA, times=k))))
        # Calculation of autocovariance function
        gamma <- apply(lag_mat, 2, function(x_lag){
                1/n * sum((x_lag[!is.na(x_lag)] - smpl_mean)*(x[!is.na(x_lag)] - smpl_mean))} )
        unname(gamma)
}


