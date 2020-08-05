#' Simulate ARMA-Process
#'
#' Description
#'
#' Details
#'
#' @param phi,theta A numeric vector specifying the AR(MA)-Coefficients of an
#'   ARMA(p,q) model.
#' @param mu A numeric vector specifying the mean of the ARMA(p,q)-Series. Default is zero mean.
#' @param n An integer specifying the length of the resulting time series.
#' @param innov.gen A function from which the random innovations are drawn.
#' @param burnin An integer specifying the number of datapoints that are going
#'   to be discarded, so that the characteristics of final series do not depend
#'   on the initial values.
#' @return Object of class "arma" containing the simulated arma series, the
#'   innovation series and the specified parameters.
#' @examples
#' arma_sim(phi = c(0.5,-0.1), theta = c(0.1,0.2,-0.3), n=100)
#' @export
arma_sim <- function(phi = NULL, theta = NULL, mu = 0, n, innov.gen = rnorm,
                     burnin = NA, ...){

        # 1. Check inputs:
        # phi
        if(!is.numeric(phi) & !is.null(phi)) stop("phi has to be numeric vector or NULL!")
        # theta
        if(!is.numeric(theta) & !is.null(theta)) stop("theta has to be numeric vector or NULL!")
        # n
        if(missing(n)) stop("n must be an integer!")
        if(!is.numeric(n)) stop("n must be an integer!")
        if(!length(n)==1) stop("n must be an integer!")
        if(n%%1!=0) stop("n must be an integer!")
        if(n<=0) stop("n must be strictly positive!")

        # Order of Process:
        p <- length(phi)
        q <- length(theta)

        # Length of burnin-period and initial innovations:
        l_max <- max(length(phi), length(theta))
        if(is.null(burnin)) stop("burnin must be an integer!")
        if(is.na(burnin)) burnin <- 10*l_max
        if(!is.numeric(burnin)) stop("burnin must be an integer!")
        if(burnin%%1!=0) stop("burnin must be an integer!")
        if(burnin < 0) stop("burnin must be an positive!")
        if(!length(burnin)==1) stop("burnin must be an integer!")
        if(burnin < max(p,q)) stop("burnin period must be at least as long as max(p,q)")


        # 2. Check for stationarity of the AR part:
        if(!is.null(phi)){
        if( any( Mod(polyroot(c(1,-phi))) <= 1)) warning("series is not stationary!")}


        # 3. Simulation:

        # Initialize:
        len <- burnin + n
        series <- double(len)
        # Innovations:
        e <- innov.gen(len, ...)

        for(t in 1:len){

                # Starting values as random innovations
                if(t <= l_max) {series[t] <- e[t]
                } else{
                        temp <- 0

                        # AR process
                        if(!is.null(phi)){
                        for(i in 1:p) temp <- temp + phi[i]*series[t-i]}

                        # MA process
                        if(!is.null(theta)){
                        for(j in 1:q) temp <- temp + theta[j]*e[t-j]}

                        # Adding random innovation
                        series[t] <- temp + e[t]

                }
        }

        # Slice non-burnin period
        arma <- series[-c(1:burnin)]
        innov <- e[-c(1:burnin)]

        # Add mu to the series
        arma <- arma + mu

        # 4.
        output <- list(arma=arma,
                       innov=innov,
                       phi=phi,
                       theta=theta,
                       burnin=burnin)

        structure(output, class="arma")
}
