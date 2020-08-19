#' Durbin-Levinson Algorithm
#'
#' Description
#'
#' Details
#'
#' @param ts A numeric vector containing a time series or an object of class "arma".
#' @param p Number of recursions or number of phi's to be determined.
#' @return Numeric vector containing the phi-Coefficients determined by the Durbin-Levinson algorithm.
#' @examples
#' DL(arma_sim(phi = c(0.8,-0.3),n = 1000,burnin = 1000))
#' @export
DL <- function(ts, p=NULL){

        # 1. Check inputs:
        # ts
        if( all(!is.numeric(ts),!(class(ts)=="arma")) ) stop("ts must be numeric vector or object of class arma!")
        if(is.numeric(ts)) x <- ts
        if(class(ts)=="arma") x <- ts$arma

        # p
        if(!is.null(p) && !(is.integer(p) && p >= 2)) stop("p must be NULL or an integer >= 2")
        if(is.null(p)) p <- length(x)

        # 2. DL-Algorithm

        # Autocovarinace function
        gamma <- c(acf(x),0) # letztes gamma nach E-Mail auf Null gesetzt

        # Starting values
        phi_nn <- gamma[2]/gamma[1]
        phi <- phi_nn
        v <- gamma[1]*(1 - phi^2) # v0 = gamma(0) --> v1

        # Recursion
        for(n in 2:p){
                phi_nn <- (gamma[n+1] - sum(phi * gamma[n:2]))*v^-1
                phi <- c(phi - phi_nn * phi[(n-1):1], phi_nn)
                v <- v * (1 - phi_nn^2)
        }
        rev(phi)
}


