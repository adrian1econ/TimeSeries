#' The Periodogram
#'
#' The Periodogram estimates the power spectral density of a signal (e.g. a time
#' series or a series of observations). Note that the Periodogram is not a consistent estimator.
#' 
#' @param ts A numeric vector or a structur of class arma which contains the
#'   time series that should be analysed.
#' @return A vector which contains the values of the Periodogram of the signal
#'   or the time series \code{ts} at the Fourier frequencys.
#' @examples
#' n<- 1:1000
#' x <- rnorm(1:1000) + 2*sin(2*pi*n)
#' periodogram(x)
#' @export
periodogram <- function(ts) {
  
  # 1. Check Input
  if( all(!is.numeric(ts),!(class(ts)=="arma"))) stop("ts must be numeric vector or object of class arma.")
  
  
  # 2. Define coefficients and vectors
  if(is.numeric(ts)) x <- ts          # Inputvector
  if(class(ts)=="arma") x <- ts$arma  # Inputvector
  n <- length(x)                      # length
  I <- double(n)                      # empty output
  F_n <- unique(floor((-(n-1):n)/2))  # Index j
  
  
  # 3. Periodogram
  for (j in F_n) {
    sum <- 0
    for (k in 1:n) {
      sum <- sum + x[k]*exp(as.complex(-1i)*k*(2*pi*j)/n)
      
    }
    
    I[j+ceiling(n/2)] <- 1/n * abs(sum)^2
    
  }
  
  # 4. Output of the estimator
  I
}
