#' The Periodogram
#'
#' Description
#'
#' Details
#'
#' @param 
#'
#' @param 
#' @return 
#' @examples
#' 
#' @export
#' 
#' 
periodogram <- function(x) {
  stopifnot("Vector must be numeric" = is.numeric(x)==T)
  
  n <- length(x)
  I <- double(n)
  
  Fn <- as.integer(unique(floor((-(n-1):n)/2)))
 
  for (j in Fn) {
    sum <- 0
    for (k in 1:n) {
      sum <- sum + x[k]*exp(as.complex(-1i)*k*(2*pi*j)/n)
      
    }
    
    I[j+ceiling(n/2)] <- 1/n * abs(sum)^2
    
  }
  I
 
}

x <- rnorm(100)
periodogram(x)
