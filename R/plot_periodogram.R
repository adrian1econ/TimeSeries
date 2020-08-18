#'Plot the Periodogram
#'
#' This function plots the periodogram of a signal (e.g. a time
#' series or a series of observations) at the Fourier frequencys between 0 and \code{pi}. 
#' 
#' @param pd A numeric vector that contains the values of the periodogram of a
#'   signal at the Fourier frequencies.
#' @return A graph of the periodogram at the Fourier frequencys.
#' @examples
#' n<- 1:1000
#' x <- rnorm(1:1000) + 2*sin(2*pi*n)
#' peri_x <- periodogram(x)
#' plot_periodogram(peri_x)
#' @export
plot_periodogram <- function(pd){
  # 1. Check Inputs
  if( !is.numeric(pd)) stop("pd must be numeric vector.")
  
  # 2. Prepare the data
  n <- length(pp)
  if(n%%2==0) peri <- pp[((n/2)+1):n]
  if(n%%2==1) peri <- pp[(floor(n/2)+1):n]
  
  nr <- length(peri)
  f <- 2*pi*(1:nr)/n
  
  # 3. Plot Periodogram
  ggplot() +
    geom_point(mapping = aes(f,peri)) + 
    geom_line(mapping = aes(f,peri)) + 
    labs(title="Periodogramm der monatlichen Inflationsraten",
         x="Frequenz", y = "I(w)") +
    theme_classic()
  
}
