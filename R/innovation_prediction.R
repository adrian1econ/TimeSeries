#' Innovation Prediction
#'
#'Description
#'The innovation prediction use the innovation algorithm to predict the n-th Element after.
#'
#'Details
#'The first step's-values are needed to detect prediction and the next lag.next values are used to detect the prediction.
#'
#'  @param ts A numeric vector containing a time series or an object of class "arma".
#' @param steps shows which x will be predicted
#' @param lag.na Number of recursions to determined prediction. (The first step's-values are needed to detect prediction)
#' @return Numeric vector containing the prediction determined by the innovation algorithm.
#' @examples
#' innovation(arma_sim(theta = c(0.8,-0.3),n = 1000,burnin = 1000))
#' @export


innovation_prediction <- function(ts,steps=1,lag.max=NA)
{
  #Inputs Check
  #ts Check
  if( all(!is.numeric(ts),!(class(ts)=="arma")) ) stop("ts must be numeric vector or object of class arma!")
  if(is.numeric(ts)) x <- ts
  if(class(ts)=="arma") x <- ts$arma
  if(length(x)<=2) stop("length ts must be greater than 2")
  #steps
  if(!is.numeric(steps)) stop("steps must a be positiv integer!")
  if(length(steps)!=1) stop("length of steps must equal 1")
  if(steps%%1!=0) stop("steps must a be positiv integer!")
  if(steps < 1) stop("steps must a be positiv integer!")
  if(steps>(length(x)-1)) stop("steps must be smaller than length(ts)-1")
  #lag.max
  # lag.max Check
  if( all(!is.numeric(lag.max),!is.na(lag.max)) ) stop("lag.max must be integer or NA!")
  if(length(lag.max)!=1) stop("length of lag.max must equal 1")
  if( is.na(lag.max) ) lag.max <- length(x)-steps
  if(lag.max<1|lag.max>=length(x)) stop("lag.max musst be 0 between length of ts")
  if( lag.max%%1!=0 ) stop("lag.max must be integer or NA!")

  if((steps+lag.max)>length(x)) stop("steps + lag.max <= length(ts)")
  #Determinate Theta
  inno_result <- innovation(x,lag.max+steps-1)
  theta <- inno_result[[1]]
  
  x_next <- 0 #Prediction for first Elements
  #Sum need to Prediction of previous Elements
  next_x_sum <- function(theta,x,x_next,i)
  {
    theta_ <- theta[i,i:1]
    x_cache <- x[2:(i+1)]
    sum(theta_*(x_cache-x_next))
  }
  x_next <- 0 # First Prediction
  #Calculate previous Elements predictions
  for (i in seq(steps+lag.max-2))
  {
    x_next[i+1] <- next_x_sum(theta,x,x_next,i)
  }
  #Calculate lag.max+steps+1 Element prediction
  theta_ <- theta[lag.max+steps-1,steps:(lag.max+steps-1)]
  x_ <-x[(lag.max+steps):(steps+1)]
  x_next_ <- x_next[(lag.max+steps-1):steps]
  x_prediction <- sum(theta_*(x_-x_next_))
  
  x_prediction
}
