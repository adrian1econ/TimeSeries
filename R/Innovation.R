#' Innovation Algorithm
#'
#'Description
#The Innovation Algorithm is one step Prediction 
#'Details
#'
#' @param ts A numeric vector containing a time series or an object of class "arma".
#' @param lag.ma Number of recursions to determined Prediction.
#' @return Numeric vector containing the Prediction determined by the Innovation algorithm.
#' @examples
#' innovation(arma_sim(theta = c(0.8,-0.3),n = 1000,burnin = 1000))
#' @export
innovation <- function(ts,lag.max=NA)
{
  #1 Check Inputs:
    #ts Check
  if( all(!is.numeric(ts),!(class(ts)=="arma")) ) stop("ts must be numeric vector or object of class arma!")
  if(is.numeric(ts)) x <- ts
  if(class(ts)=="arma") x <- ts$arma
  
    # lag.max Check
  if( all(!is.numeric(lag.max),!is.na(lag.max)) ) stop("lag.max must be integer or NA!")
  if(length(lag.max)!=1) stop("length of lag.max must equal 1")
  if(length(x)<=1) stop("length of ts must be greater than 1")
  n <- length(x)
  if( is.na(lag.max) ) lag.max <- n-1
  if( lag.max%%1!=0 ) stop("lag.max must be integer or NA!")
  if(lag.max > n-1 | lag.max <= 0) stop("lag.max must be between 1 and length(ts)-1")
  
  
  theta <- matrix(rep(0,(lag.max)*(lag.max)),nrow = lag.max)
  #acf vec
  acf_vec <- acf(ts,lag.max = lag.max)
  #mean squared errors v
  v <- acf_vec[1]
  if (acf_vec[1]==0) stop("Variance cant be zero")
  
  thetasum <- function(theta,i,k,v)
  {
    if(k==0)
      return(0)
    theta_k <- theta[k,k:1]
    theta_i <- theta[i,i:(i-k+1)]
    v_ <- v[1:k]
    sum(v_*theta_k*theta_i)
  }
 #Thetas calculate
  for (i in seq(lag.max))
  { 
    for (k in seq(0,i-1))
    {
      theta[i,i-k] <- 1/v[k+1]*(acf_vec[i-k+1] - thetasum(theta,i,k,v))
    }
    v[i+1] <- acf_vec[1]-thetasum(theta,i,i,v)
  }

list("theta"=theta,"mean_sqd_error"=v[2:length(v)])
}  
