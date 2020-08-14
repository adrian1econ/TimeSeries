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
  acf_vec <- acf(ts)
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
  next_x_sum <- function(theta,x,x_next,i)
  {
    theta_ <- theta[i,i:1]
    x_cache <- x[1:i]
    sum(theta_*(x_cache-x_next))
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
  #Calculate next step x_next
  x_next <- 0 # First Prediction
  for (i in seq(lag.max))
  {
    x_next[i+1] <- next_x_sum(theta,x,x_next,i)
  }
return(x_next)
}
d <- innovation(1:4)
