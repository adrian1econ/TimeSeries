test_that("Input test work", {
  expect_error(innovation_prediction(c("a","b")), "ts must be numeric vector or object of class arma!")
  expect_error(innovation_prediction(NA), "ts must be numeric vector or object of class arma!")
  expect_error(innovation_prediction(list(2,3,4)), "ts must be numeric vector or object of class arma!")
  expect_error(innovation_prediction(c(1,3)), "length ts must be greater than 2")
  expect_error(innovation_prediction(c(1,2,6,3,5,3,2), lag.max = 1.1), "lag.max must be integer or NA!")
  expect_error(innovation_prediction(c(1,2,6,3,5,3,2), lag.max = c(1,1)), "length of lag.max must equal 1")
  expect_error(innovation_prediction(c(1,2,6,3,5,3,2), lag.max ="a"), "lag.max must be integer or NA!")
})
test_that("Innovation_prediction works", {
  #Preparation
  vec <- c(2,0.5,3.5,2.2,6.4,7)
  Innovation_Result <- innovation(vec)
  theta <- Innovation_Result$theta
  #Innovation_prediction test
  x_new <- theta[1,1]*(vec[2])
  x_new[2] <- theta[2,1]*(vec[3]-x_new)+theta[2,2]*vec[2]
  x_new[3] <- theta[3,1]*(vec[4]-x_new[2])+theta[3,2]*(vec[3]-x_new[1])+theta[3,3]*vec[2]
  #Testing with lag.max:
  expect_equal(innovation_prediction(vec,lag.max = 3),x_new[3])
  
  x_new[4] <- theta[4,1]*(vec[5]-x_new[3])+theta[4,2]*(vec[4]-x_new[2])+theta[4,3]*(vec[3]-x_new[1])+theta[4,4]*(vec[2])
  x_new[5] <- theta[5,1]*(vec[6]-x_new[4])+theta[5,2]*(vec[5]-x_new[3])+theta[5,3]*(vec[4]-x_new[2])+theta[5,4]*(vec[3]-x_new[1])+theta[5,5]*vec[2]
                                                                                                                                              
  #Testing with other steps (step=2)
  x_new <- theta[1,1]*(vec[3])
  x_new[2] <- theta[2,1]*(vec[4]-x_new)+theta[2,2]*vec[3]
  x_new[3] <- theta[3,1]*(vec[5]-x_new[2])+theta[3,2]*(vec[4]-x_new[1])+theta[3,3]*vec[3]

  X_2steps <- theta[5,2]*(vec[6]-x_new[3])+theta[5,3]*(vec[5]-x_new[2])+theta[5,4]*(vec[4]-x_new[1])+theta[5,5]*(vec[3])
  
  expect_equal(innovation_prediction(vec,steps = 2),X_2steps)
})

