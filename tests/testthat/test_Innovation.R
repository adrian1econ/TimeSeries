test_that("Input test work", {
  #ts test
  expect_error(innovation(c("a","b")), "ts must be numeric vector or object of class arma!")
  expect_error(innovation(NA), "ts must be numeric vector or object of class arma!")
  expect_error(innovation(list(2,3,4)), "ts must be numeric vector or object of class arma!")
  expect_error(innovation(c(1)), "length of ts must be greater than 1")
  expect_error(innovation(c(1,1,1)), "Variance cant be zero")
  #lag.max test
  expect_error(innovation(c(1,2,6,3,5,3,2), lag.max = 1.1), "lag.max must be integer or NA!")
  expect_error(innovation(c(1,2,6,3,5,3,2), lag.max = c(1,1)), "length of lag.max must equal 1")
  expect_error(innovation(c(1,2,6,3,5,3,2), lag.max ="a"), "lag.max must be integer or NA!")
  })
test_that("Innovation works", {
  #preparation
  vec <- c(1,3,0.5,2,1.2)
  acf_vec <- acf(vec)
  v0 <- acf_vec[1]
  #first round
  theta11 <- acf_vec[2]/v0
  v <- v0-theta11*theta11*v0
  #second round
  theta22 <- (acf_vec[3])/v0
  theta21 <- (acf_vec[2]-theta11*theta22*v0)/v
  v[2] <- v0-theta22^2*v0-theta21^2*v
  #third round
  theta33 <- acf_vec[4]/v0
  theta32 <- 1/v[1]*(acf_vec[3]-theta33*theta11*v0)
  theta31 <- 1/v[2]*(acf_vec[2]-theta33*theta22*v0-theta21*theta32*v[1])
  v[3] <- v0 - theta33^2*v0-theta32^2*v[1]-theta31^2*v[2]
  #Testing with lag.max
  theta <- matrix(c(theta11,theta21,theta31,0,theta22,theta32,0,0,theta33),nrow = 3)
  expect_equal(innovation(vec,3),list("theta"=theta,"mean_sqd_error"=v))
  
  #fourth round
  theta44 <- acf_vec[5]/v0
  theta43 <- 1/v[1]*(acf_vec[4]-theta44*theta11*v0)
  theta42 <- 1/v[2]*(acf_vec[3]-theta44*theta22*v0-theta43*theta21*v[1])
  theta41 <- 1/v[3]*(acf_vec[2]-theta44*theta33*v0-theta43*theta32*v[1]-theta42*theta31*v[2])
  v[4] <- v0 - theta44^2*v0-theta43^2*v[1]-theta42^2*v[2]-theta41^2*v[3]
  #Testing without lag.max
  theta2 <- matrix(c(theta11,theta21,theta31,theta41,0,theta22,theta32,theta42,0,0,theta33,theta43,0,0,0,theta44),nrow = 4) 
  expect_equal(innovation(vec),list("theta"=theta2,"mean_sqd_error"=v))
  })
