#add initial weights as random numbers from 0 to 1 and all bias as 1
random_weights <-function(nrow, ncol){
  weights <- runif((nrow)*ncol, 0, 1)
  weights <- matrix(weights, nrow=nrow, ncol=ncol)
}

derivative <- function(x){
  t(attr(eval(deriv(~1/(1+exp(-x)), "x")), 'gradient'))
}

derivative_gaus <- function(x){
  t(attr(eval(deriv(~exp(-(x^2)/2), "x")), 'gradient'))
}

new_weight <- function(alpha, grad, old_weight){
  old_weight - alpha * grad
}

grad_norm <- function(dEt_da, dEt_db){
  e <- matrix(c(as.matrix(dEt_da), as.matrix(dEt_db)), byrow = TRUE)
  e <- e/norm_vec(e)
}

norm_vec <- function(x) sqrt(sum(x^2))

norm_row <- function(x) sqrt(rowSums(x^2))

#########
### Activation functions
##########

sigmoid<-function(x){
 1/(1+exp(-x))
}

gauss<-function(x){
  exp(-(x^2)/2)
}

quad_err <- function(err){
  sum(err * err)
}

quad_err_grad <- function(alpha, dEt_da, dEt_db){
  e <-matrix(c(dEt_da, dEt_db))
  -1/2*alpha*norm(e)^2
}

rprop_test <- function(prevE_x, dEt_dx, prev_delta, weights_x){
  delta_max <- 50
  delta_min <- 0.000001
  n_pos <- 1.2
  n_neg <- 0.5

  test_prevE_x <- prevE_x * dEt_dx
  #update delta for each matrix elem
  temp <- (prev_delta * n_pos)
  temp[temp > delta_max] <- delta_max
  delta <- temp

  weights_x[test_prevE_x < 0] <- weights_x[test_prevE_x < 0] - prev_delta
  delta_x <- sign(dEt_dx) * delta
  weights_x[test_prevE_x >= 0] <- weights_x[test_prevE_x >= 0] + delta_x

  temp <- (prev_delta * n_neg)
  temp[temp < delta_min] <- delta_min
  weights_x[test_prevE_x < 0] <- weights_x[test_prevE_x < 0] - prev_delta

  print(paste("delta", delta))
  list(weights_x, delta)
}

sign<- function (matrix){
  matrix[matrix < 0] <- -1
  matrix[matrix > 0] <- 1
  matrix
}
