# x -> entries
# Yd -> desired result
# weights_a -> random inicial weights
# weights_b -> random inicial weights
# N is the total instance number
first_phase <- function(x=add_bias(x), Yd, weights_a, weights_b){
  #calculate x*weights without bias, then sums the bias
  Zin <- add_bias(x) %*% t(weights_a)
  Z <- gauss(Zin)
  Yin <- add_bias(Z) %*% t(weights_b)
  Y <-gauss(Yin)
  err <- Y - Yd

  list(Zin, Z, Yin, err, Y)
}

# melhorar essa função removendo as derivadas daqui
grad <- function(x, Zin, Z, Yin, err, N, alpha, weights_a, weights_b){
  #calculate new_b
  new_b=NULL
  #uses de Et derivative in relation to b
  dEt_db <- 1/N*t(add_bias(Z)%*%(err*derivative_gaus(Yin)))
  new_b <- rbind(new_b, new_weight(alpha, dEt_db, weights_b))

#calculate new_a
  new_a=NULL
  dEt_da <-t(add_bias(x)%*%(((err*derivative_gaus(Yin))%*%weights_b[,-dim(weights_a)[1]])*derivative_gaus(Zin)))
  new_a <- rbind(new_a, new_weight(alpha, dEt_da, weights_a))

  list(new_a, new_b, dEt_da, dEt_db)
}

# melhorar essa função removendo as derivadas daqui
dEt_dx <- function(x, Zin, Z, Yin, err, N, weights_a, weights_b){
  dEt_db <- 1/N*t(add_bias(Z)%*%(err*derivative_gaus(Yin)))
  dEt_da <-t(add_bias(x)%*%(((err*derivative_gaus(Yin))%*%weights_b[,-dim(weights_a)[1]])*derivative_gaus(Zin)))
  list(dEt_da, dEt_db)
}

#add initial weights as random numbers from 0 to 1 and all bias as 1
random_weights <-function(x, nrow, ncol){
  weights <- runif((nrow)*ncol, 0, 1)
  weights <- matrix(weights, nrow=nrow, ncol=ncol)
}

# just adds a 1 col for the bias
add_bias <-function(x){
  c(x, 1)
}

derivative <- function(x){
  t(attr(eval(deriv(~1/(1+exp(-x)), "x")), 'gradient'))
}

derivative_gaus <- function(x){
  t(attr(eval(deriv(~exp(-(x^2)/2), "x")), 'gradient'))
}

new_weight_momentum <- function(alpha, grad, old_weight, m){
  (old_weight - alpha * grad) + m * old_weight
}

new_weight <- function(alpha, grad, old_weight){
  old_weight - alpha * grad
}

grad_norm <- function(dEt_da, dEt_db){
  e <-matrix(c(dEt_da, dEt_db))
  e <- norm(e)^2
}

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
  1/2*(sum(err))^2
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
