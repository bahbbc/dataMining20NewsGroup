source('~/workspace/dataMining20NewsGroup/calc_grad.r')

first_phase <- function(x, Yd, weights_a, weights_b){
  #calculate x*weights without bias, then sums the bias
  Zin <- add_bias(x) %*% t(weights_a)
  Z <- sigmoid(Zin)
  Yin <- add_bias(Z) %*% t(weights_b)
  err <- Yin - Yd
  
  list(Zin=Zin, Z=Z, Yin=Yin, err=err)
}

# melhorar essa função removendo as derivadas daqui
dEt_dx_bat <- function(x, Yd, weights_a, weights_b){
  out <- first_phase(x, Yd, weights_a, weights_b)
  
  dEt_db <- t(out$err) %*% add_bias(out$Z)
  dEt_da <- t(((out$err %*% weights_b[,1:(dim(weights_a)[1])]) * (1 - out$Z) * out$Z)) %*% add_bias(x) 
  list(dEt_da, dEt_db)
}

quad_err <- function(err){
  sum(err * err)
}

add_bias <-function(x){
  cbind(x, 1)
}