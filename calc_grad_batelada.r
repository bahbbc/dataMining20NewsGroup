source('~/workspace/dataMining20NewsGroup/helper_functions.r')

# x -> entries
# Yd -> desired result
# weights_a -> random inicial weights
# weights_b -> random inicial weights
# N is the total instance number
first_phase <- function(x, Yd, weights_a, weights_b){
  #calculate x*weights without bias, then sums the bias
  Zin <- add_bias_b(x) %*% t(weights_a)
  Z <- gauss(Zin)
  Yin <- add_bias_b(Z) %*% t(weights_b)
  err <- Yin - Yd
  
  list(Zin=Zin, Z=Z, Yin=Yin, err=err)
}

# melhorar essa função removendo as derivadas daqui
dEt_dx_bat <- function(x, Yd, weights_a, weights_b){
  out <- first_phase(x, Yd, weights_a, weights_b)
  
  dEt_db <- t(out$err) %*% add_bias_b(out$Z)
  dEt_da <- t(out$err %*% weights_b[,1:(dim(weights_a)[1])] * apply(out$Zin, 2, derivative_gaus)) %*% add_bias_b(x) 
  list(dEt_da, dEt_db)
}

add_bias_b <-function(x){
  cbind(x, 1)
}