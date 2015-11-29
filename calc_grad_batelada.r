source('~/workspace/dataMining20NewsGroup/calc_grad.r')

first_phase <- function(x=add_bias(x), Yd, weights_a, weights_b){
  #calculate x*weights without bias, then sums the bias
  Zin <- add_bias(x) %*% t(weights_a)
  Z <- sigmoid(Zin)
  Yin <- add_bias(Z) %*% t(weights_b)
  err <- Yin - Yd
  
  list(Zin=Zin, Z=Z, Yin=Yin, err=err)
}

grad <- function(x, Zin, Z, Yin, err, N, alpha, weights_a, weights_b){
  #calculate new_b
  new_b=NULL
  #uses de Et derivative in relation to b
  dEt_db <- t(err) %*% add_bias(Z)
  new_b <- rbind(new_b, new_weight(alpha, dEt_db, weights_b))
  
  #calculate new_a
  new_a=NULL
  # dim(weights_a)[1]+1 remove o nro de elementos que não serão multiplicados pelos valores de entrada da rede
  dEt_da <-t(((err %*% weights_b[,-(dim(weights_a)[1]+1)]) * (1 - Z) * Z)) %*% add_bias(x)
  new_a <- rbind(new_a, new_weight(alpha, dEt_da, weights_a))
  
  list(new_a, new_b, dEt_da, dEt_db)
}

# melhorar essa função removendo as derivadas daqui
dEt_dx_bat <- function(x, Zin, Z, err, weights_a, weights_b){
  dEt_db <- t(err) %*% add_bias(Z)
  dEt_da <-t(((err %*% weights_b[,-(dim(weights_a)[1]+1)]) * (1 - Z) * Z)) %*% add_bias(x)
  list(dEt_da, dEt_db)
}

quad_err <- function(err){
  sum(err^2)
}

add_bias <-function(x){
  cbind(x, 1)
}