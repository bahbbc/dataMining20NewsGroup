source('calc_grad.r')

mlp <- function(max_err, max_epoch, x, Yd, alpha, hidden_layers){
  epoch <- 0
  err_tot <- 999
  weights_a <- random_weights(x[1,], hidden_layers, length(x[1,])+1)
  weights_b <- random_weights(x[1,], dim(Yd)[2], hidden_layers+1)
  out = matrix(rep(0, length(x)), nrow=dim(Yd)[1], ncol=dim(Yd)[2])

  while(err_tot >= max_err && epoch <= max_epoch){
    epoch = epoch + 1
    error <- 0
    for(i in 1:dim(x)[1]){
      simple_error <- error(x[i,], Yd[i,], weights_a, weights_b)
      #saida: uma list (Zin, Z, Yin, err, Y)
      #entradas: (x, Zin, Z, Yin, err, N, alpha, weights_a, weights_b)
      new_weights <- grad(x[i,], simple_error[[1]], simple_error[[2]], simple_error[[3]], simple_error[[4]], dim(x)[1], alpha, weights_a, weights_b)
      #saida: uma list (new_a, new_b)
      weights_a <- new_weights[[1]]
      weights_b <- new_weights[[2]]
      Y <- simple_error[[5]]
      error <- error + quad_err(simple_error[[4]])
      out[i,] = Y
    }
    err_tot <-error/dim(x)[1]
    print(paste("Erro",err_tot, "I", epoch, "out", out))
  }
  out
}


mlp_tunned <- function(max_err, max_epoch, x, Yd, alpha, hidden_layers, r, q){
  epoch <- 0
  err_tot <- 999
  weights_a <- random_weights(x[1,], hidden_layers, length(x[1,])+1)
  weights_b <- random_weights(x[1,], dim(Yd)[2], hidden_layers+1)
  out = matrix(rep(0, length(x)), nrow=dim(Yd)[1], ncol=dim(Yd)[2])

  while(err_tot >= max_err && epoch <= max_epoch){
    epoch = epoch + 1
    error <- 0
    for(i in 1:dim(x)[1]){
      simple_error <- error(x[i,], Yd[i,], weights_a, weights_b)
      #saida: uma list (Zin, Z, Yin, err, Y)
      #entradas: (x, Zin, Z, Yin, err, N, alpha, weights_a, weights_b)
      new_weights <- grad(x[i,], simple_error[[1]], simple_error[[2]], simple_error[[3]], simple_error[[4]], dim(x)[1], alpha, weights_a, weights_b)
      #saida: uma list (new_a, new_b)
      weights_a <- new_weights[[1]]
      weights_b <- new_weights[[2]]
      Y <- simple_error[[5]]
      error <- error + quad_err(simple_error[[4]])
      erro_norm <- grad_norm(new_weights[[3]], new_weights[[4]])
      while(erro_norm > error){
        print(paste("Erro norm",erro_norm, "erro", error, "alpha", alpha))
        alpha <- alpha * r
        weights_a <- new_weight(alpha, new_weights[[3]] , weights_a)
        weights_b <- new_weight(alpha, new_weights[[4]], weights_b)
        erro_norm <- grad_norm(weights_a, weights_b)
      }
      error <- erro_norm
      alpha <- q * alpha
      out[i,] = Y
    }
    err_tot <-error/dim(x)[1]
    print("-----------------")
    #print(paste("Erro",err_tot, "I", epoch, "out", out))
  }
  out
}

