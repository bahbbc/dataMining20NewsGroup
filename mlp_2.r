source('~/workspace/dataMining20NewsGroup/calc_grad.r')

mlp_tunned <- function(max_err, max_epoch, x, Yd, alpha, hidden_layers, q, r){
  epoch <- 0
  err_tot <- 999
  weights_a <- random_weights(x[1,], hidden_layers, length(x[1,])+1)
  weights_b <- random_weights(x[1,], dim(Yd)[2], hidden_layers+1)
  out = matrix(rep(0, length(x)), nrow=dim(Yd)[1], ncol=dim(Yd)[2])
  En_tot <- 0

  while(err_tot >= max_err && epoch <= max_epoch){
    epoch = epoch + 1
    En <- 0
    for(i in 1:dim(x)[1]){
      net_out <- first_phase(x[i,], Yd[i,], weights_a, weights_b)
      #saida: uma list (Zin, Z, Yin, err, Y)
      Y <- net_out[[5]]
      En <- quad_err(net_out[[4]])
      #entradas: (x, Zin, Z, Yin, err, N, alpha, weights_a, weights_b)
      new_weights <- grad(x[i,], net_out[[1]], net_out[[2]], net_out[[3]], net_out[[4]], dim(x)[1], alpha, weights_a, weights_b)
      #saida: uma list (new_a, new_b, dEt_da, dEt_db)
      weights_a <- new_weights[[1]]
      weights_b <- new_weights[[2]]

      #tests to update alpha
      norm_grad = grad_norm(new_weights[[3]], new_weights[[4]])
      dEt_da <- matrix(norm_grad[1:length(weights_a)], nrow=dim(weights_a)[1], ncol=dim(weights_a)[2])
      dEt_db <- matrix(norm_grad[length(weights_b):length(norm_grad)], nrow=dim(weights_b)[1], ncol=dim(weights_b)[2])

      try_a <- new_weight(alpha, dEt_da , weights_a)
      try_b <- new_weight(alpha, dEt_db, weights_b)

      error_prov <- quad_err(first_phase(x[i,], Yd[i,], try_a, try_b)[[4]])

      while(error_prov > En){
        print(paste("New error",error_prov, "erro", En, "alpha", alpha))
        alpha <- r * alpha
        try_a <- new_weight(alpha, dEt_da , try_a)
        try_b <- new_weight(alpha, dEt_db, try_b)
        net_out_try <- first_phase(x[i,], Yd[i,], try_a, try_b)
        error_prov <- quad_err(net_out_try[[4]])
        Y <- net_out_try[[5]]
      }
      weights_a <- try_a
      weights_b <- try_b
      En <- error_prov
      En_tot <- En_tot + En
      alpha <- q * alpha
      out[i,] <- Y
    }
    err_tot <- En_tot/dim(x)[1]
    print("-----------------")
    print(paste("Erro",err_tot, "I", epoch, "alpha", alpha))
  }
  out
}
