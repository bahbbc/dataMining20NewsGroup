source('calc_grad.r')

mlp <- function(max_err, max_epoch, x, Yd, alpha, hidden_layers){
  epoch <- 0
  err_tot <- 999
  weights_a <- random_weights(x[1,], hidden_layers, length(x[1,])+1)
  weights_b <- random_weights(x[1,], dim(Yd)[2], hidden_layers+1)
  out = matrix(rep(0, length(x)), nrow=dim(Yd)[1], ncol=dim(Yd)[2])

  while(err_tot >= max_err && epoch <= max_epoch){
    epoch = epoch + 1
    En <- 0
    for(i in 1:dim(x)[1]){
      simple_error <- first_phase(x[i,], Yd[i,], weights_a, weights_b)
      #saida: uma list (Zin, Z, Yin, err, Y)
      #entradas: (x, Zin, Z, Yin, err, N, alpha, weights_a, weights_b)
      new_weights <- grad(x[i,], simple_error[[1]], simple_error[[2]], simple_error[[3]], simple_error[[4]], dim(x)[1], alpha, weights_a, weights_b)
      #saida: uma list (new_a, new_b)
      weights_a <- new_weights[[1]]
      weights_b <- new_weights[[2]]
      Y <- simple_error[[5]]
      En <- En + quad_err(simple_error[[4]])
      out[i,] = Y
    }
    err_tot <- En/dim(x)[1]
    print(paste("Erro",err_tot, "I", epoch, "delta", delta))
  }
  list(out, weights_a, weights_b)
}

# Fazer uma matriz com os deltas de cada peso - DONE
# Um delta para cada tipo de peso (a e b) - creio que sim
# Corrigir atualização dos metodos (delta deve virar o prev_delta)

mlp_t1 <- function(max_err, max_epoch, x, Yd, hidden_layers){
  epoch <- 0
  err_tot <- 999
  weights_a <- random_weights(x[1,], hidden_layers, length(x[1,])+1)
  weights_b <- random_weights(x[1,], dim(Yd)[2], hidden_layers+1)
  out = matrix(rep(0, length(x)), nrow=dim(Yd)[1], ncol=dim(Yd)[2])
  prevE_a <- matrix(rep(0.1, length(weights_a)), nrow=dim(weights_a)[1], ncol=dim(weights_a)[2])
  prevE_b <- matrix(rep(0.1, length(weights_b)), nrow=dim(weights_b)[1], ncol=dim(weights_b)[2])
  delta_a <- prevE_a
  delta_b <- prevE_a

  while(err_tot >= max_err && epoch <= max_epoch){
    epoch = epoch + 1
    En <- 0
    for(i in 1:dim(x)[1]){
      net_out <- first_phase(x[i,], Yd[i,], weights_a, weights_b)
      # saida: uma list (Zin, Z, Yin, err, Y)
      # entradas: (x, Zin, Z, Yin, err, N, weights_a, weights_b)
      # use named lists to make this more readable
      new_weights <- dEt_dx(x[i,], net_out[[1]], net_out[[2]], net_out[[3]], net_out[[4]], dim(x)[1], weights_a, weights_b)
      # saida: uma list (new_a, new_b, dEt_da, dEt_db)
      Y <- net_out[[5]][1]
      err <- net_out[[4]][1]

      # tests to update delta using RProp
      prop_weight_a <- rprop_test(prevE_a, new_weights[[1]], delta_a, weights_a)
      weights_a <- prop_weight_a[[1]]
      delta_a <- prop_weight_a[[2]]

      prop_weight_b <- rprop_test(prevE_b, new_weights[[2]], delta_b, weights_b)
      weights_b <- prop_weight_b[[1]]
      delta_b <- prop_weight_b[[2]]

      En <- En + quad_err(net_out[[4]])
      out[i,] = Y
    }
    err_tot <- En/dim(x)[1]
    print(paste("Erro",err_tot, "I", epoch))
  }
  list(out, weights_a, weights_b)
}


#a duvida é se cada peso modifica o delta de uma maneira diferente

rprop_test_fake <- function(prevE_x, dEt_dx, prev_delta, weights_x){
  delta_max <- 50
  delta_min <- 0.000001
  n_pos <- 1.2
  n_neg <- 0.5

  test_prevE_x <- prevE_x * dEt_dx

  if(test_prevE_x >= 0){
    delta <- min((prev_delta * n_pos), delta_max)
    delta_x <- sign(dEt_dx) * delta
    weights_x <- weights_x + delta_x
  }
  else{
    delta <- max((prev_delta * n_neg), delta_min)
    weights_x <- weights_x - prev_delta
  }
}


mlp_tunned <- function(max_err, max_epoch, x, Yd, alpha, hidden_layers, q, r){
  epoch <- 0
  err_tot <- 999
  weights_a <- random_weights(x[1,], hidden_layers, length(x[1,])+1)
  weights_b <- random_weights(x[1,], dim(Yd)[2], hidden_layers+1)
  out = matrix(rep(0, length(x)), nrow=dim(Yd)[1], ncol=dim(Yd)[2])

  while(err_tot >= max_err && epoch <= max_epoch){
    epoch = epoch + 1
    En <- 0
    for(i in 1:dim(x)[1]){
      net_out <- first_phase(x[i,], Yd[i,], weights_a, weights_b)
      #saida: uma list (Zin, Z, Yin, err, Y)
      #entradas: (x, Zin, Z, Yin, err, N, alpha, weights_a, weights_b)
      new_weights <- grad(x[i,], net_out[[1]], net_out[[2]], net_out[[3]], net_out[[4]], dim(x)[1], alpha, weights_a, weights_b)
      #saida: uma list (new_a, new_b, dEt_da, dEt_db)
      weights_a <- new_weights[[1]]
      weights_b <- new_weights[[2]]
      Y <- net_out[[5]][1]
      err <- net_out[[4]]
      En <- En + quad_err(err)

      #tests to update alpha
      norm_grad = grad_norm(new_weights[[3]], new_weights[[4]])
      new_weights <- grad(x[i,], net_out[[1]], net_out[[2]], net_out[[3]], norm_grad, dim(x)[1], alpha, weights_a, weights_b)
      try_a <- new_weight(alpha, new_weights[[3]] , weights_a)
      try_b <- new_weight(alpha, new_weights[[4]], weights_b)
      error_prov <- quad_err(first_phase(x[i,], Yd[i,], try_a, try_b)[[4]])
      while(error_prov > En){
        print(paste("New error",error_prov, "erro", En, "alpha", alpha))
        alpha <- r * alpha
        try_a <- new_weight(alpha, new_weights[[3]] , try_a)
        try_b <- new_weight(alpha, new_weights[[4]], try_b)
        error_prov <- quad_err(first_phase(x[i,], Yd[i,], try_a, try_b)[[4]])
      }
      weights_a <- try_a
      weights_b <- try_b
      En <- error_prov
      alpha <- q * alpha
      out[i,] = Y
    }
    err_tot <- En/dim(x)[1]
    print("-----------------")
    print(paste("Erro",err_tot, "I", epoch))
  }
  out
}




