source('~/workspace/dataMining20NewsGroup/calc_grad.r')

source('~/workspace/dataMining20NewsGroup/calc_grad_batelada.r')

mlp_batelada <- function(max_epoch, x, Yd, alpha, hidden_layers){
  epoch <- 0
  err_tot <- 999
  weights_a <- random_weights(x[1,], hidden_layers, length(x[1,])+1)
  weights_b <- random_weights(x[1,], dim(Yd)[2], hidden_layers+1)
  #weights_a <- matrix(c(0.25),nrow = 2,ncol=3)
  #weights_b <- matrix(c(0.25), nrow = 1, ncol=3)
  out = matrix(rep(0, length(x)), nrow=dim(Yd)[1], ncol=dim(Yd)[2])
  max_err <- 1e-5;
  r <- 0.4
  q <- 2
  
  net_out <- first_phase(x, Yd, weights_a, weights_b)
  En <- quad_err(net_out$err)/dim(x)[1]
  
  while(En >= max_err && epoch <= max_epoch){
    epoch = epoch + 1
    
    net_out <- first_phase(x, Yd, weights_a, weights_b)
    #saida: uma list (Zin, Z, Yin, err, Y)
    #entradas: (x, Zin, Z, Yin, err, N, weights_a, weights_b)
    der <- dEt_dx_bat(x, net_out$Zin, net_out$Z, net_out$err, weights_a, weights_b)
    #saida: uma list (dEt_da, dEt_db)
    dEt_da <- der[[1]]
    dEt_db <- der[[2]]
    #transforms to a vector and normalizes
    norm_grad = grad_norm(dEt_da, dEt_db)
    #transforms to matrixes
    dEt_da <- matrix(norm_grad[1:length(weights_a)], nrow=dim(weights_a)[1], ncol=dim(weights_a)[2])
    dEt_db <- matrix(norm_grad[(length(weights_a) + 1):length(norm_grad)], nrow=dim(weights_b)[1], ncol=dim(weights_b)[2])
    
    try_a <- new_weight(alpha, dEt_da , weights_a)
    try_b <- new_weight(alpha, dEt_db, weights_b)
    net_out_try <- first_phase(x, Yd, try_a, try_b)
    error_prov <- quad_err(net_out_try$err)/dim(x)[1]
    
    while(error_prov > En){
      alpha <- r * alpha
      try_a <- new_weight(alpha, dEt_da, weights_a)
      try_b <- new_weight(alpha, dEt_db, weights_b)
      net_out_try <- first_phase(x, Yd, try_a, try_b)
      error_prov <- quad_err(net_out_try$err)/dim(x)[1]
      Y <- net_out_try$Yin
    }
    weights_a <- try_a
    weights_b <- try_b
    En <- error_prov
    alpha <- q * alpha
    out = Y
    print(paste("Erro",En, "I", epoch, "alpha", alpha))
  }
  list(out, weights_a, weights_b, alpha)
}
