source('~/workspace/dataMining20NewsGroup/calc_grad_batelada.r')
source('~/workspace/dataMining20NewsGroup/helper_functions.r')

#mlp_batelada <- function(max_epoch, x, Yd, alpha, hidden_layers){
mlp_batelada <- function(max_epoch, x, Yd, validation, validationYd, alpha, hidden_layers){
  epoch <- 0
  weights_a <- random_weights(hidden_layers, length(x[1,])+1)
  weights_b <- random_weights(dim(Yd)[2], hidden_layers+1)
  #weights_a <- matrix(c(0.25),nrow = 3, ncol=5)
  #weights_b <- matrix(c(0.25), nrow = 3, ncol=4)
  out = matrix(rep(0, dim(Yd)[1]), nrow=dim(Yd)[1], ncol=dim(Yd)[2])
  max_err <- 1e-5;
  r <- 0.9
  q <- 10
  gl <- 0
  min_val_error <- 999
  error_val <- NULL
  
  net_out <- first_phase(x, Yd, weights_a, weights_b)
  En <- quad_err(net_out$err)/dim(x)[1]
  error_vec <- c(En)
  
  while(En >= max_err && epoch < max_epoch){
    epoch = epoch + 1
    
    #entradas: (x, Zin, Z, err, weights_a, weights_b)
    der <- dEt_dx_bat(x, Yd, weights_a, weights_b)
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
    Y <- net_out_try$Yin
    error_prov <- quad_err(net_out_try$err)/dim(x)[1]
    
    #while(error_prov > En){
    #  alpha <- alpha * r
      
    #  try_a <- new_weight(alpha, dEt_da, weights_a)
    #  try_b <- new_weight(alpha, dEt_db, weights_b)
      
    #  net_out_try <- first_phase(x, Yd, try_a, try_b)
    #  error_prov <- quad_err(net_out_try$err)/dim(x)[1]
    #  Y <- net_out_try$Yin
    #}
    weights_a <- try_a
    weights_b <- try_b
    net_out_val_error <- first_phase(validation, validationYd, weights_a, weights_b)$err
    En_val <- quad_err(net_out_val_error)/dim(x)[1]
    min_val_error <- min(En_val, min_val_error)
    error_val <- append(En_val, error_val)
    # generalization loss
    gl <- 100*((En_val/min_val_error)-1)
    print(paste("Erro",error_prov, "I", epoch, "alpha", alpha, "GL", gl))
    En <- error_prov
    error_vec <- append(error_vec, En)
    #alpha <- q * alpha
    out = Y
    
  }
  list(out=out, A=weights_a, B=weights_b, alpha=alpha, error_vec=error_vec, error_val=error_val)
}
