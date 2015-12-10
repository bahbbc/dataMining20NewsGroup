source('~/workspace/dataMining20NewsGroup/calc_grad_batelada.r')
source('~/workspace/dataMining20NewsGroup/helper_functions.r')

#mlp_batelada <- function(max_epoch, x, Yd, alpha, hidden_layers){
mlp_new <- function(max_epoch, x, Yd, validation, validationYd, alpha, hidden_layers){
  epoch <- 0
  weights_a <- random_weights(hidden_layers, length(x[1,])+1)
  weights_b <- random_weights(dim(Yd)[2], hidden_layers+1)
  old_weight_a <- matrix(0, nrow = dim(weights_a)[1], ncol = dim(weights_a)[2])
  old_weight_b <- matrix(0, nrow = dim(weights_b)[1], ncol = dim(weights_b)[2])
  #weights_a <- matrix(c(0.25),nrow = 3, ncol=5)
  #weights_b <- matrix(c(0.25), nrow = 3, ncol=4)
  out = matrix(rep(0, dim(Yd)[1]), nrow=dim(Yd)[1], ncol=dim(Yd)[2])
  max_err <- 1e-5;
  gl <- 0
  min_val_error <- 999
  error_val <- NULL
  
  net_out <- first_phase(x, Yd, weights_a, weights_b)
  En <- erro_tot(net_out$err, x)
  error_vec <- c(En)
  
  while(En >= max_err && epoch < max_epoch){
    epoch = epoch + 1
    
    #transforms to matrixes
    der <- dEt_dx_new(x, Yd, weights_a, weights_b, 0.8)
    #saida: uma list (dEt_da, dEt_db)
    dEt_da <- der[[1]]
    dEt_db <- der[[2]]
    
    try_a <- new_weight_m(alpha, dEt_da , weights_a, old_weight_a)
    try_b <- new_weight_m(alpha, dEt_db, weights_b, old_weight_b)
    
    net_out <- first_phase(x, Yd, try_a, try_b)
    Y <- net_out$Yin
    En <- erro_tot(net_out$err, x)
    
    
    old_weights_a <- weights_a
    old_weights_b <- weights_b
    weights_a <- try_a
    weights_b <- try_b
    
    # validation phase
    net_out_val_error <- first_phase(validation, validationYd, weights_a, weights_b)$err
    En_val <- erro_tot(net_out_val_error, x)
    min_val_error <- min(En_val, min_val_error)
    error_val <- append(En_val, error_val)
    # generalization loss
    gl <- 100*((En_val/min_val_error)-1)
    
    print(paste("Erro", En, "I", epoch, "alpha", alpha, "GL", gl))
    alpha <- alpha * (error_vec[length(error_vec)]/En)
    error_vec <- append(error_vec, En)
    out = Y
  }
  list(out=out, A=weights_a, B=weights_b, alpha=alpha, error_vec=error_vec, error_val=error_val)
}

erro_tot <- function(err, x){
  sum(quad_err(err)/2)/dim(x)[1]
}
