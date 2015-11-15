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
      new_weights <- grad(x[i,], Yd[i,], weights_a, weights_b, dim(x)[1], alpha)
      weights_a <- new_weights[[1]]
      weights_b <- new_weights[[2]]
      Y <- new_weights[[3]]
      error <- error + new_weights[[4]]
      out[i,] = Y
    #print(paste("E",error, "e", new_weights[[4]]))
    }
    err_tot <-error/dim(x)[1]
    print(paste("Erro",err_tot, "I", epoch, "out", out))
  }
  out
}
