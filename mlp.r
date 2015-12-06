source('~/workspace/dataMining20NewsGroup/helper_functions,r')
source('~/workspace/dataMining20NewsGroup/calc_grad_batelada.r')

mlp <- function(max_err, max_epoch, x, Yd, alpha, hidden_layers){
  epoch <- 0
  err_tot <- 999
  weights_a <- random_weights(x[1,], hidden_layers, length(x[1,])+1)
  weights_b <- random_weights(x[1,], dim(Yd)[2], hidden_layers+1)
  out = matrix(rep(0, length(x)), nrow=dim(Yd)[1], ncol=dim(Yd)[2])

  while(err_tot >= max_err && epoch <= max_epoch){
    epoch = epoch + 1
    En <- 0
    net_out <- first_phase(x, Yd, weights_a, weights_b)
    #saida: uma list (Zin, Z, Yin, err, Y)
    Y <- net_out$Yin
    #entradas: (x, Zin, Z, Yin, err, N, alpha, weights_a, weights_b)
    der <- dEt_dx_bat(x, Yd, weights_a, weights_b)
    #saida: uma list (new_a, new_b)
    weights_a <- new_weights[[1]]
    weights_b <- new_weights[[2]]
    
    out[i,] = Y
    En <- append(quad_err(net_out$err)/dim(x)[1])
    print(paste("Erro", En, "I", epoch))
  }
  list(out, weights_a, weights_b)
}