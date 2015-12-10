source('~/workspace/dataMining20NewsGroup/calc_grad_batelada.r')

weights_a <- test_gaus$A
weights_b <- test_gaus$B

test_phase <- first_phase(testx, testYd, weights_a, weights_b)
# Pega o maior numero dos resultados e usa como o indicador da classe
results <- apply(test_phase$Yin, 1, which.max)
# Faz o mesmo para os resultados originais
expected_results <- apply(testYd, 1, which.max)

compare <- table(results, expected_results)

