source('~/workspace/dataMining20NewsGroup/separate_data.r')
source('~/workspace/dataMining20NewsGroup/mlp_batelada.r')

total_data <- separate_data(iris)
binary_class(class_data)