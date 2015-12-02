source('~/workspace/dataMining20NewsGroup/separate_data.r')
source('~/workspace/dataMining20NewsGroup/mlp_batelada.r')

total_data <- separate_data(tf_idf_reduced)
test_data <- total_data$test
validation_data <- total_data$validation
training_data <- total_data$training

validation_data <- remove_class(validation_data)
validationYd <- validation_data$class
validationx <- validation_data$attributes
validationx <- matrix(unlist(validationx), ncol= dim(validationx)[2], byrow=TRUE)
validationYd <- binary_class(validationYd)

training_data <- remove_class(training_data)
trainingYd <- training_data$class
trainingx <- training_data$attributes
trainingx <- matrix(unlist(trainingx), ncol= dim(trainingx)[2], byrow=TRUE)
trainingYd <- binary_class(trainingYd)

test <-mlp_batelada(100000, trainingx, trainingYd, validationx, validationYd, 5, (2*dim(trainingYd)[2]+1))

plot(test$error_vec, type="o")
lines(test$error_val, type="o", pch=22, lty=2, col="red")