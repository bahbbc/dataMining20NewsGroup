library(Matrix)
source('~/workspace/dataMining20NewsGroup/separate_data.r')
source('~/workspace/dataMining20NewsGroup/mlp_batelada.r')

matrix_reduced <- cbind(t(tf_idf_matrix), 1:18846)
classes_bin <- binary_class(classes)$binary_data
classes_bin <- cbind(classes_bin, 1:18846)

total_data <- separate_data(matrix_reduced)
test_data <- total_data$test
validation_data <- total_data$validation
training_data <- total_data$training

# compares the class identificator with the attributes identificator.
validationYd <- classes_bin[intersect(validation_data[,dim(validation_data)[2]], classes_bin[,dim(classes_bin)[2]]),]
# removes both identificators
validationYd <- validationYd[,-dim(validationYd)[2]]
validationx <- Matrix(validation_data[,-dim(validation_data)[2]], sparse = TRUE)

trainingYd <- classes_bin[intersect(training_data[,dim(training_data)[2]], classes_bin[,dim(,dim(classes_bin)[2])[2]]),]
# removes both identificators
trainingYd <- trainingYd[,-dim(trainingYd)[2]]
trainingx <- Matrix(training_data[,-dim(training_data)[2]], sparse = TRUE)

test <-mlp_batelada(3000, trainingx, trainingYd, validationx, validationYd, 5, 30)
# t <- mlp_batelada(50, x, classes, 1, 3)
plot(test$error_vec, type="o")
lines(test$error_val, type="o", pch=22, lty=2, col="red")