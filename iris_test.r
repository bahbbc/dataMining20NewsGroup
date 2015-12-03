library(Matrix)
source('~/workspace/dataMining20NewsGroup/separate_data.r')
source('~/workspace/dataMining20NewsGroup/mlp_batelada.r')

matrix_reduced <- cbind(t(tf_idf_matrix), 1:18846)
classes <- binary_class(classes)
classes <- cbind(classes, 1:18846)

total_data <- separate_data(matrix_reduced)
test_data <- total_data$test
validation_data <- total_data$validation
training_data <- total_data$training

# compares the class identificator with the attributes identificator.
validationYd <- classes[intersect(validation_data[,dim(validation_data)[2]], classes),]
# removes both identificators
validationYd <- validationYd[,-dim(validationYd)[2]]
validationx <- Matrix(validation_data[,-dim(validation_data)[2]], sparse = TRUE)

trainingYd <- classes[intersect(training_data[,dim(training_data)[2]], classes[,21]),]
# removes both identificators
trainingYd <- trainingYd[,-dim(trainingYd)[2]]
trainingx <- Matrix(training_data[,-dim(training_data)[2]], sparse = TRUE)

test <-mlp_batelada(100000, trainingx, trainingYd, validationx, validationYd, 5, (2*dim(trainingYd)[2]+1))

plot(test$error_vec, type="o")
lines(test$error_val, type="o", pch=22, lty=2, col="red")