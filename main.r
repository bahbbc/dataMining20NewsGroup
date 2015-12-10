library(Matrix)

source('~/workspace/dataMining20NewsGroup/pre_processing.r')
source('~/workspace/dataMining20NewsGroup/separate_data.r')
source('~/workspace/dataMining20NewsGroup/mlp_batelada.r')

str(tf_idf_norm <- readMM("~/tf_idf_norm-2.mtx"))
str(tf_idf <- readMM("~/tf_idf-2.mtx"))

folders = list.dirs('~/workspace/dataMining20NewsGroup/20news-bydate/20news-bydate-train')

file_names = list.files(folders, full.names=TRUE, pattern='[0-9]')

classes <- getClasses(file_names)
classes <- gsub("/home/barbara/workspace/dataMining20NewsGroup/20news-bydate/20news-bydate-train/", "", file_names)
classes <- gsub("[0-9]", "", classes)

matrix_reduced <- cbind(t(tf_idf), 1:18846)
backup_classes <- cbind(classes, 1:18846)
classes <- binary_class(classes)
classes <- cbind(classes, 1:18846)

total_data <- separate_data(matrix_reduced)
test_data <- total_data$test
validation_data <- total_data$validation
training_data <- total_data$training

## VALIDATION
# compares the class identificator with the attributes identificator.
validationYd <- classes[intersect(validation_data[,dim(validation_data)[2]], classes[,dim(classes)[2]]),]
# removes both identificators
validationYd <- validationYd[,-dim(validationYd)[2]]
validationx <- Matrix(validation_data[,-dim(validation_data)[2]], sparse = TRUE)

## TRAINING
trainingYd <- classes[intersect(training_data[,dim(training_data)[2]], classes[,dim(classes)[2]]),]
# removes both identificators
trainingYd <- trainingYd[,-dim(trainingYd)[2]]
trainingx <- Matrix(training_data[,-dim(training_data)[2]], sparse = TRUE)

## TEST
testYd <- classes[intersect(test_data[,dim(test_data)[2]], classes[,dim(classes)[2]]),]
# removes both identificators
testYd <- testYd[,-dim(testYd)[2]]
testx <- Matrix(test_data[,-dim(test_data)[2]], sparse = TRUE)

# Treina a rede
test <-mlp_batelada(5000, trainingx, trainingYd, validationx, validationYd, 5, 30)

# Plota a curva de erros
plot(test$error_vec, type="o")
lines(test$error_val, type="o", pch=22, lty=2, col="red")