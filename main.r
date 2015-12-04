library(Matrix)

source('~/workspace/dataMining20NewsGroup/pre_processing.r')
source('~/workspace/dataMining20NewsGroup/tf_idf_matrix.r')
source('~/workspace/dataMining20NewsGroup/separate_data.r')
source('~/workspace/dataMining20NewsGroup/mlp_batelada.r')

# Gets all the files and saves in 2 vectors (class and text)
# Does the pre-processing
# First, tokenizing then stopWords and stemming
folders = list.dirs('~/workspace/dataMining20NewsGroup/20news-bydate/20news-bydate-train')

file_names = list.files(folders, full.names=TRUE, pattern='[0-9]')

classes <- getClasses(file_names)
classes <- gsub("/home/barbara/workspace/dataMining20NewsGroup/20news-bydate/20news-bydate-train/", "", file_names)
classes <- gsub("[0-9]", "", classes)

print("Realizando o Tokenizing")
tokenized_text <- tokenizing(file_names, c())

print("Removendo StopWords e realizando o stemming")
stemmed_files <- stemming(tokenized_text, c())

print("Matriz TF-IDF! -- Isso pode demorar um pouco...")
b_matrix <- binary_matrix(stemmed_files)
tf_matrix <- tf_matrix(stemmed_files)

#remove termos mais e menos frequentes
freq <- rowSums(tf_matrix[[1]])
freq <- freq[freq > 60] # mudar aqui termos menos frequentes
freq <- freq[freq < 2000] # mudar aqui termos mais frequentes

tf_matrix <- tf_matrix[[1]][freq,]

# soma as linhas da matrix binaria para obter todos os docs de um termo
idf <- log(18846/rowSums(b_matrix[[1]]))
tf_idf_matrix <- tf_matrix * idf

# faz o tf_idf normalizado
tf_idf_norm <- tf_idf_matrix/sqrt(rowSums(tf_idf_matrix)^2)

#tf_idf_reduced <- removeSparseTerms(tf_idf, 0.9995) # This makes a matrix that is 99.6% empty space, maximum. Remove termos pouco frequentes do corpus.
rm(list=(c('file_names', 'stemmed_files', 'tokenized_text')))
gc()

matrix_reduced <- cbind(t(tf_idf_norm), 1:18846)
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