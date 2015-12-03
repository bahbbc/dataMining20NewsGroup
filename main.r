library(bigmemory)

source('~/workspace/dataMining20NewsGroup/pre_processing.r')
source('~/workspace/dataMining20NewsGroup/tf_idf_matrix.r')

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

# soma as linhas da matrix binaria para obter todos os docs de um termo
idf <- log(18846/rowSums(b_matrix[[1]]))
tf_idf_matrix <- tf_matrix[[1]] * idf

#tf_idf_reduced <- removeSparseTerms(tf_idf, 0.9995) # This makes a matrix that is 99.6% empty space, maximum. Remove termos pouco frequentes do corpus.
rm(list=(c('file_names', 'stemmed_files', 'tokenized_text')))
gc()