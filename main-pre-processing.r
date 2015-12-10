library(Matrix)

source('~/workspace/dataMining20NewsGroup/pre_processing.r')
source('~/workspace/dataMining20NewsGroup/tf_idf_matrix.r')

# Pega todos os arquivos e salva em 2 vetores (classes e texto)
# Realiza o pre-processamento
# Primeiro, tokenizing e depois stopWords e stemming
folders = list.dirs('~/workspace/dataMining20NewsGroup/20news-bydate/20news-bydate-train')

file_names = list.files(folders, full.names=TRUE, pattern='[0-9]')

print("Realizando o Tokenizing")
tokenized_text <- tokenizing(file_names, c())

print("Removendo StopWords e realizando o stemming")
stemmed_files <- stemming(tokenized_text, c())

print("Matriz TF-IDF! -- Isso pode demorar um pouco...")
b_matrix <- binary_matrix(stemmed_files)
tf_matrix <- tf_matrix(stemmed_files)

# Remove termos mais e menos frequentes
freq <- rowSums(tf_matrix[[1]])
tf_matrix <- tf_matrix[[1]][freq < 2000,] # mudar aqui termos mais frequentes
reduced_b_matrix <- b_matrix[[1]][freq < 2000,] # é necessario mudar nas 2 matrizes
freq <- rowSums(tf_matrix)
tf_matrix <- tf_matrix[freq > 60,] # mudar aqui termos menos frequentes
reduced_b_matrix <- reduced_b_matrix[freq > 60,] # é necessario mudar nas 2 matrizes


# Soma as linhas da matrix binaria para obter todos os docs de um termo
idf <- log(18846/rowSums(reduced_b_matrix))
tf_idf_matrix <- tf_matrix * idf

print("Salvando arquivo com as matrizes de representação...")
writeMM(tf_idf_matrix, "tf_idf-2.mtx")
#str(tf_idf <- readMM("~/tf_idf.mtx"))

# Faz o tf_idf normalizado
tf_idf_norm <- tf_idf_matrix/sqrt(rowSums(tf_idf_matrix)^2)
writeMM(tf_idf_norm, "tf_idf_norm-2.mtx")
#str(tf_idf_norm <- readMM("~/tf_idf_norm.mtx"))

rm(list=(c('file_names', 'stemmed_files', 'tokenized_text')))
gc()