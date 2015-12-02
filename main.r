library(tm)
library(bigmemory)

source('~/workspace/dataMining20NewsGroup/pre_processing.r')

# Gets all the files and saves in 2 vectors (class and text)
# Does the pre-processing
# First, tokenizing then stopWords and stemming
folders = list.dirs('~/workspace/dataMining20NewsGroup/20news-bydate/20news-bydate-train')

file_names = list.files(folders, full.names=TRUE, pattern='[0-9]')

classes <- getClasses(file_names)

print("Realizando o Tokenizing")
tokenized_text <- tokenizing(file_names, c())

print("Removendo StopWords e realizando o stemming")
stemmed_files <- stemming(tokenized_text, c())

print("Matriz TF-IDF!")
corpus <- Corpus(VectorSource(stemmed_files))
tf_idf_test <- TermDocumentMatrix(corpus,control = list(bounds = list(global = c(5, 50))))
#tf_idf <- TermDocumentMatrix(corpus, control = list(bounds = list(global = c(5, 70)), removePunctuation = TRUE, stopwords = TRUE, removeNumbers= TRUE))
#tf_idf <- TermDocumentMatrix(corpus)

#tf_idf_reduced <- removeSparseTerms(tf_idf, 0.9995) # This makes a matrix that is 99.6% empty space, maximum. Remove termos pouco frequentes do corpus.
rm(list=(c('corpus', 'file_names', 'stemmed_files', 'tokenized_text')))
gc()