library(tm)

source('/media/barbara/326AA9966AA9577F/EACH/Mestrado/Mineração de dados/dataMining20NewsGroup/pre_processing.r')
#source('/media/barbara/326AA9966AA9577F/EACH/Mestrado/Mineração de dados/dataMining20NewsGroup/tf_idf_matrix.r')

# Gets all the files and saves in 2 vectors (class and text)
# Does the pre-processing
# First, tokenizing then stopWords and stemming
folders = list.dirs('./20news-bydate/20news-bydate-train')

file_names = list.files(folders, full.names=TRUE, pattern='[0-9]')

classes <- getClasses(file_names)

print("Realizando o Tokenizing")
tokenized_text <- tokenizing(file_names, c())

print("Removendo StopWords e realizando o stemming")
stemmed_files <- stemming(tokenized_text, c())

print("Matriz TF-IDF!")
corpus <- Corpus(VectorSource(stemmed_files))
#tf_idf <- TermDocumentMatrix(corpus,control = list(bounds = list(global = c(5, 70))))
tf_idf <- TermDocumentMatrix(corpus, control = list(bounds = list(global = c(5, 70)), removePunctuation = TRUE, stopwords = TRUE, removeNumbers= TRUE))

#document_term <- tf_idf(stemmed_files)
tf <- findFreqTerms(tf_idf, 1, Inf)

freq <- colSums(as.matrix(tf_idf))
o_freq <- sort(freq), decreasing=TRUE)
length(freq)

library(ggplot2)
p <- ggplot(subset(wf, freq>50), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p


test <- removeSparseTerms(tf_idf, 0.5) # This makes a matrix that is 10% empty space, maximum.
