#term frequency
# times the term <term> occured in document <document>
tf <- function(term, document){
  #separates all terms from the document
  words <- split_terms(document)

  document_frequency = as.data.frame(table(words))
  #find the frequency of the given term
  term_position = which(document_frequency$Var1 == term
  #localize it in the original data_frame
  term_frequency = document_frequency[term_position, ]$Freq
}

tf_idf <- function(corpus, final_matrix){
  words  = unlist(strsplit(corpus, ' '))
  all_terms = table(words)
  names(all_terms)
  n_doc = length(corpus)

  for document in corpus{
    for term in all_terms {
      tf_idf <- tf(term, document) * log(n_doc/)
    }
  }
}


binary_matrix <- function(corpus){
  all_terms <- unique(split_terms(corpus))
  binary <- data.frame(row.names = all_terms)
  for(document in corpus){

    doc_terms <- split_terms(document)
    doc_terms_in_all_terms <- match(all_terms, doc_terms)
    binary_terms <- as.numeric(!is.na(doc_terms_in_all_terms))


    binary <- cbind(binary, binary_terms)
  }
}

split_terms <- function(char_vector){
  unlist(strsplit(char_vector, " "))
}


  idf <- function(term, corpus){
    #nro de documenttos / documentos em que o termo aparece
    log(ndoc/docs(term))
  }

