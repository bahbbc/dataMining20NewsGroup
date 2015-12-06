#term frequency
# times the term <term> occured in document <document>
tf <- function(term, document){
  #separates all terms from the document
  words <- split_terms(document)

  document_frequency = as.matrix(table(words))
  #find the frequency of the given term
  term_position = which(document_frequency$Var1 == term)
  #localize it in the original data_frame
  term_frequency = document_frequency[term_position, ]$Freq
}

find_binary_terms <- function(document, all_terms){
  doc_terms <- split_terms(document)
  doc_terms_in_all_terms <- match(all_terms, doc_terms)
  binary_terms <- as.numeric(!is.na(doc_terms_in_all_terms))
}

binary_matrix <- function(corpus){
  all_terms <- unique(split_terms(corpus))
  binary_terms <- find_binary_terms(corpus[1], all_terms)
  binary <- Matrix(binary_terms, sparse = TRUE)
  
  for(i in 2:length(corpus)){
    binary_terms <- find_binary_terms(corpus[i], all_terms)

    binary <- cbind(binary, binary_terms)
  }
  list(binary, all_terms)
}

tf_matrix <- function(corpus){
  all_terms <- unique(split_terms(corpus))
  tf_terms <- term_frequency_row(corpus[1], all_terms)
  tf_matrix <- Matrix(tf_terms, sparse = TRUE)
  for(i in 2:length(corpus)){
    tf <- term_frequency_row(corpus[i], all_terms)
    
    tf_matrix <- cbind(tf_matrix, tf)
  }
  list(tf_matrix, all_terms)
}

term_frequency_row <- function(document, all_terms){
  words <- split_terms(document)
  
  term_frequency <- as.matrix(table(words))
  doc_terms_in_all_terms <- match(all_terms, words)
  doc_terms_in_all_terms[is.na(doc_terms_in_all_terms)] <- 0
  doc_terms_in_all_terms[doc_terms_in_all_terms != 0] <- term_frequency
  doc_terms_in_all_terms
}

split_terms <- function(char_vector){
  unlist(strsplit(char_vector, " "))
}


