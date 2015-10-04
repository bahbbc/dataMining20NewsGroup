getClasses <- function(original_files){
  classes <- original_files
  classes <- gsub("[0-9]", "", classes)
  classes <- gsub("/", "", classes)
  classes <- gsub(".news-bydatenews-bydate-train", "", classes)
}

tokenizing <- function(original_files, concatenated_files){
  # if the merged dataset does exist, append to it
  for (file in original_files){
    text <-read.table(file, sep="\n", quote="", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

    #change the list to a vector with all the chars
    formatted_text = paste(unlist(text), collapse='')
    #Tokenizing
    #lower case all strings AND replace all digits with 'D'
    formatted_text = gsub("[0-9]", " ", tolower(formatted_text))

    #replace special characters with empty space
    formatted_text = gsub("\\W", " ", formatted_text)
    formatted_text = gsub("\\[^a-z]", "", formatted_text)

    concatenated_files <- append(concatenated_files, formatted_text)
  }
  concatenated_files
}

#Stemmer
library(SnowballC)

stemming <- function(concatenated_files) {}
  for (file in tokenized_text){
    #separe the original vector in a char vector, than unlist to be able to use the stem
    vectorized_text = wordStem(unlist(strsplit(file, " ")))

    concatenated_files <- append(concatenated_files, paste(vectorized_text, collapse=" "))
  }
  concatenated_files
}

# Gets all the files and saves in 2 vectors (class and text)

folders = list.dirs('./20news-bydate/20news-bydate-train')

file_names = list.files(folders, full.names=TRUE, pattern='[0-9]')

classes <- getClasses(file_names)

tokenized_text <- tokenizing(file_names, c())

stemmed_files <- stemming(c())

#StopWords



