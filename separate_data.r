# Separates the data in training, test and validation
# only receives a matriz or a dataframe
# TODO: Falta colocar aleatoriedade no conjunto

separate_data <- function(data){
  data <- data[sample(nrow(data)), ]
  size <- dim(data)[1]
  training_size <- floor(size * 0.5)
  test_size <- floor(size * 0.25)
  validation_size <- ceiling(training_size + test_size)
  training <- data[1:training_size,]
  test <- data[(training_size + 1):(test_size + training_size),]
  validation <- data[(validation_size + 1):size,]
  
  list(training=training, validation=validation, test=test)
}

remove_class <- function(data){
  attributes <- data[,-dim(data)[2]]
  class <- data[,-dim(data)[2]]

  list(attributes=attributes, class=class)
}

binary_class <- function(class_data){
  class_data <- as.vector(unlist(class_data))
  classes <- unlist(unique(class_data))
  class_matrix <- class_data
  for (class in 1:length(classes)){
    binary_class <- rep(0, length(classes))
    binary_class[class] <- 1
    binary_class <- paste(binary_class, collapse= "")
    # to use gsub every binary has to be a string
    class_matrix <- gsub(classes[class], binary_class, class_matrix)
  }
  #transforms all the strings to an integer matrix
  binary_data = matrix(as.integer(unlist(strsplit(class_matrix, ""))), nrow=length(class_data), ncol=length(classes), byrow=TRUE)
  binary_data=binary_data
}