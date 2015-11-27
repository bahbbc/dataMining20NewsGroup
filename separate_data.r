# Separates the data in training, test and validation
# only receives a matriz or a dataframe
# TODO: Falta colocar aleatoriedade no conjunto

separate_data <- function(data, class){
  
  size <- dim(data)[1]
  training_size <- floor(size * 0.5)
  test_size <- floor(size * 0.25)
  validation_size <- ceiling(training_size + test_size)
  training <- data[1:training_size,]
  test <- data[(training_size + 1):(test_size + training_size),]
  validation <- data[(validation_size + 1):size,]
  
  list(training=training, validation=validation, test=test)
}