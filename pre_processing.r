library(SnowballC)

getClasses <- function(original_files){
  classes <- original_files
  classes <- gsub("[0-9]", "", classes)
  classes <- gsub("/", "", classes)
  classes <- gsub(".news-bydatenews-bydate-train", "", classes)
}

#Tokenizing
tokenizing <- function(original_files, concatenated_files){
  # if the merged dataset does exist, append to it
  for (file in original_files){
    text <-readLines(file, encoding="latin1")
    
    
    #finds where the number of lines are in the file
    nLine_line <- grep("Lines", text)
    final_line <- as.integer(sub("Lines: ", "", text[nLine_line]))
    #threads the 'dog' case
    if(is.na(final_line[1])){
      final_line <- 26
    }
    if(final_line < length(text)){
      header_line <- length(text) - final_line
      #removes the meta-info headers
      text <- text[header_line:final_line]
    }
    
    #removes replies
    #replied_lines <- grep("^>", text)
    #if(length(replied_lines) > 0){
    #  text <- text[-replied_lines]
    #}

    #change the list to an array with all the chars
    formatted_text = paste(unlist(text), collapse=' ')
    #Tokenizing
    #lower case all strings AND replacwith 'D'
    formatted_text = gsub("[0-9]", " ", tolower(formatted_text))
    formatted_text = gsub("_", " ", formatted_text)

    #replace special characters with empty space
    formatted_text = gsub("\\W", " ", formatted_text)

    concatenated_files <- append(concatenated_files, formatted_text)
  }
  concatenated_files
}

#StopWords
#only remove stopWords from a given char vector.
remove_stop_words <- function(vectorized_text){
  english_stop_words = c('i','my','myself','we','us','our','ours','ourselves','you','your','yours','yourself','yourselves','he','him','his','himself','she',
    'her','hers','herself','it','its','itself','they','them','their','theirs','themselves','what','which','who','whom','this','that','these','those','am',
    'is','are','was','were','be','been','being','have','has','had','having','do','does','did','doing','would','could','should','ought','might','however',
    'will','would','shall','should','can','could','may','might','must','ought','im','youre','hes','shes', 'its','were', 'theyre','ive', 'youve','weve',
    'theyve', 'id','youd','hed','shed','wed','theyd','ill','youll', 'hell','shell','well','theyll','isn','aren','wasn','weren','hasn', 'haven',
    'hadn','doesn','don','didn','won','wouldn','shan','shouldn','cant','cannot','couldn','mustn','let','thats','whos','whats','heres',
    'theres','whens','wheres','whys','hows','a','an','the','and','but','if','or','because','as','until','while','of','at','by','for','with',
    'about','against','between','into','through','during','before','after','above','below','to','from','up','down','in','out','on','off','over',
    'under','again','further','then','once','here','there','when','where','why','how','all','any','both','each','few','more','most','other','some',
    'such','no','nor','not','only','own','same','so','than','too','very','one','every','least','less','many','now','ever','never','say','says','said',
    'also','get','go','goes','just','made','make','put','see','seen','whether','like','well','back','even','still','way','take','since','another',
    'however','two','three','four','five','first','second','new','old','high','long','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o',
    'p','q','r','s','t','u','v','x','w','y','z','re',"","ll","co","uk","subject", "edu","therefore", "fromyour", "himto", "com", "write", "post", "article",
    "from", "lines", "organization", "summary", "keywords", "writing", "\"")
  removed_stop_words = match(vectorized_text, english_stop_words)
  #Words removed will have a match in the array, words that does not appear will always be NA.
  remaning_words = vectorized_text[is.na(removed_stop_words)]
  remaning_words
}

#Stemmer e StopWords
stemming <- function(tokenized_text, concatenated_files) {
  for (file in tokenized_text){
    #separe the original vector in a char vector, then unlist to be able to use the stopWords and stem
    vectorized_text = unlist(strsplit(file, " "))

    smaller_text <- remove_stop_words(vectorized_text)

    final_text = wordStem(smaller_text)

    concatenated_files <- append(concatenated_files, paste(final_text, collapse=" "))
  }
  concatenated_files
}
