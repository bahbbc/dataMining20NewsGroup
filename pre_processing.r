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
    formatted_text = paste(unlist(text), collapse=)
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

#StopWords
english_stop_words = c('i','my','myself','we','us','our','ours','ourselves','you','your','yours','yourself','yourselves','he','him','his','himself','she',
  'her','hers','herself','it','its','itself','they','them','their','theirs','themselves','what','which','who','whom','this','that','these','those','am',
  'is','are','was','were','be','been','being','have','has','had','having','do','does','did','doing','would','could','should','ought','might','however',
  'will','would','shall','should','can','could','may','might','must','ought','im','youre','hes','shes', 'its','were', 'theyre','ive', 'youve','weve',
  'theyve', 'id','youd','hed','shed','wed','theyd','ill','youll', 'hell','shell','well','theyll','isnt','arent','wasnt','werent','hasnt', 'havent',
  'hadnt','doesnt','dont','didnt','wont','wouldnt','shant','shouldnt','cant','cannot','couldnt','mustnt','lets','thats','whos','whats','heres',
  'theres','whens','wheres','whys','hows','a','an','the','and','but','if','or','because','as','until','while','of','at','by','for','with',
  'about','against','between','into','through','during','before','after','above','below','to','from','up','down','in','out','on','off','over',
  'under','again','further','then','once','here','there','when','where','why','how','all','any','both','each','few','more','most','other','some',
  'such','no','nor','not','only','own','same','so','than','too','very','one','every','least','less','many','now','ever','never','say','says','said',
  'also','get','go','goes','just','made','make','put','see','seen','whether','like','well','back','even','still','way','take','since','another',
  'however','two','three','four','five','first','second','new','old','high','long','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o',
  'p','q','r','s','t','u','v','x','w','y','z','re')



#Stemmer
library(SnowballC)

stemming <- function(concatenated_files) {}
  for (file in tokenized_text){
    #separe the original vector in a char vector, then unlist to be able to use the stem
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




