source('/media/barbara/326AA9966AA9577F/EACH/Mestrado/Mineração de dados/dataMining20NewsGroup/pre_processing.r')

# Gets all the files and saves in 2 vectors (class and text)

folders = list.dirs('./20news-bydate/20news-bydate-train')

file_names = list.files(folders, full.names=TRUE, pattern='[0-9]')

classes <- getClasses(file_names)

print("Realizando o Tokenizing")
tokenized_text <- tokenizing(file_names, c())

print("Removendo StopWords e realizando o stemming")
stemmed_files <- stemming(tokenized_text, c())
