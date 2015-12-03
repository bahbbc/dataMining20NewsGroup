library(Matrix)

#save the term-frequency and sort it
#freq <- rowSums(tf_idf_matrix)
freq <- rowSums(tf_matrix[[1]])
names(freq) = tf_matrix[[2]]
o_freq <- sort(freq, decreasing=TRUE)
length(freq) #346_902 sem corte
wf <- data.frame(word=names(o_freq), freq=o_freq)

#gráfico de barras que mostra o termo e sua frequencia no copus
library(ggplot2)
p <- ggplot(subset(wf, o_freq), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#gráfico de bolinhas que mostra a frequencia dos termos no corpus (Zipft)
plot(wf$freq)
