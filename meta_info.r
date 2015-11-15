

#Termos usados. Freq < 5 e Freq > 5_942
tf <- findFreqTerms(tf_idf, 5, 600)

#save the frequency and sort it
freq <- rowSums(as.matrix(tf_idf_reduced))
o_freq <- sort(freq, decreasing=TRUE)
length(freq) #18_038 termos para 0.997
wf <- data.frame(word=names(o_freq), freq=o_freq)


#gráfico de barras que mostra o termo e sua frequencia no copus
library(ggplot2)
p <- ggplot(subset(wf, freq>200), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#gráfico de bolinhas que mostra a frequencia dos termos no corpus (Zipft)
plot(wf$freq)
