library(Matrix)

#save the term-frequency and sort it
#freq <- rowSums(tf_idf_matrix)
freq <- rowSums(tf_matrix)
o_freq <- sort(freq, decreasing=TRUE)
length(freq) #346_902 sem corte

# gráfico que mostra as freq. mais comuns
plot(table(freq[freq > 15]))


#gráfico de bolinhas que mostra a frequencia dos termos no corpus (Zipft)
plot(wf$freq)
