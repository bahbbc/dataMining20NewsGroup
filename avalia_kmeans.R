# Avaliação do kmeans
p = factor(r$groups, levels=c(1:20), labels = sort(unique(classes)))
compare <- table(p, classes)

comp2 <- gsub("comp.graphics", "computer", comp2)
comp2 <- gsub("comp.os.ms-windows.misc", "computer", comp2)
comp2 <- gsub("comp.sys.ibm.pc.hardware", "computer", comp2)
comp2 <- gsub("comp.sys.mac.hardware", "computer", comp2)
comp2 <- gsub("comp.windows.x", "computer", comp2)
comp2 <- gsub("misc.forsale", "forsale", comp2)
comp2 <- gsub("rec.autos", "rec", comp2)
comp2 <- gsub("rec.motorcycles", "rec", comp2)
comp2 <- gsub("rec.sport.baseball", "rec", comp2)
comp2 <- gsub("rec.sport.hockey", "rec", comp2)
comp2 <- gsub("talk.politics.misc", "talk", comp2)
comp2 <- gsub("talk.politics.guns", "talk", comp2)
comp2 <- gsub("talk.politics.mideast", "talk", comp2)
comp2 <- gsub("sci.crypt", "sci", comp2)
comp2 <- gsub("sci.electronics", "sci", comp2)
comp2 <- gsub("sci.med", "sci", comp2)
comp2 <- gsub("sci.space", "sci", comp2)
comp2 <- gsub("talk.politics.misc", "politics", comp2)
comp2 <- gsub("talk.politics.guns", "politics", comp2)
comp2 <- gsub("talk.politics.mideast", "politics", comp2)
comp2 <- gsub("talk.religion.misc", "religion", comp2)
comp2 <- gsub("alt.atheism", "religion", comp2)
comp2 <- gsub("soc.religion.christian", "religion", comp2)

p2 = factor(tt$groups, levels=c(1:6), labels = sort(unique(comp2)))
compare2 <- table(p2, comp2)

# SOMA A
sum_a <- sum(diag(compare))

# SOMA B
sum_b <- upper.tri(compare)

# SOMA C
sum_c <- lower.tri(compare)

# SOMA D


rand <- (sum_a + sum_d)/(sum_a + sum_b + sum_c + sum_d)
jaccard <- sum_a/(sum_a + sum_b + sum_c)
folkes_mallows <- sqrt((sum_a/(sum_a + sum_b)) * (sum_a/(sum_a + sum_c)))

