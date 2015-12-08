m <- apply(q$Yin, 1, which.max)
d <- factor(rep(c("A","B","C"), 10), levels = c("A","B","C","D","E"))
b <- factor(rep(c("A","B","C"), 10))
print(table(b, d), zero.print = ".")