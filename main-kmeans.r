source('~/workspace/dataMining20NewsGroup/kmeans.r')

kmeans_test <- my_kmeans(tf_idf, 20 , myCor, 0.002)
groups <- as.factor(kmeans_test$groups)