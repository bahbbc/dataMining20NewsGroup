source('~/workspace/dataMining20NewsGroup/helper_functions.r')

my_kmeans <- function(data, k, distFun, error) {
  #tf_idf = t(tf_idf)
  centroids <- data[sample(nrow(data)), ]
  centroids <- centroids[1:k,]
  
  centroid_stabilized <- 999
  epoch = 0
  while(centroid_stabilized > error){
    epoch <- epoch + 1
    #using euclidian distance
    #euclidian <- data - centroids
    #distance <- sqrt(euclidian * euclidian)
    #shortest_distance <- apply(distance, 1, which.min)
    
    #using cosine distance
    distance <- cosine_distance(data, centroids)
    
    distance[is.na(distance)] <- 0
    # pos of elements with shortest dist
    clusters <- apply(distance, 1, which.min)
    clusters <- unlist(clusters)
    
    #recalculate the centroids pos
    new_centroids = NULL
    for(i in 1:k){
      cluster_elem <- which(clusters == i)
      new_centroids <- rbind(new_centroids, colSums(data[cluster_elem,])/length(cluster_elem))
    }
    # if there isn't elements for all centroids...
    if(length(table(clusters)) < k){
      # sorts centroids again
      print("...")
      centroids <- data[sample(nrow(data)), ]
      # take the initial k elements as centroids
      centroids <- centroids[1:k,]
      next
    }
    old_centroids <- centroids
    centroids <- new_centroids
    centroid_stabilized <- sum(abs(centroids - old_centroids))
    print(paste('E', epoch, 'distance', centroid_stabilized))
  }
  
  list(groups=clusters, centroids=centroids)
}

cosine_distance <- function(x, y){
  #distanceMatrix <- Matrix(NA, nrow=dim(x)[1], ncol=dim(y)[1], sparse = TRUE)
  distanceMatrix <- matrix(NA, nrow=dim(x)[1], ncol=dim(y)[1])
  
  for(i in 1:nrow(y)){
    distanceMatrix[,i] <- as.matrix(unlist(1-(x %*% y[i,])/(norm(x) * norm(as.matrix(y[i,])))))
  }
  distanceMatrix
}

correlation_distance <- function(points1, points2) {
  return(1 - ((cor(t(points1), t(points2))+1)/2))
}

euclidian_distance <- function(points1, points2) {
  distanceMatrix <- Matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1], sparse = TRUE)
  for(i in 1:nrow(points2)) {
    distanceMatrix[,i] <- sqrt(rowSums(t(t(points1)-points2[i,])^2))
  }
  distanceMatrix
}