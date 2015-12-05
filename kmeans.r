source('~/workspace/dataMining20NewsGroup/helper_functions.r')
my_kmeans <- function(data, k, error){
  
  # chooses k random column points from the dataset
  # those will be the initial group centroids
  
  # TODO: Remover elementos iguais
  
  # shuffles
  centroids <- data[sample(nrow(data)), ]
  #take the initial k elements as centroids
  centroids <- centroids[1:k,]
  
  #calculate each element distance to the centroid
  
 
  centroid_stabilized <- 999
  epoch = 0
  # while old_centroids = new_centroids
  while(centroid_stabilized > error){
    epoch <- epoch + 1
    #using euclidian distance
    #euclidian <- t(t(elements) - centroids)
    #distance <- sqrt(euclidian * euclidian)
    
    #using cosine distance
    distance <- data %*% t(centroids)/t((norm_row(centroids) %*% t(norm_row(data))))
    shortest_distance <- apply(distance, 1, which.max)
    
    #recalculate the centroids pos
    new_centroids = NULL
    for(i in 1:k){
      cluster_elem <- which(shortest_distance == i)
      new_centroids <- rbind(new_centroids, colSums(data[cluster_elem,])/length(cluster_elem))
    }
    old_centroids <- centroids
    centroids <- new_centroids
    centroid_stabilized <- abs(sum(centroids) - sum(old_centroids))
    print(paste('E', epoch))
  }
  list(groups=shortest_distance)
}

# r <- my_kmeans(x, 3, 0.002)
# groups <- as.factor(r$groups)
# plot(rowSums(x), pch=19, col=groups)