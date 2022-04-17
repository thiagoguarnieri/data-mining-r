#SCRIPT TO EMPLOY DATA CLUSTERING
#--------------------------------------------------------------------------------
data = iris
#--------------------------------------------------------------------------------
#normalizing attributes (max min). Ensure the same contribution of all attributes
for(i in c(1:4)){
  data[,i] = (data[,i]-min(data[,i]))/(max(data[,i])-min(data[,i]))
}

#encoding nominal attributes. You can use nominal attributes through binarization
#otherwise, simple ignore nominal attributes
dataCluster = model.matrix(~.+0,data = data)
#--------------------------------------------------------------------------------
#evaluating the optimal number of clusters through elbow method (1 to 30)
k.max <- 30
wss <- sapply(1:k.max, function(k){kmeans(dataCluster, k, iter.max = 80, nstart = 3)$tot.withinss})
plot(1:k.max, wss,type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")

#other way:
#best K is the one that maximize distance to the line which intersect the first and last point of the previous graph
distances = data.frame("k" = integer(28),"distance" = integer(28))
c = 1
for(cnt in 2:(k.max-1)){
  distances[c,]$k = cnt
  distances[c,]$distance = abs((wss[k.max] - wss[1]) * cnt - (k.max - 1) * wss[cnt] + k.max * wss[1] - wss[k.max] * 1)/(sqrt((wss[k.max] - wss[1])^2 + (k.max - 1)^2))
  c = c + 1
}
print(paste("the best k is", distances$k[distances$distance == max(distances$distance)]))
#--------------------------------------------------------------------------------
#cluster using the optimal value (the one that maximizes distance)
clusterCenters = kmeans(dataCluster, distances$k[distances$distance == max(distances$distance)], iter.max = 80, nstart = 3)
rawData = iris
rawData$Clusters = clusterCenters$cluster
