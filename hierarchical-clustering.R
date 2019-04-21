#=========Hierarchical Clustering==========

library(ISLR)
A = data.frame(Credit)
A1 = A[1:100,]
model = hclust(dist(A1),method = "complete")

plot(model)
A1$clusters = as.factor(cutree(model,k=4))
A1
plot(A1$Age,A1$Limit,col = A1$clusters)

#=========KMEANS ==========

library(ISLR)
B = data.frame(Credit)
B1 = B[1:100,-c(8:11)] #-c remove factor 
model = kmeans(B1,4)
B1$clusters = model$cluster
plot(B1$Age,B1$Limit,col = B1$clusters)

par(mfrow=c(2,1)) # mfrow - to divide the graph

plot(B1$Age,B1$Limit,col = B1$clusters) #Kmeans
plot(A1$Age,A1$Limit,col = A1$clusters) # Hierarchical
