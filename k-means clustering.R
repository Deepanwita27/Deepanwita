# https://www.quora.com/How-can-we-choose-a-good-K-for-K-means-clustering

#=========KMEANS CREDIT==========

A1 = data.frame(Credit)
A = na.omit(A1[,c(2:7,12)])

#--k2--

m1 = kmeans(A,3)
plot(A$Age,A$Income,col = m1$cluster)
plot(A$Age,A$Limit,col = m1$cluster)
plot(A$Age,A$Rating,col = m1$cluster)
points(x = m1$centers[,5],y = m1$centers[,3],col = 2,pch = 2,cex = 1,lwd = 4)
