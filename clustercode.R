Data<- read.csv('C:/Ankur/data.csv')

install.packages('cluster')
library(cluster)

cl<-kmeans(Data[,2:3],5)


plot(Data[,2], Data[,3],  xlab="X", ylab="Y")
ind<-which(cl$cluster==1)
points(Data[ind,2], Data[ind,3], col="blue", xlab="X", ylab="Y")

ind<-which(cl$cluster==2)
points(Data[ind,2], Data[ind,3], col="red", xlab="X", ylab="Y")

ind<-which(cl$cluster==3)
points(Data[ind,2], Data[ind,3], col="green", xlab="X", ylab="Y")

ind<-which(cl$cluster==4)
points(Data[ind,2], Data[ind,3], col="yellow", xlab="X", ylab="Y")

ind<-which(cl$cluster==5)
points(Data[ind,2], Data[ind,3], col="orange", xlab="X", ylab="Y")

cl$betweenss
cl$withinss