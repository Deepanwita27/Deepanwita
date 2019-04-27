install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)


A=data.frame(iris)
A=na.omit(A)
nc=unlist(lapply(A, is.numeric))
B=A[,nc]


#species is a target variable/ need to predict. Now it become a classification prob
model1=prcomp(B) #prcomp run background to findout prpportion of variable
model1

psych::pairs.panels(model1$x)
str(model1)
plot(model1$x[,1],model1$x[,2])
ggbiplot(model1)
plot(model1)

pred = stats::predict(model1,A)
pred = data.frame(pred,A$Species)
pred
dim(pred)

Q = naivebayes::naive_bayes(pred$A.Species ~ PC1 + PC2,data = pred)

QP = stats::predict(Q,pred)
cbind(QP,pred$A.Species)
table(QP,pred$A.Species)


#--------------------------------------------------------#

A=data.frame(MASS::Cars93)
A=na.omit(A)
nc=unlist(lapply(A, is.numeric))
B=A[,nc]
model1=prcomp(B) 

plot(model1$x[,4],model1$x[,3])
ggbiplot(model1)



