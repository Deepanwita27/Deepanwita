#Ridge/Lasso/ElasticNet:
  
#Ridge:  
install.packages("caret")
install.packages("glmnet")

library(caret)
library(glmnet)
library(ISLR)

set.seed(123) 
tc = trainControl(method = "cv", number = 10,verboseIter = T)
#A = read.csv(file.choose())
A = data.frame(Hitters)
A1 = na.omit(A)


model1 = train(Salary ~ AtBat + Hits + League ,method = "glmnet",data = A1, trControl = tc,tuneGrid = expand.grid(alpha = 0,lambda = seq(0,1,length=20)))

varImp(model1)
model1$bestTune
model1$results   #manually check minimum value of MAE


#Lasso:
model1 = train(Salary ~ AtBat + Hits + League ,method = "glmnet",data = A1, trControl = tc,tuneGrid = expand.grid(alpha = 1,lambda = seq(0,1,length=20)))


#ElasticNet:
model1 = train(Salary ~ AtBat + Hits + League ,method = "glmnet",data = A1, trControl = tc,tuneGrid = expand.grid(alpha = seq(0,1,length=20),lambda = seq(0,100,length=100)))

#value between 0-0.5  Ridge
0.5 -> Lasso
