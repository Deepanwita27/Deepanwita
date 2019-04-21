library(caret)
library(ISLR)

set.seed(123) 
tc = trainControl(method = "cv", number = 10,verboseIter = T)
A = data.frame(Hitters)
A
A1 = na.omit(A)
A1
model1=train(Salary~AtBat+Hits+ League ,method ="glmnet",data = A1, trControl = tc,tuneGrid = expand.grid(alpha = 1,lambda = seq(0,100,length=100)))
varImp(model1)
model1$bestTune

