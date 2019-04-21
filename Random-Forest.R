library(randomForest)

A = data.frame(iris)
A1 = na.omit(A)

str(A1)
levels(A1$Species)

sf = sample(2,nrow(A1), replace = TRUE, prob = c(0.8,0.2))

trd = A1[sf==1,]
tsd = A1[sf==2,]

model2 = randomForest(Species ~ .,data = iris)
?randomForest

model2

tsp = predict(model2,newdata=tsd,type="Class")

trp = predict(model2,newdata=trd,type="Class")

model2$confusion

varImpPlot(model2)
