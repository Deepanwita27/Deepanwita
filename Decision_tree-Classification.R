#library(tree)

#A = data.frame(iris)
#A1 = na.omit(A)
#str(A1)
#levels(A1$Species)

#sf = sample(2,nrow(A1),replace = TRUE,prob = c(0.7,0.3))
#trd = A1[sf==1,]
#tsd = A1[sf==2,]

#model1 = tree(Species ~ .,data = trd)

#plot(model1)
#text(model1)

#pred = predict(model1,tsd)
#cbind(pred,tsd$Species)



library(MASS)

A = data.frame(Cars93)
A1 = na.omit(A)

str(A1)
levels(A1$AirBags)

sf = sample(2,nrow(A1), replace = TRUE, prob = c(0.7,0.3))

trd = A1[sf==1,]
tsd = A1[sf==2,]

model1 = tree(AirBags~.-Make-Model,data=trd) # we donts consider factors at decision tree (-Make-Model)

plot(model1)
text(model1)

pred = predict(model1,tsd)

cbind(pred, tsd$AirBags)
table(pred)
table(A$AirBags)


