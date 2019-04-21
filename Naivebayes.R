install.packages("naivebayes")
install.packages("e1071")
library(naivebayes)
library(e1071)
library(MASS)

A = data.frame (Cars93)
A1 = na.omit(A)
A1=A[, c(-1,-2,-3,-10,-11,-16,-26,-27)]

A1

sf = sample(2,nrow(A1),replace = TRUE,prob = c(0.7,0.3))

trd = A1[sf == 1,]
tsd = A1[sf == 2,]

#mod1 = naive_bayes(AirBags ~ .,data = trd)
mod1 = naive_bayes(AirBags ~ .-Min.Price,data = trd)

pred = predict(mod1, tsd)

W = cbind(pred,tsd$AirBags)
W
table(pred,tsd$AirBags)

#Type

A = data.frame (Cars93)
A1 = na.omit(A)
A1=A[, c(-1,-2,-9,-10,-11,-16,-26,-27)] #remove categorical variable

A1

sf = sample(2,nrow(A1),replace = TRUE,prob = c(0.6,0.4))

trd = A1[sf == 1,]
tsd = A1[sf == 2,]

#mod1 = naive_bayes(Type ~ .,data = trd)
mod1 = naive_bayes(Type ~ Min.Price+Price+Max.Price+MPG.city+MPG.highway,data = trd)
#mod1 = naive_bayes(Type ~ .-Min.Price,data = trd)

pred = predict(mod1, tsd)
pred

W = cbind(pred,tsd$Type)
W
table(pred,tsd$Type)
