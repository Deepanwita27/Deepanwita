Data<- read.csv('svmdata.csv')

install.packages('e1071')
library(e1071)

# SVM function for model fitting

#fit <-svm (Data[,1:2], Data[,3], type='C', kernel='linear')
fit <-svm(x = Data[,1:2], y = Data[,3], kernel = 'linear')
summary(fit)

# Prediction based on fitted model
# predicted= predict(fit,x_test)