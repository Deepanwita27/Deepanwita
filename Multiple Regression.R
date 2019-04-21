#Multiple Regression:
  
library(psych)
A=read.csv(file.choose())
head(A)

B=na.omit(A)
pairs.panels(A)
model1=lm(PROFIT~RND+ADMIN+MKT,data=B)
summary(model1)

model2=lm(PROFIT~RND+MKT,data=B)
summary(model2)

model3=lm(PROFIT~RND+ADMIN,data=B)
summary(model3)

model4=lm(PROFIT~RND,data=B)
summary(model4)


#Multiple regressions when a variable is categorical

head(A)
B=na.omit(A)
pairs.panels(A)
TrainingSet=B[1:40,]
TestSet=B[41:50,]
model_cat=lm(PROFIT~RND+MKT+STATE,data=TrainingSet)
summary(model_cat)
predict(model_cat,TestSet)





