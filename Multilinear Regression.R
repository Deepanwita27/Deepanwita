# Multilinear Regression

A = read.csv(file.choose())

model2 = lm(A$Profit ~ A$R.D.Spend + A$Administration + A$Marketing.Spend + A$State)
summary(model2)
model3 = lm(A$Profit ~ A$R.D.Spend + A$Administration + A$Marketing.Spend)
summary(model3)
model4 = lm(A$Profit ~ A$R.D.Spend + A$Marketing.Spend)
summary(model4)
model5 = lm(A$Profit ~ A$R.D.Spend)
summary(model5)

M = cor(A$R.D.Spend,A$Profit)

TBP = data.frame(RND = c(325349.2,345349.2),MKT = c(266897,296897))






