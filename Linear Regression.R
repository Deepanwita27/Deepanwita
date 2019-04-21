#Linear Regression

A = read.csv(file.choose())
head(A)
cor(A$PROFIT,A$RND)
cor(A$PROFIT,A$ADMIN)
cor(A$MKT,A$PROFIT)

model1 = lm(PROFIT ~ RND, data = A)

summary(model1)

RND = c(160000,170000,180000)

TBP = data.frame(RND)

Q = predict(model1,TBP)

plot(A$RND,A$PROFIT)

abline(model1)
