# Missing Value

library(psych)
A = data.frame(iris)
A[1,1]=NA
A[2,2]=NA
A[3,3]=NA
head(A)
str(A)
#Finding strong co-relation 
pairs.panels(A)

#without using psych we can do this way also
#A1=na.omit(A)
#cor(A1[,1:4])

#----------TREATMENT FOR petal length-------------

is.na(A$Petal.Length)

#P1 is a training data to find missing value
P1 = A[is.na(A$Petal.Length)==TRUE,]
#P2 is a test data
P2 = A[is.na(A$Petal.Length)==FALSE,]
nrow(P1)
nrow(P1)

m1 = lm(Petal.Length ~ Petal.Width,data = P2)
P3 = predict(m1,P1)

P1[,3]=P3[1]

#addind the missing value
P4 = sqldf::sqldf("SELECT * FROM P1 union all select * from P2")
nrow(P4)

#-----------TREATMENT FOR sepal length------------

is.na(P4$Sepal.Length)

Q1 = P4[is.na(P4$Sepal.Length)==TRUE,]
Q2 = P4[is.na(P4$Sepal.Length)==FALSE,]

m2 = lm(Sepal.Length ~ Petal.Width+Petal.Length,data = Q2)
Q3 = predict(m2,Q1)

Q1[,1]=Q3[1]

Q4 = sqldf::sqldf("SELECT * FROM Q1 union all select * from Q2")

#----------TREATMENT FOR sepal wedth-------------

is.na(Q4$Sepal.Width)

R1 = Q4[is.na(Q4$Sepal.Width)==TRUE,]
nrow(R1)
R2 = Q4[is.na(Q4$Sepal.Width)==FALSE,]
nrow(R2)

m3 = lm(Sepal.Width ~ Petal.Width+Petal.Length+Sepal.Length,data = R2)
R3 = predict(m3,R1)
nrow(R3)

R1[,1]=R3[1]

R4 = sqldf::sqldf("SELECT * FROM R1 union all select * from R2")
nrow(R4)



