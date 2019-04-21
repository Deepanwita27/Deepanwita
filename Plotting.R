#Plotting

library(ISLR)
library(sqldf)

A = na.omit(EMP$DEPARTMENT_ID)
A
hist(A)

hist(A,labels = "DIDNO")
hist(A,labels = A)

data("Wage")
A = data.frame(Wage)
A

colnames(A)
hist(A$race)
hist(A$age)
hist(A$wage)
hist(A$wage,main = "Wages of employees")

head(A)
A1 = as.factor(A$race)
A1
A1 = as.numeric(A1)
A1
str(A)
hist(A1)

EMP$DEPARTMENT_ID
A = EMP

A = sqldf("select * from EMP")
A
A = sqldf("select DEPARTMENT_ID,SUM(SALARY) from EMP GROUP BY DEPARTMENT_ID")
A

barplot(A$`SUM(SALARY)`,names.arg = A$DEPARTMENT_ID)
