install.packages("tree")
library(tree)
library(ISLR)
library(psych)
## Decision tree for Regression tree
A = data.frame(Hitters)

# no need to refrence if attached

# create a model where salary is linear function of Years and Hits

model1 = tree(Salary~Years+Hits,A)


# plot your model and put decision texts
plot(model1)
text(model1)
summary(model1)
model1

# Create partitions
partition.tree(model1)
