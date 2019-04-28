#Association Mining

library(arules)
data(Groceries) 
head(Groceries)
summary(Groceries)
inspect(Groceries[1:10])
itemFrequencyPlot(Groceries,support = 0.1)
itemFrequencyPlot(Groceries,topN = 10)

# Implement apriori
model = apriori(Groceries)
inspect(model)
# Implement apriori with support=0.01, confidence=0.5
model1 <- apriori(Groceries, parameter=list(support=0.01, confidence=0.58))
inspect(model1[])
summary(model1)

# Sort model1
model2=sort(model1,by="confidence")
inspect(model2[])
# plot model1
plot(model2) # default shading = "lift" 

#subset -- findout specifically for yogurt
yog = subset(model1, items %in% "yogurt")
inspect(yog)
