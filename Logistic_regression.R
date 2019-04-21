library(ISLR)

A = data.frame(Hitters) #create data frame
A = na.omit(A)  # remove null value
str(A) # check data type

A$NewLeague1 = ifelse(A$NewLeague == "A", 1 , 0 ) # convert NewLeague from A/N to 1/0

sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3)) #create sampling

trd = A[sf == 1,] # training data set
tsd = A[sf == 2,] # test data set

model1 = glm(NewLeague1 ~ AtBat + League ,data = trd,family = "binomial") #create model, family indicate this is logistic regression

TRP = predict(model1,tsd,type = "response") # create prediction model type = "response" for probability value

TRP1 = ifelse(TRP > 0.5,"A","N") # distribute value A/N

W = data.frame(tsd$AtBat,tsd$League,tsd$NewLeague,TRP1) # compare actual vs predictive value

table(W$tsd.NewLeague,W$TRP1)  # create confusion matrix

W
