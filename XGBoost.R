#XGBoost

library(xgboost)
library(Matrix)
A = read.csv(file.choose())
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.8,0.2))
trd = A[sf==1,]
tsd = A[sf==2,]
sparse_matrix_trd = sparse.model.matrix(PROFIT ~ .-1, data = trd)
sparse_matrix_tsd = sparse.model.matrix(PROFIT ~ .-1, data = tsd)
label_trd = trd$PROFIT
label_tsd = tsd$PROFIT

model = xgboost(sparse_matrix_trd,label = label_trd,nrounds = 20)


#pred = 
#cbind()