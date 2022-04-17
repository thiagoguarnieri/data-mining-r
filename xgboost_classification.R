#SCRIPT TO EMPLOY CLASSIFICATION
library("xgboost")
library(data.table)
library("caret")
library("ROSE")
#--------------------------------------------------------------------------------
data = iris
#--------------------------------------------------------------------------------
#Split train-test
set.seed(45678)
rowsTrain <- sample(nrow(data), round(nrow(data) * 0.7), replace = FALSE)
training =  data[rowsTrain,];
test = data[-rowsTrain,];

#hot encoding
labels <- training[["Species"]] 
ts_label <- test[["Species"]]
new_tr <- model.matrix(~.+0,data = training[,-length(training)])
new_ts <- model.matrix(~.+0,data = test[,-length(training)])

#coding classes
labels <- as.numeric(labels)-1
ts_label <- as.numeric(ts_label)-1

#coded dataset
dtrain <- xgb.DMatrix(data = new_tr,label = labels)
dtest <- xgb.DMatrix(data = new_ts,label= ts_label)
#--------------------------------------------------------------------------------
#Model training-evaluation"
params <- list(booster = "gbtree", objective = "multi:softmax", eta=0.1, gamma=0, max_depth=7, min_child_weight=1, subsample=1, colsample_bytree=1)

#binary:
#params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.1, gamma=0, max_depth=7, min_child_weight=1, subsample=1, colsample_bytree=1)

#training
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 450, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, maximize = F, num_class = 3)

#binary
#xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 450, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, maximize = F , eval_metric = "error")

#teste
xgbpred <- predict (xgb1,dtest)

#binary
#xgbpred <- ifelse (xgbpred > 0.5,1,0)
#--------------------------------------------------------------------------------
print(confusionMatrix(as.factor(ts_label), mode="prec_recall", as.factor(xgbpred)))
