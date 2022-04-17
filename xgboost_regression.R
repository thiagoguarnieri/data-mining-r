#SCRIPT TO EMPLOY REGREESSION
library("xgboost")
library(data.table)
library("caret")
library("ROSE")
library(FSelector)
#--------------------------------------------------------------------------------
data(Sacramento)
#selecting best attributes
att.scores <- information.gain(price ~ ., Sacramento)
dset <- cbind(Sacramento[,cutoff.biggest.diff(att.scores)],"price" = Sacramento$price)
#--------------------------------------------------------------------------------
#Split train-test
set.seed(45678)
rowsTrain <- sample(nrow(dset), round(nrow(dset) * 0.7), replace = FALSE)
training =  dset[rowsTrain,];
test = dset[-rowsTrain,];

#hot encoding
labels <- training[["price"]] 
ts_label <- test[["price"]]
new_tr <- model.matrix(~.+0,data = training[,-c(3)])
new_ts <- model.matrix(~.+0,data = test[,-c(3)])

#coded dataset
dtrain <- xgb.DMatrix(data = new_tr,label = labels)
dtest <- xgb.DMatrix(data = new_ts,label= ts_label)
#--------------------------------------------------------------------------------
#Model training-evaluation"
params <- list(booster = "gbtree", objective = "reg:squarederror", eta=0.1, gamma=0, max_depth=7, min_child_weight=1, subsample=1, colsample_bytree=1)

#training
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 450, watchlist = list(val=dtest,train=dtrain), print_every_n = 10, maximize = F)
 
#teste
xgbpred <- predict (xgb1,dtest)
#--------------------------------------------------------------------------------
mae = caret::MAE(ts_label,xgbpred)
rmse = caret::RMSE(ts_label,xgbpred)
cat("MAE: ", mae, " RMSE: ", rmse)
