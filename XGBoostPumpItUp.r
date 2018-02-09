library(xgboost)
library(caret)
data.train <- Training_Data_Complete
data.test <- Testing_Data

ind <- sample(2, nrow(Training_Data_Complete), replace = TRUE, prob= c(0.9,0.1))
data.train <- Training_Data_Complete[ind==1,]
data.test <- Training_Data_Complete[ind==2,]

data.train <- sapply(data.train, as.numeric)
data.train[,21] <- data.train[,21] - 1
matrix<-data.matrix(data.train)

data.test <- sapply(data.test, as.numeric)
data.test[,21] <- data.test[,21] - 1
testMatrix <- data.matrix(data.test)
testData <- xgb.DMatrix(testMatrix[,1:20])

Xtreme <- xgboost(data = matrix[,1:20],
                  nfold =5,
                  label = matrix[,21],
                  max_depth = 21,
                  eta = 0.02,
                  nthread = 12,
                  objective = "multi:softmax",
                  num_class =3,
                  subsample = 0.7,
                  colsample_bytree = 0.5,
                  min_child_weight = 3,
                  nrounds = 200, 
                  maximize = FALSE)

pred <- round(predict(Xtreme, testData))

XGAccuracy <- mean(pred==testMatrix[,21])*100
XGAccuracy

confusion <- table(pred, testMatrix[,21])
confusionMatrix(confusion)