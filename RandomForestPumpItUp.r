library(randomForest)
library(mlbench)
library(caret)
library(party)
set.seed(42)

TrainingDataComplete2 <- Training_Data_Complete

ind <- sample(2, nrow(TrainingDataComplete2), replace = TRUE, prob= c(0.9,0.1))
data.train.forest <- TrainingDataComplete2[ind==1,]
data.test.forest <- TrainingDataComplete2[ind==2,]

model_forest <- randomForest(data.train.forest$status_group~.,data = data.train.forest, ntree = 15, mtry = 10, nodesize = 2)

#tuning <- tuneRF(TrainingDataComplete2[,1:19], TrainingDataComplete2[,20], stepFactor = 2, improve = 0.05, trace = TRUE, doBest = TRUE, ntreeTry = 1)

forest_prediction <- predict(model_forest, data.test.forest)
pred <- predict(model_forest, data.test.forest)

forest_accuracy <- mean(forest_prediction == data.test.forest$status_group)* 100
forest_accuracy

confusion <- table(pred, data.test.forest[,21])
confusionMatrix(confusion)
