library(h2o)
h2o.init(nthreads = -1)
train_data <- Training_Data_Complete
splits <- h2o.splitFrame(train_data, 0.75, seed=42)

deep_learning_model <- h2o.deeplearning(x=3:21, y="status_group",training_frame=splits[[1]], 
                                        validation_frame = splits[[2]],distribution="multinomial",
                                        activation = "MaxoutWithDropout",hidden = c(285,285,285,285),
                                        input_dropout_ratio = 0.2,hidden_dropout_ratios = c(0.1,0.1,0.1,0.1),
                                        adaptive_rate=TRUE,sparse = TRUE,l1 = 1e-5,l2 = 1e-5,epochs = 20,
                                        nfolds = 5,fold_assignment="Modulo")

performance = h2o.performance(deep_learning_model, valid=TRUE ) 
print(performance)
confusion <- h2o.confusionMatrix(performance)

h2o.mse(deep_learning_model, xval = TRUE)
pred <- h2o.predict(deep_learning_model, newdata = Testing_Data)
head(pred)
s1 <- as.data.frame(pred$predict[1:14850])

accuracy <- mean(pred$predict == testdata$status_group)
accuracy