library(rpart)
library(MASS)
data.train.decision <- Training_Data_Complete
data.test.decision <- Testing_Data

ind <- sample(2, nrow(Training_Data_Complete), replace = TRUE, prob= c(0.9,0.1))
data.train.decision <- Training_Data_Complete[ind==1,]
data.test.decision <- Training_Data_Complete[ind==2,]

Decisionfit		<- rpart(status_group ~amount_tsh	+ funder + gps_height +	longitude +	latitude + basin + region +	population + public_meeting +	permit +	extraction_type_class + management + payment + water_quality + quantity + source + source_class + waterpoint_type + OperationAge,	
                      data	=data.train.decision,	
                      method	=	 'class',
                      parms	=	list(split	=	"information"), 
                      control=rpart.control(minsplit=5, cp=0.000075, maxdepth = 30))

DecisionPrediction = predict(Decisionfit, data.test.decision, type='class')

DecisionAccuracy <- mean(DecisionPrediction == data.test.decision[,21])*100
DecisionAccuracy

confusion <- table(DecisionPrediction, data.test.decision[,21])
confusionMatrix(confusion)
