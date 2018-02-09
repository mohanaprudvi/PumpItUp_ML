# PumpItUp

                                                  READ ME 
------------------------------------------------------------------------------------------------------
------------------------------------------ ML 6375 Machine Learning---------------------------------
-------------------------------------Pump It Up:DataMining the WaterTable-----------------------------

Instructions to run the code

We are totally submitting 6 codes

->preProcessPumpItUp.r-This is the preprocess code
->XGBoostPumpItUp.r-Code for XGBOOST classifier
->DecisionTreePumpItUp-Code for Decision Tree Classifier
->RandomForestPumpItUp-Code for RandomForest Classifier
->DeepLearningPumpItUp-Code for DeepLearning Classifier
->projectSubmitted-the final code which is submitted for competition

Before running the files load and if necessary install the required packages for the libraries
mentioned below

library(xgboost)
library(MASS)
library(caret)
library(h2o)
library(ggplot2)
library(randomForest)
library(rpart)

If the packages are not present, then they can be installed by follwing commands in R console

install.packages("library name")

Step 1:- Run preProcessPumpItUp.r file

	This will preprocess the data and will create the training and testing data frames.

step 2:- Run the classifier files.

	 Each Classifier file will output the CrossValidation accuracy and the confusion matrix
	 of the respective Model in the console.

	 Random Forest - RandomForestPumpItUp.r
	 Decision Tree - DecisionTreePumpItUp.r
	 XGBoost       - XGBoostPumpItUp.r
	 Deep Learning - DeepLearningPumpItUp.r

 
The other file projectSubmitted.r contains the combined files of the classifiers that we have submitted
for the turnItIn.

To run the projectSubmitted.r file, load the above mentioned libraries first and then run the entire file.

In the above  mentioned files, we used XGBoost accuracy as it is the highest for our dataset, for submitting
to the data driven competition.

It outputs the respective csv files that needs to be uploaded to the competition for all classifiers.
The csv file associated with the XGBoost gets the highest testing accuracy. 
