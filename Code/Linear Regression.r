#to load the file into R
library(readr)
Bigger_data_set_test <- read_csv("~/Desktop/Bigger_data_set_test.csv")

#this command simply attaches the column name as a variable (for simplicity)
attach(Bigger_data_set_test)

#creating a dataframe data2 after the data is loaded

data2 = data.frame(attempt_Mean,blck_dat_Mean,avgqty,complete_Mean,custcare_Mean,churn)

#creating training and sample sets

split <-sample(seq_len(nrow(data2)),size=floor(0.75*nrow(data2)))
trainData <-data2[split, ]
testData <-data2[-split, ]
head(trainData)
head(testData)

#creating linear model with multiple dimensions

predictionModel <-lm(churn~avgqty+complete_Mean+attempt_Mean,data=trainData)

prediction <-predict(predictionModel,newdata=testData)

head(prediction)
head(testData$churn)
#The code below is used to check performance of the model 
plot(predictionModel)

#creating linear model using 1 dimension 

predictionModel <-lm(churn~avgqty,data=trainData)
prediction <-predict(predictionModel,newdata=testData)

head(prediction)
head(testData$churn)

predictionModel <-lm(churn~complete_Mean,data=trainData)
prediction <-predict(predictionModel,newdata=testData)

head(prediction)
head(testData$churn)

predictionModel <-lm(churn~complete_Mean+avgqty,data=trainData)
prediction <-predict(predictionModel,newdata=testData)

head(prediction)
head(testData$churn)

#you can do the following command after training every model to check stats of the lm model 
summary(predictionModel)
