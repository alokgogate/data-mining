split <-sample(seq_len(nrow(data2)),size=floor(0.75*nrow(data2)))
trainData <-data2[split, ]
testData <-data2[-split, ]
head(trainData)
head(testData)
predictionModel <-lm(churn~avgqty+complete_Mean+attempt_Mean,data=trainData)

prediction <-predict(predictionModel,newdata=testData)

head(prediction)
head(testData$churn)


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
