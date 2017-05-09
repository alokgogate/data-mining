library(FSelector)

#Complete Dataset
inputData <- read.csv("C:/Users/Sukankana/Documents/DataSets/ChurnDataset.csv",header = T)

#Imputing Missing Values with roughfix
inputData.roughfix <- na.roughfix(inputData)
inputData.roughfix$churn <- as.character(input.roughfix$churn)
inputData.roughfix$churn <- as.factor(input.roughfix$churn)


#Using Dataset cleaned by Boruta
inputData <-final_data


#Calculating importance of the variables using inputData.roughfix
weights <- information.gain(churn~., inputData.roughfix)

#Calculating importance of the variables using Dataset cleaned by Boruta
weights <- information.gain(churn~., inputData)

#Print the weights
print(weights)

#Selecting variables with importance more than 0.15
subset <- cutoff.k.percent(weights, 0.15)
print(subset)

