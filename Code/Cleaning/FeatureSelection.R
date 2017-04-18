library(FSelector)

inputData <- read.csv("C:/Users/Sukankana/Documents/DataSets/ChurnDataset.csv",header = T)
weights <- information.gain(churn~., inputData)
print(weights)
subset <- cutoff.k.percent(weights, 0.18)
print(subset)