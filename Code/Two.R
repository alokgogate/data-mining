#Loading new data set into R
library(readxl)
new <- read_excel("~/Desktop/new.xlsx")
attach(new)
#creating a new data frame for this 
data_uncleaned =data.frame(new)

#Cleaning script 
data =data.frame(replace(data_uncleaned,is.na(data_uncleaned),"999"))

#defining a correlation value variable corr
corr <-cor(data,churn)
View(corr)

#Decision tree example 
library(rpart)
target <-churn~.
dt <-rpart(target,data=data)
plot(dt)
text(dt)

#normalisation in R using max-min method 
#Function normalize can be reused 
normalize <-function(x){
   return((x-min(x))/(max(x)-min(x)))
}
normalized_data <-as.data.frame(lapply(data1,normalize))
plot(cor(normalized_data,normalized_data$churn))

#Normalization using Z-score (direct function)

Z_normalized_data <-as.data.frame(scale(data1))
plot(cor(Z_normalized_data,Z_normalized_data$churn))

