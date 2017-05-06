#Load the data from csv file 

#Split into 80:20 ratio

library(dplyr)
set.seed(123)
train <-sample_frac(data,0.8)
test <-sample_frac(data,0.2)


#R-Part

rpart_tree <-rpart(formula = churn~.,data=train,method='class')
summary(rpart_tree)
plot(rpart_tree)
text(rpart_tree)

pred <-predict(rpart_tree,newdata = test)

comp <-cbind(pred,test$churn)
