#Load the data from csv file 

#Split into 80:20 ratio

library(dplyr)
set.seed(123)
train <-sample_frac(data,0.8)
test <-sample_frac(data,0.2)


#R-Part
  #make churn a factor to predict 
  test$churn <-as.factor(test$churn)
  #R-Part using method="class"
  rpart_tree <-rpart(formula = churn~.,data=train,method='class')
  summary(rpart_tree)
  plot(rpart_tree)
  text(rpart_tree)

  pred <-predict(rpart_tree,newdata = test,type = "class")
  conf<-confusionMatrix(pred,test$churn)
  #comp <-cbind(pred,test$churn)
  #R-part with twicking some parameters
  
  rpart_tree_1 <-rpart(churn~.,data=train,method="class",control = rpart.control(minsplit = 2,cp=0))
  pred_1 <-predict(rpart_tree_1,newdata = test,type="class")
  #comp_1 <-cbind(pred_1,test$churn)
  
  conf_1<-confusionMatrix(pred_1,test$churn)
