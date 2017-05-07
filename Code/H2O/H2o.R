install.packages("h2o")
library(h2o)

#Start h2o server
  h2o.init()
  #Better to have churn as a factor for prediction purpose and confusion matrix 
  
  train$churn <-as.factor(train$churn)
  test$churn <-as.factor(test$churn)
  
#convert existing data frames to h2o data frames 
  
  train_h2o <-as.h2o(train) #use the same train variable from previous code 
  test_h2o <-as.h2o(test)   #use the same test variable from previous code  
  
#Adding response variables and regressor variables 
response ="churn"
predictors <- setdiff(names(train_h20), response)


#First model using H2O
  m2 <- h2o.deeplearning(
    +     model_id="dl_model_faster", 
    +     training_frame=train_h2o, 
    +     validation_frame=test_h2o,
    +     x=predictors,
    +     y=response,
    +     hidden=c(32,32,32),                  ## small network, runs faster
    +     epochs=1000000,                      ## hopefully converges earlier...
    +     score_validation_samples=10000,      ## sample the validation dataset (faster)
    +     stopping_rounds=2,
    +     stopping_metric="misclassification", ## could be "MSE","logloss","r2"
    +     stopping_tolerance=0.01
    + )
#Summary of model 
summary(m2)

#Plot the roc curve of traininng set
plot(h2o.performance(m2))

#Plot the roc curve of test set 
plot(h2o.performance(m2,newdata=test_h2o))



#Shut down H2o

h2o.shutdown(prompt=FALSE)
