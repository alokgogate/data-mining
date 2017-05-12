setwd("C:/Users/ABS/Documents/R/churn-project/")

library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest) #random forest library
library(leaps)
# neural net libraries
library(MASS)
library(neuralnet)
library(readr)
library(boot)   #library used in cross validation
library(plyr)   # progress bar library
# -----------------------------------------------------------------
#               READ DATA AND CHECK FOR MISSING VALUES
# -----------------------------------------------------------------
# read 100k data - drop customer ID
final_data <- read.csv("final_csv.csv")
churn_100k <- final_data
churn_100k <- churn_100k[complete.cases(churn_100k),]
churn_100k <- churn_100k[, !names(churn_100k) %in% c("X","Customer_ID")]

# create subset of 1k for test purpose
# churn_10k <- subset(churn_100k[1:1000,])

# check for missing rows - if FALSE then make sure your workspace has final_data in it
# any(is.na(churn_10k))
any(is.na(churn_10k))

# -----------------------------------------------------------------
#             SCALE DATA, CREATE TEST/TRAIN SETS
# -----------------------------------------------------------------
# read variable names
allVars <- colnames(churn_1k)
allVars
predictorVars <- allVars[!allVars %in% "churn"]
predictorVars <- paste(predictorVars, collapse = "+")
predictorVars
form = as.formula(paste("churn~", predictorVars, collapse = "+"))
form

# normalise data - 100k
max = apply(churn_100k, 2, max)
min = apply(churn_100k, 2, min)
scaled <- as.data.frame(scale(churn_100k, center = min, scale = max-min))
# normalise data - 1k
max2 = apply(churn_1k, 2, max)
min2 = apply(churn_1k, 2, min)
scaled2 <- as.data.frame(scale(churn_1k, center = min2, scale = max2-min2))

# create training and test sets - 100k
index <- sample(1:nrow(churn_100k), 0.8 * nrow(churn_100k))
train_data <- scaled[index,]
test_data <- scaled[-index,]
# create training and test sets - 1k
index2 <- sample(1:nrow(churn_1k), 0.8 * nrow(churn_1k))
train_data2 <- scaled2[index,]
test_data2 <- scaled2[-index,]

# -----------------------------------------------------------------
#             CREATE REGRESSION MODELS FOR COMPARISON
# -----------------------------------------------------------------
# linear regression
lm.fit = lm(form, train_data[, !names(train_data) %in% "complete_Mean"])
summary(lm.fit)
lm.pred = predict(lm.fit, test_data)
lm.mse = sum((lm.pred - test_data$churn)^2)/nrow(test_data)
lm.mse

# logistic regression
glm.fit = glm(form, data = train_data, family = binomial(link='logit'))
summary(glm.fit)
# do predictions
glm.pred = predict(glm.fit, test_data, type = 'response')
glm.pred = ifelse(glm.pred >0.5, 1, 0)
# get accuracy and error percentage
glm.mse = sum((glm.pred - test_data$churn)^2)/nrow(test_data)
glm.mse
print(paste('Accuracy',1-glm.mse))
# plotting ROC curve
library(ROCR)
p <- predict(glm.fit, test_data, type="response")
pr <- prediction(p, test_data$churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main = "ROC Curve for Logistic Regression")
# get AUC value
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# -----------------------------------------------------------------
#                RF FOR VARIABLE IMPORTANCE GRAPHS
# -----------------------------------------------------------------
# random forest to determine important variables
randomForest = randomForest(form, data = churn_100k, importance = TRUE)
varImpPlot(randomForest, main = "Random Forest Variable Importance Plot")
importance(randomForest)

# -----------------------------------------------------------------
#                     NEURAL NETWORK MODEL
# -----------------------------------------------------------------
# build and plot neural net model
# config = predictor-hidden(2nd)-hidden(3rd)-output e.g. 13-4-2-1
set.seed(333)  #set seed to get same result every time

# DO NOT RE-COMPUTE NN UNLESS 100K DATA BEING USED
# IF NEW NN NEEDS TO BE PLOTTED, RUN NN2 CODE
nn <- neuralnet(form, hidden=1, linear.output = F, 
                       data = train_data, err.fct = "sse", 
                       act.fct = "logistic", algorithm = "rprop+", stepmax = 1e+06)
plot(nn)

# RUN ME IF YOU WANT TO TEST NEW NN
nn2 <- neuralnet(form, hidden=1, linear.output = F, 
                data = train_data2, err.fct = "sse", 
                act.fct = "logistic", algorithm = "rprop+", stepmax = 1e+05)
plot(nn2)

nn$weights     #weights for neural network model
nn$net.result  #output for each replication
nn$result.matrix #neural net result matrix
nn$covariate    #neural net covariates

# do predictions
nn.predictions <- compute(nn, test_data[, !names(test_data) %in% "churn"])
nn.predictions$net.result

# unscale predicted and actual values
nn.predictions <- nn.predictions$net.result*(max(churn_100k$churn)-min(churn_100k$churn))+min(churn_100k$churn)
test.actual = (test_data$churn)*(max(churn_100k$churn)-min(churn_100k$churn))+min(churn_100k$churn)
nn.predictions

# side by side comparison of actual and predicted values
nn.realVSpred = cbind(test.actual, nn.predictions)
nn.realVSpred

# find mean squared error
nn.mse <- sum((nn.predictions-test.actual)^2)/nrow(test_data)
nn.mse
# compare error with lm and glm
print(paste(lm.mse, glm.mse, nn.mse))

# plot error comparison graph
dev.off()
plot(test_data$churn, nn.predictions, col = 'red', main = c("Neural Network Model", "Actual vs. Predicted Values"), pch = 1, cex = 0.9, type = "p", xlab = "Actual", ylab = "Predicted")
abline(0,1, col = "black")

# -----------------------------------------------------------------
#               PERFORMANCE PLOTS + COMPARISON WITH GLM
# -----------------------------------------------------------------
# side by side comparison of NN and GLM
par(mfrow=c(1,2))

plot(test.actual,nn.predictions,col='red',main='Real vs Predicted NN', xlab = "Actual", ylab = "Predicted", pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test.actual,glm.pred,col='blue',main='Real vs Predicted GLM', xlab = "Actual", ylab = "Predicted", pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='GLM',pch=18,col='blue', bty='n', cex=.95)

# single graph comparison of NN and GLM
dev.off()
plot(test.actual,nn.predictions,col='red',main=c('Real vs Predicted', "NN vs GLM"), xlab = "Actual", ylab = "Predicted",pch=18,cex=0.7)
points(test.actual,glm.pred,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','GLM'),pch=18,col=c('red','blue'))

# -----------------------------------------------------------------
#                       CROSS VALIDATION
# -----------------------------------------------------------------
# cross validation for nn
set.seed(999)
nn.cv <- NULL
k <- 10
alpha <- 8

# cv for 100k
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  index <- sample(1:nrow(churn_100k),round(0.8*nrow(churn_100k)))
  train_data <- scaled[index,]
  test_data <- scaled[-index,]
  
  nn <- neuralnet(form,data=train_data,hidden=(nrow(train_data)/(alpha*94)),linear.output=F, stepmax = 1e+06)
  
  nn.predictions <- compute(nn, test_data[, !names(test_data) %in% "churn"])
  nn.predictions <- nn.predictions$net.result*(max(churn_1k$churn)-min(churn_1k$churn))+min(churn_1k$churn)
  
  test.actual <- (test_data$churn)*(max(churn_100k$churn)-min(churn_100k$churn))+min(churn_100k$churn)
  
  nn.cv[i] <- sum((test.actual - nn.predictions)^2)/nrow(test_data)
  
  pbar$step()
}
nn.cv

nn2.cv <- NULL
# cv for 1k
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  index2 <- sample(1:nrow(churn_1k),round(0.8*nrow(churn_1k)))
  train_data2 <- scaled2[index,]
  test_data2 <- scaled2[-index,]
  
  nn2 <- neuralnet(form,data=train_data2,hidden=(nrow(train_data2)/(alpha*94)),linear.output=F, stepmax = 1e+05)
  
  nn2.predictions <- compute(nn2, test_data2[, !names(test_data2) %in% "churn"])
  nn2.predictions <- nn2.predictions$net.result*(max(churn_1k$churn)-min(churn_1k$churn))+min(churn_1k$churn)
  
  test2.actual <- (test_data2$churn)*(max(churn_1k$churn)-min(churn_1k$churn))+min(churn_1k$churn)
  
  nn2.cv[i] <- sum((test2.actual - nn2.predictions)^2)/nrow(test_data2)
  
  pbar$step()
}
nn2.cv

# calculate average mse
nn.cvAvg = mean(nn.cv)
nn.cvAvg

nn2.cvAvg = mean(nn2.cv)
nn2.cvAvg

# plot cv error as box plot
boxplot(nn2.cv,xlab='CV MSE',col='cyan',
        border='blue',names='CV Error (MSE)',
        main='Cross Validation Error for NN', horizontal=TRUE)

# -----------------------------------------------------------------
# PLOTS TO SEE WHICH VARIABLES AFFECT OUTPUT MOST
# ignore for now...need to test things here
# -----------------------------------------------------------------
par(mfrow = c(3,1))
gwplot(nn, selected.covariate = "mou_Mean", min = -5, max =5)
gwplot(nn, selected.covariate = "mou_Range", min = -5, max =5)
gwplot(nn, selected.covariate = "eqpdays", min = -5, max =5)