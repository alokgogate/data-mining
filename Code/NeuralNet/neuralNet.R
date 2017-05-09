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
churn_1k <- subset(churn_100k[1:1000,])

# check for missing rows - if FALSE then make sure your workspace has final_data in it
any(is.na(churn_1k))
any(is.na(churn_100k))

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

# normalise data
max = apply(churn_1k, 2, max)
min = apply(churn_1k, 2, min)
scaled <- as.data.frame(scale(churn_1k, center = min, scale = max-min))

# create training and test sets
index <- sample(1:nrow(churn_1k), 0.8 * nrow(churn_1k))
train_data <- scaled[index,]
test_data <- scaled[-index,]

# -----------------------------------------------------------------
#                CREATE LOG R MODEL FOR COMPARISON
# -----------------------------------------------------------------
# logistic regression
glm.fit = glm(form, data = train_data)
summary(glm.fit)
glm.pred = predict(glm.fit, test_data)
glm.mse = sum((glm.pred - test_data$churn)^2)/nrow(test_data)
glm.mse

# random forest to determine important variables
# TODO: ADD RANDOM FOREST MODEL HERE TO USE AS COMPARISON
randomForest = randomForest(churn ~., data = churn_1k, importance = TRUE)
varImpPlot(randomForest, main = "Random Forest Variable Importance Plot")
importance(randomForest)

# -----------------------------------------------------------------
#                     NEURAL NETWORK MODEL
# -----------------------------------------------------------------
# build and plot neural net model
# config = predictor-hidden(2nd)-hidden(3rd)-output e.g. 13-4-2-1
set.seed(333)  #set seed to get same result every time
nn <- neuralnet(formula = form, hidden = 2, linear.output = F, 
                data = train_data, err.fct = "sse", 
                act.fct = "logistic", algorithm = "rprop+", stepmax = 1e+06)
plot(nn)

nn$weights     #weights for neural network model
nn$net.result  #output for each replication
nn$result.matrix #neural net result matrix
nn$covariate    #neural net covariates

# do predictions
nn.predictions <- compute(nn, test_data[, !names(test_data) %in% "churn"])
nn.predictions$net.result

# unscale predicted and actual values
nn.predictions <- nn.predictions$net.result*(max(churn_1k$churn)-min(churn_1k$churn))+min(churn_1k$churn)
test.actual = (test_data$churn)*(max(churn_1k$churn)-min(churn_1k$churn))+min(churn_1k$churn)
nn.predictions

# side by side comparison of actual and predicted values
nn.realVSpred = cbind(test.actual, nn.predictions)
nn.realVSpred

# find mean squared error
nn.mse <- sum((nn.predictions-test.actual)^2)/nrow(test_data)
nn.mse
# compare error with lm and glm
print(paste(glm.mse, nn.mse))

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
# cross validation for glm
set.seed(666)
glm.fit <- glm(form,data=churn_1k)
glm.cv = cv.glm(churn_1k,glm.fit,K=10)$delta[1]
glm.cv

# cross validation for nn
set.seed(999)
nn.cv <- NULL
k <- 10

pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  index <- sample(1:nrow(churn_1k),round(0.8*nrow(churn_1k)))
  train_data <- scaled[index,]
  test_data <- scaled[-index,]
  
  nn <- neuralnet(form,data=train_data,hidden=2,linear.output=F, stepmax = 1e+06)
  
  nn.predictions <- compute(nn, test_data[, !names(test_data) %in% "churn"])
  nn.predictions <- nn.predictions$net.result*(max(churn_1k$churn)-min(churn_1k$churn))+min(churn_1k$churn)
  
  test.actual <- (test_data$churn)*(max(churn_1k$churn)-min(churn_1k$churn))+min(churn_1k$churn)
  
  nn.cv[i] <- sum((test.actual - nn.predictions)^2)/nrow(test_data)
  
  pbar$step()
}

nn.cv

# calculate average mse
nn.cvAvg = mean(nn.cv)
nn.cvAvg

# plot cv error as box plot
boxplot(nn.cv,xlab='CV MSE',col='cyan',
        border='blue',names='CV Error (MSE)',
        main='Cross Validation Error for NN', horizontal=TRUE)

-# -----------------------------------------------------------------
# PLOTS TO SEE WHICH VARIABLES AFFECT OUTPUT MOST
# ignore for now...need to test things here
# -----------------------------------------------------------------
par(mfrow = c(3,1))
gwplot(nn, selected.covariate = "mou_Mean", min = -5, max =5)
gwplot(nn, selected.covariate = "mou_Range", min = -5, max =5)
gwplot(nn, selected.covariate = "eqpdays", min = -5, max =5)