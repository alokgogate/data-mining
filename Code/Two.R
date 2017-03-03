#Loading new data set into R
library(readxl)
new <- read_excel("~/Desktop/new.xlsx")
attach(new)
#creating a new data frame for this 
data =data.frame(new)

#defining a correlation value variable corr
corr <-cor(data,churn)
View(corr)
