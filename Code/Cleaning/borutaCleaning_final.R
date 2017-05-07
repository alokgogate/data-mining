library(Boruta)
library(mice)
library(VIM)
library(xlsx)

setwd("C:/Users/ABS/Documents/R/churn-project")
original_data <- read.csv("churn_original.csv")

cleaned_data <- data.frame(original_data)
attach(cleaned_data)

# -----------------------------------------------------------------
# -------------------------DATA CONVERSION-------------------------
# -----------------------------------------------------------------
# convert blanks to NA
cleaned_data[cleaned_data == ""] <- NA

# converting categorical to factor
convert <- c(100:102, 124:129, 135:170)
cleaned_data[,convert] <- data.frame(apply(cleaned_data[convert], 2, as.factor))

# -----------------------------------------------------------------
# -------------------------NA% CALCULATION-------------------------
# -----------------------------------------------------------------
# compute NA% for each feature, remove those with value higher than 5%
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(cleaned_data,2,pMiss)
apply(cleaned_data,1,pMiss)

# remove features with high NA% - variables reduced to 140
cleaned_data = cleaned_data[, !names(cleaned_data) %in% c("crtcount", "rmcalls", "rmmou",  
"rmrev", "REF_QTY", "tot_ret", "tot_acpt", "prizm_social_one", "div_type", "pre_hnd_price",
"last_swap", "occu1", "ownrent", "lor", "dwlltype", "mailordr", "wrkwoman", "mailresp",
"children", "adults", "infobase", "income", "numbcars", "cartype", "HHstatin", "mailflag",
"solflag", "dwllsize", "educ1", "proptype", "pcowner", "retdays", "hnd_webcap")]

# -----------------------------------------------------------------
# -----------MICE PACKAGE TO LOOK AT MISSING DATA PATTERN----------
# -----------------------------------------------------------------
# visualise missing data pattern
md.pattern(cleaned_data)
aggr_plot <- aggr(cleaned_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(cleaned_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# remove features with missing count higher than 0.003 - variables reduced to 122
pattern_data = cleaned_data
pattern_data = pattern_data[, !names(pattern_data) %in% c("avg6mou", "avg6qty", "avg6rev", "truck", "mtrcycle", "rv", "marital",
"age1", "age2", "forgntvl", "ethnic", "kid0_2", "kid3_5", "kid6_10", "kid11_15", "kid16_17", "creditcd", "car_buy")]

# -----------------------------------------------------------------
# -------------------------NA ROW OMISSION-------------------------
# -----------------------------------------------------------------
# remove any rows with NAs from data
pattern_data <- na.omit(pattern_data)

# -----------------------------------------------------------------
# -------------------------BORUTA CLEANING-------------------------
# -----------------------------------------------------------------
set.seed(666)
boruta.train <- Boruta(churn~.-Customer_ID, data = pattern_data, doTrace = 2, maxRuns = 100, holdHistory = TRUE)
print(boruta.train)

# plot boruta variable importance chart
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory), function(i)boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels), at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

# dealing with tentative attributes
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)
# plot final variable importance graph
plot(final.boruta, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(final.boruta$ImpHistory), function(i)final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
names(lz) <- colnames(final.boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels), at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)

# get list of confirmed attributes - drop hnd_webcap from it
confirmedAttributeNames <- getSelectedAttributes(final.boruta, withTentative = F)
confirmedAttributeNames <- confirmedAttributeNames[-93]

# create data frame of final results and print - drop hnd_webcap from it
boruta.df <- attStats(final.boruta)
boruta.df <- boruta.df[-120,]
print(boruta.df)
# save data frame as excel file
write.csv(boruta.df, "C:/Users/ABS/Documents/R/churn-project/borutaAttributeStats_final.csv") 

# save boruta cleaned data with final number of variables = 93
# add churn and Customer_ID variables back for modelling purposes
final_data = pattern_data[, names(pattern_data) %in% c(confirmedAttributeNames, "churn", "Customer_ID")]
final_data = as.data.frame(final_data)
write.csv(final_data, file = "churn_boruta_cleaned_100k.csv",row.names=FALSE)

# -----------------------------------------------------------------
# ----------------------FOR CHECKING PURPOSE-----------------------
# -----------------------------------------------------------------

str(cleaned_data)
summary(cleaned_data)