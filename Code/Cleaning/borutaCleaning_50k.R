library(Boruta)
library(mice)
library(VIM)
library(xlsx)

setwd("C:/Users/ABS/Documents/R/churn-project")
original_data <- read.csv("churn_original.csv")

cleaned_data <- data.frame(original_data[1:50000,])
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
# remove features with high NA% - variables reduced to 141
cleaned_data = cleaned_data[, !names(cleaned_data) %in% c("crtcount", "rmcalls", "rmmou",  
"rmrev", "REF_QTY", "tot_ret", "tot_acpt", "prizm_social_one", "div_type", "pre_hnd_price",
"last_swap", "occu1", "ownrent", "lor", "dwlltype", "mailordr", "wrkwoman", "mailresp",
"children", "adults", "infobase", "income", "numbcars", "cartype", "HHstatin", "mailflag",
"solflag", "dwllsize", "educ1", "proptype", "pcowner", "retdays")]

# -----------------------------------------------------------------
# -----------MICE PACKAGE TO LOOK AT MISSING DATA PATTERN----------
# -----------------------------------------------------------------
# visualise missing data pattern
md.pattern(cleaned_data)
aggr_plot <- aggr(cleaned_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(cleaned_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# remove features with missing count higher than 0.003 - variables reduced to 123
pattern_data = cleaned_data
pattern_data = pattern_data[, !names(pattern_data) %in% c("avg6mou", "avg6qty", "avg6rev", "truck", "mtrcycle", "rv", "marital",
"age1", "age2", "forgntvl", "ethnic", "kid0_2", "kid3_5", "kid6_10", "kid11_15", "kid16_17",
"creditcd", "car_buy")]

# -----------------------------------------------------------------
# -------------------------NA ROW OMISSION-------------------------
# -----------------------------------------------------------------
# remove NA from data via na.omit
pattern_data <- na.omit(pattern_data)

# remove NA from data via function
# delete.na <- function(DF, n=0) {
#   DF[rowSums(is.na(DF)) <= n,]
# }
# delete.na(pattern_data)

# remove rows with NA via complete.cases - this step gives error for full data but boruta still works
pattern_data <- pattern_data[complete.cases(pattern_data)]

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
# plot variable importance graph
plot(final.boruta, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(final.boruta$ImpHistory), function(i)final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
names(lz) <- colnames(final.boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels), at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)

# get list of confirmed attributes
getSelectedAttributes(final.boruta, withTentative = F)

# create data frame of final results and print
boruta.df <- attStats(final.boruta)
print(boruta.df)
# save data frame as excel file
write.xlsx(boruta.df, "C:/Users/ABS/Documents/R/churn-project/borutaAttributeStats.xlsx") 

# filter only boruta confirmed attributes and get list of confirmed attribute names
boruta.dfConfirmed <- subset(boruta.df,  boruta.df$decision == "Confirmed")
confirmedAttributeNames <- rownames(boruta.dfConfirmed, do.NULL = TRUE, prefix = "row")

# save boruta cleaned data with final number of variables as csv
final_data = pattern_data[, names(pattern_data) %in% c(confirmedAttributeNames)]
write.table(pattern_data, "churn_boruta_cleaned_1-50k.csv") 

# -----------------------------------------------------------------
# ----------------------FOR CHECKING PURPOSE-----------------------
# -----------------------------------------------------------------

str(cleaned_data)
summary(cleaned_data)