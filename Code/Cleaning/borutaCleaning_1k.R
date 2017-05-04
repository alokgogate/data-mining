library(Boruta)
library(mice)
library(VIM)

setwd("C:/Users/ABS/Documents/R/churn-project")
original_data <- read.csv("churn_original.csv", na = "null")

clean_data <- data.frame(original_data[1:1000,])
attach(clean_data)

# -----------------------------------------------------------------
# -------------------------DATA CONVERSION-------------------------
# -----------------------------------------------------------------
# convert blanks to NA
clean_data[clean_data == ""] <- NA

# converting categorical to factor
convert <- c(100:102, 124:129, 135:170)
clean_data[,convert] <- data.frame(apply(clean_data[convert], 2, as.factor))

# -----------------------------------------------------------------
# -------------------------NA% CALCULATION-------------------------
# -----------------------------------------------------------------
# compute NA% for each feature, remove those with value higher than 5%
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(clean_data,2,pMiss)
apply(clean_data,1,pMiss)
# remove features with high NA% - variables reduced to 141
clean_data = clean_data[, !names(clean_data) %in% c("crtcount", "rmcalls", "rmmou",  
"rmrev", "REF_QTY", "tot_ret", "tot_acpt", "prizm_social_one", "div_type", "pre_hnd_price",
"last_swap", "occu1", "ownrent", "lor", "dwlltype", "mailordr", "wrkwoman", "mailresp",
"children", "adults", "infobase", "income", "numbcars", "cartype", "HHstatin", "mailflag",
"solflag", "dwllsize", "educ1", "proptype", "pcowner", "retdays")]

# -----------------------------------------------------------------
# -----------MICE PACKAGE TO LOOK AT MISSING DATA PATTERN----------
# -----------------------------------------------------------------
# visualise missing data pattern
md.pattern(clean_data)
aggr_plot <- aggr(clean_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(clean_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# remove features with missing count higher than 0.003 - variables reduced to 124
pattern_data = clean_data
pattern_data = pattern_data[, !names(pattern_data) %in% c("truck", "mtrcycle", "rv", "marital",
"age1", "age2", "forgntvl", "ethnic", "kid0_2", "kid3_5", "kid6_10", "kid11_15", "kid16_17",
"creditcd", "car_buy", "hnd_price", "change_mou", "change_rev")]

# -----------------------------------------------------------------
# -------------------------NA ROW OMISSION-------------------------
# -----------------------------------------------------------------
# remove NA from data via na.omit
pattern_data <- na.omit(pattern_data)
# remove NA from data via function
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}
delete.na(pattern_data)
# remove rows with NA via complete.cases
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

# -----------------------------------------------------------------
# ----------------------FOR CHECKING PURPOSE-----------------------
# -----------------------------------------------------------------
str(clean_data)
summary(clean_data)



library(xlsx)
boruta.dfConfirmed <- subset(boruta.df,  c(boruta.df$decision == "Confirmed"))
confirmedAttributeNames <- rownames(boruta.dfConfirmed, do.NULL = TRUE, prefix = "row")
write.csv(boruta.df, file = "borutaAttributeStats_1k.csv")

final_data = pattern_data[, names(pattern_data) %in% c(confirmedAttributeNames, "churn", "Customer_ID")]

final_data = as.data.frame(final_data)
write.csv(final_data, file = "churn_boruta_cleaned_1k.csv",row.names=FALSE)
