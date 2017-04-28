
library(Boruta)
library(mice)
library(VIM)


original_data <-read.csv("/Users/Alok/Downloads/Bigger_churn_dataset.csv",na="null")

#Data conversion

clean_data <-data.frame(original_data[50001:100000,])
attach(clean_data)

clean_data[clean_data == ""] <- NA
convert <- c(100:102, 124:129, 135:170)
clean_data[,convert] <- data.frame(apply(clean_data[convert], 2, as.factor))

#NA calculation 
pMiss <- function(x){sum(is.na(x))/length(x)*100}

apply(clean_data,2,pMiss)
apply(clean_data,1,pMiss)

#remove features with high NA% - reducing to 141
clean_data = clean_data[, !names(clean_data) %in% c("crtcount", "rmcalls", "rmmou",  
                                                    "rmrev", "REF_QTY", "tot_ret", "tot_acpt", "prizm_social_one", "div_type", "pre_hnd_price",
                                                    "last_swap", "occu1", "ownrent", "lor", "dwlltype", "mailordr", "wrkwoman", "mailresp",
                                                    "children", "adults", "infobase", "income", "numbcars", "cartype", "HHstatin", "mailflag",
                                                    "solflag", "dwllsize", "educ1", "proptype", "pcowner", "retdays")]

#Mice pakage to look at missing data pattern

md.pattern(clean_data)
aggr_plot <- aggr(clean_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(clean_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# remove features with missing count higher than 0.003 - variables reduced to 124
pattern_data = clean_data
pattern_data = pattern_data[, !names(pattern_data) %in% c("truck", "mtrcycle", "rv", "marital",
                                                          "age1", "age2", "forgntvl", "ethnic", "kid0_2", "kid3_5", "kid6_10", "kid11_15", "kid16_17",
                                                          "creditcd", "car_buy", "hnd_price", "change_mou", "change_rev")]


#NA rows omission

pattern_data <- na.omit(pattern_data)
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}
delete.na(pattern_data)

#DID NOT WORK 
#pattern_data <- pattern_data[complete.cases(pattern_data)]

#DID NOT WORK 

#BORUTA 

set.seed(666)
boruta.train <- Boruta(churn~.-Customer_ID, data = pattern_data, doTrace = 2, maxRuns = 20, holdHistory = TRUE)
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory), function(i)boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels), at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

plot(final.boruta, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(final.boruta$ImpHistory), function(i)final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
names(lz) <- colnames(final.boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels), at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)
getSelectedAttributes(final.boruta, withTentative = F)


boruta.df <- attStats(final.boruta)
print(boruta.df)


write.table(boruta.df,"/Users/Alok/Desktop/borutaAttributeStats.csv")

boruta.dfConfirmed <- subset(boruta.df,  boruta.df$decision == "Confirmed")
confirmedAttributeNames <- rownames(boruta.dfConfirmed, do.NULL = TRUE, prefix = "row")
 
# save boruta cleaned data with final number of variables as csv
final_data = pattern_data[, names(pattern_data) %in% c(confirmedAttributeNames)]

write.table(pattern_data, "/Users/Alok/Desktop/churn_boruta_cleaned_50-100k.csv") 



print(boruta.train)
  
  
#Creating test and training set 

train_data <-data.frame(pattern_data[1:70000,])
test_data <-data.frame(pattern_data[70001:100000,])

#Removing all non-numeric terms
t <-train_data[sapply(train_data,is.numeric)]

#making churn as the first column
t <-t1k[c(93,1:92,94:115)]
t <-train_data[c(93,1:92,94:115)]

#implementing PCA therefore scaling the datapoints with mean 0 and variance 1
log.t <-scale(t[,2:115])
t.churn <-t[,1]
t.pca <-prcomp(log.t,center=TRUE,scale. = TRUE)

#plot usefull to decide how many PC's to retain 
plot(t.pca,type="l")
