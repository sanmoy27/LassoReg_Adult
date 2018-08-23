setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\Project\\data\\logistic")
library(MASS)
library(caTools)
library(ISLR)
library(glmnet)
library(caret)
library(class)
library(gridExtra)

adultData<-read.csv("adult.csv", header = TRUE, stringsAsFactors = FALSE)
dim(adultData)
str(adultData)
adultData[adultData=='?']<-NA


cat("\014")
adultData$workclass[adultData$workclass == "Without-pay" | 
                      adultData$workclass == "Never-worked"] <- "Unemployed"

adultData$workclass[adultData$workclass == "Local-gov" | 
                      adultData$workclass == "Federal-gov" | 
                      adultData$workclass == "State-gov"] <- "govEmployed"

adultData$workclass[adultData$workclass == "Self-emp-inc" |
                      adultData$workclass == "Self-emp-not-inc"] <- "Self-employed"


adultData$marital.status[adultData$marital.status=="Married-civ-spouse" |
                           adultData$marital.status=="Married-AF-spouse" |
                         adultData$marital.status=="Married-spouse-absent"] <- "Married"

adultData$marital.status[adultData$marital.status == "Divorced" |
                           adultData$marital.status == "Separated" |
                           adultData$marital.status == "Widowed" |
                           adultData$marital.status == "Never-Married"] <- "Not-Married"


adultData$education[adultData$education=="10th" |
                           adultData$education=="11th" |
                         adultData$education=="7th-8th" |
                      adultData$education=="12th" |
                      adultData$education=="1st-4th" |
                      adultData$education== "9th" |
                      adultData$education== "5th-6th"|
                      adultData$education== "Prof-school"|
                      adultData$education== "Preschool" |
                      adultData$education== "HS-grad"] <- "Under Grad"


adultData$education[adultData$education=="Bachelors" |
                      adultData$education=="Some-college"] <- "Grad"



north.america <- c("Canada", "Cuba", "Dominican-Republic", "El-Salvador", "Guatemala",
                   "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua",
                   "Outlying-US(Guam-USVI-etc)", "Puerto-Rico", "Trinadad&Tobago",
                   "United-States")
asia <- c("Cambodia", "China", "Hong", "India", "Iran", "Japan", "Laos",
          "Philippines", "Taiwan", "Thailand", "Vietnam")
south.america <- c("Columbia", "Ecuador", "Peru")
europe <- c("England", "France", "Germany", "Greece", "Holand-Netherlands",
            "Hungary", "Ireland", "Italy", "Poland", "Portugal", "Scotland",
            "Yugoslavia")
other <- c("South", "?")

adultData$native.country[adultData$native.country %in% north.america] <- "North America"
adultData$native.country[adultData$native.country %in% asia] <- "Asia"
adultData$native.country[adultData$native.country %in% south.america] <- "South America"
adultData$native.country[adultData$native.country %in% europe] <- "Europe"
adultData$native.country[adultData$native.country %in% other] <- "Other"


adultData$native.country<-as.factor(adultData$native.country)
adultData$education<-as.factor(adultData$education)
adultData$marital.status<-as.factor(adultData$marital.status)
adultData$workclass<-as.factor(adultData$workclass)
adultData$race<-as.factor(adultData$race)
adultData$gender<-as.factor(adultData$gender)
adultData$relationship<-as.factor(adultData$relationship)



cat("\014")
detectNAs<-function(x){
  return(sum(is.na(x)))
}
lapply(adultData, detectNAs)

adultData<-na.omit(adultData)


library(ggplot2)
library(gridExtra)
library(plotly)

p1<-ggplot(adultData, aes(x=age)) + geom_histogram(aes(fill = income), color = "black",
                                         binwidth = 1)

p2<-ggplot(adultData, aes(x=workclass, fill=income)) + geom_bar(position="dodge", color = "black")
p3<-ggplot(adultData, aes(x=marital.status, fill=income)) + geom_bar(position="dodge", color = "black")
p4<-ggplot(adultData, aes(x=education, fill=income)) + geom_bar(position="dodge", color = "black")
p5<-ggplot(adultData, aes(x=native.country, fill=income)) + geom_bar(position="dodge", color = "black")
p6<-ggplot(adultData, aes(x=gender, fill=income)) + geom_bar(position="dodge", color = "black")
p7<-ggplot(adultData, aes(hours.per.week)) + geom_histogram(binwidth = 3)
grid.arrange(p1,p2, p3, p4, p5, p6, p7, nrow=4, ncol=2)

adultData$income<-ifelse(adultData$income==">50K", 1, 0)
adultData$income<-as.factor(adultData$income)
#View(adultData)

set.seed(100)
#split<-sample.split(adultData$income, SplitRatio=0.8)
#trainSet<-subset(adultData, split==TRUE)
#testSet<-subset(adultData, split==FALSE)


cat("\014")
x=model.matrix(income~.,adultData)[,-15]  ##qualitative variable to quantitative
y=adultData$income
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)



log.model <- cv.glmnet(x[train,], y[train], alpha=1, family = "binomial")
#log.model <- glm(income ~ ., data=trainSet, family = binomial())
summary(log.model)

log.model$lambda.min
plot(log.model)



cat("\014")
predicted <- predict(log.model, newx=x[test,], s="lambda.min", type="response")

model.coeff <- predict(log.model, newx=x[test,], s="lambda.min", type="coefficients")[1:15,]
model.coeff[model.coeff!=0]
conf<-table(predicts=predicted>0.5, actuals=y[test])
conf

tab<-table(predicted>0.5, y[test])
dimnames(tab)[[1]] = c(0,1)
confusionMatrix(tab)

#accuracy=sum(diag(conf))/sum(conf)
#accuracy

pred <- ifelse(predicted > 0.5, ">50K", "<=50k")
head(pred)


###### ROCR Curve ######
library(ROCR)
ROCRpred <- prediction(predicted, y[test])
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE)
auc.tmp <- performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc

