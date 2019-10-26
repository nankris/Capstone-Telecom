#What is Regression problem and classification problem?

#Regression problem: WHen you are trying to predict continuous outcome
#Ex: Price, Revenue, Income, Sales etc
#Classification problem: When youa re trying to predict an event/class/label.
#Ex: Good/Bad, Yes/No, High/Medium/Low etc

#Data set: Telecome domain data, has information about customer who has left the network.
#Capstone project: Predict churn (1/0)
#Churn=1: The customers who left the network
#Churn=0: The customers who are with the network
#Algorithm: Use a classification algorithm to predict

setwd("C:\\Users\\Kafeel Basha\\Desktop\\R Files\\R assignment solutions\\Folder Structure\\Assignments\\Graded Assignments\\Class 18 Fnal Case Study Course Wrap up")
telecom=read.csv("telecomfinal.csv",stringsAsFactors = FALSE)
dim(telecom)

View(telecom[1:5,1:10])
#Whole data is in lab
#JLC has sample

#Data Quality Report
library(dataQualityR)
checkDataQuality(telecom,out.file.num="numeric.csv",out.file.cat="character.csv")

#Remove the columns more than 15% of missing values.

#correlation, scatter plot, boxplot, histogram and so on
#Decile analysis is one of the data exploratory technique, to understand the relation between two variables

#Decile Analysis
library(dplyr)
telecom%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat45
dat45$N<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%count(dec))[[2]]
dat45$churn_perc<-dat45$n/dat45$N
dat45$min<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat45$max<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat45$varname<-rep("totrev",nrow(dat45))
dat45

#ntile() function works based on the ranking system
#If a variable totreve shows increasing or decreasing then keep the variable in your model.
#IF a variables shows both increasing and decreasing trend then ignore the variable, but if you feel that a variable which shows both increasing and decreasing trend is important for the analysis, either transform that variable or keep in the model to understand the impact.
#ignore the variable with less than 4 groups

#Assume that you have a variable with 1000 levels
#Creating 1000 dummies will increase variance in the model, we should avoid this
#Reducing levels
head(mtcars)
data=data.frame(text=rownames(mtcars),target=mtcars$vs)
unique(data$text)
unique(data$target)

#Use decision tree to prepare
library(rpart)
#classification: method will be class
#Regression: method will be anova
mod<-rpart(target~text,data=data,method="class")
unique(mod$where)
data$Nodes<-mod$where
head(data,30)

#Decision tree will not always promise you a split, if you are not getting more than 1 terminal node, then you have to play around with the parameters like depth of the tree, gini, entorpy min obser 

#Model validation
#glm() is done.
#VIF

predict( type="response")

#Confusion matrix and Area under the curve
#How many cut offs are possible between [0,1], as cut off changes the accuracy from confusion matrix will also change. 

#Confusion matrix will help us in understand how the model behaves at a give point.

#there are five business questions need to be answered
#Segmenting customers
data=data.frame(rev=c(1,3,4,5,6,7,9,11,12,14,13,16,19,20,21,22,23),Prob=c(sample(0.1:0.4,5,replace=TRUE),sample(0.41:0.7,6,replace=TRUE),sample(0.71:1,6,replace=TRUE)))
data

