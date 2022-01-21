install.packages("rcmdr")
install.packages()
install.packages("rcmdr")
install.packages("Rcmdr")
install.packages("rattle")
install.packages("GUI")
library(readr)
FDSActivity2 <- read_csv("FDSActivity2.csv")
View(FDSActivity2)
library(MASS)
library(tree)
library(tidyverse)
install.package("tree")
install.packages("tree")
install.packages("tidyverse")
library(MASS)
library(tree)
library(tidyverse)
head(FDSActivity2)
#check for NAs in whole dataset (found 0 NA from the whole dataset)
sum(is.na(FDSActivity2))
#check data type
sapply(FDSActivity2, typeof)
#convert binary response to 1 and 0
#new column smoker_num
FDSActivity2$smoker_num <- ifelse(FDSActivity2$smoker=="yes",1,0)
#new column female
FDSActivity2$female <- ifelse(FDSActivity2$sex=="female",1,0)
summary(FDSActivity2)
#num of male vs female
sum(FDSActivity2$sex=="male")
sum(FDSActivity2$sex=="female")
#mean of charges smoker vs non smoker
mean(insurance$charges[FDSActivity2$smoker_num==1])
mean(insurance$charges[FDSActivity2$smoker_num==0])
#mean of charges smoker vs non smoker
mean(FDSActivity2$charges[FDSActivity2$smoker_num==1])
mean(FDSActivity2$charges[FDSActivity2$smoker_num==0])
#mean of charges female vs male
mean(FDSActivity2$charges[FDSActivity2$female==1])
mean(FDSActivity2$charges[FDSActivity2$female==0])
#histogram of charges variable
#checking whether the data is normally distributed or not
hist(FDSActivity2$charges)
#qqplot and qqline for charges variable
#checking on how linear the data is
qqnorm(FDSActivity2$charges)
qqline(FDSActivity2$charges)
#plot bmi vs charges
plot(x=FDSActivity2$bmi,y=FDSActivity2$charges)
#plot age vs charges
plot(x=FDSActivity2$age,y=FDSActivity2$charges)
#ggplot for bmi vs charges grouped by region
ggplot(data=FDSActivity2,aes(x=bmi,y=charges)) + geom_point(aes(color=region)) +facet_wrap(~region)
#ggplot for age vs charges grouped by region
ggplot(data=FDSActivity2,aes(x=age,y=charges)) + geom_point(aes(color=region)) +facet_wrap(~region)
#LLSR with 1 predictor (age)
model1 <- lm(charges~age, data=FDSActivity2)
summary(model1)
#LLSR with all predictors
model2 <- lm(charges~bmi+age+female+smoker_num+children, data=FDSActivity2)
summary(model2)
#LLSR with no female variable
model3 <- lm(charges~bmi+age+smoker_num+children, data=FDSActivity2)
summary(model3)
#creating decision tree
insurance.tree <- tree(charges~bmi+age+smoker_num+children, data=FDSActivity2)
#plot or make the decision tree more presentable
plot(FDSActivity2.tree)
text(FDSActivity2.tree, pretty = 0)
#plot or make the decision tree more presentable
plot(insurance.tree)
text(insurance.tree, pretty = 0)
load("~/.RData")
hist(FDSActivity2$charges)
qqnorm(FDSActivity2$charges)
qqline(FDSActivity2$charges)
load("~/.RData")
