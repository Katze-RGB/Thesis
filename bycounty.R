library(tidyverse)
library(ggplot2)
library(plm)
library(readxl)
library(miceadds)
setwd("~/Documents/Thesis/")

#import data
SBcounty<-read.csv("~/Documents/Thesis/data/EconomicTracker-main/data/Womply - County - Daily.csv", na.strings=".")
countylevelincome <- read_excel("data/countylevelincome.xlsx", skip = 1)
#merging county median income with sb revenue/closures data
CAincome<- select(countylevelincome,1,2,12)
CAincome$FIPS<-as.numeric(CAincome$FIPS)
SBcountyCA<-filter(SBcounty, grepl("^6", countyfips))
SBcountyCA$FIPS<-SBcountyCA$countyfips
CAincome$quartile<-ntile(CAincome$`Median Household Income (2019)`,4)
FullData<-full_join(SBcountyCA,CAincome,by="FIPS")
DataCut<-select(FullData,1:6,10)
DataCut<-(unite(DataCut, "date", c("year","month","day"), sep = "/"))
DataCut$date <- as.Date(DataCut$date)

#generating variables 
DataCut$lowInc<-0
DataCut$middleInc<-0
DataCut$highInc<-0
DataCut$highInc<-ifelse(DataCut$quartile == 4, DataCut$highInc<-1, DataCut$highInc<-0)
DataCut$lowInc<-ifelse(DataCut$quartile == 1, DataCut$lowInc<-1, DataCut$lowInc<-0)
DataCut$middleInc<-ifelse(DataCut$lowInc == 0 & DataCut$highInc == 0, DataCut$middleInc<-1, DataCut$middleInc<-0)
DataCut$post <- 0
DataCut$post <-(ifelse(DataCut$date >= "2020-03-19",DataCut$post+1,DataCut$post+0))

ClusterReg<-lm.cluster(DataCut,revenue_all~lowInc+middleInc+highInc+post,DataCut$countyfips)
RegReg<-lm(revenue_all~lowInc+middleInc+highInc+post+date,DataCut)
summary(ClusterReg)
summary(RegReg)
library(orcutt)
dwtest(RegReg)
plot(RegReg$residuals)
plmtest<-(plm(revenue_all~lowInc+middleInc+highInc+post+date,DataCut))
summary(plmtest)


DataCut2<- dplyr::filter(DataCut, date>="2020-06-20")
RegReg2<-lm(revenue_all~lowInc+middleInc+date,DataCut2)
RegCluster2<-lm.cluster(DataCut2, revenue_all~lowInc+middleInc+highInc, DataCut2$countyfips)
summary(RegCluster2)
plot(RegReg2$residuals)
dwtest(RegReg2)
summary(RegReg2)


#try a one-day cross-sectional regression
DataCut3<-dplyr::filter(DataCut, date=="2020-10-21")
crossreg<-lm(merchants_all~lowInc+highInc+middleInc, data=DataCut3)
summary(crossreg)

#Try ARIMA model for univariate models
DataCut2Low<-dplyr::filter(DataCut2, lowInc==1)
DataCut2Low<- select(DataCut2Low, 1,4)
DataCut2Low$date<-as.Date(DataCut2Low$date)
DataCut2Med<-dplyr::filter(DataCut2, middleInc==1)
DataCut2Med<- select(DataCut2Med, 1,4)
DataCut2Med$date<-as.Date(DataCut2Low$date)
library(forecast)
auto.arima(DataCut2Low)
