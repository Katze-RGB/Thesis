library(dplyr)
library(tidyr)
library(ggplot2)
library(plm)
library(broom)
setwd("~/Documents/Thesis/")

SlapChop<- function(n){
  n <- unite(n, "date", c("year","month","day_endofweek"), sep = "/")
  n$date <- as.Date(n$date)
  n <- filter(n, statefips==6)
}

SlapChopDaily<- function(n){
  n <- unite(n, "date", c("year","month","day"), sep = "/")
  n$date <- as.Date(n$date)
  n <- filter(n, statefips==6)
}


Zearn...State...Weekly <- read.csv("~/Documents/Thesis/data/EconomicTracker-main/data/Zearn - State - Weekly.csv", na.strings=".")
gdataCA <- SlapChop(Zearn...State...Weekly)
#scatter.smooth(as.Date(gdataCA$date), gdataCA$engagement_inclow)

graph2

UItest <- read.csv("~/Documents/Thesis/data/EconomicTracker-main/data/UI Claims - State - Weekly.csv", na.strings=".")
UItest <- SlapChop(UItest)
UItest$post<- 0
UItest$post <- ifelse(UItest$date >= "2020-03-19",UItest$post+1,UItest$post+0)
scatter.smooth((UItestCA$date), UItestCA$initclaims_count_regular)
graph3 <-(ggplot(data=UItestCA, aes(x=date, y=initclaims_count_regular),)
                 +geom_line(color="red")
                 +geom_point(color="red")
                 +geom_point(aes(y=initclaims_count_pua), color="green")
                 +geom_line(aes(y=initclaims_count_pua), color="green")
                 +labs(title="Unemployment Claims Over Time", x="Date", y="Claims")
          )
graph3  

EmpTest <- read.csv("~/Documents/Thesis/data/EconomicTracker-main/data/Employment Combined - State - Daily.csv", na.strings=".")
EmpTest <- SlapChopDaily(EmpTest)
EmpTest <- pivot_longer(EmpTest, 4:6, names_to = "empunified")
graph5 <- (ggplot(data=EmpTest, aes(x=date, y=value, group=empunified, color=empunified),)
           +geom_line()
           +geom_point()
           +geom_smooth(method="lm")
           +scale_y_continuous(labels = scales::percent)
           +labs(title="Employment By Income", x="Date", y="Percent Change")
           +theme(legend.title=element_blank())
           +scale_fill_discrete(name="Income Quartile", labels=c("Low","Middle","High"))
)
summary(lm(value ~ date+empunified+post, data=EmpTest))
graph5

spendingTest <-read.csv("~/Documents/Thesis/data/EconomicTracker-main/data/Affinity - State - Daily.csv", na.strings=".")
spendingTest <- (unite(spendingTest, "date", c("year","month","day"), sep = "/"))
spendingTest$date <- as.Date(spendingTest$date)
spendingTestCA <- filter(spendingTest, statefips == 6)
spendingTestCAdaily <- filter(spendingTestCA, freq == "d")
spendingTestCAweekly <- filter(spendingTestCA, freq == "w")
graph6 <-(ggplot(data=spendingTestCAdaily, aes(x=date, y=spend_all),)
+geom_line()
+geom_point()
)
graph6

sbTest<- read.csv("~/Documents/Thesis/data/EconomicTracker-main/data/Womply - State - Daily.csv", na.strings=".")
sbTest<- (unite(sbTest, "date", c("year","month","day"), sep = "/"))
sbTest$date <- as.Date(sbTest$date)
sbTestCA <- filter(sbTest, statefips == 6)
sbTestCA$post <- 0
sbTestCA$post <- as.factor(ifelse(sbTestCA$date >= "2020-03-19",sbTestCA$post+1,sbTestCA$post+0))
graph7 <- (ggplot(data= sbTestCA,  aes(x=date, y=merchants_all, color=post)) 
           +geom_point()
           +stat_smooth()
           +geom_vline(xintercept=as.Date("2020-03-19"), linetype="longdash")
           +scale_color_discrete(name="Lockdown", breaks=c("0","1"), labels=c("Before","After"))
           +labs(x="Date",y="Merchants", title="Merchants operating relative to Jan. 2020")
           )
graph7

sbTestbyclass <- pivot_longer(sbTestCA, 4:6, names_to = "merchantsAll")
sbTestbyclass$postlow <- 0
sbTestbyclass$postlow <- ifelse(sbTestbyclass$post ==1 && sbTestbyclass$merchantsAll=="merchants_inclow",sbTestbyclass$postlow+1, sbTestbyclass$postlow+0)
graph8 <- (ggplot(data=sbTestbyclass, aes(x=date, y=value, group=merchantsAll, color=merchantsAll),)
           +geom_line()
           +stat_smooth(method="lm")
           +scale_y_continuous(labels = scales::percent)
           +labs(title="% change in small businesses open by income", x="Date", y="% relative to jan 2020")
)
graph8
sbTest<- gather(sbTestbyclass, date)
table1<-summary(lm(value~merchantsAll+post,sbTestbyclass))
table1<-tidy(table1)
californiacounties <- c(6001:6115)
CountyMobilitydata<- read.csv("~/Documents/Thesis/data/EconomicTracker-main/data/Google Mobility - County - Daily.csv", na.strings=".")
CountyMobilitydataCA<- filter(CountyMobilitydata, grepl("^6", countyfips))
###TODOS
#diff-in diff for math learning
#diff1: income catagory
#diff2: timing, find the proper dates for that
Zearn...State...Weekly <- read.csv("~/Documents/Thesis/data/EconomicTracker-main/data/Zearn - State - Weekly.csv", na.strings=".")
gdataCA <- SlapChop(Zearn...State...Weekly)
#classgraphdataByClass<-pivot_longer(gdataCA,c(5,7,9), names_to = "AllEngagement", values_to =  "EngageValue" )
#classgraphdataByClass<-pivot_longer(classgraphdataByClass,c(10,12,14), names_to = "AllEngagementBreak", values_to="BreakEngage")
mathdataCA <-gdataCA
mathdataCA$breakind <-0
mathdataCA$breakind <- ifelse(is.na(mathdataCA$engagement), mathdataCA$breakind+1, mathdataCA$breakind+0)
mathdataCA$post <-0
mathdataCA<- unite(mathdataCA,"contengage_inchigh", c(5,13), na.rm=T)
mathdataCA<- unite(mathdataCA,"contengage_incmed", c(9,16), na.rm=T)
mathdataCA<- unite(mathdataCA,"contengag_inclow", c(7,14), na.rm=T)
mathdataCA<-pivot_longer(mathdataCA,c(5,7,9), names_to = "AllEngagement", values_to =  "EngageValue" )

mathdataCA$post <- ifelse(mathdataCA$date >= "2020-03-19",mathdataCA$post+1,mathdataCA$post+0)
mathdataCA<- select(mathdataCA, date, post, breakind, AllEngagement, EngageValue)
graph9 <- (ggplot(data=mathdataCA, aes(x=date, y=as.numeric(EngageValue), group=AllEngagement, color=AllEngagement),)
           +geom_line()
           +geom_point()
           +scale_y_continuous(labels = scales::percent)
           +labs(title="% change in engagement by income")
)
graph9
mathdataCA$AllEngagement <- as.factor(mathdataCA$AllEngagement)
lm(EngageValue ~ date+breakind+post+AllEngagement, mathdataCA)

