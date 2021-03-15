library(dplyr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(broom)
library(ggthemes)
setwd("~/Documents/Thesis/")
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
           +scale_y_continuous(labels = scales::percent)
           +scale_color_discrete(name="Lockdown", breaks=c("0","1"), labels=c("Before","After"))
           +labs(x="Date",y="Merchants", title="Merchants operating relative to Jan. 2020")
           +theme_economist()
)
graph7
summary(lm(merchants_all ~ post, sbTestCA))
graph7a <- (ggplot(data= sbTestCA,  aes(x=date, y=merchants_inchigh, color=post)) 
           +geom_point()
           +stat_smooth()
           +geom_vline(xintercept=as.Date("2020-03-19"), linetype="longdash")
           +scale_y_continuous(labels = scales::percent)
           +scale_color_discrete(name="Lockdown", breaks=c("0","1"), labels=c("Before","After"))
           +labs(x="Date",y="Merchants", title="Merchants operating relative to Jan. 2020:High Income")
           +theme_economist()
)
graph7a

graph7b <- (ggplot(data= sbTestCA,  aes(x=date, y=merchants_inclow, color=post)) 
           +geom_point()
           +stat_smooth()
           +geom_vline(xintercept=as.Date("2020-03-19"), linetype="longdash")
           +scale_y_continuous(labels = scales::percent)
           +scale_color_discrete(name="Lockdown", breaks=c("0","1"), labels=c("Before","After"))
           +labs(x="Date",y="Merchants", title="Merchants operating relative to Jan. 2020:Low Income")
           +theme_economist()
)
graph7b

sbTestbyclass <- pivot_longer(sbTestCA, 4:6, names_to = "merchantsAll")
sbTestbyclass$postlow <- 0
sbTestbyclass$postlow <- ifelse(sbTestbyclass$post ==1 && sbTestbyclass$merchantsAll=="merchants_inclow",sbTestbyclass$postlow+1, sbTestbyclass$postlow+0)
graph8 <- (ggplot(data=sbTestbyclass, aes(x=date, y=value, group=merchantsAll, color=merchantsAll),)
           +geom_point()
           +stat_smooth()
           +geom_vline(xintercept=as.Date("2020-03-19"), linetype="longdash")
           +scale_fill_economist(name="Income Level", labels=c("High","Low","Middle"))
           +scale_y_continuous(labels = scales::percent)
           +labs(title="Small Business Closures", x="Date", y="% Relative to Jan. 2020")
           +theme_economist()
          
)
graph8
#Now working with revenue rather than if business has closed
sbTestRevenue<- dplyr::select(sbTestCA, date,revenue_inchigh, revenue_incmiddle, revenue_inclow,post)
Revenuebyclass<- pivot_longer(sbTestRevenue, 2:4, names_to="MerchantClass", values_to="PercentDelta")
graph9<-(ggplot(data=Revenuebyclass,aes(x=date, y=PercentDelta, group=MerchantClass, color=MerchantClass))
         +geom_line()
         +stat_smooth()
         +scale_y_continuous(labels=scales::percent)
         +labs(title="% change in small biz revenue by income quartiles", x="Date", y="% relative to jan. 2020")
         +theme_economist()
         )
graph9
Revenue<-(lm(PercentDelta~MerchantClass+post+date,Revenuebyclass))

RevenueCut<- dplyr::filter(Revenuebyclass, date>="2020-06-20")
ClosuresCut<- dplyr::filter(Revenuebyclass, date>="2020-06-20")

Revenue<-(lm(PercentDelta~MerchantClass+date,Revenuebyclass))
RegCutRevenue<-(lm(PercentDelta~MerchantClass+date,RevenueCut))

#Data seems to show that small businesses in low/middle income quartiles face a less elastic market. very cool
#However, we can't say much about the significance of the values we got for our simple regressions. time for something different
#TODOS: COCHRANE ORCUTT on both of the tables
#test for serial correlation with DW test
library(orcutt)

dwtest(Revenue)
#testing for autocorrelation
#Significant autocorrelation
CO.Revenue<-cochrane.orcutt(Revenue)
CO.RevenueCut<-cochrane.orcutt(RegCutRevenue)
#Applying cochrane.orcutt correction

plot(Revenue$residuals)
plot(CO.Revenue$residuals)
summary(CO.Revenue)
summary(CO.RevenueCut)
#looking better.
dwtest(CO.Revenue)

