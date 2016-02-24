library(ggplot2)
library(dplyr)
library(Lahman)
library(VIM)
df<-merge(Batting,Master,by="playerID")
df$stint <- NULL
df$birthMonth <- NULL
df$birthState <- NULL
df$birthCountry <- NULL
df$birthCity <- NULL
df$birthDay <- NULL
df$deathDay <-NULL
df$deathYear<-NULL
df$deathCity<-NULL
df$deathState<-NULL
df$deathCountry<-NULL
df$deathMonth<-NULL
df$nameGiven<-NULL
df$finalGame<-NULL
df$debut<-NULL
df$deathDate<-NULL
df$retroID<-NULL
df$bbrefID<-NULL
df$birthDate<-NULL
head(sdf)
summary(sdf)
qplot(data = sdf, x =  sdf$RBI) + ylab("Number of RBI per year")
a=aggr(sdf,prop=FALSE,numbers=TRUE)
summary(a)
sdf <- as.data.frame(subset(df,  yearID > 2004 & H > 0 & AB > 10))

hist(sdf$HR,sdf$HR>0)
describe(sdf)
boxplot(sdf$HR)

listvar=c("weight","height","R","H","RBI","SB")
mydpc=sdf[listvar]
summary(mydpc)
M = cor(mydpc[listvar], use="complete.obs")
#correlogram
library("corrplot")
corrplot(M, method="circle")
