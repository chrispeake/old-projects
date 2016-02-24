library(Lahman)

data("Batting")
data("Master")
data("battingLabels")
batting <- battingStats()

df<-merge(batting,Master,by="playerID")
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

#changed to 2003
#sdf <- as.data.frame(subset(df,  yearID > 2003 & H > 0 & AB > 10))
sdf <- as.data.frame(subset(df,  yearID > 2003 & H > 0 & AB > 75))


head(sdf)
summary(sdf)

library(VIM)
a=aggr(sdf,prop=FALSE,numbers=TRUE)
summary(a)
# no missing values

hist(sdf$HR)
describe(sdf)
boxplot(sdf$HR)

# add age variable to yearly stats info
sdf$age <- sdf$yearID - sdf$birthYear

# create ndf with player's next year's offensive stats,
# only contains people that played next year
library(plyr)
ssdf <- sdf[c("playerID","yearID","BA","SlugPct","OBP", "OPS", "BABIP") ]
ssdf = rename(ssdf,c("BA"="nextBA","SlugPct"="nextSlugPct","OBP"="nextOBP",
                    "OPS"="nextOPS","BABIP"="nextBABIP"))

ssdf$yearID <- ssdf$yearID - 1
ndf <- merge(sdf, ssdf)
ndf = scale(ndf)

###### NEW
#create new binary columns that determines if player's offensive stats improves next year
ndf$imprvBA <- (ndf$nextBA > ndf$BA)
ndf$imprvSlugPct <- (ndf$nextSlugPct > ndf$SlugPct)
ndf$imprvOBP <- (ndf$nextOBP > ndf$OBP)
ndf$imprvOPS <- (ndf$nextOPS > ndf$OPS)
ndf$imprvBABIP <- (ndf$nextBABIP > ndf$BABIP)

# build training/validation sets
train.size = round(0.75*nrow(ndf))
set.seed(1234)
id.train = sample(1:nrow(ndf),train.size,replace=F)
ndf.train = ndf[id.train,]
ndf.val = ndf[-id.train,]

# regression decision tree using BA
library(rpart)
library(rpart.plot)
#rdtree = rpart(nextBA ~ G + AB + R  + RBI + BA + bats + age, data=ndf.train, method="anova", parms=list(split="information"))
rdtree = rpart(nextBA ~ G + AB + BA + bats + age, data=ndf.train, method="anova", parms=list(split="information"))
prp(rdtree, type=4, fallen.leaves=F, main="Decision Tree", faclen=0)
barplot(rdtree$variable.importance)

rdtree.pred = predict(rdtree, newdata=ndf.val,type="matrix")
rdtree.perf = table(ndf.val$nextBA, rdtree.pred, dnn=c("Actual","Predicted"))
rdtree.perf

prop.table(rdtree.perf,1)

#bin nextBA and see if decision tree works better
bin = 3
cutpoints <- quantile(ndf$nextBA, (0:bin)/bin)
ndf$nextBAbin = cut(ndf$nextBA, cutpoints, include.lowest = T)
ndf$nextBAbinf = factor(ndf$nextBAbin, levels = c("[0.092,0.246]","(0.246,0.276]","(0.276,0.396]"), labels = c("Lower3rd","Middle3rd","Upper3rd"))
ndf.train = ndf[id.train,]
ndf.val = ndf[-id.train,]

#classification decision tree using binned nextBA
rdtree = rpart(nextBAbinf ~ G + AB + BA + bats + age, data=ndf.train, method="class", parms=list(split="gini"))
prp(rdtree, type=4, extra=104, fallen.leaves=F, main="Decision Tree", faclen=0)
barplot(rdtree$variable.importance)

rdtree.pred = predict(rdtree, newdata=ndf.val,type="class")
rdtree.perf = table(ndf.val$nextBAbinf, rdtree.pred, dnn=c("Actual","Predicted"))
rdtree.perf

prop.table(rdtree.perf,1)

#bin nextBA and see if decision tree works better
bin = 4
cutpoints <- quantile(ndf$nextBA, (0:bin)/bin)
ndf$nextBAbin = cut(ndf$nextBA, cutpoints, include.lowest = T)
ndf$nextBAbinf = factor(ndf$nextBAbin, levels = c("[0.02,0.209]","(0.209,0.248]","(0.248,0.277]","(0.277,0.636]"), labels = c("1stQtr","2ndQtr","3rdQtr","4thQtr"))
ndf.train = ndf[id.train,]
ndf.val = ndf[-id.train,]

#classification decision tree using binned nextBA
rdtree = rpart(nextBAbinf ~ G + AB + BA + bats + age, data=ndf.train, method="class", parms=list(split="gini"))
prp(rdtree, type=4, extra=104, fallen.leaves=F, main="Decision Tree", faclen=0)
barplot(rdtree$variable.importance)

rdtree.pred = predict(rdtree, newdata=ndf.val,type="class")
rdtree.perf = table(ndf.val$nextBAbinf, rdtree.pred, dnn=c("Actual","Predicted"))
rdtree.perf

prop.table(rdtree.perf,1)

#bin nextBA and see if decision tree works better
bin = 5
cutpoints <- quantile(ndf$nextBA, (0:bin)/bin)
ndf$nextBAbin = cut(ndf$nextBA, cutpoints, include.lowest = T)
ndf$nextBAbinf = factor(ndf$nextBAbin, 
                        levels = c("[0.02,0.195]","(0.195,0.235]",
                                   "(0.235,0.26]","(0.26,0.283]","(0.283,0.636]"),
                        labels = c("1stQtl","2ndQtl","3rdQtl","4thQtl","5thQtl"))
ndf.train = ndf[id.train,]
ndf.val = ndf[-id.train,]

#classification decision tree using binned nextBA
rdtree = rpart(nextBAbinf ~ G  + SlugPct + OBP + AB + BA + bats + age, data=ndf.train, method="class", parms=list(split="information"))
prp(rdtree, type=4, extra=104, fallen.leaves=F, main="Decision Tree", faclen=0)
barplot(rdtree$variable.importance)

rdtree.pred = predict(rdtree, newdata=ndf.val,type="class")
rdtree.perf = table(ndf.val$nextBAbinf, rdtree.pred, dnn=c("Actual","Predicted"))
rdtree.perf

prop.table(rdtree.perf,1)

##### NEW ####
#classification decision tree using imprvBA
#rdtree = rpart(imprvBA ~ G + AB + BA + bats + age, data=ndf.train, method="class", parms=list(split="gini"))
rdtree = rpart(imprvBA ~ G  + SlugPct + OBP + OPS + AB + BA + bats + age, data=ndf.train, method="class", parms=list(split="gini"))
prp(rdtree, type=4, extra=104, fallen.leaves=F, main="Decision Tree", faclen=0)
barplot(rdtree$variable.importance)

rdtree.pred = predict(rdtree, newdata=ndf.val,type="class")
rdtree.perf = table(ndf.val$imprvBA, rdtree.pred, dnn=c("Actual","Predicted"))
rdtree.perf

prop.table(rdtree.perf,1)

#classification decision tree using imprvOPS

#rdtree = rpart(imprvBA ~ G + AB + BA + bats + age, data=ndf.train, method="class", parms=list(split="gini"))
rdtree = rpart(imprvOPS ~ G  + SlugPct + OPS + OBP + AB + BA + bats + age, data=ndf.train, method="class", parms=list(split="gini"))
prp(rdtree, type=4, extra=104, fallen.leaves=F, main="Decision Tree", faclen=0)
barplot(rdtree$variable.importance)

rdtree.pred = predict(rdtree, newdata=ndf.val,type="class")
rdtree.perf = table(ndf.val$imprvOPS, rdtree.pred, dnn=c("Actual","Predicted"))
rdtree.perf

prop.table(rdtree.perf,1)


#classification decision tree using imprvSlugPct
rdtree = rpart(imprvSlugPct ~ G  + SlugPct + OPS + OBP + AB + BA + bats + age, data=ndf.train, method="class", parms=list(split="gini"))
prp(rdtree, type=4, extra=104, fallen.leaves=F, main="Decision Tree", faclen=0)
barplot(rdtree$variable.importance)

rdtree.pred = predict(rdtree, newdata=ndf.val,type="class")
rdtree.perf = table(ndf.val$imprvSlugPct, rdtree.pred, dnn=c("Actual","Predicted"))
rdtree.perf

prop.table(rdtree.perf,1)

#classification decision tree using imprvBA
#rdtree = rpart(imprvBA ~ G + AB + BA + bats + age, data=ndf.train, method="class", parms=list(split="gini"))
rdtree = rpart(imprvBA ~ G + AB + R + H + weight  + bats + age, data=ndf.train, method="class", parms=list(split="gini"))
prp(rdtree, type=4, extra=104, fallen.leaves=F, main="Decision Tree", faclen=0)
barplot(rdtree$variable.importance)

rdtree.pred = predict(rdtree, newdata=ndf.val,type="class")
rdtree.perf = table(ndf.val$imprvBA, rdtree.pred, dnn=c("Actual","Predicted"))
rdtree.perf

prop.table(rdtree.perf,1)

