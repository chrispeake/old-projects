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
sdf <- as.data.frame(subset(df,  yearID > 2003 & H > 0 & AB > 100))

head(sdf)
summary(sdf)

library(VIM)
a=aggr(sdf,prop=FALSE,numbers=TRUE)
summary(a)
# no missing values


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

###### NEW
#create new binary columns that determines if player's offensive stats improves next year
ndf$imprvBA <- (ndf$nextBA > ndf$BA)
ndf$imprvSlugPct <- (ndf$nextSlugPct > ndf$SlugPct)
ndf$imprvOBP <- (ndf$nextOBP > ndf$OBP)
ndf$imprvOPS <- (ndf$nextOPS > ndf$OPS)
ndf$imprvBABIP <- (ndf$nextBABIP > ndf$BABIP)

# list of quantitative variables for analysis
lstvar = c("AB", "R", "H", "X2B", "X3B", "HR", "RBI", "OPS", "OBP", 
           "SlugPct", "CS", "SO", "IBB", "BB", "SH", "SF", "GIDP")

boxplot(ndf$AB)
boxplot( ndf$R,ndf$RBI)
boxplot( ndf$H,ndf$SO, ndf$BB)
boxplot(ndf$X2B, ndf$X3B, ndf$HR)
boxplot( ndf$BA,  ndf$OPS, ndf$OBP,ndf$SlugPct)
boxplot( ndf$CS,  ndf$IBB, ndf$SH, ndf$SF, ndf$GIDP)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


ndfpc = ndf[lstvar]
ndfpc$SlugPct = log(ndfpc$SlugPct)

pairs(ndfpc)
library(car)
M = cor(ndfpc, use="complete.obs")
library(corrplot)
corrplot(M, method="ellipse")

#Compute the KMO test of Sampling Adequacy (Compute correlation matrix M=cor(mydpc), 
##   and use kmo(M) function in psych package)
library(psych)
KMO(M)

cortest.bartlett(M, n=length(ndfpc[,1]))

# Identify the number of principal components to be extracted using a scree plot
fa.parallel(ndfpc, fa="pc", n.iter=100, show.legend=T, main="Scree Plot")
## says to use 3

principal(ndfpc, nfactor=4, rotate="none")

# Compute the principal component analysis with 3 
fit = principal(ndfpc, nfactor=3, rotate="varimax")
fa.sort(fit)
fit
fit$weights

prcomps=as.matrix(ndf[lstvar])%*%as.matrix(fit$weights)
ndf$PC1 = (ndf$AB* 0.17445417) + (ndf$R*0.08627033) + (ndf$H*0.12790066) + (ndf$X2B*0.13678334) + (ndf$X3B*-0.09506978) + 
  (ndf$HR* 0.09885707) + (ndf$RBI*0.14706620) + (ndf$OPS*-0.13145144)  + (ndf$OBP*-0.16449559) + (ndf$SlugPct*-0.09877089) +
  (ndf$CS*-0.05218464) + (ndf$SO*0.13875641) + (ndf$IBB*0.05095483) + (ndf$BB*0.06530387) + (ndf$SH*-0.02750932) +
  (ndf$SF* 0.18066201) + (ndf$GIDP*0.22284714)
  
ndf$PC3 = (ndf$AB*-0.1052758530) + (ndf$R*0.0271591032) + (ndf$H*-0.0351535320) + (ndf$X2B*-0.0314805183) + (ndf$X3B*0.0948309103) +
  (ndf$HR*0.0676158711) + (ndf$RBI*-0.0009443172) + (ndf$OPS*0.3502214759)  + (ndf$OBP*0.3501395918) + (ndf$SlugPct*0.3067862219) +
  (ndf$CS*0.0250060490) + (ndf$SO*-0.0556509649) + (ndf$IBB*0.0816131802) + (ndf$BB*0.0689607728) + (ndf$SH*-0.0838717247) +
  (ndf$SF*-0.1164000678) + (ndf$GIDP*-0.1712432872)

ndf$PC2 = (ndf$AB*0.07408775) + (ndf$R*0.12005927) + (ndf$H*0.09821545) + (ndf$X2B*0.01958059) + (ndf$X3B*0.38318085) +
  (ndf$HR*-0.13363142) + (ndf$RBI*-0.08472443) + (ndf$OPS*0.02454732)  + (ndf$OBP*0.11398889) + (ndf$SlugPct*-0.01655067) +
  (ndf$CS*0.38290446) + (ndf$SO*-0.01190202) + (ndf$IBB*-0.10802535) + (ndf$BB*0.02412646) + (ndf$SH*0.31580452) +
  (ndf$SF*-0.09347695) + (ndf$GIDP*-0.13466420)


# build training/validation sets
train.size = round(0.75*nrow(ndf))
set.seed(4321)
id.train = sample(1:nrow(ndf),train.size,replace=F)
ndf.train = ndf[id.train,]
ndf.val = ndf[-id.train,]

# regression decision tree using BA
library(rpart)
library(rpart.plot)

#classification decision tree using imprvBA
#rdtree = rpart(imprvBA ~ G + AB + BA + bats + age, data=ndf.train, method="class", parms=list(split="gini"))
#rdtree = rpart(imprvBA ~ G  +  SlugPct + OBP + AB + bats + age, data=ndf.train, method="class", parms=list(split="gini"))
#rdtree = rpart(imprvBA ~ AB + R + H + X2B + X3B + HR + RBI, data=ndf.train, method="class", parms=list(split="gini"))
#rdtree = rpart(imprvBA ~ AB + R + H + X2B + X3B + HR + RBI + OPS + OBP + age + CS + SO + IBB +BB +SH + SF + GIDP, data=ndf.train, method="class", parms=list(split="gini"))
#rdtree = rpart(imprvBA ~ PC1 + BA + PC3, data=ndf.train,control=rpart.control(minsplit=5, cp=0.0001), method="class", parms=list(split="gini"))
rdtree = rpart(imprvBA ~ PC1 + BA + PC3, data=ndf.train, method="class", parms=list(split="gini"))
prp(rdtree, type=4, extra=104, fallen.leaves=F, main="Decision Tree", faclen=0)
barplot(rdtree$variable.importance)

rdtree.pred = predict(rdtree, newdata=ndf.val,type="class")
rdtree.perf = table(ndf.val$imprvBA, rdtree.pred, dnn=c("Actual","Predicted"))
rdtree.perf

prop.table(rdtree.perf,1)

#bin nextBA and see if decision tree works better
bin = 4
cutpoints <- quantile(ndf$nextBA, (0:bin)/bin)
ndf$nextBAbin = cut(ndf$nextBA, cutpoints, include.lowest = T)
ndf$nextBAbinf = factor(ndf$nextBAbin, levels = c("[0.127,0.239]","(0.239,0.263]","(0.263,0.284]","(0.284,0.396]"), labels = c("1stQtr","2ndQtr","3rdQtr","4thQtr"))
ndf.train = ndf[id.train,]
ndf.val = ndf[-id.train,]

#classification decision tree using binned nextBA
rdtree = rpart(nextBAbinf ~ PC1 + PC3 + PC2, data=ndf.train, method="class", parms=list(split="gini"))
prp(rdtree, type=4, extra=104, fallen.leaves=F, main="Decision Tree", faclen=0)
barplot(rdtree$variable.importance)

rdtree.pred = predict(rdtree, newdata=ndf.val,type="class")
rdtree.perf = table(ndf.val$nextBAbinf, rdtree.pred, dnn=c("Actual","Predicted"))
rdtree.perf

prop.table(rdtree.perf,1)
