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

boxplot(ndf$AB)
boxplot( ndf$R,ndf$RBI)
boxplot( ndf$H,ndf$SO, ndf$BB)
boxplot(ndf$X2B, ndf$X3B, ndf$HR)
boxplot( ndf$BA,  ndf$OPS, ndf$OBP,ndf$SlugPct)
boxplot( ndf$CS,  ndf$IBB, ndf$SH, ndf$SF, ndf$GIDP)

scdf = ndf
 # min - max standardization
scdf$AB = (scdf$AB-min(scdf$AB))/(max(scdf$AB)-min(scdf$AB))
scdf$R = (scdf$R-min(scdf$R))/(max(scdf$R)-min(scdf$R))
scdf$H = (scdf$H-min(scdf$H))/(max(scdf$H)-min(scdf$H))
scdf$RBI = (scdf$RBI-min(scdf$RBI))/(max(scdf$RBI)-min(scdf$RBI))

# list of quantitative variables for analysis
lstvar = c("AB", "R", "H", "X2B", "X3B", "HR", "RBI", "OPS", "OBP", "BA",
           "SlugPct", "CS", "SO", "IBB", "BB", "SH", "SF", "GIDP", "imprvOPS")
lstvar = c("AB", "R", "H", "RBI", "OPS", "OBP", "BA", "SlugPct", "imprvBA")
odf = ndf[lstvar]
odf = scdf[lstvar]
# build training/validation sets
train.size = round(0.75*nrow(odf))
set.seed(1234)
id.train = sample(1:nrow(odf),train.size,replace=F)
odf.train = odf[id.train,]
odf.val = odf[-id.train,]

#### KNN ####
library(class)

# set minimum and maximum values for k here
kmin=5
kmax=30


summary=matrix(nrow=kmax-kmin+1, ncol=2) #dimnames=c("k","Correctly classified rate"))
summary=data.frame(1,1)
names(summary)=c("k","Accuracy")
#list of k values
list=c(kmin:kmax)
for(i in list){
  result=knn(odf.train, odf.val, cl=odf.train$imprvBA, k=i)
  summary[i-(kmin-1),]= c(i, sum(diag(table(result,odf.val$imprvBA)))/nrow(odf.val))
}
summary

# k = 5 has 99% accuracy

#apply KNN classifier for k=5 and save results in test set scdf.val
pred=knn(odf.train, odf.val, cl=odf.train$imprvBA, k=5, prob=T)
#pred contains predicted values
odf.val$pred=as.numeric(pred)

#retrieve probabilities of classifications in testing set
attr(pred,"prob")

#compute miclassification matrix
table(odf.val$imprvBA,pred)
sum(diag(table(pred,odf.val$imprvBA)))/nrow(odf.val)

##### Clustering #####
lstvar = c("AB", "R", "H", "RBI", "OPS", "OBP", "BA", "SlugPct", "age", "weight")
lstvar = c("AB", "R", "H", "RBI", "OPS", "OBP", "BA", "SlugPct")
ndfns = ndf[lstvar]
ndfcl = ndf[lstvar]
ndfcl = scale(ndfcl)

#select optimal number k of clusters.
library(NbClust)
set.seed(4321)
devAskNewPage(ask=F)
nc=NbClust(ndfcl, min.nc=2,max.nc=10, method="kmeans", distance = "euclidean", index = "duda")
nc

par(mfrow=c(1,1))
#list of clustering metrics
nc$Best.nc

#apply k-means for k=4
fit=kmeans(ndfcl, 3, nstart=25)
#size of clusters
fit$size
#centroids
fit$centers
# total within-cluster sum of squares
fit$tot.withinss

#compute centroids in original scale
centers=aggregate(ndfns, by=list(cluster=fit$cluster), mean)
centers

#mydcl$cluster=fit$cluster
table(ndf$imprvSlugPct, fit$cluster)

# vary parameters for most readable graph
library(cluster) 
clusplot(ndfns, fit$cluster, color=TRUE, shade=TRUE, 
         labels=1, lines=0)

#add cluster assignment
myd$cluster=fit$cluster
