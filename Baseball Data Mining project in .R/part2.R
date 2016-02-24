library(Lahman)

data("Batting")
data("Master")
data("battingLabels")
batting <- battingStats()
str(batting)
str(battingLabels)

# get data on batters in last 10 years with at least 1 hit and 10 At-bats
myBatting <- as.data.frame(subset(batting, yearID > 2004 & H > 0 & AB > 10))

listvar = c("G","AB","R","H","X2B","X3B",
           "HR","RBI","SO","GIDP","BA")

mydpc = myBatting[listvar]

#analyze presence of missing values in dataset
summary(myBatting)
summary(mydpc)

#  visualize descriptive statistics of variables
library(psych)
describe(myBatting)
describe(mydpc)

# analyze if boxplots show presence of outliers
boxplot(myBatting$AB)
boxplot(myBatting$BA)
boxplot(myBatting$X3B)

# g) Create a scatterplot matrix to visualize association among variables (pairs(mydpc))
(pairs(mydpc))
library(car)
scatterplotMatrix(mydpc, spread=FALSE, smoother.args=list(lty=2))

# correlation matrix and corrplot()
M=cor(mydpc, use="complete.obs")
library(corrplot)
corrplot(M, method="ellipse")

# KMO test of Sampling Adequacy
KMO(M)

# Bartlett’s test of sphericity 
cortest.bartlett(M,n=length(mydpc[,1]))

# frequency associations
library(arules)
data(mydpc)
itemFrequencyPlot(Batting, topN=20, type="absolute")
