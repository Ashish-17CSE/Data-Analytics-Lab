library(tidyr)
library(dplyr)
library(ggplot2)
library(knitr)

batsmen <- read.csv("C:/Users/Ashish/Desktop/R2/batsmen.csv")


##-------Descriptive statistics-------------##
#---Minimum-------------#
min(batsmen$Mat)
min(batsmen$Inns)
min(batsmen$NO)
min(batsmen$Runs)
min(batsmen$HS)
min(batsmen$Ave)

#---Maximum-------------#
max(batsmen$Mat)
max(batsmen$Inns)
max(batsmen$NO)
max(batsmen$Runs)
max(batsmen$HS)
max(batsmen$Ave)
max(batsmen$BF)

#---Mean------------------#
mean(batsmen$Mat)
mean(batsmen$Inns)
mean(batsmen$NO)
mean(batsmen$Runs)
mean(batsmen$HS)
mean(batsmen$Ave)

#---Variance-------------#
var(batsmen$Mat)
var(batsmen$Inns)
var(batsmen$NO)
var(batsmen$Runs)
var(batsmen$HS)
var(batsmen$Ave)

#---Standard Deviation------------#
sd(batsmen$Mat)
sd(batsmen$Inns)
sd(batsmen$NO)
sd(batsmen$Runs)
sd(batsmen$HS)

##----------------Bowlers-------------------------##

#---Taking bowlers data---------------#
bowler<-read.csv("C:/Users/Ashish/Desktop/R2/bowler.csv")

#---Find Minimum------------------------#
min(bowler$Mat)
min(bowler$Inns)
min(bowler$Overs)
min(bowler$Mdns)
min(bowler$Runs)
min(bowler$Wkts)

#---Find Maximum---------------#
max(bowler$Mat)
max(bowler$Inns)
max(bowler$Overs)
max(bowler$Mdns)
max(bowler$Runs)
max(bowler$Wkts)

#---Find Mean-----------------#
mean(bowler$Mat)
mean(bowler$Inns)
mean(bowler$Overs)
mean(bowler$Mdns)
mean(bowler$Runs)
mean(bowler$Wkts)

#---Find Variance--------------#
var(bowler$Mat)
var(bowler$Inns)
var(bowler$Overs)
var(bowler$Mdns)
var(bowler$Runs)
var(bowler$Wkts)

#---Find Standard Deviation--------------#
sd(bowler$Mat)
sd(bowler$Inns)
sd(bowler$Overs)
sd(bowler$Mdns)
sd(bowler$Runs)
sd(bowler$Wkts)

#---Bar Plot of Batsmen vs Runs
ggplot(batsmen) + geom_bar(aes(Runs,Player, fill = Runs), stat = 'identity') + coord_flip()+ labs(title="Batsmen: Player vs Runs") + theme(axis.text.x = element_text(angle = 90))



#---Bar Plot of Batsmen: Player vs Economy
barplot(bowler$Econ,col = "red", pch = 19, main = "Batsmen: Player vs Economy", xlab = "Player", ylab = "Economy")

#---Histogram for Batsmen
par(mfrow = c(5, 1))
par(mar = rep(2, 4))
hist(batsmen$Mat,main = "Histogram of Batsmen Matches")
hist(batsmen$Inns,main = "Histogram of Batsmen Innings")
hist(batsmen$Runs,main = "Histogram of Batsmen Runs")
hist(batsmen$Ave,,main = "Histogram of Batsmen Average")
hist(batsmen$SR,main = "Histogram of Batsmen Strike-Rate")

#---Histogram for Bowlers Data
par(mfrow = c(3, 3))
par(mar = rep(2, 4))
hist(bowler$Mat,main = "Histogram of Bowler Matches")
hist(bowler$Inns,main = "Histogram of Bowler Innings")
hist(bowler$Overs,main = "Histogram of Bowler overs")
hist(bowler$Ave,,main = "Histogram of Bowler Average")
hist(bowler$Econ,main = "Histogram of Bowler Economy")
hist(bowler$Wkts,main = "Histogram of Bowlers Wickets")

#---Outliers in Batsmen Runs
outlierKD(batsmen,  Runs)


#---Outliers in Batsmen Matches
outlierKD(batsmen,  Mat)

#---Outliers in Batsmen Average
outlierKD(batsmen,  Ave)

#---Outliers in Batsmen Strike Rate
outlierKD(batsmen,  SR)	

data <- read.csv("C:/Users/Ashish/Desktop/R2/batsmen.csv")
data

Bpoints <- ((data$Runs*10)+(data$Ave*6)+(data$SR*2)+(data$Fours*9)+(data$Sixes*11)+(data$HF*9))
Bpoints

data1 <- read.csv("C:/Users/Ashish/Desktop/R2/bowler.csv")
data1
data1$bat = "Bpoints"
data1[with(data1, order("bat")),]
data1
rum1 <- data1[order(data1$bat, decreasing = TRUE),]

poin <- c((data$Runs*10), (data$Ave*6), (data$SR*2), (data$Fours*9), (data$Sixes*11), (data$HF*9))
barplot(poin)

pairs(data[,2:7], pch=20, col="#FC4E07")
d <- data[-1,-1]
head(d)
sapply(d,class)
sapply(d, is.factor)
cor(d[sapply(d, function(x) !is.factor(x))])

d1 <- cor(d[sapply(d, function(x) !is.factor(x))])
head(d1)

d2 <- eigen(d1)$vectors
head(d2)

pc <- princomp(d1, cor = TRUE, scores = TRUE) #principal component   
pca <- prcomp(t(d1), scale = TRUE) #Principal Component Analysis 
pca$x
plot(pca$x[,1], pca$x[,2])
pca.var <- pca$sdev^2
pca.var
summary(pc)
plot(pc)
plot(pc,type="l")
biplot(pc)
dim(d)
attributes(pc)
pc$loadings
pc$scores
pc$call
pc$sdev
pc$center
pc$scale
pc$n.obs
str(pc)


data2 <- read.csv("C:/Users/Ashish/Desktop/R2/bowler.csv")
data2
pairs(data2[,2:5], pch=20, col="#FC4E07")
d3 <- data2[,-1]
head(d3)

sapply(d3,class)
sapply(d3, is.factor)
cor(d3[sapply(d3, function(x) !is.factor(x))])
#*cov(d3)
d4 <- cor(d3[sapply(d3, function(x) !is.factor(x))])
head(d4)
d5 <- eigen(d4)$vectors
head(d5)
pc1 <- princomp(d4, cor = TRUE, scores = TRUE)
pca1 <- prcomp(t(d4), scale = TRUE)
pca1$x
plot(pca1$x[,1], pca1$x[,2])
pca1.var <- pca1$sdev^2
pca1.var
summary(pc1)
plot(pc1)
plot(pc1,type="l")
biplot(pc1)
dim(d3)
attributes(pc1)
pc1$loadings
pc1$scores
pc1$call
pc1$sdev
pc1$center
pc1$scale
pc1$n.obs
str(pc1)

