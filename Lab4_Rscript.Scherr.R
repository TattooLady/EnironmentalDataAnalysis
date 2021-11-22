##ENV Data Analysis Spring 2017
##Lab 4: Advanced exploratory analysis
##Run hierarchical and K means cluster analysis
#Due: Feb 14, 2017
#Author: Lydia Scherr
#########################################

#Preliminaries
rm(list=ls())

#libraries
library(stats)
library(plyr)
library(Hmisc)
library(statisticalModeling)
library(ggplot2)
require(gridExtra)
library(lattice)
library(car)

#set up working directory
data.dir<-"~/EDA2017"
setwd("~/EDA2017")

#read data
library(readr)
extensive_infectionrates_of_tsetse <- read_csv("/Volumes/Sony_16SA1/EnvDataAnalysis 2017/labs/Lab 4/extensive_infectionrates of tsetse.csv")
View(extensive_infectionrates_of_tsetse)
##too little instances of other villages so subset for main region
emboreet<-subset(extensive_infectionrates_of_tsetse,village=="Emboreet")
#dimension of dataset
dim(extensive_infectionrates_of_tsetse)
dim(emboreet)
#structure of the dataset
str(extensive_infectionrates_of_tsetse)
str(emboreet)
#restructure data
infectionRate2<-round(emboreet$`mean infection rates`,2)
#summary statistics of population and main village infection rates
summary(extensive_infectionrates_of_tsetse$`mean infection rates`)
summary(emboreet$`mean infection rates`)
#histogram of infection rates
par(mfrow=c(1,2))
hist(emboreet$`mean infection rates`, 
     breaks=9, 
     col="blue", 
     xlab="Mean infection rates",
     main = "Emboreet Infection Rates")
hist(extensive_infectionrates_of_tsetse$`mean infection rates`, 
     breaks=16, 
     col="blue", 
     xlab="Mean infection rates",
     main = "Population Infection Rates")
dev.off()
#boxplot
par(mfrow=c(1,2))
boxplot(emboreet$`mean infection rates`, 
        col="blue", main="Emboreet Variation")
boxplot(extensive_infectionrates_of_tsetse$`mean infection rates`, 
        col="blue", main="Population Variation")
dev.off()
#scatterplot for Emboreet
par(mfrow=c(1,2))
plot(emboreet$`mean infection rates`,
     col="blue",
     ylab="mean infection rate",
     main="Emboreet Infection Rates")
#scatterplot for Population
plot(extensive_infectionrates_of_tsetse$`mean infection rates`,
     col="blue",
     ylab="mean infection rate",
     main="Population Infection Rates")
dev.off()

#correlation between month and infection rate
rcorr(emboreet$month,emboreet$`mean infection rates`)

#scatterplot view latitude and longitude data
plot(emboreet$longitude,emboreet$latitude,
     col="blue",
     pch=19,
     cex=1,
     xlab = "Longitude",
     ylab = "Latitude",
     main = "Plot of Longitude and Latitude")

#calculate the distance for Emboreet using different merging methods
library(lattice)
library(permute)
library(cluster)
library(vegan)
lat_long<-data.frame(x=emboreet$latitude,y=emboreet$longitude)
dist(lat_long)
emboreet.bray<- vegdist(lat_long, method= "bray")
print(emboreet.bray)
#calculate clusters and generate a dendogram
emboreet.bray.agnes<-agnes(emboreet.bray)
emboreet.agnes.ALT<-agnes(lat_long, metric = "manhattan", method = "ward", stand=TRUE)
plot(emboreet.bray.agnes, which.plots = 2, main = "Dendogram of Emboreet")


#k means cluster calculation
kmeans_emboreet<-kmeans(lat_long, centers=3)
names(kmeans_emboreet)
#see clusters
kmeans_emboreet$cluster
#centroids
kmeans_emboreet$centers

#change numbers of iterations
?kmeans
kmeans_emboreet<-kmeans(lat_long, centers=2, iter.max=25)
kmeans_emboreet$cluster
#centroids
kmeans_emboreet$centers

#plot clusters
plot(emboreet$longitude,emboreet$latitude,col=kmeans_emboreet$cluster,pch=19,cex=1, 
     xlab = "longitude", 
     ylab = "latitude", 
     main="Emboreet K Means Clusters")
text(emboreet$longitude+.002,emboreet$latitude+.002,labels=as.character(1:26))

#
p<-ggplot(emboreet, aes(longitude, latitude))+geom_point(aes(colour = factor(kmeans_emboreet$cluster)))
p

#set seed for random number generation
set.seed(12435) 
par(mar=c(0,0,0,0),
    x <- rnorm(20,mean=rep(1:3,each=5),sd=0.2),
    y <- rnorm(20,mean=rep(c(1,3,2,1),each=5),sd=0.2),
    Scherr<-data.frame(x=x,y=y))
          
#end script
