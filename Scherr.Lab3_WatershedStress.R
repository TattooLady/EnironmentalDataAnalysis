#ENVPU Lab 3: Identifying Watershed STressors Using Exploratory Data Analysis
#Due: 2/7/17
#Author: Lydia Scherr

#preliminaries
rm(list=ls())
#libraries
library(plyr)
library(Hmisc)
library(statisticalModeling)
library(lubridate)
library(ggplot2)
library(readr)
library(car)

#Greenmarket .csv filepath only valid for creator, new user must upload .csv manually
#set up working directory
getwd()
setwd("~/EDA2017/Lab3")
data.dir<-"~/EDA2017/Lab3"

#read data
lab3watershed <- read_csv("/Volumes/Sony_16SA1/EnvDataAnalysis 2017/labs/Lab 3/lab3watershed.csv")
View(lab3watershed)

#dimension of data set
dim(lab3watershed)
#structure of the data set
str(lab3watershed)

#Population ADVISORY summary statistics
summary(lab3watershed$ADVISORY)
#Population ADVISORY histogram of water shed advisory warngings
hist(lab3watershed$ADVISORY, 
     col="blue", 
     breaks=122, 
     xlab = "Number of Advisory Warnings", 
     ylab = "Observed Frequency",
     main="Fish and Wildlife Advisory Warnings at US Watersheds 1990-1999" )
#box plot
boxplot(lab3watershed$ADVISORY, 
        col='green', 
        xlab = "Observed Frequency",
        ylab = "Number of Advisory",
        main="Box Plot of Watershed Advisory Outliers")

#List of REGIONS
lab3watershed$REGION
#subset Northeast 
Northeast<-subset(lab3watershed, REGION == "northeast")
#Southeast subset
Southeast<-subset(lab3watershed, REGION == "southeast")
#Midwest subset
Midwest<-subset(lab3watershed, REGION == "midwest")
#Northwest Subset
Northwest<-subset(lab3watershed, REGION == "northwest")
#Southwest subset
Southwest<-subset(lab3watershed, REGION == "southwest")

#summary statistics of 
summary(Northeast$ADVISORY)
summary(Southeast$ADVISORY)
summary(Midwest$ADVISORY)
summary(Northwest$ADVISORY)
summary(Southwest$ADVISORY)

#histogram of
par(mfrow=c(2,3))
hist(Northeast$ADVISORY,
     xlab = "Fish and Wildlife Consumption Advisory",
     main="Northeast")
hist(Southeast$ADVISORY, 
     xlab = "Fish and Wildlife Consumption Advisory",
     main="Southeast")
hist(Midwest$ADVISORY, 
     xlab = "Fish and Wildlife Consumption Advisory",
     main="Midwest")
hist(Northwest$ADVISORY,
     xlab = "Fish and Wildlife Consumption Advisory",
     main="Northwest")
hist(Southwest$ADVISORY,
     xlab = "Fish and Wildlife Consumption Advisory",
     main="Southwest")
dev.off()

#comparison boxplot
unique(lab3watershed$REGION)
boxplot(log(lab3watershed$ADVISORY)~lab3watershed$REGION, 
        col="red",
        varwidth = TRUE,
        main="Regional Caomparison",
        ylab="Advisories")

#Test four numerical variables as potential stressors (independent variables)
#They are: 
#1. “Atmospheric deposition” (ATMOSDEP); 
#2. “Ambient water quality (conventional pollutants)” (CONPOLUT); 
#3. “Urban runoff potential” (URBNRNOF); 
#4. and “Population change” (POPUINCR).

#summary statistics of independent variables
summary(lab3watershed$ATMOSDEP)
summary(lab3watershed$CONPOLUT)
summary(lab3watershed$URBNRNOF)
summary(lab3watershed$POPUINCR)

#histograms of 
#show all four histgrams together
par(mfrow=c(2,2))
hist(lab3watershed$ATMOSDEP,
     xlab = "Atmospheric deposition",
     col = "grey",
     main = "Atmospheric deposition")
hist(lab3watershed$CONPOLUT,
xlab = "Ambient Water Quality",
col = "brown",
main = "Ambient water quality")
hist(lab3watershed$URBNRNOF,
     xlab = "Urban runoff potential",
     col = "purple",
     main = "Urban runoff potential")
hist(lab3watershed$POPUINCR, 
     xlab = "Percent population change",
     col = "green",
     main = "Population Change")
dev.off()

#Regional comparisons boxplot for each potential stressor
#atmospheric deposition
unique(lab3watershed$REGION)
boxplot(log(lab3watershed$ATMOSDEP)~lab3watershed$REGION, 
        col="grey",
        varwidth = TRUE,
        main="Atmospheric Deposition",
        ylab="Deposition")
#Ambient air quality
unique(lab3watershed$REGION)
boxplot(log(lab3watershed$CONPOLUT)~lab3watershed$REGION, 
        col="brown",
        varwidth = TRUE,
        main = "Ambient water quality",
        ylab="Quality")

#urban runoff potential 
unique(lab3watershed$REGION)
boxplot(log(lab3watershed$URBNRNOF)~lab3watershed$REGION, 
        col="purple",
        varwidth = TRUE,
        main="Urban Runoff Potential",
        ylab="Runoff")
#population change
unique(lab3watershed$REGION)
boxplot(log(lab3watershed$POPUINCR)~lab3watershed$REGION, 
        col="green",
        varwidth = TRUE,
        main="Regional Caomparison",
        ylab="Advisories")

#Part Three: Create scatterplots 

###I am struglling with plotting the values together with a legend####

#look at all the graphs together
#For (x-axis) Advisory and the (y-axis) are the four independent variables 

par(mfrow=c(2,2))
plot(log(lab3watershed$ATMOSDEP),log(lab3watershed$ADVISORY),
     pch=10,
     cex=0.5,
     cex.lab=.90, 
     cex.axis=1,
     cex.main=1, 
     ylab="Advisory",
     xlab="Atmospheric Deposition", 
     main="Test of Atmospheric Deposition and Advisory")
plot(log(lab3watershed$CONPOLUT),log(lab3watershed$ADVISORY),
     pch=10,
     cex=0.5,
     cex.lab=.90, 
     cex.axis=1,
     cex.main=1, 
     ylab="Advisory",
     xlab = "Ambient water quality",
     col = "brown",
     main = "Ambient water quality") 

#plot Urban Runoff Potential and Advisory
plot(log(lab3watershed$URBNRNOF),log(lab3watershed$ADVISORY),
     pch=10,
     cex=0.5,
     cex.lab=.90, 
     cex.axis=1,
     cex.main=1, 
     ylab="Advisory",
     xlab = "Urban Runoff Potential",
     col = "purple",
     main = "Urban Runoff Potential") 

#plot Population Change and Advisory
plot(log(lab3watershed$POPUINCR),log(lab3watershed$ADVISORY),
     pch=10,
     cex=0.5,
     cex.lab=.90, 
     cex.axis=1,
     cex.main=1, 
     ylab="Advisory",
     xlab = "Percent population change",
col = "green",
     main = "Population Change and Advisory")

palette()
legend(5,1,
       legend=c("Ad","AwQ","UrP","PopCh"),
       col=c("black","red","green3","blue","cyan","magenta"),
       pch=c(10,10),)
dev.off()

#print correlation matrix to test null hypothesis
rcorr(as.matrix(lab3watershed[c("POPUINCR","URBNRNOF","CONPOLUT","ATMOSDEP","ADVISORY")]),
      type="pearson")

#end script
