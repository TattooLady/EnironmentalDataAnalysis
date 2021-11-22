#######################
###Author: Lydia Scherr
###Date: 4/11/2017
###Lab 9 Linear regression
#######################################################

#Preliminaries
rm(list=ls())

#libraries
library(readr)
install.packages("car",dependencies=TRUE)
library(car)
library(ggplot2)
require(gridExtra)
library(arm)
library(Hmisc)

#set up working directory
setwd("~/EDA2017/Lab 9")

#read block and tract data
dis_dev <- read_csv("~/Desktop/dis_dev.csv")
View(dis_dev)
#169 obs, 51 variables

##########################
#exploratory data analysis
##########################

#explore control variables: investment and aid
#investments
summary(dis_dev$investments)
summary(dis_dev_reg$investments)
#aid
summary(dis_dev$odagni)
summary(dis_dev_reg$odagni)

#explore dependent variable: HDI average annual change, 1970-2010
summary(dis_dev$hdi_ac)

par(mfrow=c(1,2), cex=0.7)
hist(dis_dev$hdi_ac, col="brown", breaks=45,main="Histogram of HDI average annual change (1970-2010)",cex.main=1, cex.lab=1, xlab="HDI average annual change")
Boxplot(dis_dev$hdi_ac, col="orange",id.method="y",labels = rownames(dis_dev), main="Boxplot of of HDI average annual change (1970-2010)",cex.main=1, cex.lab=1,outcex=1, ylab="Boxplot of of HDI average annual change")

row.names(dis_dev)<-dis_dev$isov10


#explore the independent variable - climate disasters
dis_dev$climate_emdat<-dis_dev$cyc_emdat+dis_dev$dro_emdat+dis_dev$flood_emdat
summary(dis_dev$climate_emdat)
par(mfrow=c(1,2), cex=.7)
hist(dis_dev$climate_emdat, col="brown", breaks=55,main="Histogram of cliamte disaster events",cex.main=1, cex.lab=1, xlab="Number of natural disasters")
Boxplot(dis_dev$climate_emdat, col="orange", id.method="identify",labels = rownames(dis_dev), main="Boxplot of climate disaster events",cex.main=1, cex.lab=1,ylab="Number of disasters")

#identify outliers 
##"AUS" "BGD" "BRA" "CHN" "ETH" "IND" "JPN" "MDG" "MEX" "PHL" "USA" "VNM"
par(mfrow=c(1,2), cex=0.7)
hist(dis_dev$climate_emdat, col="brown", breaks=55,main="Histogram of climate natural disaster events",cex.main=1, cex.lab=1, xlab="Number of natural disasters")
Boxplot(dis_dev$climate_emdat, col="orange", id.method="y",labels = rownames(dis_dev), main="Boxplot of cliamte natural disaster events",cex.main=1, cex.lab=1,outcex=1, ylab="Number of climate disasters")

#transform variable to remove skewness
summary(log(dis_dev$climate_emdat+1))
par(mfrow=c(1,2), cex=0.7)
hist(log(dis_dev$climate_emdat+1), col="brown",main="Histogram of climate disaster events",cex.main=1, cex.lab=1, xlab="Number of climate disasters(log)")
Boxplot(log(dis_dev$climate_emdat+1), col="orange", id.method="y",labels = rownames(dis_dev), main="Boxplot of climate disaster events per land area",cex.main=1, cex.lab=1,outcex=1, ylab="Number of climate disasters(log)")


#calculate second independent variable: disaster per area
dis_dev$climate_per_land<-dis_dev$climate_emdat/dis_dev$landarea*1000000
summary(dis_dev$climate_per_land)

#plot disasters per area
par(mfrow=c(1,2), cex=0.7)
hist(dis_dev$climate_per_land, col="brown",breaks=35,main="Histogram of climate disaster events per land area",cex.main=1, cex.lab=1, xlab="Number of climate disasters per land area")
Boxplot(dis_dev$climate_per_land, col="orange", id.method="y",labels = rownames(dis_dev), main="Boxplot of natural disaster events per land area",cex.main=1, cex.lab=1,outcex=1, ylab="Number of natural disasters per land area")

#transform skewness for disaster per land variable
#plot disasters per area
par(mfrow=c(1,2), cex=0.7)
hist(log(dis_dev$climate_per_land+1), col="brown",main="Histogram of climate disaster events per land area",cex.main=1, cex.lab=1, xlab="Number of climate disasters per land area(log)")
Boxplot(log(dis_dev$climate_per_land+1), col="orange", id.method="y",labels = rownames(dis_dev), main="Boxplot of natural disaster events per land area",cex.main=1, cex.lab=1,outcex=1, ylab="Number of natural disasters per land area(log)")

#remove small islands TLS because impact is minimal (low population)
#remove ZWE, AFG, YEM, MOZ, MLI, GNB, BFA as it is an outlier because of the history of conflict
#remove CHN, due to rapid urbanization
dis_dev_reg<- subset(dis_dev, !(isov10=="TLS" |isov10=="AFG"|isov10=="YEM"|isov10=="MOZ"|isov10=="MLI"|isov10=="CHN"|isov10=="ZWE"|isov10=="GNB"|isov10=="BFA"))

#how did the distribution change?
par(mfrow=c(1,2), cex=0.7)
hist(dis_dev$hdi_ac, col="brown", breaks=45,main="Histogram of HDI average annual change (1970-2010)",cex.main=1, cex.lab=1, xlab="HDI average annual change")
hist(dis_dev_reg$hdi_ac, col="brown", breaks=45,main="Histogram of HDI average annual change (1970-2010)",cex.main=1, cex.lab=1, xlab="HDI average annual change")


par(mfrow=c(1,2), cex=0.7)
hist(log(dis_dev$climate_emdat+1), col="brown",breaks=15,main="Histogram of climatic natural disaster events",cex.main=1, cex.lab=1, xlab="Number of climate disasters")
##Removal of HDI outliers does not drastically change climatic disaster distribution
hist(log(dis_dev_reg$climate_emdat+1), col="brown",breaks=15,main="Histogram of climatic natural disaster events",cex.main=1, cex.lab=1, xlab="Number of climate disasters")

par(mfrow=c(1,2), cex=0.7)
hist(log(dis_dev$climate_per_land+1), col="brown",breaks=15,main="Histogram of climatic natural disaster events per land area",cex.main=1, cex.lab=1, xlab="Number of climatic disasters per land area")
hist(log(dis_dev_reg$climate_per_land+1), col="brown",breaks=15,main="Histogram of climatic natural disaster events per land area",cex.main=1, cex.lab=1, xlab="Number of climatic disasters per land area")


#########
#explore relationships between dependent and independent variables
#before removing outliers
#multi disaster events
plot_climate_old<-ggplot(dis_dev, aes(log(climate_emdat+1),hdi_ac))
plot_climate_old+geom_point(aes(colour = continent), size = 1)+stat_smooth(method=lm)+geom_text(aes(label=isov10, colour=continent),size=3.5,hjust=0,vjust=0)+scale_x_continuous("Number of total disaster events, natural logharitm")+scale_y_continuous("HDI average annual change")

#after removing outliers
plot_climate_new<-ggplot(dis_dev_reg, aes(log(climate_emdat+1),hdi_ac))
plot_climate_new+geom_point(aes(colour = continent), size = 1)+stat_smooth(method=lm)+geom_text(aes(label=isov10, colour=continent),size=3.5,hjust=0,vjust=0)+scale_x_continuous("Number of total disaster events, natural logharitm")+scale_y_continuous("HDI average annual change")

#per land
#before removing outliers
plot_climate_land_old<-ggplot(dis_dev, aes(log(climate_per_land+1),hdi_ac))
plot_climate_land_old+geom_point(aes(colour = continent), size = 1)+stat_smooth(method=lm)+geom_text(aes(label=isov10, colour=continent),size=3.5,hjust=0,vjust=0)+scale_x_continuous("Number of total disaster events per land, natural logharitm")+scale_y_continuous("HDI average annual change")

#after removing outliers
plot_climate_land_new<-ggplot(dis_dev_reg, aes(log(climate_per_land+1),hdi_ac))
plot_climate_land_new+geom_point(aes(colour = continent), size = 1)+stat_smooth(method=lm)+geom_text(aes(label=isov10, colour=continent),size=3.5,hjust=0,vjust=0)+scale_x_continuous("Number of total disaster events per land, natural logharitm")+scale_y_continuous("HDI average annual change")

#relationship between disaster and HDI annual change by region

plot_climate_region<-ggplot(dis_dev_reg, aes(log(climate_emdat+1),hdi_ac))+facet_grid(. ~ continent)
plot_climate_region+geom_point(aes(colour = continent), size = 1)+stat_smooth(method=lm)+geom_text(aes(label=isov10, colour=continent),size=3.5,hjust=0,vjust=0)+scale_x_continuous("Number of total disaster events, natural logharitm")+scale_y_continuous("HDI, average annual change")

plot_climate_land_region<-ggplot(dis_dev_reg, aes(log(climate_per_land+1),hdi_ac))+facet_grid(. ~ continent)
plot_climate_land_region+geom_point(aes(colour = continent), size = 1)+stat_smooth(method=lm)+geom_text(aes(label=isov10, colour=continent),size=3.5,hjust=0,vjust=0)+scale_x_continuous("Number of total disaster events per land area, natural logharitm")+scale_y_continuous("HDI average annual change")

#run bivariate regression
model1<-lm(hdi_ac~log(climate_emdat+1), data=dis_dev_reg)
summary(model1)

model2<-lm(hdi_ac~log(climate_per_land+1), data=dis_dev_reg)
summary(model2)



#interpretation
dev.off()
plot(log(dis_dev_reg$climate_emdat+1), dis_dev_reg$hdi_ac,pch=19,col="blue")
lines(log(dis_dev_reg$multi_emdat+1),model1$fitted,col="red",lwd=1)

model1<-lm(hdi_ac~log(climate_emdat+1), data=dis_dev_reg)
summary(model1)
#install.packages("arm")
library(arm)
display(model1)
confint(model1,level=0.95)


###############
#Multivariate regression
##explore control variables - initial level of HDI
summary(dis_dev_reg$hdi_initial)
par(mfrow=c(1,2),cex=0.7)
hist(dis_dev_reg$hdi_initial, col="brown", breaks=55,main="Histogram of HDI in 1970",cex.main=1, cex.lab=1, xlab="HDI")
Boxplot(dis_dev_reg$hdi_initial, col="orange", id.method="y",labels = rownames(dis_dev_reg),main="Boxplot of HDI in 1970",cex.main=1, cex.lab=1,ylab="HDI")


summary(log(dis_dev_reg$hdi_initial))
par(mfrow=c(1,2),cex=0.7)
hist(log(dis_dev_reg$hdi_initial), col="brown", breaks=15,main="Histogram HDI in 1970",cex.main=1, cex.lab=1, xlab="Log of HDI")
Boxplot(log(dis_dev_reg$hdi_initial), col="orange", id.method="y",labels = rownames(dis_dev_reg),main="Boxplot of HDI in 1970",cex.main=1, cex.lab=1,ylab="Log of HDI")

#investments
summary(dis_dev_reg$investments)
par(mfrow=c(1,2),cex=0.7)
hist(dis_dev_reg$investments, col="brown", main="Histogram of average investments \n as percentage of hdi",cex.main=1, cex.lab=1, xlab="Percent")
Boxplot(dis_dev_reg$investments, col="orange", id.method="y",labels = rownames(dis_dev_reg),main="Boxplot of average investments \n as percentage of hdi",cex.main=1, cex.lab=1,ylab="Percent")

#aid
summary(dis_dev_reg$odagni)
par(mfrow=c(1,2),cex=0.7)
hist(dis_dev_reg$odagni, col="brown", main="Histogram of Aid",cex.main=1, cex.lab=1, xlab="Aid")
Boxplot(dis_dev_reg$odagni, col="orange", id.method="y",labels = rownames(dis_dev_reg),main="Boxplot of Aid rate",cex.main=1, cex.lab=1,ylab="Rate")

#Multicollinearity
library(Hmisc)
dis_dev_reg$climate_log<-log(dis_dev_reg$climate_emdat+1)
dis_dev_reg$hdi_acl_log<-log(dis_dev_reg$hdi_ac+1)
dis_dev_reg$hdi_initial_log<-log(dis_dev_reg$hdi_initial+1)
dis_dev_reg$investments_log<-log(dis_dev_reg$investments+1)
dis_dev_reg$odagni_log<-log(dis_dev_reg$odagni+1)

rcorr(as.matrix(dis_dev_reg[c("hdi_ac","hdi_initial_log","climate_log","investments","odagni_log")]),type="pearson")

#run multiple regressions

model2<-lm(hdi_ac~hdi_initial_log+investments+odagni_log+climate_log, data=dis_dev_reg)
summary(model2)

model3<-lm(hdi_ac~hdi_initial_log+sy_initial+fertrate+investments+govcons_log+trade_log+log(climate_per_land+1), data=dis_dev_reg)
summary(model11)

