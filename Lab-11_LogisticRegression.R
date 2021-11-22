#######################
###Author: Lydia Scherr
###Date: 4/18/2017
###Multiple Linear & Logonal Regression
#######################################################

#Preliminaries
rm(list=ls())

#libraries
install.packages("car",dependencies=TRUE)
library(car)
library(ggplot2)
require(gridExtra)
library(arm)
library(Hmisc)
library(LOGIT)
#set up working directory
data.dir<-"R:/EDA2017/Class_11/"

#read block and tract data
library(readr)
dis_dev <- read_csv("~/Desktop/dis_dev.csv")
#attach dis_dev data
attach(dis_dev)
View(dis_dev)
names(dis_dev)

 #169 obs, 52 variables
#1 variable was added to the original dataset to reflect the dichotomy of hdi_di>=0 and hdi_di<0.

##################################################################################################
#Mannually Create Dependent Variable: (disdev$hdi_di)
#hdi_di uses 1 represents average human development annual change (hdi_ac) >= 0 
#and hdi_di 0 represents average (hdi_ac) < 0
#159=1, 10=0
#Dichotomy of the average annual change in HDI, 
#1970-2010 (1 if hdi_di>=0, 0 if hdi_di<0)
table(dis_dev$hdi_di)
#  0   1 
# 10  159
#159 countries experienced positive growth and 10 did not
##################################################################################################
#Create Independent Variables:
#Climate disaster, geologic, and multi disaster
######################################################
#Cliamte disasters: cyclones, droughts, and floods
dis_dev$climate_emdat<-dis_dev$cyc_emdat+dis_dev$dro_emdat+dis_dev$flood_emdat
#Geologic disasters: earthquakes
dis_dev$geologic_emdat<-dis_dev$equake_emdatnoaancedc
#All natural disasters: climate and geologic combined
dis_dev$multi_emdat<-dis_dev$climate_emdat+dis_dev$geologic_emdat

#######################################
#Exploratory statistics: summary, boxplot, histogram
#####################################################
###summary statistics
summary(dis_dev$hdi_initial) #control var
summary(dis_dev$hdi_ac) #old dependent
summary(dis_dev$hdi_di) #new dichotomy dependent 94% of countries have hdi_ac >= 0
summary(dis_dev$climate_emdat) #climate disasters
summary(dis_dev$geologic_emdat) #geologic disasters
summary(dis_dev$multi_emdat)#All disasters
summary(dis_dev$investments) #investmetns
summary(dis_dev$odagni) #aid
summary(un_dev_ldr_least$hdi_di) #un dev ldr_least
summary(un_dev_grp_ldr_noleast$hdi_di) #un dev grp ldr_noleast
summary(as.factor(dis_dev$un_dev_grp))#un dev grp mdr

#####Box plots and Histograms
#Dependent variable_old: Average annual change in HDI (hdi_ac), 1970-2010 
par(mfrow=c(1,2), cex=.7)
hist(dis_dev$hdi_ac, col="brown", breaks=55,main="Histogram of average human development (HDI)",cex.main=1, cex.lab=1, xlab="HDI")
row.names(dis_dev)<-dis_dev$isov10
Boxplot(dis_dev$hdi_ac, col="orange", id.method="identify",labels = rownames(dis_dev), main="Boxplot of average human development (HDI)",cex.main=1, cex.lab=1,ylab="HDI")
dev.off()
##Independent A 
#boxplot of HDI growth and climate disasters 
c<- ggplot(dis_dev, aes(factor(hdi_di), climate_emdat)) 
c + geom_boxplot()+scale_x_discrete("0 - HDI <0 , 1 - HDI>=0 ")+scale_y_continuous("climate disasters")
#Independent B 
#boxplot of HDI growth and geologic disasters
g<- ggplot(dis_dev, aes(factor(hdi_di), geologic_emdat))
g + geom_boxplot()+scale_x_discrete("0 - HDI <0 , 1 - HDI>=0 ")+scale_y_continuous("geologic disasters")
##Independent C 
#box plot of HDI growth and both climatic and geologic disasters (multi_emdat)
m<- ggplot(dis_dev, aes(factor(hdi_di), dis_dev$multi_emdat))
m + geom_boxplot()+scale_x_discrete("0 - HDI <0 , 1 - HDI>=0 ")+scale_y_continuous("all disasters")
#Control variable A: Investments
#box plot of HDI growth and investments
i<- ggplot(dis_dev, aes(factor(hdi_di), investments))
i + geom_boxplot()+scale_x_discrete("0 - HDI <0 , 1 - HDI>=0 ")+scale_y_continuous("investments")
#Control variable B: Aid (odagni)
#boxplot of HDI growth and aid 
o<- ggplot(dis_dev, aes(factor(hdi_di), odagni))
o + geom_boxplot()+scale_x_discrete("0 - HDI <0 , 1 - HDI>=0 ")+scale_y_continuous("Aid")
#Control variable C: inital HDI
z<- ggplot(dis_dev, aes(factor(hdi_di), hdi_initial))
z + geom_boxplot()+scale_x_discrete("0 - HDI <0 , 1 - HDI>=0 ")+scale_y_continuous("Initial HDI")
#Control Variable D: UN Development Group
d<- ggplot(dis_dev, aes(factor(hdi_di), as.factor(un_dev_grp)))
d + geom_boxplot()+scale_x_discrete("0 - HDI <0 , 1 - HDI>=0 ")+scale_y_discrete("Initial HDI")

############################
#Bivariate Logistic Regression
############################
#Explore relationships between dependent and independent variables 
#1 cliamte disaster events
fit1 <- glm (dis_dev$hdi_di ~ dis_dev$climate_emdat, family=binomial(link="logit"), data=dis_dev)
display (fit1)
invlogit(2.58+.01) #[1] 0.9302152, the model estimates 93% probability of HDI growth if the country experiences climate disasters
#divide coefficient (.01) by 4, .01/4-.0025, thus the probability of HDI growth if the country does not experience climate disasters is .25%
plot(dis_dev$climate_emdat, dis_dev$hdi_di, xlab="Climate disasters", ylab="Pr (HDI growth)")
points(dis_dev$climate_emdat,fit1$fitted,pch=19,col="blue",ylab="Prob of HDI grwoth",xlab="Climate disaster events")
#probability of growth is about 93% for countries with climate disasters.

#2 geologic disaster events
fit2 <- glm (hdi_di ~ geologic_emdat, family=binomial(link="logit"), data=dis_dev)
display (fit2)
invlogit(2.48+.1) #0.9295633, the model estimates 92.9% probability of HDI growth if the country experiences geologic disasters
#divide coefficent (.1) by 4, .1/4=.025, thus 2.5% chance of growth without geologic disasters
plot(dis_dev$geologic_emdat, dis_dev$hdi_di, xlab="Geologic disasters", ylab="Pr (HDI growth)")
points(dis_dev$geologic_emdat,fit1$fitted,pch=19,col="blue",ylab="Prob of HDI grwoth",xlab="Geologic disaster events")
#there does not appear to be a trend line, but geologic disasters are associated with growth 

#3 all natural disaster events
fit3 <- glm (hdi_di ~ multi_emdat, family=binomial(link="logit"), data=dis_dev)
display (fit3)
invlogit(2.49+.01) #[1]  0.9241418, model estimates 92% chance of growth with natural disasters 
#divide coefficent (.01) by 4, .01/4=.0025, thus the model predicts a .25% chance for growth without presence of natural disasters
plot(dis_dev$multi_emdat, dis_dev$hdi_di, xlab="climate and geologic disasters", ylab="Pr (HDI growth)")
points(dis_dev$multi_emdat,fit1$fitted,pch=19,col="blue",ylab="Prob of HDI grwoth",xlab="Climate and geologic disaster events")
#about 93% probability of growth for countries with climate and geologic disasters, and is almost 100% for countries with 100 or more disastersd

#########################
#Multivariate Regression

# Regression 4, Independent variable, climate disasters, intitial level of HDI, UN Development group, aid, and other investments.
fit4 <- glm (dis_dev$hdi_di ~ dis_dev$climate_emdat+ dis_dev$investments+ dis_dev$hdi_initial+ as.factor(dis_dev$un_dev_grp)+ dis_dev$odagni, family=binomial(link="logit"))
display (fit4)
#graphical representation for regression 4
par(mfrow=c(1,2))
plot(dis_dev$climate_emdat, dis_dev$hdi_di, xlab="Climate disasters", ylab="Pr (HDI growth)")
curve (invlogit(coef(fit4)[1]+coef(fit4)[2]*x), lwd=1, add=TRUE)

plot(dis_dev$investments, dis_dev$hdi_di, xlab="Investments", ylab="Pr (HDI growth)")
curve (invlogit(coef(fit4)[1]+coef(fit4)[3]*x), lwd=1, add=TRUE)
dev.off()

par(mfrow=c(1,2))
plot(dis_dev$hdi_initial, dis_dev$hdi_di, xlab="HDI Intitial", ylab="Pr (HDI growth)")
curve (invlogit(coef(fit4)[1]+coef(fit4)[4]*x), lwd=1, add=TRUE)

plot(as.factor(dis_dev$un_dev_grp), dis_dev$hdi_di, xlab="As factor of UN Development Group", ylab="Pr (HDI growth)")
curve (invlogit(coef(fit4)[1]+coef(fit4)[5]*x), lwd=1, add=TRUE)
dev.off()
##countries that received Aid grew almost 100%
plot(dis_dev$odagni, dis_dev$hdi_di, xlab="Aid", ylab="Pr (HDI growth)")
curve (invlogit(coef(fit4)[1]+coef(fit4)[6]*x), lwd=1, add=TRUE)

#5 regression, aid
fit5 <- glm (dis_dev$hdi_di ~ dis_dev$geologic_emdat+ dis_dev$investments+ dis_dev$hdi_initial+ as.factor(dis_dev$un_dev_grp)+ dis_dev$odagni, family=binomial(link="logit"))
display (fit5)
#graphical representation for regression 5
dev.off()
par(mfrow=c(1,2))
plot(dis_dev$geologic_emdat, dis_dev$hdi_di, xlab="Geologic disasters", ylab="Pr (HDI growth)")
curve (invlogit(coef(fit4)[1]+coef(fit4)[2]*x), lwd=1, add=TRUE)

plot(dis_dev$investments, dis_dev$hdi_di, xlab="Investments", ylab="Pr (HDI growth)")
curve (invlogit(coef(fit5)[1]+coef(fit5)[3]*x), lwd=1, add=TRUE)
dev.off()
par(mfrow=c(1,2))
plot(dis_dev$hdi_initial, dis_dev$hdi_di, xlab="HDI Intitial", ylab="Pr (HDI growth)")
curve (invlogit(coef(fit5)[1]+coef(fit5)[4]*x), lwd=1, add=TRUE)

plot(as.factor(dis_dev$un_dev_grp), dis_dev$hdi_di, xlab="As factor of UN Development Group", ylab="Pr (HDI growth)")
curve (invlogit(coef(fit5)[1]+coef(fit5)[5]*x), lwd=1, add=TRUE)
dev.off()
plot(dis_dev$odagni, dis_dev$hdi_di, xlab="Aid", ylab="Pr (HDI growth)")
curve (invlogit(coef(fit5)[1]+coef(fit5)[6]*x), lwd=1, add=TRUE)

#6 hdi initial
fit6 <- glm (dis_dev$hdi_di ~ dis_dev$multi_emdat+ dis_dev$investments+ dis_dev$hdi_initial+ as.factor(dis_dev$un_dev_grp)+ dis_dev$odagni, family=binomial(link="logit"))
display (fit6)

#graphical representation for regression 6
dev.off()
par(mfrow=c(1,2))
plot(dis_dev$multi_emdat, dis_dev$hdi_di, xlab="All disasters", ylab="Pr (HDI growth)")
curve (invlogit(coef(fit6)[1]+coef(fit6)[2]*x), lwd=1, add=TRUE)

plot(dis_dev$investments, dis_dev$hdi_di, xlab="Investments", ylab="Pr (HDI growth)")
curve (invlogit(coef(fit6)[1]+coef(fit6)[3]*x), lwd=1, add=TRUE)
dev.off()
par(mfrow=c(1,2))

plot(dis_dev$hdi_initial, dis_dev$hdi_di, xlab="HDI Intitial", ylab="Pr (HDI growth)")
curve (invlogit(coef(fit6)[1]+coef(fit6)[4]*x), lwd=1, add=TRUE)

plot(as.factor(dis_dev$un_dev_grp), dis_dev$hdi_di, xlab="As factor of UN Development Group", ylab="Pr (HDI growth)")
curve (invlogit(coef(fit6)[1]+coef(fit6)[5]*x), lwd=1, add=TRUE)
dev.off()
plot(dis_dev$odagni, dis_dev$hdi_di, xlab="Aid", ylab="Pr (HDI growth)")
curve (invlogit(coef(fit6)[1]+coef(fit6)[6]*x), lwd=1, add=TRUE)

###############
#Step 3: Regressions 7, 8 and 9
#Add interaction to each of the multivariate regressions ran at Step 2
#interaction: investments and aid 

#Regression 7: Interaction method I
fit7<- glm(dis_dev$hdi_di ~ dis_dev$climate_emdat+dis_dev$investments+dis_dev$odagni+dis_dev$hdi_initial+as.factor(dis_dev$un_dev_grp)+dis_dev$climate_emdat:dis_dev$investments:dis_dev$odagni:dis_dev$hdi_initial:as.factor(dis_dev$un_dev_grp), family=binomial(link="logit"))
display(fit7)
##graphical representation of 7
#Multivariate logisitc regression with interactions -graphical representation
#Investments
dev.off()
par(mfrow=c(1,3))
plot(dis_dev$investments,dis_dev$hdi_di, xlab="Cliamte Disasters",ylab="Pr(growth)")
curve(invlogit(coef(fit7)[1]+coef(fit7)[2]*x), lwd=1, add=TRUE)
plot(dis_dev$investments,dis_dev$hdi_di, xlab="Investments Concentration",ylab="Pr(growth)")
curve(invlogit(coef(fit7)[1]+coef(fit7)[3]*x), lwd=1, add=TRUE)
plot(dis_dev$climate_emdat,dis_dev$hdi_di, xlab="Interaction",ylab="Pr(growth)")
curve(invlogit(coef(fit7)[1]+coef(fit7)[4]*x), lwd=1, add=TRUE)
dev.off()
#Aid
par(mfrow=c(1,3))
plot(dis_dev$odagni,dis_dev$hdi_di, xlab="Climate Disasters",ylab="Pr(growth)")
curve(invlogit(coef(fit7)[1]+coef(fit7)[2]*x), lwd=1, add=TRUE)
plot(dis_dev$odagni,dis_dev$hdi_di, xlab="Aid Concentration",ylab="Pr(growth)")
curve(invlogit(coef(fit7)[1]+coef(fit7)[3]*x), lwd=1, add=TRUE)
plot(dis_dev$odagni,dis_dev$hdi_di, xlab="Interaction",ylab="Pr(growth)")
curve(invlogit(coef(fit7)[1]+coef(fit7)[5]*x), lwd=1, add=TRUE)
dev.off()
#Regression 8: Interaction method I
fit8<- glm(dis_dev$hdi_di ~ dis_dev$geologic_emdat+dis_dev$investments+dis_dev$odagni+dis_dev$hdi_initial+as.factor(dis_dev$un_dev_grp)+dis_dev$climate_emdat:dis_dev$investments:dis_dev$odagni:dis_dev$hdi_initial:as.factor(dis_dev$un_dev_grp), family=binomial(link="logit"))
display(fit8)
##graphical representation of 8
#Multivariate logisitc regression with interactions -graphical representation
#interaction with investements 
par(mfrow=c(1,3))
plot(dis_dev$investments,dis_dev$hdi_di, xlab="Geologic Disaster",ylab="Pr(growth)")
curve(invlogit(coef(fit8)[1]+coef(fit8)[2]*x), lwd=1, add=TRUE)
plot(dis_dev$investments,dis_dev$hdi_di, xlab="Investments Concentration",ylab="Pr(growth)")
curve(invlogit(coef(fit8)[1]+coef(fit8)[3]*x), lwd=1, add=TRUE)
plot(dis_dev$investments,dis_dev$hdi_di, xlab="Interaction", ylab="Pr(growth)")
curve(invlogit(coef(fit8)[1]+coef(fit8)[4]*x), lwd=1, add=TRUE)
dev.off()

#interaction with aid 
par(mfrow=c(1,3))
plot(dis_dev$odagni,dis_dev$hdi_di, xlab="Climate Disasters",ylab="Pr(growth)")
curve(invlogit(coef(fit8)[1]+coef(fit8)[2]*x), lwd=1, add=TRUE)
plot(dis_dev$odagni,dis_dev$hdi_di, xlab="Aid Concentration",ylab="Pr(growth)")
curve(invlogit(coef(fit8)[1]+coef(fit8)[3]*x), lwd=1, add=TRUE)
plot(dis_dev$odagni,dis_dev$hdi_di, xlab="Interaction", ylab="Pr(growth)")
curve(invlogit(coef(fit8)[1]+coef(fit8)[5]*x), lwd=1, add=TRUE)
dev.off()

#Regression 9: interaction method I
fit9<- glm(dis_dev$hdi_di ~ dis_dev$multi_emdat+dis_dev$investments+dis_dev$odagni+dis_dev$hdi_initial+as.factor(dis_dev$un_dev_grp)+dis_dev$multi_emdat:dis_dev$investments:dis_dev$odagni:dis_dev$hdi_initial:as.factor(dis_dev$un_dev_grp), family=binomial(link="logit"))
display(fit9)
##graphical representation of 8
#Multivariate logisitc regression with interactions -graphical representation
#interaction with investements 
par(mfrow=c(1,3))
plot(dis_dev$investments,dis_dev$hdi_di, xlab="All Disasters",ylab="Pr(growth)")
curve(invlogit(coef(fit9)[1]+coef(fit9)[2]*x), lwd=1, add=TRUE)
plot(dis_dev$investments,dis_dev$hdi_di, xlab="Investments Concentration",ylab="Pr(growth)")
curve(invlogit(coef(fit9)[1]+coef(fit9)[3]*x), lwd=1, add=TRUE)
plot(dis_dev$investments,dis_dev$hdi_di, xlab="Interaction", ylab="Pr(growth)")
curve(invlogit(coef(fit9)[1]+coef(fit9)[4]*x), lwd=1, add=TRUE)
dev.off()

#interaction with aid 
par(mfrow=c(1,3))
plot(dis_dev$odagni,dis_dev$hdi_di, xlab="All Disasters",ylab="Pr(growth)")
curve(invlogit(coef(fit9)[1]+coef(fit9)[2]*x), lwd=1, add=TRUE)
plot(dis_dev$odagni,dis_dev$hdi_di, xlab="Aid concentration",ylab="Pr(growth)")
curve(invlogit(coef(fit9)[1]+coef(fit9)[3]*x), lwd=1, add=TRUE)
plot(dis_dev$odagni,dis_dev$hdi_di, xlab="Interaction", ylab="Pr(growth)")
curve(invlogit(coef(fit9)[1]+coef(fit9)[5]*x), lwd=1, add=TRUE)
dev.off()

###############
#Step 4: ANOVA
#Perform ANOVA for all 9 regressions

##Regression 1-3
anova(fit1, test="Chisq")
anova(fit2, test="Chisq")
anova(fit3, test="Chisq")

##Regression 4-6
anova(fit4, test="Chisq")
anova(fit5, test="Chisq")
anova(fit6, test="Chisq")

##Regression 7-9
anova(fit7, test="Chisq")
anova(fit8, test="Chisq")
anova(fit9, test="Chisq")

#Interpret the coefficients using the standard deviation for each variable and multiply by coefficient
sd(dis_dev$climate_emdat) # 39.79534
sd(dis_dev$geologic_emdat) # 9.233093
sd(dis_dev$multi_emdat) # 45.66078
sd(dis_dev$hdi_initial) # 0.1737214
sd(dis_dev$investments) # NA
sd(dis_dev$odagni)  #NA
sd(as.factor(dis_dev$un_dev_grp)) #0.7233574

##Would be curious to investigate relationshp between: 
#education and GDP, education and HDI, education and investments, 
#education and climate_emdat, ect.. 
#to investigate relationships between education and on other variables
##End