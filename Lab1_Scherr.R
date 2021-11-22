#ENVPU Class 1 – Introduction to R
#Date: 1/17/17
#Author: Lydia Scherr
ls()
getwd()
dir()
help(setwd)
#setting relative paths
setwd("./Lab1")#folder Lab 1 is included as working directory
setwd("../")#return to directory EDA2017
datadir<-"//SIPAXFSC/Users/mv2197/Documents/EDA2017"
greenMarket<-read.csv("greenMarket.csv")
help(read.table)
names(greenMarket)
View(greenMarket)
list.files(data.dir)
column1<- greenMarket[,1]
write.table(greenMarket[,1],"./georeference.csv",sep=",")
