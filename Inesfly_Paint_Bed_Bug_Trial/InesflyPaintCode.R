###This code cleans up the data for the bed bug inesfly paint study
## Review Read me at

###############################################################################
###GENERAL SET UP###
###============================================================================
##Install and load necessary packages
#Install packages
#install.packages(c("reshape","survival","tables", "doBy", "ggplot2"))

#load packages
library(reshape) #Used to change data between short and long formats.
library(survival) #for cox proportional hazaard
library(tables)
library(doBy)#use summaryBy function
library(ggplot2)

##set up the working directory
#PC for Dylan
setwd("C:/Users/tradylan/Documents/Laboratory/Inesfly_Paint_Bed_Bug_Trial")
#MAC for Mike
#setwd("/Users/mzlevy/Laboratory/Inesfly_Paint_Bed_Bug_Trial")

###############################################################################
###bring in data
###============================================================================

##Orignal Excel Document on Dropbox at 
##https://www.dropbox.com/s/2e1cibn7vls22cq/InesflyPilotData4_10_Updated_LM%20%281%29.xlsx?dl=0
##or ~Dropbox\Inesfly Paint for Bed Bugs\Data Entry\InesflyPilotData4_10_Updated_LM (1)
##data was saved as CSV then moved to repository.
##data was double entered and cleaned in "InesflyPaintCode_Cleanformat.R"e

#Pilot Data 
##Bring in Pilot
#Pilot1D <- read.csv("DATA/InesflyPilot_1D.csv")
#Pilot100D <- read.csv("DATA/InesflyPilot_100D.csv")
#250 Days <- read.csv("DATA/")
#1 Year <- read.csv("DATA/")

#Data For Full Study: Individual Data for 1 Month
DataMelt <- read.csv("DATA/DataMelt.csv")

#bring in paint data
PAINTDIST<- read.csv("DATA/InesflyPaintDistribution.csv")

#bring in temperature data
TEMPHUM<- read.csv("DATA/InesflyTempHum.csv")

###============================================================================
#Proportions of bugs alive at each date
###============================================================================
#reshape/summaryBy to see the proportion of bugs at each and each treatment
DataMelt$treat.group <- as.factor(DataMelt$treat.gro)
treatmentsum<-summaryBy(alive+dead+knockdown+unviable+living ~ treat.group,
                        data = DataMelt, FUN=sum, na.rm=TRUE,
                        keep.names=TRUE)
treatmentsum$total <- treatmentsum$alive+treatmentsum$dead+treatmentsum$knockdown



SummaryData<- summaryBy(DataMelt, alive ~ treat.group, fun.aggregate = mean)
dcast(DataMelt, living~ day + ,)

