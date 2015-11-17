###This code cleans up the data for the bed bug inesfly paint study

##Install and load necessary packages
#Install packages
#install.packages("")
#load packages
#library()

##set up the working directory
setwd("C:/Users/tradylan/Documents/Laboratory/Inesfly_Paint_Bed_Bug_Trial")

###bring in data
#for exposure 1 day post paint with individual measurements
D1Ind <- read.csv("DATA/Inesfly_Ind_1D.csv")
#for exposure 1 day post paint with jar counts
D1Jar <- read.csv("DATA/Inesfly_Jar_1D.csv")
#bring in temperature data
TEMPHUM<- read.csv("DATA/InesflyTempHum.csv")
#bring in paint data
PAINTDIST<- read.csv("DATA/InesflyPaintDistribution.csv")
##Bring in Pilot
Pilot1D<-read.csv("DATA/InesflyPilot_1D.csv")
Pilot100D<-read.csv("DATA/InesflyPilot_100D.csv")

##
#Split the unicode into relevant information for Individual observations
D1Ind$INSECT <- as.character(D1Ind$INSECT)
D1Ind$EXPOSE <- substr(D1Ind$INSECT, 1, 3)
D1Ind$TREATMENT <- substr(D1Ind$INSECT, 5, 6)
D1Ind$QUAD <- substr(D1Ind$INSECT, 8, 8)
#D1Ind$NUM <- substr(D1Ind$INSECT, 10, 11)

#Split the exposure code into relevant information for the group level observations
#first make Exposure a character
D1Jar$Exposure <- as.character(D1Jar)
#substring that character to split the exposure time by paint
igr <- grep("5A",D1Jar$Exposure)
cloro <- grep("CF", D1Jar$Exposure )
control <- grep("CO", D1Jar$Exposure )
D1Jar$paint<-(c(1:length(D1Jar$Exposure))*0)
D1Jar$paint[igr]<- "5A"
D1Jar$paint[cloro]<- "CF"
D1Jar$paint[control]<- "CO"
#and by length of time
#make vector of indecies for each time
oneh <- grep("1",D1Jar$Exposure)
threeh <- grep("3", D1Jar$Exposure )
sixh <- grep("6", D1Jar$Exposure )
oned<- grep("24", D1Jar$Exposure )
#make a blank table
D1Jar$time<-c(1:length(D1Jar$Exposure))*0
#Insert the corresponding time into the table
D1Jar$time[oneh]<- 1
D1Jar$time[threeh]<- 3
D1Jar$time[sixh]<- 6
D1Jar$time[oned] <- 24

#Remove blank column
chop<-which(names(D1Ind)=="X")
D1Ind<-D1Ind[,-chop]

#In order to get to cox test, reshape the data.

LD1Ind<-melt(D1Ind, id=c("INSECT","STAGE", "EXPOSE", "QUAD", "NOTES"))

#the variable needs to be turned into a date object
#so first make it a character
LD1Ind$variable<-as.character(LD1Ind$variable)
#remove the X's


#replace the "." with "-"

#now turn

