###This code cleans up the data for the bed bug inesfly paint study

##Install and load necessary packages
#Install packages
#install.packages(c("reshape","survival","tables"))
#load packages
library(reshape)
library(survival) #for cox proportional hazaard
library(tables)
library(doBy)#use summaryBy function
library(ggplot2)
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
D1Ind$TIME <- substr(D1Ind$INSECT, 1, 3)
D1Ind$TREATMENT <- substr(D1Ind$INSECT, 5, 6)
D1Ind$QUAD <- substr(D1Ind$INSECT, 8, 8)
#D1Ind$NUM <- substr(D1Ind$INSECT, 10, 11)
D1Ind$EXPOSE<-substr(D1Ind$INSECT, 1, 6)
#Split the exposure code into relevant information for
#the group level observations

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

LD1Ind<-melt(D1Ind, id=c("INSECT","STAGE","TIME","EXPOSE", "QUAD", "NOTES",
                         "TREATMENT"))

#the variable needs to be turned into a date object
#so first make it a character
LD1Ind$variable<-as.character(LD1Ind$variable)
#remove the X's
LD1Ind$variable <- gsub("X","",LD1Ind$variable)
#replace the "." with "-"
LD1Ind$variable <- gsub("[.]","-", LD1Ind$variable)
LD1Ind$variable<-as.Date(LD1Ind$variable)

#to prevent confusion lets rename "variable" to "date"
chngname<-which(names(LD1Ind)=="variable")
names(LD1Ind)[chngname] <- "DATE"
#lets also rename "value" to "status"
chval<-which(names(LD1Ind)=="value")
names(LD1Ind)[chval] <- "STATUS"

#==============================================================================
###Now that we have the data in a usable table, lets split the status into binary
#find 
alive <- which(LD1Ind$STATUS== "A")
knockdown <- which(LD1Ind$STATUS=="K")
dead <- which(LD1Ind$STATUS=="D")
unviable <- c(dead, knockdown)
living <- c(alive, knockdown)

##create columns 
#make Quad numeric in order to create blank columns
LD1Ind$QUAD<-as.numeric(LD1Ind$QUAD)

#create blank columns for each status.
LD1Ind$alive <- LD1Ind$QUAD*0
LD1Ind$alive[alive] <- 1
LD1Ind$knockdown <- LD1Ind$QUAD*0
LD1Ind$knockdown[knockdown] <- 1
LD1Ind$dead <- LD1Ind$QUAD*0
LD1Ind$dead[dead] <- 1
LD1Ind$unviable <- LD1Ind$QUAD*0
LD1Ind$unviable[unviable] <- 1
LD1Ind$living <- LD1Ind$QUAD*0
LD1Ind$living[living] <- 1

#==============================================================================
##Lets clean up the data so that we get smooth transitions
#If dead, then becomes knockdown mark as knock down.

#also consider case where knock down went to alive.

#consider removing 2015-09-11 and 13 since data not available for 24hr.

#also add back collective data to 1H-5A 2015-09-2015

#==============================================================================
#lets do simple calculations finding the number and proportion by group

##Use the summary By funciton on Expose and Date to get counts
treatmentsum<-summaryBy(alive+dead+knockdown+unviable+living~EXPOSE+DATE+
                        TIME+TREATMENT,data=LD1Ind, FUN=sum,na.rm=TRUE,
                        keep.names=TRUE)

#create column for total insect
treatmentsum$totalbug<-(treatmentsum$alive+treatmentsum$unviable)

#remove rows with totalbug=0
nobug<-which(treatmentsum$totalbug==0)
treatmentsum<-treatmentsum[-nobug,]

##add rows for total on each treatment and the total for each day
#treatment
sumpaint <- summaryBy(alive+dead+knockdown+unviable+living+totalbug~TREATMENT,
                      data=treatmentsum, FUN=sum,na.rm=TRUE, keep.names=TRUE)
#day
sumtdate <-summaryBy(alive+dead+knockdown+unviable+living+totalbug~DATE,
                     data=treatmentsum, FUN=sum,na.rm=TRUE, keep.names=TRUE)
##now add empty rows so you can join both tables together.
#treatment
sumpaint$EXPOSE<-sumpaint$alive*NA
sumpaint$DATE<-sumpaint$alive*NA
sumpaint$DATE<-as.Date(sumpaint$DATE, origin="1970-01-01")
sumpaint$TIME<-sumpaint$alive*NA
#DATE
sumtdate$EXPOSE<-sumtdate$alive*NA
sumtdate$TREATMENT<-sumtdate$alive*NA
sumtdate$TIME<-sumtdate$alive*NA

##join the three tables together
joined<-rbind(treatmentsum, sumtdate)
treatmentsum<-rbind(joined, sumpaint)

#make proportionality
treatmentsum$palive<treatmentsum$alive/treatmentsum$totalbug
treatmentsum$pdead<-treatmentsum$dead/treatmentsum$totalbug
treatmentsum$pKD<-treatmentsum$knockdown/treatmentsum$totalbug
treatmentsum$pUV<- treatmentsum$unviable/treatmentsum$totalbug
treatmentsum$pliving<-treatmentsum$living/treatmentsum$totalbug

#==============================================================================
#Create Nice table with summary 



#==============================================================================
###Lets create curves showing the proportion of status seperated by treatment.
#Control

#5A-IGR
g<-ggplot(aes( y= eggs, x= week, fill = infected, na.rm=TRUE),
          data= Compile[enona,]) +geom_boxplot(data=Compile[enona,])
g<-g+ggtitle("Distribution of Number of Eggs Laid by Infection Status Each Week")
g<-g+scale_fill_manual(values=c("blue", "red"))
#Clorofenapyr

##Lets plot death and unviable by time on each data set 

