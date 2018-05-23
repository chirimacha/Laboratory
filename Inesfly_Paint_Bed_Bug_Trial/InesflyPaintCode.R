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
DataMelt$treatment <- as.factor(DataMelt$treatment)
treatmentsum<-summaryBy(alive+dead+knockdown+unviable+living ~ treatment + day 
                        + paint + days.after.paint + exp.time,
                        data = DataMelt, FUN=sum, na.rm=TRUE,
                        keep.names=TRUE)

treatmentsum$total <- (treatmentsum$alive + treatmentsum$dead 
                       + treatmentsum$knockdown)

###Remove rows with 0 in total (taken as placed into microcentrofuge tubes)
###All bugs were alive when exposed to pesticide.  
#add/replace a column that shows everything at alive at day 0, before treatment
treatments <- unique(treatmentsum$treatment)
zeros <- which(treatmentsum$day == 0)
treatmentsum<- treatmentsum[-zeros,]

treatment.frame <- treatmentsum[1:length(treatments),]
treatment.frame$treatment <- treatments
treatment.frame$day <- treatment.frame$day * 0
treatment.frame$days.after.paint <- treatment.frame$day * 0
treatment.frame$alive <- treatment.frame$day * 0
treatment.frame$dead <- treatment.frame$day * 0
treatment.frame$knockdown <- treatment.frame$day * 0
treatment.frame$unviable <- treatment.frame$day * 0
treatment.frame$living <- treatment.frame$day * 0
treatment.frame$total <- treatment.frame$day * 0

for(i in 1 : length(treatments)) {
  tr <- which(treatmentsum$treatment == treatments[i])
  treatment.frame$total[i]  <- treatmentsum$total[min(tr)]                          
  treatment.frame$paint[i]  <- treatmentsum$paint[min(tr)]                          
  treatment.frame$days.after.paint[i] <- treatmentsum$days.after.paint[min(tr)]  
  treatment.frame$exp.time[i] <- treatmentsum$exp.time[min(tr)]   
}

treatment.frame$alive  <- treatment.frame$total                          
treatment.frame$living  <- treatment.frame$total                          

treatmentsum <- rbind(treatment.frame, treatmentsum)

###Creat Proportions of each of the status colums
treatmentsum$prop.alive <- treatmentsum$alive / treatmentsum$total
treatmentsum$prop.dead <- treatmentsum$dead / treatmentsum$total
treatmentsum$prop.kd <- treatmentsum$knockdown / treatmentsum$total
treatmentsum$prop.uv <- treatmentsum$unviable / treatmentsum$total
treatmentsum$prop.liv <- treatmentsum$living / treatmentsum$total

dia <- which(treatmentsum$days.after.paint == 1)
tresmes <- which(treatmentsum$days.after.paint == 90)
seismes <- which(treatmentsum$days.after.paint == 180)

treatmentsum$pch[dia] <- 18
treatmentsum$pch[tresmes] <- 20 
treatmentsum$pch[seismes] <- 17

hora <- which(treatmentsum$exp.time == "01H")
treshora <- which(treatmentsum$exp.time == "03H")
seishora <- which(treatmentsum$exp.time == "06H")
dias <- which(treatmentsum$exp.time == "24H")

treatmentsum$lty[hora] <- 1
treatmentsum$lty[treshora] <- 2 
treatmentsum$lty[seishora] <- 3
treatmentsum$lty[dias] <- 4

#Plot proportion alive
plot(y = treatmentsum$prop.alive, x = treatmentsum$day, 
     pch = treatmentsum$pch, col = treatmentsum$paint, type = "n")
for(i in 1:length(treatments)){
  tr <- which(treatmentsum$treatment == treatments[i])
  temp <- treatmentsum[tr,]
  points(y = temp$prop.alive, x = temp$day, 
        pch = temp$pch[1], col = temp$paint[1]   
  )
  lines(y = temp$prop.alive, x = temp$day, 
        col = temp$paint[1], lty = temp$lty[1]    
       )
}

#Plot Prop Proportion living (alive+knockdown)
plot(y = treatmentsum$prop.alive, x = treatmentsum$day, 
     pch = treatmentsum$pch, col = treatmentsum$paint, type = "n")
for(i in 1:length(treatments)){
  tr <- which(treatmentsum$treatment == treatments[i])
  temp <- treatmentsum[tr,]
  points(y = temp$prop.liv, x = temp$day, 
         pch = temp$pch[1], col = temp$paint[1]   
  )
  lines(y = temp$prop.liv, x = temp$day, 
        col = temp$paint[1], lty = temp$lty[1]    
  )
}

cdf<- cast(treatmentsum, treatment + day~ prop.alive)

###Plot each proption by day
SummaryData<- summaryBy(DataMelt, alive ~ treatment, fun.aggregate = mean)
dcast(DataMelt, living~ day + ,)

