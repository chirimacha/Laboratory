###This code cleans up the data for the bed bug inesfly paint study
## Review Read me at

###############################################################################
###GENERAL SET UP###
###============================================================================
##Install and load necessary packages
#Install packages
#install.packages(c("reshape","survival","tables", "doBy", "ggplot2", "plyr",
#                "stargazer"))

#load packages
library(reshape) #Used to change data between short and long formats.
library(survival) #for cox proportional hazaard
library(tables)
library(doBy)#use summaryBy function
library(ggplot2)
library(plyr)
library(stargazer)

##set up the working directory
#PC for Dylan
#setwd("C:/Users/tradylan/Documents/Laboratory/Inesfly_Paint_Bed_Bug_Trial")
#MAC for office
setwd("/Users/mzlevy/Laboratory/Inesfly_Paint_Bed_Bug_Trial")

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

dia <- which(treatmentsum$paint == "5A")
tresmes <- which(treatmentsum$paint == "CO")
seismes <- which(treatmentsum$paint == "CF")

treatmentsum$pch <- treatmentsum$prop.alive * 0
treatmentsum$pch[dia] <- 15
treatmentsum$pch[tresmes] <- 20 
treatmentsum$pch[seismes] <- 17

hora <- which(treatmentsum$exp.time == "01H")
treshora <- which(treatmentsum$exp.time == "03H")
seishora <- which(treatmentsum$exp.time == "06H")
dias <- which(treatmentsum$exp.time == "24H")

treatmentsum$lty <- treatmentsum$prop.alive * 0
treatmentsum$lty[hora] <- 1
treatmentsum$lty[treshora] <- 2 
treatmentsum$lty[seishora] <- 3
treatmentsum$lty[dias] <- 4

#################################All Plot##################################
treatmentsum$paint <- factor(treatmentsum$paint, levels = c("CO","5A","CF"))

#Plot proportion alive
par(mfrow = c(1,1)) #4 across 3 
plot(y = treatmentsum$prop.alive, x = treatmentsum$day, 
     pch = treatmentsum$pch, col = treatmentsum$paint, type = "n",
     xaxt = 'n') 
udays <- c( 0, 1, 2, 7, 14, 21, 28)
axis(2, at =((1:9) / 10), labels = as.character(1:9 / 10))
axis(1, at = udays, labels = udays)
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

###################################An Array####################################
#Plot Prop Proportion living (alive+knockdown)
treatmentsum$exp.time <- revalue(treatmentsum$exp.time, c("01H" = "1", 
                                                          "03H" = "3", 
                                                          "06H" = "6",
                                                          "24H" = "24"))
pdf("TABLES_GRAPHS/Bioassay_Array/Bioassay_Graphs_Array.pdf", width = 6, 
    height = 9)
#jpeg("TABLES_GRAPHS/Bioassay_Array/Bioassay_Graphs_Array.jpeg", width = 6, 
#     height = 9, units = "in", res = 300)
dap <- unique(treatmentsum$days.after.paint)
dap <- dap[order(dap)]
ext <- unique(treatmentsum$exp.time)
par(mfrow = c(4,3), oma = c(1,1,2,1)) #4 across 3 
for(k in 1:length(ext)){
  tsdap <- which(treatmentsum$exp.time == ext[k])
  for(j in 1:length(dap)){
    tsext <- which(treatmentsum$days.after.paint == dap[j])
    tsde <- intersect(tsdap, tsext)  
    #d <- "days"
    #if(dap[j] == 1){d <- "day"}
    plot(y = treatmentsum$prop.alive, x = treatmentsum$day, 
         pch = treatmentsum$pch, col = treatmentsum$paint,
         type = "n", #main = as.character(paste("J =", ext[k], "hrs:", "K =", dap[j], 
                      #            d, sep = " ")), 
         ylab = "Proportion Alive", xlab = "Days Since Exposure", 
         xaxt = 'n', yaxt = 'n')
    udays <- c( 0, 7, 14, 21, 28)
    axis( 2, at = c(0:5 / 5), las = 2,
          labels = as.character(c(0:5 / 5)))
    axis( 1, at = udays, labels = udays)
    treatments.tmp <- unique(treatmentsum$treatment[tsde])
    for(i in 1:length(treatments.tmp)){
      tr <- which(treatmentsum$treatment == treatments.tmp[i])
      temp <- treatmentsum[tr,]
      points(y = temp$prop.liv, x = temp$day, pch = temp$pch[1],
             col = temp$paint[1])
      lines(y = temp$prop.liv, x = temp$day, pch = temp$pch[1], 
            col = temp$paint[1])      
    }
  }
}

mtext("Proportion of Alive Bugs", side = 3, line = 0, outer = TRUE, cex = 1.2)

mtext("1 Day After Painting", side = 3, line =-2 , outer = TRUE, cex=0.8, at = 0.18)
mtext("90 days after painting", side = 3, line =-2 , outer = TRUE, cex=0.8, at = 0.50)
mtext("180 days after painting", side = 3, line =-2 , outer = TRUE, cex=0.8, at = 0.84)
mtext("Exposed 1 Hour", side = 2, line = -0.2, outer = T, at = 0.938, adj = 1, cex = 0.8)
mtext("Exposed 3 Hours", side = 2, line = -0.2, outer =T, at = 0.69, adj = 1, cex = 0.8)
mtext("Exposed 6 Hours", side = 2, line = -0.2, outer = T, at = 0.4392, adj = 1, cex = 0.8)
mtext("Exposed 24 Hours", side = 2, line = -0.2, outer = T, at = 0.198, adj = 1, cex = 0.8)

dev.off()


#same graph but flipped.

#write.csv(treatmentsum, "DATA/treatmentsum.csv")
stargazer(treatmentsum, summary = FALSE)

#Create table with Proportion alive for each treatment at each day
cdf <- cast(treatmentsum, treatment ~ day, value = "prop.alive")
cdf$"2" <- NULL
thirteen<- which(is.na(cdf$"13") == FALSE)
cdf$"14"[thirteen] <- cdf$"13"[thirteen]
cdf$"13" <- NULL
cdf$"0" <- NULL
names(cdf) <- c("Treatment","1 day","1 week", "2 weeks", "3 weeks", "4 weeks")
missing<- which(is.na(cdf$"4 weeks") == TRUE)
cdf$"4 weeks"[missing] <- "No Data"

stargazer(cdf, summary = FALSE, type = "html", 
          out = "TABLES_GRAPHS/Bioassay_Table.html" )

