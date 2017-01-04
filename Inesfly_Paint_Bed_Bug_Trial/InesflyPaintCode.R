###This code cleans up the data for the bed bug inesfly paint study
## Review Read me at

###############################################################################
###GENERAL SET UP###
###============================================================================
##Install and load necessary packages
#Install packages
#install.packages( c("reshape","survival","tables", "doBy", "ggplot2", "plyr",
#                "stargazer", "interval" ))
#source("https://bioconductor.org/biocLite.R")
#biocLite("Icens")
#http://www.bioconductor.org/packages/release/bioc/html/Icens.html

#load packages
library(reshape) #Used to change data between short and long formats.
library(survival) #for cox proportional hazaard
library(tables)
library(doBy) #use summaryBy function
library(ggplot2)
library(plyr)
library(stargazer)
library(interval)

##set up the working directory
#PC for Dylan
setwd("C:/Users/dtracy198/Documents/GitHub/Laboratory/Inesfly_Paint_Bed_Bug_Trial")
#MAC for office
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
#pdf("TABLES_GRAPHS/Bioassay_Array/Bioassay_Graphs_Array.pdf", width = 6, 
#    height = 9)
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

#title
mtext("Proportion of Alive Bugs", side = 3, line = 0, outer = TRUE, cex = 1.2)
#Top Headings
mtext("1 Day After Painting", side = 3, line =-2 , outer = TRUE, cex=0.8, at = 0.18) 
mtext("90 days after painting", side = 3, line =-2 , outer = TRUE, cex=0.8, at = 0.50) 
mtext("180 days after painting", side = 3, line =-2 , outer = TRUE, cex=0.8, at = 0.84) 
#side headings
mtext("Exposed 1 Hour", side = 2, line = -0.2, outer = T, at = 0.938, adj = 1, cex = 0.8) 
mtext("Exposed 3 Hours", side = 2, line = -0.2, outer =T, at = 0.69, adj = 1, cex = 0.8) 
mtext("Exposed 6 Hours", side = 2, line = -0.2, outer = T, at = 0.4392, adj = 1, cex = 0.8) 
mtext("Exposed 24 Hours", side = 2, line = -0.2, outer = T, at = 0.198, adj = 1, cex = 0.8) 
#turn off pdf or jpeg
#dev.off() 


#same graph but flipped.

#write.csv(treatmentsum, "DATA/treatmentsum.csv")
#stargazer(treatmentsum, summary = FALSE) 

#Create table with Proportion alive for each treatment at each day
cdf <- cast(treatmentsum, treatment ~ day, value = "prop.liv") 
cdf$"2" <- NULL 
thirteen<- which(is.na(cdf$"13") == FALSE) 
cdf$"14"[thirteen] <- cdf$"13"[thirteen] 
cdf$"13" <- NULL
cdf$"0" <- NULL
names(cdf) <- c("Treatment","1 day","1 week", "2 weeks", "3 weeks", "4 weeks")
missing<- which(is.na(cdf$"4 weeks") == TRUE)
cdf$"4 weeks"[missing] <- "No Data"

#stargazer(cdf, summary = FALSE, type = "html", 
#          out = "TABLES_GRAPHS/Bioassay_Table.htm
write.csv(cdf,"cdf.csv")

###############################################################################
###We have to look at quadrants
#We have a column for quadrant(1-4), but we need to create unique columns
#for each dish
DataMelt$TreatQuad <- paste(DataMelt$quad, DataMelt$treatment, sep = "-")

#Lets summarize the data by treatquad
quadsum<-summaryBy(alive+dead+knockdown+unviable+living ~ TreatQuad + treatment + day 
                   + paint + days.after.paint + exp.time,
                   data = DataMelt, FUN=sum, na.rm=TRUE,
                   keep.names=TRUE)

quadsum$total <- (quadsum$alive + quadsum$dead 
                  + quadsum$knockdown)

##Remove rows with 0 in total (some moved pre-maturely to jars)
no.obs <- which(quadsum$total == 0)
quadsum <- quadsum[-no.obs,]

###All bugs were alive when exposed to pesticide.  
#remove then replace day 0 obs to show this. 
#(Current 0 obs were made the day of treatment, after treatment)
quads <- unique(quadsum$TreatQuad)
zeros <- which(quadsum$day == 0)
quadsum<- quadsum[-zeros,]

#create empty data frame for 0's 
quad.frame <- quadsum[1:length(quads),]
#fill in numeric data and quads
quad.frame$TreatQuad <- quads
quad.frame$day <- quad.frame$day * 0
quad.frame$days.after.paint <- quad.frame$day * 0
quad.frame$alive <- quad.frame$day * 0
quad.frame$dead <- quad.frame$day * 0
quad.frame$knockdown <- quad.frame$day * 0
quad.frame$unviable <- quad.frame$day * 0
quad.frame$living <- quad.frame$day * 0
quad.frame$total <- quad.frame$day * 0

#fill in categorical data and calc total
for(i in 1 : length(quads)) {
  qt <- which(quadsum$TreatQuad == quads[i])
  quad.frame$total[i]  <- quadsum$total[min(qt)]                          
  quad.frame$paint[i]  <- quadsum$paint[min(qt)]  
  quad.frame$treatment[i] <-quad.frame$treatment[min(qt)]
  quad.frame$days.after.paint[i] <- quadsum$days.after.paint[min(qt)]  
  quad.frame$exp.time[i] <- quadsum$exp.time[min(qt)]   
}

#everything is alive on day 0 so set equal to total
quad.frame$alive  <- quad.frame$total                          
quad.frame$living  <- quad.frame$total                          

#now bind the 0 data to the treatmentsum file 
quadsum <- rbind(quad.frame, quadsum)

###Creat Proportions of each of the status colums
quadsum$prop.alive <- quadsum$alive / quadsum$total
quadsum$prop.dead <- quadsum$dead / quadsum$total
quadsum$prop.kd <- quadsum$knockdown / quadsum$total
quadsum$prop.uv <- quadsum$unviable / quadsum$total
quadsum$prop.liv <- quadsum$living / quadsum$total

#identify paints
fap <- which(quadsum$paint == "5A")
cop <- which(quadsum$paint == "CO")
cfp <- which(quadsum$paint == "CF")

#set pch based on paint for graph below
quadsum$pch <- quadsum$prop.alive * 0
quadsum$pch[fap] <- 15
quadsum$pch[cop] <- 20 
quadsum$pch[cfp] <- 17

#identify length of exposure
hora <- which(quadsum$exp.time == "01H")
treshora <- which(quadsum$exp.time == "03H")
seishora <- which(quadsum$exp.time == "06H")
dias <- which(quadsum$exp.time == "24H")

#set lty for graph below
quadsum$lty <- quadsum$prop.alive * 0
quadsum$lty[hora] <- 1
quadsum$lty[treshora] <- 2 
quadsum$lty[seishora] <- 3
quadsum$lty[dias] <- 4


#################################All Plot##################################
quadsum$paint <- factor(quadsum$paint, levels = c("CO","5A","CF"))

#Plot proportion alive
par(mfrow = c(1,1)) #4 across 3 
plot(y = quadsum$prop.alive, x = quadsum$day, 
     pch = quadsum$pch, col = quadsum$paint, type = "n",
     xaxt = 'n') 
udays <- c( 0, 1, 2, 7, 14, 21, 28)
axis(2, at =((1:9) / 10), labels = as.character(1:9 / 10))
axis(1, at = udays, labels = udays)
for(i in 1:length(quads)){
  tr <- which(quadsum$TreatQuad == quads[i])
  temp <- quadsum[tr,]
  points(y = temp$prop.liv, x = temp$day, 
         pch = temp$pch[1], col = temp$paint[1]   
  )
  lines(y = temp$prop.liv, x = temp$day, 
        col = temp$paint[1], lty = temp$lty[1]    
  )
}

###################################An Array####################################
#Plot Prop Proportion living (alive+knockdown)
quadsum$exp.time <- revalue(quadsum$exp.time, c("01H" = "1", 
                                                "03H" = "3", 
                                                "06H" = "6",
                                                "24H" = "24"))
#pdf("TABLES_GRAPHS/Bioassay_Array/Bioassay_Graphs_Array_Quads.pdf", width = 6, 
height = 9)
#jpeg("TABLES_GRAPHS/Bioassay_Array/Bioassay_Graphs_Array_Quads.jpeg", width = 6, 
#     height = 9, units = "in", res = 300)
dap <- unique(quadsum$days.after.paint)
dap <- dap[order(dap)]
ext <- unique(quadsum$exp.time)
par(mfrow = c(4,3), oma = c(1,1,2,1)) #4 across 3 
for(k in 1:length(ext)){
  tsdap <- which(quadsum$exp.time == ext[k])
  for(j in 1:length(dap)){
    tsext <- which(quadsum$days.after.paint == dap[j])
    tsde <- intersect(tsdap, tsext)  
    #d <- "days"
    #if(dap[j] == 1){d <- "day"}
    plot(y = quadsum$prop.alive, x = quadsum$day, 
         pch = quadsum$pch, col = quadsum$paint,
         type = "n", #main = as.character(paste("J =", ext[k], "hrs:", "K =", dap[j], 
         #            d, sep = " ")), 
         ylab = "Proportion Alive", xlab = "Days Since Exposure", 
         xaxt = 'n', yaxt = 'n')
    udays <- c( 0, 7, 14, 21, 28)
    axis( 2, at = c(0:5 / 5), las = 2,
          labels = as.character(c(0:5 / 5)))
    axis( 1, at = udays, labels = udays)
    quads.tmp <- unique(quadsum$TreatQuad[tsde])
    for(i in 1:length(quads.tmp)){
      tr <- which(quadsum$TreatQuad == quads.tmp[i])
      temp <- quadsum[tr,]
      points(y = temp$prop.liv, x = temp$day, pch = temp$pch[1],
             col = temp$paint[1])
      lines(y = temp$prop.liv, x = temp$day, pch = temp$pch[1], 
            col = temp$paint[1])      
    }
  }
}

#title
mtext("Proportion of Alive Bugs", side = 3, line = 0, outer = TRUE, cex = 1.2)
#Top Headings
mtext("1 Day After Painting", side = 3, line =-2 , outer = TRUE, cex=0.8, at = 0.18) 
mtext("90 days after painting", side = 3, line =-2 , outer = TRUE, cex=0.8, at = 0.50) 
mtext("180 days after painting", side = 3, line =-2 , outer = TRUE, cex=0.8, at = 0.84) 
#side headings
mtext("Exposed 1 Hour", side = 2, line = -0.2, outer = T, at = 0.938, adj = 1, cex = 0.8) 
mtext("Exposed 3 Hours", side = 2, line = -0.2, outer =T, at = 0.69, adj = 1, cex = 0.8) 
mtext("Exposed 6 Hours", side = 2, line = -0.2, outer = T, at = 0.4392, adj = 1, cex = 0.8) 
mtext("Exposed 24 Hours", side = 2, line = -0.2, outer = T, at = 0.198, adj = 1, cex = 0.8) 
#turn off pdf or jpeg
#dev.off() 

###############################################################################

### To compare curves we need to do a log-rank test, which in R requiress us to
##transform data into a Survival Object
##This requires us to have a time (start of interval), time2(end of interval),
## as well as the event we care about,ie, death. 

#put status and interval together so they can be used in same function

#indexs are not being found

DataMelt$UID <- paste(DataMelt$INSECT, DataMelt$days.after.paint, sep="-")
DataMelt$day <- as.numeric(DataMelt$day)
#Remove day 0 and 2, so that each data is the same.
ceros <- which(DataMelt$day == 0)
twos <- which(DataMelt$day == 2)
xtra <- union(ceros, twos)
nodata<-which(DataMelt$status == "")
rdc<- union(xtra,nodata)
rMelt <- DataMelt[-rdc,]

UID<-unique(DataMelt$UID)
SurvTab<-data.frame(UID)

fillTime <- function(UIDs){
  indexs <- which(rMelt$UID == UIDs)  
  if(length(indexs) == 0){print("index not found")}
  subtab <- rMelt[indexs,]
  if(sum(subtab$dead) == 0){
    output<- max(subtab$day)
  }
  else{
    deaths <- which(subtab$dead == 1)
    output <- min(subtab$day[deaths])
  }
  return(output) 
}

fillTime.test <- function(UIDs){
  indexs <- which(rMelt$UID == UIDs)  
  if(length(indexs) == 0){print("index not found")}
  subtab <- rMelt[indexs,]
  if(sum(subtab$dead) == 0){
    output<- 1
  }
  else{
    deaths <- which(subtab$dead == 1)
    output <- 2
  }
  return(output) 
}

#Beginging of interval 
fillTimeTwo <- function(UIDs){
  indexs <- which(rMelt$UID == UIDs)  
  if(length(indexs) == 0){print("index not found")}
  subtab <- rMelt[indexs,]
  livs <- which(subtab$dead == 0)
  n <- length(subtab$day)
  if(sum(subtab$dead) <  n ) {
    output <- max(subtab$day[livs])}
  #<- sort(subtab$day, decreasing = FALSE)[length(livs)]}
  if(sum(subtab$dead) == n){output <- 0}
  if(sum(subtab$dead) == 0){output <- sort(c(subtab$day), decreasing = FALSE)[n-1]}
  return(output) 
}

fillTimeTwo.test <- function(UIDs){
  indexs <- which(rMelt$UID == UIDs)  
  if(length(indexs) == 0){print("index not found")}
  subtab <- rMelt[indexs,]
  livs <- which(subtab$dead == 0)
  n <- length(subtab$day)
  if(sum(subtab$dead) <  n ) {
    output <- 1}
  #<- sort(subtab$day, decreasing = FALSE)[length(livs)-1]}
  if(sum(subtab$dead) == n){output <- 2}
  if(sum(subtab$dead) == 0){output <- 3}
  return(output) 
}

#interval is sometimes (0:2 not 0:1 or 1:2) It appears all 1's are there, but not 


fillStat <- function(UIDs){
  indexs <- which(rMelt$UID == UIDs)  
  if(length(indexs) == 0){print("index not found")}
  subtab <- rMelt[indexs,]
  if(sum(subtab$dead) < 1){
    output <- 0
  }
  else{
    output <- 1
  }
  return(output) 
}

thirdtest <- function(UIDs){
  indexs <- which(rMelt$UID == UIDs)  
  output <- length(indexs)
}

fourtest <- function(UIDs){
  indexs <- which(rMelt$UID == UIDs)  
  subtab <- rMelt[indexs,]
  livs <- which(subtab$dead == 0)
  n <- length(subtab$day)
  output <- paste(n, length(livs), sep = "-")
}

SurvTab$Status <- sapply(SurvTab$UID, FUN = fillStat)
SurvTab$Time <- as.numeric(sapply(SurvTab$UID, FUN = fillTime))
SurvTab$strTime <-as.numeric(sapply(SurvTab$UID, FUN = fillTimeTwo))

SurvTab$Test1 <- sapply(SurvTab$UID, FUN = fillTime.test)
SurvTab$Test2 <- sapply(SurvTab$UID, FUN = fillTimeTwo.test)
SurvTab$Test3 <- sapply(SurvTab$UID, FUN = thirdtest)
SurvTab$Test4 <- sapply(SurvTab$UID, FUN = fourtest)

#Now we have the data we can make the survival object
SurvTab$Surv <- Surv(time = SurvTab$Time, event = SurvTab$Status)
SurvTab$Surv2 <- Surv(time = SurvTab$strTime, time2 = SurvTab$Time, 
                      event = SurvTab$Status, type = "interval")


# fst<-which(DataMelt$UID == DataMelt$UID[1])
# DataMelt[fst,]
# 
# naa <- which(is.na(SurvTab$Surv2)==TRUE)
# errortab<-SurvTab[naa,]
# View(errortab)
# probData1<-which(DataMelt$UID == errortab$UID[1])
# probData2<-which(DataMelt$UID == errortab$UID[2])
# probData2<-which(DataMelt$UID == errortab$UID[3])
# 
# View(DataMelt[probData1,])
# View(DataMelt[probData2,])
# View(DataMelt[probData2,])

initial <- which(DataMelt$day == 1)
init.data <- DataMelt[initial,]

#Merge Surv data with factor data
IndTab <- merge(SurvTab, init.data,by = "UID")

###############################################################################
###Now we can actually start analysis
#but day is IV.
#so make indicators for both variables for connections
don <- which(IndTab$days.after.paint == 1)
dni <- which(IndTab$days.after.paint == 90)
doe <- which(IndTab$days.after.paint == 180)

##It should be powered to compare the curves between
#5A and Control at each time point.
#so create paint indicators
ifa <- which(IndTab$paint == "5A")
ico <- which(IndTab$paint == "CO")
icf <- which(IndTab$paint == "CF")

#create exposure time indicators
oneh <- which(IndTab$exp.time == "01H")
thrh <- which(IndTab$exp.time == "03H")
sixh <- which(IndTab$exp.time == "06H")
tweh <- which(IndTab$exp.time == "24H")

##join indicators as appropriate
#5A and Controls for comp
ifc <- union(ifa, ico)
icc <- union(icf, ico)
iff <- union(ifa, icf)

#intersect paint and exposure
fcon <- intersect(ifc, oneh)
fcth <- intersect(ifc, thrh)
fcsi <- intersect(ifc, sixh)
fctw <- intersect(ifc, tweh)
cctw <- intersect(icc, tweh)
fftw <- intersect(iff, tweh)

#interset above with days since paint.
onfcon <- intersect(fcon, don)
onfcth <- intersect(fcth, don)
onfcsi <- intersect(fcsi, don)
onfctw <- intersect(fctw, don)
oncctw <- intersect(cctw, don)
onfftw <- intersect(fftw, don)


nifcon <- intersect(fcon, dni)
nifcth <- intersect(fcth, dni)
nifcsi <- intersect(fcsi, dni)
nifctw <- intersect(fctw, dni)
nicctw <- intersect(cctw, dni)
nifftw <- intersect(fftw, dni)


oefcon <- intersect(fcon, doe)
oefcth <- intersect(fcth, doe)
oefcsi <- intersect(fcsi, doe)
oefctw <- intersect(fctw, doe)
oecctw <- intersect(cctw, doe)
oefftw <- intersect(fftw, doe)


#I need to look into how to compare for 24hrs, 5A an Cf to con 
#w/o comparing to each other.
survdiff(IndTab$Surv2[onfcon] ~ IndTab$paint[onfcon], rho = 1)
survdiff(IndTab$Surv2[onfcon] ~ IndTab$paint[onfcon], rho = 0)
coxph(IndTab$Surv2[onfcon] ~ IndTab$paint[onfcon])
ictest(IndTab$Surv2[onfcon] ~ IndTab$paint[onfcon], rho = 0)

#Conventional Log-rank
survdiff(IndTab$Surv[onfcon] ~ IndTab$paint[onfcon])
survdiff(IndTab$Surv[onfcth] ~ IndTab$paint[onfcth], rho = 0)
survdiff(IndTab$Surv[onfcsi] ~ IndTab$paint[onfcsi], rho = 0)
survdiff(IndTab$Surv[onfctw] ~ IndTab$paint[onfctw], rho = 0)
#oncctw: One day, clorfenapyr, 24 hours exposure, 
survdiff(IndTab$Surv[oncctw] ~ IndTab$paint[oncctw], rho = 0)

survdiff(IndTab$Surv[nifcon] ~ IndTab$paint[nifcon], rho = 0)
survdiff(IndTab$Surv[nifcth] ~ IndTab$paint[nifcth], rho = 0)
survdiff(IndTab$Surv[nifcsi] ~ IndTab$paint[nifcsi], rho = 0)
survdiff(IndTab$Surv[nifctw] ~ IndTab$paint[nifctw], rho = 0)
survdiff(IndTab$Surv[nicctw] ~ IndTab$paint[nicctw], rho = 0)

survdiff(IndTab$Surv[oefcon] ~ IndTab$paint[oefcon], rho = 0)
survdiff(IndTab$Surv[oefcth] ~ IndTab$paint[oefcth], rho = 0)
survdiff(IndTab$Surv[oefcsi] ~ IndTab$paint[oefcsi], rho = 0)
survdiff(IndTab$Surv[oefctw] ~ IndTab$paint[oefctw], rho = 0)
survdiff(IndTab$Surv[oecctw] ~ IndTab$paint[oecctw], rho = 0)

#Now lets compare clorfenapry to 5A-IGR directly
survdiff(IndTab$Surv[onfftw] ~ IndTab$paint[onfftw], rho = 0)
survdiff(IndTab$Surv[nifftw] ~ IndTab$paint[nifftw], rho = 0)
survdiff(IndTab$Surv[oefftw] ~ IndTab$paint[oefftw], rho = 0)



#ON = "One day after painting", ni="90 days", "oe"=180 days, 
#"fc"=5A IGR vs Control, "cc"=Clorfenapyr vs Control
#on=1hour, th = 3hr, si= 6hr, tw=24 hours
ictest(IndTab$Surv2[onfcon] ~ IndTab$paint[onfcon], scores = "logrank1")
ictest(IndTab$Surv2[onfcth] ~ IndTab$paint[onfcth], scores = "logrank1")
ictest(IndTab$Surv2[onfcsi] ~ IndTab$paint[onfcsi], scores = "logrank1")
ictest(IndTab$Surv2[onfctw] ~ IndTab$paint[onfctw], scores = "logrank1")
#oncctw: One day, clorfenapyr, 24 hours exposure, 
ictest(IndTab$Surv2[oncctw] ~ IndTab$paint[oncctw], scores = "logrank1")

ictest(IndTab$Surv2[nifcon] ~ IndTab$paint[nifcon], scores = "logrank1")
ictest(IndTab$Surv2[nifcth] ~ IndTab$paint[nifcth], scores = "logrank1")
ictest(IndTab$Surv2[nifcsi] ~ IndTab$paint[nifcsi], scores = "logrank1")
ictest(IndTab$Surv2[nifctw] ~ IndTab$paint[nifctw], scores = "logrank1")
ictest(IndTab$Surv2[nicctw] ~ IndTab$paint[nicctw], scores = "logrank1")

ictest(IndTab$Surv2[oefcon] ~ IndTab$paint[oefcon], scores = "logrank1")
ictest(IndTab$Surv2[oefcth] ~ IndTab$paint[oefcth], scores = "logrank1")
ictest(IndTab$Surv2[oefcsi] ~ IndTab$paint[oefcsi], scores = "logrank1")
ictest(IndTab$Surv2[oefctw] ~ IndTab$paint[oefctw], scores = "logrank1")
ictest(IndTab$Surv2[oecctw] ~ IndTab$paint[oecctw], scores = "logrank1")

#Now lets compare clorfenapry to 5A-IGR directly
ictest(IndTab$Surv2[onfftw] ~ IndTab$paint[onfftw], scores = "logrank1")
ictest(IndTab$Surv2[nifftw] ~ IndTab$paint[nifftw], scores = "logrank1")
ictest(IndTab$Surv2[oefftw] ~ IndTab$paint[oefftw], scores = "logrank1")

#right censored data only?

###############################################################################
#Cluster considered

#Conventional Log-rank
survdiff(IndTab$Surv[onfcon] ~ IndTab$paint[onfcon]+ cluster(IndTab$TreatQuad[onfcon]))
survdiff(IndTab$Surv[onfcth] ~ IndTab$paint[onfcth]+ cluster(IndTab$TreatQuad[onfcth]))
survdiff(IndTab$Surv[onfcsi] ~ IndTab$paint[onfcsi]+ cluster(IndTab$TreatQuad[onfcsi]))
survdiff(IndTab$Surv[onfctw] ~ IndTab$paint[onfctw]+ cluster(IndTab$TreatQuad[onfctw]))
#oncctw: One day, clorfenapyr, 24 hours exposure, 
survdiff(IndTab$Surv[oncctw] ~ IndTab$paint[oncctw]+ cluster(IndTab$TreatQuad[oncctw]))

survdiff(IndTab$Surv[nifcon] ~ IndTab$paint[nifcon]+ cluster(IndTab$TreatQuad[nifcon]))
survdiff(IndTab$Surv[nifcth] ~ IndTab$paint[nifcth]+ cluster(IndTab$TreatQuad[nifcth]))
survdiff(IndTab$Surv[nifcsi] ~ IndTab$paint[nifcsi]+ cluster(IndTab$TreatQuad[nifcsi]))
survdiff(IndTab$Surv[nifctw] ~ IndTab$paint[nifctw]+ cluster(IndTab$TreatQuad[nifctw]))
survdiff(IndTab$Surv[nicctw] ~ IndTab$paint[nicctw]+ cluster(IndTab$TreatQuad[nicctw]))

survdiff(IndTab$Surv[oefcon] ~ IndTab$paint[oefcon]+ cluster(IndTab$TreatQuad[oefcon]))
survdiff(IndTab$Surv[oefcth] ~ IndTab$paint[oefcth]+ cluster(IndTab$TreatQuad[oefcth]))
survdiff(IndTab$Surv[oefcsi] ~ IndTab$paint[oefcsi]+ cluster(IndTab$TreatQuad[oefcsi]))
survdiff(IndTab$Surv[oefctw] ~ IndTab$paint[oefctw]+ cluster(IndTab$TreatQuad[oefctw]))
survdiff(IndTab$Surv[oecctw] ~ IndTab$paint[oecctw]+ cluster(IndTab$TreatQuad[oecctw]))

#Now lets compare clorfenapry to 5A-IGR directly
survdiff(IndTab$Surv[onfftw] ~ IndTab$paint[onfftw]+ cluster(IndTab$TreatQuad[onfftw]))
survdiff(IndTab$Surv[nifftw] ~ IndTab$paint[nifftw]+ cluster(IndTab$TreatQuad[nifftw]))
survdiff(IndTab$Surv[oefftw] ~ IndTab$paint[oefftw]+ cluster(IndTab$TreatQuad[oefftw]))
###############################################################################
#Let's consider that the data for every exposure is repeated 4 times 
#on 4 different painted quadrants.

IndTab$days.after.paint <- factor(IndTab$days.after.paint)
IndTab$TreatQuad <- factor(IndTab$TreatQuad)

coxm<-coxph(IndTab$Surv ~ IndTab$days.after.paint+IndTab$exp.time+ IndTab$paint +cluster(IndTab$TreatQuad))
stargazer(coxm, type ="html", out = "cox_model.html")

stm <- surv_test(IndTab$Surv[oefcon] ~ IndTab$paint[oefcon])

ictest(IndTab$Surv2[onfftw] ~ IndTab$paint[onfftw] + cluster(IndTab$TreatQuad[onfftw]), scores = "logrank1")

survdiff(IndTab$Surv[onfcon] ~ IndTab$paint[onfcon]+ cluster(IndTab$TreatQuad[onfcon]))


stargazer(coxm, type ="html", out = "cox_model.html")

###Create citations in r for mendeley
capture.output(utils:::print.bibentry(citation("videoplayR"), style = "Bibtex"),
               file = "endnote_import.bib")

