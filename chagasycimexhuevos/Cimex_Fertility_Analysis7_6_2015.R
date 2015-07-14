#packages needed
library(lubridate) #para extracting dates 
library(reshape2) #para make the wide data into long data
library(vioplot)
library(matrixStats)
library(ggplot2)
library(plyr)  #para rbind.fill function

#set directory and bring in files to be analyzed.
setwd("c:\\Users\\tradylan\\Documents\\Laboratory\\chagasycimexhuevos")
#setwd("/Users/mzlevy/Laboratory/chagasycimexhuevos")


#bring in hatching data
cimfertpilot <- read.csv("Cimex_FertP.csv")
#https://docs.google.com/spreadsheets/d/1E-GRO1_Ybrgqj0wjgz5s9YHY1KwJUVPov0I2PwgC2CQ/edit
cimfert1 <- read.csv("Cimex_FertR1.csv")
##https://docs.google.com/spreadsheets/d/1iDBITasgMrbmwGJJSwsPkcal1b7b3kdfmviRtw8wbqA/edit#gid=709304485
cimfert2 <- read.csv("Cimex_FertR2.csv")
#https://docs.google.com/spreadsheets/d/13RvsL-uZaKgJPN3RBf8nTlsR8BQ6U-BrEQ_7BmxYgsI/edit

#bring in temperature and humidity data.
#the pilot has the temp and RH data for all sections
tempRH <- read.csv("TEMP_Y_RH.csv")
#from same table as above.

###Put all the data together.
##Create a table with all the insects.
#first create marker so we can identify each trial.
cimfertpilot$trial <- 0
cimfert1$trial <- 1
cimfert2$trial <- 2

#bring the tables together.".fill" allows different table lengths fill w/ NA
cimfert<-rbind.fill(cimfertpilot, cimfert1, cimfert2)

#create unicode for insect pair
cimfert$ID <- paste(cimfert$Nro_.pareja, cimfert$trial, sep="-")
#create column that specifies specific mouse
cimfert$raton <-paste(cimfert$Procedencia, cimfert$trial, sep="-")
#first use Procedencia to get certain groups
ControlP <- which(cimfert$Procedencia=="CO")
InfectPA <- which(cimfert$Procedencia=="I-R1")
InfectPB <- which(cimfert$Procedencia=="I-R2")
ControlA <- which(cimfert$Procedencia=="CO-A")
ControlB <- which(cimfert$Procedencia=="CO-B")
InfectA <- which(cimfert$Procedencia=="I-A")
InfectB <- which(cimfert$Procedencia=="I-B")

##Now create some indexes using the mouse.
#the R stands for which Rep it was in. RA is rep 1 and RB is rep2.
#r doesn't like numbers in the names
InfectARA <-which(cimfert$raton=="I-A-1")
InfectARB <-which(cimfert$raton=="I-A-2")
InfectBRA <-which(cimfert$raton=="I-B-1")
InfectBRB <-which(cimfert$raton=="I-B-2")
ControlARA <- which(cimfert$raton=="CO-A-1")
ControlARB <- which(cimfert$raton=="CO-A-2")
ControlBRA <- which(cimfert$raton=="CO-B-1")
ControlBRB <- which(cimfert$raton=="CO-B-2")
#group the appropriate ones for later usage
controls <- c(ControlP, ControlA, ControlB)
infect <- c(InfectPA, InfectPB, InfectA, InfectB)
InfectP <- c(InfectPA, InfectPB)
InfectRA <- c(InfectARA, InfectBRA)
InfectRB <- c(InfectARB, InfectBRB)
ControlRA <- c(ControlARA, ControlBRA)
ControlRB <- c(ControlARB, ControlBRB)

#make column designating infected/control
cimfert$infected[infect]<-1
cimfert$infected[controls]<-0

#find which columns are hatch and which are eggs.
postura<-which(substr(names(cimfert), nchar(names(cimfert)), nchar(names(cimfert)))=="p")
viabilidad <-which(substr(names(cimfert), nchar(names(cimfert)), nchar(names(cimfert)))=="v")

#We need to create outputs for the data.
blank <- (1:(length(postura)*length(cimfert$Procedencia))*0)
Compile <- data.frame(blank,0,0,0,0,0,0, 0, 0, 0, 0, 0)

Compile <- rename(Compile, replace = c("blank"="id", "X0"="parents","X0.1"="infected","X0.2"="start",
                                       "X0.3"="week", "X0.4"="date", "X0.5"="eggs", "X0.6"="hatch", "X0.7"="death",
                                       "X0.8"="mouse", "X0.9"="procedencia", "X0.10"="trial"))
#if adding colums, be sure that you capitalize the Xx

#make sure the cimfert columns are characters or they will not transfer.
cimfert$Nro_.pareja <- as.character(cimfert$Nro_.pareja)
cimfert$Procedencia <- as.character(cimfert$Procedencia)
cimfert$Fecha_Inicio_.Pareja <-as.character(cimfert$Fecha_Inicio_.Pareja)

#now we need to create a nested loop to get the data into a new data frame
for (d in 1:(length(postura))){
  for (i in 1:(length(cimfert$s1_p))){ #i for each insect
    Compile$week[i+((d-1)*length(cimfert$ID))] <- d
    Compile$trial[i+((d-1)*length(cimfert$ID))] <- cimfert$trial[i]
    Compile$mouse[i+((d-1)*length(cimfert$ID))] <- cimfert$raton[i]
    Compile$death[i+((d-1)*length(cimfert$ID))] <- cimfert$Longevidad[i]
    Compile$id[i+((d-1)*length(cimfert$ID))] <- cimfert$ID[i]
    Compile$parents[i+((d-1)*length(cimfert$ID))] <- cimfert$Nro_.pareja[i]
    Compile$procedencia[i+((d-1)*length(cimfert$ID))] <- cimfert$Procedencia[i]
    Compile$infected[i+((d-1)*length(cimfert$ID))] <- cimfert$infected[i]
    Compile$start[i+((d-1)*length(cimfert$ID))] <- cimfert$Fecha_Inicio_.Pareja[i]
    Compile$start[i+((d-1)*length(cimfert$ID))] <- cimfert$Fecha_Inicio_.Pareja[i]
    Compile$hatch[i+((d-1)*length(cimfert$ID))] <-cimfert[i,(2*d+3)]
    Compile$eggs[i+((d-1)*length(cimfert$ID))]<-cimfert[i,(2*d+2)]
  }
}

#We need to make a colum for alive  
#I need to check with them to ensure correct interpretation
for (i in 1:length(Compile$week))  
if(Compile$week[i] >= Compile$death[i]){
    Compile$alive[i]<-0
  } else(Compile$alive[i]<-1)
  
#Now that table is made, make date so that humidity and temperature data can be easily entered.
Compile$start <- parse_date_time(Compile$start, "dmy", tz="EST")
Compile$start <- as.Date(Compile$start)
Compile$date <- (Compile$start+(Compile$week*7))


###add temperature and humidity values.
#make the tempRH also date format
tempRH$FECHA <- parse_date_time(tempRH$FECHA, "dmy", tz="EST")
tempRH$FECHA <- as.Date(tempRH$FECHA)

#Once we have full data, we justneed to comment out the following line.
pilot<-which(Compile$trial==0)
Compile<-Compile[pilot,]
 
#-----------------------------
#Make function to calculate avg high temperature of week.
avtemphigh<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  mean(tempRH$TEMP.MAX..Â.C.[a:b], na.rm=TRUE)
}
date<-Compile$date
Compile$avtemphigh <-sapply(date, avtemphigh)

#Make function to calculate avg low temperature of week.
avtemplow<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  mean(tempRH$TEMP.MIN..Â.C.[a:b], na.rm=TRUE)
}
Compile$avtemplow <-sapply(date, avtemplow)

#tot average temp
avtemp <- function(rdate){
  rdate <- as.Date(rdate)
  fday <- (rdate-6)
  a <- which(tempRH$FECHA==fday)
  b <- which(tempRH$FECHA==rdate)
  c <- sum(tempRH$TEMP.MAX..Â.C.[a:b], na.rm=TRUE)
  d <- sum(tempRH$TEMP.MIN..Â.C.[a:b], na.rm=TRUE)
  ((c+d)/(2*((length(tempRH$TEMP.MAX..Â.C.[a:b]))-(length(which(is.na(tempRH$TEMP.MAX..Â.C.[a:b])==TRUE))))))
}

Compile$avtemp <- sapply(date, avtemp)
#week max
tempmax<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  max(tempRH$TEMP.MAX..Â.C.[a:b], na.rm=TRUE)
}
Compile$tempmax <-sapply(date, tempmax)

#week min
tempmin<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  min(tempRH$TEMP.MIN..Â.C.[a:b], na.rm=TRUE)
}
Compile$tempmin <-sapply(date, tempmin)

#greatest difference(heat shock)
tempRH$tempdiff<- tempRH$TEMP.MAX..Â.C.-(tempRH$TEMP.MIN..Â.C.)

tempdiff<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  max(tempRH$tempdiff[a:b], na.rm=TRUE)
}
Compile$tempdiff<-sapply(date, tempdiff)

#==============================================================
#Do the same for humidity
avhumhigh<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  mean(tempRH$HR.MAX....[a:b], na.rm=TRUE)
}
date<-Compile$date
Compile$avhumhigh <-sapply(date, avhumhigh)

#Make function to calculate avg low temperature of week.
avhumlow<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  mean(tempRH$HR.MIN....[a:b], na.rm=TRUE)
}
Compile$avhumlow <-sapply(date, avhumlow)

#tot average temp
avhum <- function(rdate){
  rdate <- as.Date(rdate)
  fday <- (rdate-6)
  a <- which(tempRH$FECHA==fday)
  b <- which(tempRH$FECHA==rdate)
  c <- sum(tempRH$HR.MAX....[a:b], na.rm=TRUE)
  d <- sum(tempRH$HR.MIN....[a:b], na.rm=TRUE)
  ((c+d)/(2*((length(tempRH$HR.MAX....[a:b]))-(length(which(is.na(tempRH$HR.MAX....[a:b])==TRUE))))))
}

Compile$avhum <- sapply(date, avhum)

#week max
hummax<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  max(tempRH$HR.MAX....[a:b], na.rm=TRUE)
}
Compile$hummax <-sapply(date, hummax)

#week min
hummin<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  min(tempRH$HR.MIN....[a:b], na.rm=TRUE)
}
Compile$hummin <-sapply(date, hummin)

#greatest difference(humidity shock?  I figured we had it for temp so why not)
tempRH$humdiff<- (tempRH$HR.MAX....)-(tempRH$HR.MIN....)

humdiff<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  max(tempRH$humdiff[a:b], na.rm=TRUE)
}
Compile$humdiff<-sapply(date, humdiff)

#Now lets not look at the number of eggs but the number of leg laying events.
events <- function(x){
  if(is.na(x)==TRUE){
    event <-NA
  } else if(x == 0){ 
    event <- 0
  } else if(x >=1){
    event <- 1}
  event
}
#egg events
Compile$eggevent<- (1:length(Compile$eggs))*NA
eggs <- as.list(Compile$eggs)
Compile$eggevent<-sapply(eggs, events)
Compile$eggevent <- as.numeric(Compile$eggevent)

#hatch events
Compile$hatchevent<- (1:length(Compile$hatch))*NA
hatch <- as.list(Compile$hatch)
Compile$hatchevent <-sapply(hatch, events)
Compile$hatchevent <- as.numeric(Compile$hatchevent)


########################################################################
#=======================================================================
#We now have all the data together and can now do some analysis.
#=======================================================================

###Using Cimfert
#solution: use "p" and "v" from substring of names to identify which are postura y viability columns
#be sure to check that no other columns of new data contain these letters at the end.
postura<-which(substr(names(cimfert), nchar(names(cimfert)), nchar(names(cimfert)))=="p")
viabilidad <-which(substr(names(cimfert), nchar(names(cimfert)), nchar(names(cimfert)))=="v")
#now use colMeans to find averages for each subset desired for eggs laid
toteggmean <- colMeans(cimfert[,postura], na.rm= TRUE, dims=1)
infeggmean <- colMeans(cimfert[infect,postura], na.rm= TRUE, dims=1)
coneggmean <- colMeans(cimfert[controls,postura], na.rm= TRUE, dims=1)
cPLeggmean <- colMeans(cimfert[ControlP,postura], na.rm= TRUE, dims=1)
iPLeggmean <- colMeans(cimfert[InfectP,postura], na.rm= TRUE, dims=1)
cRAeggmean <- colMeans(cimfert[ControlRA,postura], na.rm= TRUE, dims=1)
iRAeggmean <- colMeans(cimfert[InfectRA,postura], na.rm= TRUE, dims=1)
cRBeggmean <- colMeans(cimfert[ControlRB,postura], na.rm= TRUE, dims=1)
iRBeggmean <- colMeans(cimfert[InfectRB,postura], na.rm= TRUE, dims=1)
#now use colMeans to find averages for each subset desired for eggs hatched
totviamean <- colMeans(cimfert[, viabilidad], na.rm= TRUE, dims=1)
infviamean <- colMeans(cimfert[infect, viabilidad], na.rm= TRUE, dims=1)
conviamean <- colMeans(cimfert[controls, viabilidad], na.rm= TRUE, dims=1)
cPLviamean <- colMeans(cimfert[ControlP,viabilidad], na.rm= TRUE, dims=1)
iPLviamean <- colMeans(cimfert[InfectP,viabilidad], na.rm= TRUE, dims=1)
cRAviamean <- colMeans(cimfert[ControlRA,viabilidad], na.rm= TRUE, dims=1)
iRAviamean <- colMeans(cimfert[InfectRA,viabilidad], na.rm= TRUE, dims=1)
cRBviamean <- colMeans(cimfert[ControlRB,viabilidad], na.rm= TRUE, dims=1)
iRBviamean <- colMeans(cimfert[InfectRB,viabilidad], na.rm= TRUE, dims=1)

#now use colMedians to find averages for each subset desired for eggs laid
#the median function requires a data frame so...
mpostura<-as.matrix(cimfert[, postura])
mviabilidad<-as.matrix(cimfert[, viabilidad])
toteggmedians<- colMedians(mpostura, na.rm= TRUE)
infeggmedians <- colMedians(mpostura[infect,], na.rm= TRUE)
coneggmedians <- colMedians(mpostura[controls,], na.rm= TRUE)
cPLeggmedians <- colMedians(mpostura[ControlP,], na.rm= TRUE)
iPLeggmedians <- colMedians(mpostura[InfectP,], na.rm= TRUE)
cRAeggmedians<- colMedians(mpostura[ControlRA,], na.rm= TRUE)
iRAeggmedians<- colMedians(mpostura[InfectRA,], na.rm= TRUE)
cRBeggmedians <- colMedians(mpostura[ControlRB,], na.rm= TRUE)
iRBeggmedians <- colMedians(mpostura[InfectRB,], na.rm= TRUE)
#now use colMeans to find averages for each subset desired for eggs hatched
totviamedians <- colMedians(mviabilidad, na.rm= TRUE)
infviamedians <- colMedians(mviabilidad[infect,], na.rm= TRUE)
conviamedians <- colMedians(mviabilidad[controls,], na.rm= TRUE)
cPLviamedians <- colMedians(mviabilidad[ControlP,], na.rm= TRUE)
iPLviamedians <- colMedians(mviabilidad[InfectP,], na.rm= TRUE)
cRAviamedians <- colMedians(mviabilidad[ControlRA,], na.rm= TRUE)
iRAviamedians <- colMedians(mviabilidad[InfectRA,], na.rm= TRUE)
cRBviamedians <- colMedians(mviabilidad[ControlRB,], na.rm= TRUE)
iRBviamedians <- colMedians(mviabilidad[InfectRB,], na.rm= TRUE)

postura<-which(substr(names(cimfert), nchar(names(cimfert)), nchar(names(cimfert)))=="p")
viabilidad <-which(substr(names(cimfert), nchar(names(cimfert)), nchar(names(cimfert)))=="v")

##We can now plot this data for eggs
#all the repetitions pooled togehter.
par(mfrow=c(1,1))
plot(infeggmean, type="o", main="Average Eggs Laid Between Infected and Control Insects",
     ylab="Number of eggs", xlab="Week in Study", col="darkorange1", pch=18)
lines(coneggmean, type="o", pch=16, col="dodgerblue1")
legend("topright", c("infected","controls"), col=c("darkorange1", "dodgerblue1"), pch=c(18,16))

#Por Pilot
plot(iPLeggmean, type="o", main="Average Eggs Laid Between Infected and Control Insects in Pilot",
     ylab="Number of eggs", xlab="Week in Study", col="darkorange1", pch=18)
lines(cPLeggmean, type="o", pch=16, col="dodgerblue1")
legend("topright", c("infected","controls"), col=c("darkorange1", "dodgerblue1"), pch=c(18,16))

#Por Rep1
plot(iRAeggmean, type="o", main="Average Eggs Laid Between Infected and Control Insects in Rep 1",
     ylab="Number of eggs", xlab="Week in Study", col="darkorange1", pch=18)
lines(cRAeggmean, type="o", pch=16, col="dodgerblue1")
legend("topright", c("infected","controls"), col=c("darkorange1", "dodgerblue1"), pch=c(18,16))

#Por Rep2
plot(iRBeggmean, type="o", main="Average Eggs Laid Between Infected and Control Insects in Rep 2",
     ylab="Number of eggs", xlab="Week in Study", col="darkorange1", pch=18, ylim=c(1,9))
lines(cRBeggmean, type="o", pch=16, col="dodgerblue1")
legend("topright", c("infected","controls"), col=c("darkorange1", "dodgerblue1"), pch=c(18,16))

#Put them al on one graph
plot(iPLeggmean, type="o", main="Average Eggs Laid Between Infected and Control Insects",
     ylab="Number of eggs", xlab="Week in Study", col=2, pch=18, lty=1, ylim=c(0,10))
lines(cPLeggmean, type="o", pch=1, col=4, lty=1)
lines(iRAeggmean, type="o", pch=2, col=2, lty=2)
lines(cRAeggmean, type="o", pch=2, col=4, lty=2)
lines(iRBeggmean, type="o", pch=3, col=2, lty=3)
lines(cRBeggmean, type="o", pch=3, col=4, lty=3)
legend("topright", c("Pilot Infected","Pilot Controls", "Rep1 Infected", 
                     "Rep1 Controls", "Rep2 Infected", "Rep2 Controls"),
       col=c(2,4,2,4,2,4), pch=c(1,1,2,2,3,3), lty=c(1,1,2,2,3,3))

##Do the same analysis por hatching
#all the repetitions pooled togehter.
plot(infviamean, type="o", main="Average Hatched Eggs Between Infected and Control Insects",
     ylab="Number of Hatched Insects", xlab="Week in Study", col="darkorange1", pch=18)
lines(conviamean, type="o", pch=16, col="dodgerblue1")
legend("topright", c("infected","controls"), col=c("darkorange1", "dodgerblue1"), pch=c(18,16))

#Por Pilot
plot(iPLviamean, type="o", main="Average Hatched Eggs Between Infected and Control Insects in Pilot",
     ylab="Number of Hatched Insects", xlab="Week in Study", col="darkorange1", pch=18)
lines(cPLviamean, type="o", pch=16, col="dodgerblue1")
legend("topright", c("infected","controls"), col=c("darkorange1", "dodgerblue1"), pch=c(18,16))

#Por Rep1
plot(iRAviamean, type="o", main="Average Hatched Eggs Between Infected and Control Insects in Rep 1",
     ylab="Number of Hatched Insects", xlab="Week in Study", col="darkorange1", pch=18, ylim=c(0,9))
lines(cRAviamean, type="o", pch=16, col="dodgerblue1")
legend("topright", c("infected","controls"), col=c("darkorange1", "dodgerblue1"), pch=c(18,16))

#Por Rep2
plot(iRBviamean, type="o", main="Average Hatched Eggs Between Infected and Control Insects in Rep 2",
     ylab="Number of Hatched Insects", xlab="Week in Study", col="darkorange1", pch=18, ylim=c(0,9))
lines(cRBviamean, type="o", pch=16, col="dodgerblue1")
legend("topright", c("infected","controls"), col=c("darkorange1", "dodgerblue1"), pch=c(18,16))

#Put them al on one graph
plot(iPLeggmean, type="o", main="Average Hatched Eggs Between Infected and Control Insects",
     ylab="Number of Hatched Insects", xlab="Week in Study", col=2, pch=18, lty=1, ylim=c(0,10))
lines(cPLviamean, type="o", pch=1, col=4, lty=1)
lines(iRAviamean, type="o", pch=2, col=2, lty=2)
lines(cRAviamean, type="o", pch=2, col=4, lty=2)
lines(iRBviamean, type="o", pch=3, col=2, lty=3)
lines(cRBviamean, type="o", pch=3, col=4, lty=3)
legend("topright", c("Pilot Infected","Pilot Controls", "Rep1 Infected", 
                     "Rep1 Controls", "Rep2 Infected", "Rep2 Controls"),
       col=c(2,4,2,4,2,4), pch=c(1,1,2,2,3,3), lty=c(1,1,2,2,3,3))

###plotting the medians
##lets make the table with the total medians and then medians with the mean
#Lets start with the median plots for eggs and such
par(mfrow=c(1,1))
plot(infeggmedians, type="o", main="Median Eggs Laid Between Infected and Control Insects",
     ylab="Number of eggs", xlab="Week in Study", col="darkorange1", pch=18)
lines(coneggmedians, type="o", pch=16, col="dodgerblue1")
legend("topright", c("infected","controls"), col=c("darkorange1", "dodgerblue1"), pch=c(18,16))

plot(infviamedians, type="o", main="Median Eggs Hatched Between Infected and Control Insects",
     ylab="Number of eggs", xlab="Week in Study", col="darkorange1", pch=18)
lines(conviamedians, type="o", pch=16, col="dodgerblue1")
legend("topright", c("infected","controls"), col=c("darkorange1", "dodgerblue1"), pch=c(18,16))

#Put them al on one graph
plot(iPLeggmedians, type="o", main="Median Eggs Laid Between Infected and Control Insects",
     ylab="Number of eggs", xlab="Week in Study", col=2, pch=18, lty=1, ylim=c(0,10))
lines(cPLeggmedians, type="o", pch=1, col=4, lty=1)
lines(iRAeggmedians, type="o", pch=2, col=2, lty=2)
lines(cRAeggmedians, type="o", pch=2, col=4, lty=2)
lines(iRBeggmedians, type="o", pch=3, col=2, lty=3)
lines(cRBeggmedians, type="o", pch=3, col=4, lty=3)
legend("topright", c("Pilot Infected","Pilot Controls", "Rep1 Infected", 
                     "Rep1 Controls", "Rep2 Infected", "Rep2 Controls"),
       col=c(2,4,2,4,2,4), pch=c(1,1,2,2,3,3), lty=c(1,1,2,2,3,3))

plot(iPLviamedians, type="o", main="Median Eggs Hatched Between Infected and Control Insects",
     ylab="Number of eggs", xlab="Week in Study", col=2, pch=18, lty=1, ylim=c(0,10))
lines(cPLviamedians, type="o", pch=1, col=4, lty=1)
lines(iRAviamedians, type="o", pch=2, col=2, lty=2)
lines(cRAviamedians, type="o", pch=2, col=4, lty=2)
lines(iRBviamedians, type="o", pch=3, col=2, lty=3)
lines(cRBviamedians, type="o", pch=3, col=4, lty=3)
legend("topright", c("Pilot Infected","Pilot Controls", "Rep1 Infected", 
                     "Rep1 Controls", "Rep2 Infected", "Rep2 Controls"),
       col=c(2,4,2,4,2,4), pch=c(1,1,2,2,3,3), lty=c(1,1,2,2,3,3))
######################################################################
#=====================================================================
#Using Compile
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Now all the data should be in place.  We can plot some more.
#Lets begin to make the box plots over time
#sete how eggs and hatching vary by date and week
#first limit cimfert to the pilot
cimpilot<-which(cimfert$trial==0)
cimfert <- cimfert[cimpilot,]
#rowna<-which(is.na(cimfert$ID)==TRUE)
#cimfert<-cimfert[-rowna,]

par(mfrow=c(2,2))
eggweek<-lm(Compile$eggs ~Compile$week)
hatchweek<-lm(Compile$hatch ~Compile$week)
plot(Compile$week, Compile$eggs+rnorm(length(Compile$eggs), 0, 0.5), main="Eggs by Week")
abline(eggweek)
boxplot(Compile$eggs ~Compile$week, main="Eggs by Week")
plot(Compile$week, Compile$hatch+rnorm(length(Compile$hatch), 0, 0.5), main="Hatching by Week")
abline(hatchweek)
boxplot(Compile$hatch ~Compile$week, main="Hatching by Week")

par(mfrow=c(2,4))
plot(Compile$week, Compile$eggs, main="Eggs by Week")
boxplot(Compile$eggs ~Compile$week, main="Eggs by Week")
plot(Compile$week, Compile$hatch, main="Hatching by Week")
boxplot(Compile$hatch ~Compile$week, main="Hatching by Week")
plot(Compile$week, Compile$eggs, main="Eggs by Week")
boxplot(Compile$eggs ~Compile$week, main="Eggs by Week")
plot(Compile$week, Compile$hatch, main="Hatching by Week")
boxplot(Compile$hatch ~Compile$week, main="Hatching by Week")

par(mfrow=c(1,1))
boxplot(Compile$eggs ~Compile$date, main="Eggs by Date")
boxplot(Compile$hatch ~Compile$date, main="Hatching by Date")

##lets makle a combined plot of the two groups together
enona<-which(is.na(Compile$eggs)==FALSE)
hnona<-which(is.na(Compile$hatch)==FALSE)
Compile$infected<-as.factor(Compile$infected)
Compile$week<-as.factor(Compile$week)

#the box plot for eggs laid
g<-ggplot(aes( y= eggs, x= week, fill = infected, na.rm=TRUE),
       data= Compile[enona,]) +geom_boxplot(data=Compile[enona,])
g<-g+ggtitle("Distribution of Number of Eggs Laid by Infection Status")
g

#hatch plot
h<-ggplot(aes( y= hatch, x= week, fill = infected, na.rm=TRUE), 
       data= Compile[hnona,])+geom_boxplot(data=Compile[hnona,])
h<-h+ggtitle("Distribution of Number of Eggs Hatched by Infection Status")
h

#Other parts of this study show that life span changes depending on inf
#lets look at the number alive by week
alivetotweek <- c(1:length(unique(Compile$week))*NA)
aliveinfweek <- c(1:length(unique(Compile$week))*NA)
aliveconweek <- c(1:length(unique(Compile$week))*NA)
infected <- which(Compile$infected==1)
controlled <- which(Compile$infected==0)
#loop counts the number alive in each week (total, infected, and controls)
for (i in 1:length(unique(Compile$week))){
  week<-which(Compile$week==i)
  alivetotweek[i]<-sum(Compile$alive[week])
  inf<-intersect(week, infected)
  con<-intersect(week, controlled)
  aliveinfweek[i]<-sum(Compile$alive[inf])
  aliveconweek[i]<-sum(Compile$alive[con])
}

plot(alivetotweek)
  points(aliveinfweek, col="red")
  points(aliveconweek, col="steelblue")
  legend("topright", c("All Bugs","infected","controls"), 
        col=c("black","red", "steelblue"), pch=c(1,1,1))
#a quick check seeing if alivetotweek is the sum of the other two.  
#test<-aliveinfweek+aliveconweek
#testing<-data.frame(test, alivetotweek, aliveinfweek, aliveconweek)

#lets get a better view by looking at (percents)
 which(cimfert$infected==1)
 peraltotweek<-alivetotweek/(length(cimfert$ID))
  peralinfweek <-aliveinfweek/(length(which(cimfert$infected==1)))
  peralconweek <-aliveconweek/(length(which(cimfert$infected==0)))

plot(peraltotweek, col="black", xlab="Proportion Alive", ylab="Week", 
     main="Proportion Alive in Infected and Controls by Week")
  points(peralconweek, col="steelblue")
  points(peralinfweek, col="red")  
  legend("topright", c("All Bugs","infected","controls"), 
         col=c("black","red", "steelblue"), pch=c(1,1,1))
#lets do something similar to see the number of egg events   
  eggetotweek <- c(1:length(unique(Compile$week))*NA)
  eggeinfweek <- c(1:length(unique(Compile$week))*NA)
  eggeconweek <- c(1:length(unique(Compile$week))*NA)
  for (i in 1:length(unique(Compile$week))){
    week<-which(Compile$week==i)
    eggetotweek[i]<-sum(Compile$eggevent[week], na.rm=TRUE)
    inf<-intersect(week, infected)
    con<-intersect(week, controlled)
    eggeinfweek[i]<-sum(Compile$eggevent[inf], na.rm=TRUE)
    eggeconweek[i]<-sum(Compile$eggevent[con], na.rm=TRUE)
  }
  
plot(eggetotweek)
  points(eggeinfweek, col="red")
  points(eggeconweek, col="steelblue")
  legend("topright", c("infected","controls"), 
         col=c("red", "steelblue"), pch=c(1,1))
  
  #lets get a better view by looking at percents
  peregeinfweek<-eggeinfweek/length(infect)
  peregeconweek<-eggeconweek/length(controls)
  
  plot(peregeinfweek, type="o", col="red", xlab="Week", ylab="Proportion of Insects that Laid Eggs", 
       main="Proportion Insects that Laid in Infected and Controls by Week")
  lines(peregeconweek, col="steelblue")
  legend("topright", c("infected","controls"), 
         col=c("red", "steelblue"), pch=c(1,1))

#now we need to see proportion of alive insects    
  proeggeinfweek<-eggeinfweek/aliveinfweek
  proeggeconweek<-eggeconweek/aliveconweek 
  
  plot(proeggeinfweek, type="o", col="red", xlab="Week", ylab="Proportion of Alive Insects that Laid Eggs", 
       main="Proportion of Alive Insects that Laid in Infected and Controls by Week")
  lines(proeggeconweek, col="steelblue")
  legend("bottomleft", c("infected","controls"), 
         col=c("red", "steelblue"), pch=c(1,1))
  
#now to do that with the total number of eggs.  
  eggstotweek <- c(1:length(unique(Compile$week))*NA)
  eggsinfweek <- c(1:length(unique(Compile$week))*NA)
  eggsconweek <- c(1:length(unique(Compile$week))*NA)
  for (i in 1:length(unique(Compile$week))){
    week<-which(Compile$week==i)
    eggstotweek[i]<-sum(Compile$eggs[week], na.rm=TRUE)
    inf<-intersect(week, infected)
    con<-intersect(week, controlled)
    eggsinfweek[i]<-sum(Compile$eggs[inf], na.rm=TRUE)
    eggsconweek[i]<-sum(Compile$eggs[con], na.rm=TRUE)
  }
  
  plot(eggstotweek)
  points(eggsinfweek, col="red")
  points(eggsconweek, col="steelblue")
  legend("topright", c("infected","controls"), 
         col=c("red", "steelblue"), pch=c(1,1))
  
  #lets get a better view by looking at percents
  peregsinfweek<-eggsinfweek/length(infect)
  peregsconweek<-eggsconweek/length(controls)
  
  plot(peregsinfweek, type="o", col="red", xlab="Week", ylab="Number of Eggs Laid Per Insect", 
       main="Eggs Laid Per Insect by Week")
  lines(peregsconweek, col="steelblue")
  legend("topright", c("infected","controls"), 
         col=c("red", "steelblue"), pch=c(1,1))
  
  #now we need to see proportion of alive insects    
  proeggsinfweek<-eggsinfweek/aliveinfweek
  proeggsconweek<-eggsconweek/aliveconweek 
  par(mfrow=c(1,1))
  plot(proeggsinfweek, type="o", col="red", xlab="Week", ylab="Num Eggs Per Alive Insect", 
       main="Number of Eggs Per Alive Insect by Week and Treatment Group")
  lines(proeggsconweek, col="steelblue")
  legend("topright", c("infected","controls"), 
         col=c("red", "steelblue"), pch=c(1,1))
  
  
######################################################################  
#plot humidity and temperature over time
Compile$avhum <- as.numeric(Compile$avhum)
plot(Compile$date, Compile$avhum,col="dodgerblue", ylim=c(24,60),
     main="Temperature and Humidity", ylab="Relative Humidity(%) and Temperature(C)",
     xlab="Date")
points(Compile$date, Compile$avtemp, col="tomato")
legend("topleft", c("Humidity", "Temperature"), text.col=c("dodgerblue","tomato"))


#eggs laid by humidity
humeggs<-lm(Compile$eggs ~Compile$avhum)
plot(Compile$avhum, Compile$eggs+rnorm(length(Compile$eggs), 0, 0.5),
     main="Eggs by Humidity", ylab="Number of Eggs",
     xlab="Humidity(%)")
abline(humeggs)
summary(humeggs)

egghum<-ggplot(aes( y= eggs, x= avhum, na.rm=TRUE), 
               data=Compile[enona,])+geom_point(data=Compile[enona,])
egghum<-egghum+ggtitle("Number of Eggs Laid by Humidity and Infection Status") 
egghum<-egghum+facet_grid(. ~infected)+geom_smooth(method= "lm")
egghum

#plot temperature by eggs
Compile$avtemp <- as.character(Compile$avtemp)
tempeggs<-lm(Compile$eggs ~Compile$avtemp)
plot(Compile$avtemp, Compile$eggs,
     main="Eggs by Temperature", ylab="Number of Eggs",
     xlab="Temperature(C)")

Compile$avtemp <- as.numeric(Compile$avtemp)
eggtem<-ggplot(aes( y= eggs, x= avtemp, na.rm=TRUE), 
               data=Compile[enona,])+geom_point(data=Compile[enona,])
eggtem<-eggtem+ggtitle("Number of Eggs Laid by Temperature and Infection Status") 
eggtem<-eggtem+facet_grid(. ~infected)+geom_smooth(method= "lm")
eggtem

eggtembx<-ggplot(aes( y= eggs, x= avtemp, na.rm=TRUE), 
               data=Compile[enona,])+geom_(data=Compile[enona,])
eggtembx<-eggtem+ggtitle("Number of Eggs Laid by Temperature and Infection Status") 
eggtembx<-eggtem+facet_grid(. ~infected)
eggtembx
#hatch by humidity
humhatch<-lm(Compile$hatch ~Compile$avhum)
plot(Compile$avhum, Compile$hatch,
     main="Hatching by Humidity", ylab="Number of Eggs",
     xlab="Humidity(%)")

#hatch by temp
temphatch<-lm(Compile$hatch ~Compile$avtemp)
plot(Compile$avtemp, Compile$hatch,
     main="Hatching by Temperature", ylab="Number of Eggs",
     xlab="Temperature(C)")

# ##For each time line
# glm(cases~rhs(data$year,2003)+lhs(data$year,2003)+ offset(log(population)), data=data, 
#subset=28:36, family=poisson())


#Now lets make somoe pretty pictures.
infected<-which(Compile$infected==1)
controled<-which(Compile$infected==0)
uniquebugs<-unique(Compile$id)
plot(Compile$week[infected], Compile$eggs[infected]+rnorm(length(Compile$eggs[infected]), 0.5, 1), col="red")

#Now lets make some pretty pictures.
infected<-which(Compile$infected==1)
controled<-which(Compile$infected==0)
uniquebugs<-unique(Compile$id)
plot(Compile$week[infected], Compile$eggs[infected], col="red")
plot(Compile$date[infected], Compile$eggs[infected], col="red")     

#Compare infected hatch and by week.
par(mfrow=c(2,4))
#infected
plot(Compile$week[infected], Compile$eggs[infected], main="Eggs by Week")
boxplot(Compile$eggs[infected] ~c(Compile$week[infected]), main="Eggs by Week")
plot(Compile$week[infected], Compile$hatch[infected], main="Hatching by Week")
boxplot(Compile$hatch[infected] ~Compile$week[infected], main="Hatching by Week")
#not infected
plot(Compile$week[controled], Compile$eggs[controled], main="Eggs by Week")
boxplot(Compile$eggs[controled] ~Compile$week[controled], main="Eggs by Week")
plot(Compile$week[controled], Compile$hatch[controled], main="Hatching by Week")
boxplot(Compile$hatch[controled] ~Compile$week[controled], main="Hatching by Week")

