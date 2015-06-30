#packages needed
library(lubridate) #para extracting dates 
library(reshape2) #para make the wide data into long data
library(vioplot)
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

##Create a master table with all the insects.
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

#clear na and replace with 0
#cimfert[is.na(cimfert)]<-0

###Create indexes for each treatment group, rat, and so para hacer graphs .
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
 
##instead of a loop, lets try to use apply or the colMeans function
#the issue now is defining our array or input given that later data
#may have different dimensions.

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

##We can now plot this data for eggs
#all the repetitions pooled togehter.
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

##for violin plots and lme4 analysis we need to make data frame by entry
#entry meaning every line represents week of egg)) and hatch from normal jar.
#try using the reshape function
#Compile<-melt(cimfert)

#We need to create outputs for the data.
blank <- (1:(length(postura)*length(cimfert$Procedencia))*0)
Compile <- data.frame(blank,0,0,0,0,0,0, 0, 0, 0, 0, 0)

Compile <- rename(Compile, replace = c("blank"="id", "X0"="parents","X0.1"="infected","X0.2"="start",
       "X0.3"="week", "X0.4"="date", "X0.5"="eggs", "X0.6"="hatch", "X0.7"="rh",
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
     Compile$id[i+((d-1)*length(cimfert$ID))] <- cimfert$ID[i]
     Compile$parents[i+((d-1)*length(cimfert$ID))] <- cimfert$Nro_.pareja[i]
     Compile$procedencia[i+((d-1)*length(cimfert$ID))] <- cimfert$Procedencia[i]
     Compile$infected[i+((d-1)*length(cimfert$ID))] <- cimfert$infected[i]
     Compile$start[i+((d-1)*length(cimfert$ID))] <- cimfert$Fecha_Inicio_.Pareja[i]
     Compile$hatch[i+((d-1)*length(cimfert$ID))] <-cimfert[i,(2*d+3)]
     Compile$eggs[i+((d-1)*length(cimfert$ID))]<-cimfert[i,(2*d+2)]
     }
 }

#Now that table is made, make date so that humidity and temperature data can be easily entered.
Compile$start <- parse_date_time(Compile$start, "dmy", tz="EST")
Compile$start <- as.Date(Compile$start)
Compile$date <- (Compile$start+(Compile$week*7))

###add temperature and humidity values.
#make the tempRH also date format
tempRH$FECHA <- parse_date_time(tempRH$FECHA, "dmy", tz="EST")
tempRH$FECHA <- as.Date(tempRH$FECHA)

###############################################################################
#From here one I will reduce the data table to include only data from the pilot
###############################################################################
#Once we have ful data, we justneed to comment out the following line.
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
Compile$avtemphigh <-lapply(date, avtemphigh)

#Make function to calculate avg low temperature of week.
avtemplow<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  mean(tempRH$TEMP.MIN..Â.C.[a:b], na.rm=TRUE)
}
Compile$avtemplow <-lapply(date, avtemplow)

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

Compile$avtemp <- lapply(date, avtemp)
#week max
tempmax<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  max(tempRH$TEMP.MAX..Â.C.[a:b], na.rm=TRUE)
}
Compile$tempmax <-lapply(date, tempmax)

#week min
tempmin<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  min(tempRH$TEMP.MIN..Â.C.[a:b], na.rm=TRUE)
}
Compile$tempmin <-lapply(date, tempmin)

#greatest difference(heat shock)
tempRH$tempdiff<- tempRH$TEMP.MAX..Â.C.-(tempRH$TEMP.MIN..Â.C.)

tempdiff<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  max(tempRH$tempdiff[a:b], na.rm=TRUE)
}
Compile$tempdiff<-lapply(date, tempdiff)

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
Compile$avhumhigh <-lapply(date, avhumhigh)

#Make function to calculate avg low temperature of week.
avhumlow<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  mean(tempRH$HR.MIN....[a:b], na.rm=TRUE)
}
Compile$avhumlow <-lapply(date, avhumlow)

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

Compile$avhum <- lapply(date, avhum)

#week max
hummax<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  max(tempRH$HR.MAX....[a:b], na.rm=TRUE)
}
Compile$hummax <-lapply(date, hummax)

#week min
hummin<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  min(tempRH$HR.MIN....[a:b], na.rm=TRUE)
}
Compile$hummin <-lapply(date, hummin)

#greatest difference(humidity shock?  I figured we had it for temp so why not)
tempRH$humdiff<- (tempRH$HR.MAX....)-(tempRH$HR.MIN....)

humdiff<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  max(tempRH$humdiff[a:b], na.rm=TRUE)
}
Compile$humdiff<-lapply(date, humdiff)

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
Compile$eggevent<-lapply(eggs, events)
Compile$eggevent <- as.numeric(Compile$eggevent)

#hatch events
Compile$eggevent<- (1:length(Compile$hatch))*NA
hatch <- as.list(Compile$hatch)
Compile$hatchevent <-lapply(hatch, events)
Compile$hatchevent <- as.numeric(Compile$hatchevent)
#===========================================
#Now all the data should be in place.  We can plot some more.
#plot humidity and temperature over time
plot(Compile$date, Compile$avhum,col="dodgerblue", ylim=c(24,60),
     main="Temperature and Humidity", ylab="Relative Humidity(%) and Temperature(C)",
     xlab="Date")
 points(Compile$date, Compile$avtemp, col="tomato")
 legend("topleft", c("Humidity", "Temperature"), text.col=c("dodgerblue","tomato"))
#see how eggs and hatching vary by date and week
par(mfrow=c(2,2))
plot(Compile$week, Compile$eggs, main="Eggs by Week")
boxplot(Compile$eggs ~Compile$week, main="Eggs by Week")
plot(Compile$week, Compile$hatch, main="Hatching by Week")
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

#hatching by humidity
plot(Compile$avhum, Compile$eggs,
     main="Eggs by Humidity", ylab="Number of Eggs",
     xlab="Humidity(%)")

#plot temperature by eggs
plot(Compile$avtemp, Compile$eggs,
     main="Eggs by Temperature", ylab="Number of Eggs",
     xlab="Temperature(C)")

#hatch by humidity
plot(Compile$avhum, Compile$hatch,
     main="Hatching by Humidity", ylab="Number of Eggs",
     xlab="Humidity(%)")

#hatch by temp
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
     
#Now lets make somoe pretty pictures.
infected<-which(Compile$infected==1)
controled<-which(Compile$infected==0)
uniquebugs<-unique(Compile$id)
plot(Compile$week[infected], Compile$eggs[infected], col="red")
plot(Compile$date[infected], Compile$eggs[infected], col="red")     

#Compare infected hatch and by week.
par(mfrow=c(2,4))
#infected
plot(Compile$week[infected], Compile$eggs[infected], main="Eggs by Week")
boxplot(Compile$eggs[infected] ~Compile$week[infected], main="Eggs by Week")
plot(Compile$week[infected], Compile$hatch[infected], main="Hatching by Week")
boxplot(Compile$hatch[infected] ~Compile$week[infected], main="Hatching by Week")
#not infected
plot(Compile$week[controled], Compile$eggs[controled], main="Eggs by Week")
boxplot(Compile$eggs[controled] ~Compile$week[controled], main="Eggs by Week")
plot(Compile$week[controled], Compile$hatch[controled], main="Hatching by Week")
boxplot(Compile$hatch[controled] ~Compile$week[controled], main="Hatching by Week")

