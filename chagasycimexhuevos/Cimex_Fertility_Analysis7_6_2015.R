#packages needed/experimented with
#install.packages(c("lubridate","reshape2","vioplot", "matrixStats","ggplot",
#     "plyr", "geeM", "MASS", "lmtest", "survval", "doBy"))

library(lubridate) #para extracting dates 
library(reshape2) #para make the wide data into long data
library(vioplot)
library(matrixStats)
library(ggplot2)
library(plyr)  #para rbind.fill function
library(geeM)
library(MASS)
library(lmtest)
#library(pscl)
library(survival)
library(doBy)

#set directory and bring in files to be analyzed.
setwd("c:\\Users\\tradylan\\Documents\\Laboratory\\chagasycimexhuevos")
#setwd("/Users/mzlevy/Laboratory/chagasycimexhuevos")

# ###############################################################################
# #==============================================================================
# #Data Formatting. The file "CompiledFertilityData.csv" was created
# #Do not run this part of code to save time unless corrections are necessary
# #Graphs may be helpful, many of which are already in github.
# #Modeling begins at line 1000.  
# #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
#bring in hatching data
#cimfertpilot <- read.csv("Cimex_FertP.csv")
cimfertpilot <- read.csv("Cimex_FertP_update12_3.csv")

#Original Found https://docs.google.com/spreadsheets/d/1E-GRO1_Ybrgqj0wjgz5s9YHY1KwJUVPov0I2PwgC2CQ/edit
cimfert1 <- read.csv("Cimex_FertR1_8_12.csv")
##https://docs.google.com/spreadsheets/d/1iDBITasgMrbmwGJJSwsPkcal1b7b3kdfmviRtw8wbqA/edit#gid=709304485
cimfert2 <- read.csv("Cimex_FertR2_8_12.csv")
#https://docs.google.com/spreadsheets/d/13RvsL-uZaKgJPN3RBf8nTlsR8BQ6U-BrEQ_7BmxYgsI/edit

#Bring in Mortality Data
mortR1 <- read.csv("Cimex_Mortality_R1.csv")
mortR2 <- read.csv("Cimex_Mortality_R2.csv")
#another tab in the two R1 and R2 Google docs above.

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
cimfert<-rbind.fill(cimfert2, cimfert1, cimfertpilot)

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
tot<-c(1:length(cimfert$raton))
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

#make id numbers for analysis(character string doesn't play friendly with other pkgs)
cimfert$idnum<-c(1:length(cimfert$ID))

#We need to create outputs for the data.
blank <- (1:(length(postura)*length(cimfert$Procedencia))*0)
Compile <- data.frame(blank,0,0,0,0,0,0, 0, 0, 0, 0, 0, 0)

Compile <- rename(Compile, replace = c("blank"="id", "X0"="parents","X0.1"="infected","X0.2"="start",
                                       "X0.3"="week", "X0.4"="date", "X0.5"="eggs", "X0.6"="hatch", "X0.7"="alive",
                                       "X0.8"="mouse", "X0.9"="procedencia", "X0.10"="trial", "X0.11"="idnum"))
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
    Compile$idnum[i+((d-1)*length(cimfert$ID))] <- cimfert$idnum[i]
  }
}

#We need to make a colum for alive  
#I need to check with them to ensure correct interpretation
#for (i in 1:length(Compile$week))  
#if(Compile$week[i] >= Compile$death[i]){
#    Compile$alive[i]<-0
#  } else(Compile$alive[i]<-1)

#This is assumes that if a number is entered the insect was alive during that week.\
Compile$eggs<-as.numeric(Compile$eggs)
dead<-which(is.na(Compile$eggs)==TRUE)
alive<-which(is.na(Compile$eggs)==FALSE)
Compile$alive[dead]<-0
Compile$alive[alive]<-1

#make a column with the percent viability
Compile$perferc<-(Compile$hatch/Compile$eggs)

#Now that table is made, make date so that humidity and temperature data can be easily entered.
Compile$start <- parse_date_time(Compile$start, "dmy", tz="EST")
Compile$start <- as.Date(Compile$start)
Compile$date <- (Compile$start+(Compile$week*7))


###add temperature and humidity values.
#make the tempRH also date format
tempRH$FECHA <- parse_date_time(tempRH$FECHA, "dmy", tz="EST")
tempRH$FECHA <- as.Date(tempRH$FECHA)

#Once we have full data, we justneed to comment out the following line.
pilot <- which(Compile$trial==0)
repone <- which(Compile$trial==1)
reptwo <- which(Compile$trial==2)
#Compile<-Compile[pilot,]
 
#-----------------------------
#Make function to calculate avg high temperature of week.
avtemphigh<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  if(rdate %in% tempRH$FECHA){
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  mean(tempRH$TEMP.MAX..Â.C.[a:b], na.rm=TRUE)
  } else{NA}
}

date<-Compile$date
Compile$avtemphigh <-sapply(date, avtemphigh)

#Make function to calculate avg low temperature of week.
avtemplow<-function(rdate){
  rdate<-as.Date(rdate)
  fday<- (rdate-6)
  if(rdate %in% tempRH$FECHA){
    a<-which(tempRH$FECHA==fday)
    b<-which(tempRH$FECHA==rdate)
    mean(tempRH$TEMP.MIN..Â.C.[a:b], na.rm=TRUE)
  } else{NA}
}
Compile$avtemplow <-sapply(date, avtemplow)

#tot average temp
avtemp <- function(rdate){
  rdate <- as.Date(rdate)
  fday <- (rdate-6)
  if(rdate %in% tempRH$FECHA){
    a <- which(tempRH$FECHA==fday)
    b <- which(tempRH$FECHA==rdate)
    c <- sum(tempRH$TEMP.MAX..Â.C.[a:b], na.rm=TRUE)
    d <- sum(tempRH$TEMP.MIN..Â.C.[a:b], na.rm=TRUE)
   ((c+d)/(2*((length(tempRH$TEMP.MAX..Â.C.[a:b]))-(length(which(is.na(tempRH$TEMP.MAX..Â.C.[a:b])==TRUE))))))
  } else{NA}
}
Compile$avtemp <- sapply(date, avtemp)

#week max
tempmax<-function(rdate){
  rdate<-as.Date(rdate)
  if(rdate %in% tempRH$FECHA){
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  max(tempRH$TEMP.MAX..Â.C.[a:b], na.rm=TRUE)
  }else{NA}
}
Compile$tempmax <-sapply(date, tempmax)

#week min
tempmin<-function(rdate){
  rdate<-as.Date(rdate)
  if(rdate %in% tempRH$FECHA){
  fday<- (rdate-6)
    a<-which(tempRH$FECHA==fday)
    b<-which(tempRH$FECHA==rdate)
    min(tempRH$TEMP.MIN..Â.C.[a:b], na.rm=TRUE)
  }else{NA}
}  
  
Compile$tempmin <-sapply(date, tempmin)

#greatest difference(heat shock)
tempRH$tempdiff<- tempRH$TEMP.MAX..Â.C.-(tempRH$TEMP.MIN..Â.C.)

tempdiff<-function(rdate){
  rdate<-as.Date(rdate)
  if(rdate %in% tempRH$FECHA){
   fday<- (rdate-6)
   a<-which(tempRH$FECHA==fday)
   b<-which(tempRH$FECHA==rdate)
   max(tempRH$tempdiff[a:b], na.rm=TRUE)
  }else{NA}
}
Compile$tempdiff<-sapply(date, tempdiff)

#==============================================================
#Do the same for humidity
avhumhigh<-function(rdate){
  rdate<-as.Date(rdate)
  if(rdate %in% tempRH$FECHA){
    fday<- (rdate-6)
    a<-which(tempRH$FECHA==fday)
    b<-which(tempRH$FECHA==rdate)
    mean(tempRH$HR.MAX....[a:b], na.rm=TRUE)
  }else{NA}
}
date<-Compile$date
Compile$avhumhigh <-sapply(date, avhumhigh)

#Make function to calculate avg low temperature of week.
avhumlow<-function(rdate){
  rdate<-as.Date(rdate)
  if(rdate %in% tempRH$FECHA){
    fday<- (rdate-6)
    a<-which(tempRH$FECHA==fday)
    b<-which(tempRH$FECHA==rdate)
    mean(tempRH$HR.MIN....[a:b], na.rm=TRUE)
  }else{NA}
}
Compile$avhumlow <-sapply(date, avhumlow)

#tot average temp
avhum <- function(rdate){
  rdate <- as.Date(rdate)
  if(rdate %in% tempRH$FECHA){
    fday <- (rdate-6)
    a <- which(tempRH$FECHA==fday)
    b <- which(tempRH$FECHA==rdate)
    c <- sum(tempRH$HR.MAX....[a:b], na.rm=TRUE)
    d <- sum(tempRH$HR.MIN....[a:b], na.rm=TRUE)
   ((c+d)/(2*((length(tempRH$HR.MAX....[a:b]))-(length(which(is.na(tempRH$HR.MAX....[a:b])==TRUE))))))
  }else{NA}
}

Compile$avhum <- sapply(date, avhum)

#week max
hummax<-function(rdate){
  rdate<-as.Date(rdate)
  if(rdate %in% tempRH$FECHA){
    fday<- (rdate-6)
    a<-which(tempRH$FECHA==fday)
    b<-which(tempRH$FECHA==rdate)
    max(tempRH$HR.MAX....[a:b], na.rm=TRUE)
  }else{NA}
}
Compile$hummax <-sapply(date, hummax)

#week min
hummin<-function(rdate){
  rdate<-as.Date(rdate)
  if(rdate %in% tempRH$FECHA){
  fday<- (rdate-6)
  a<-which(tempRH$FECHA==fday)
  b<-which(tempRH$FECHA==rdate)
  min(tempRH$HR.MIN....[a:b], na.rm=TRUE)
  }else{NA}
}
Compile$hummin <-sapply(date, hummin)

#greatest difference(humidity shock?  I figured we had it for temp so why not)
tempRH$humdiff<- (tempRH$HR.MAX....)-(tempRH$HR.MIN....)

humdiff<-function(rdate){
  rdate<-as.Date(rdate)
  if(rdate %in% tempRH$FECHA){
    fday<- (rdate-6)
    a<-which(tempRH$FECHA==fday)
    b<-which(tempRH$FECHA==rdate)
    max(tempRH$humdiff[a:b], na.rm=TRUE)
  }else{NA}
}
Compile$humdiff<-sapply(date, humdiff)

##############################################
#after initial analysis we need to combine the data from the first two or three weeks.
#

# resegmentcompile<-function(segl){
#   maxn<-max(Compile$week)/segl
#   maxn<-round(maxn, digits=0)
#   topin<-(segl*c(1:maxn))
#   lowin<-topin-(segl-1)   
#   ids<-unique(Compile$idnum)
#   blanki <- (1:(maxn*max(ids))*NA)
#   Compilesegl <- data.frame(blanki, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#   Compilesegl <- rename(Compile, replace = c("blank"="idnum", "X0"="eggs","X0.1"="hatch",
#                                              "X0.2"="period"))
#   for(i in 1:max(ids)){
#     observationsi<-which(Compile$idnum==i)
#     obtablei<-Compile[observationsi]
#       for(j in 1:maxn){
#         Compilesegl$eggs[maxn*(i-1)+j] <- sum(Compile$eggs[topin[j]:lowin[j]], na.rm=TRUE)
#         Compilesegl$hatch[maxn*(i-1)+j] <- sum(Compile$hatch[topin[j]:lowin[j]], na.rm=TRUE)
#         Compilesegl$idnum[maxn*(i-1)+j] <- Compile$idnum[topin[j]]
#         Compilesegl$period[maxn*(i-1)+j] <- j
#         Compilesegl$infected[maxn*(i-1)+j] <- Compile$infected[lowin[j]]
#         Compilesegl$alive[maxn*(i-1)+j] <- Compile$alive[lowin[j]] 
#         Compilesegl$mouse[maxn*(i-1)+j] <- Compile$mouse[lowin[j]]
#         Compilesegl$trial[maxn*(i-1)+j] <- Compile$trial[lowin[j]]
#         Compilesegl$avtemp[maxn*(i-1)+j] <-mean(Compile$avtemp[((segl*j)-segl+1):(segl*j)], na.rm=TRUE)
#         Compilesegl$avtemphigh[maxn*(i-1)+j]<-mean(obtabli$avtemphigh[((segl*j)-segl+1):(segl*j)], na.rm=TRUE)
#         Compilesegl$avtemplow[maxn*(i-1)+j]<-mean(obtabli$avtemplow[((segl*j)-segl+1):(segl*j)], na.rm=TRUE)
#         Compilesegl$tempmax[maxn*(i-1)+j]<-max(obtabli$tempmax[((segl*j)-segl+1):(segl*j)], na.rm=TRUE)
#         Compilesegl$tempmin[maxn*(i-1)+j]<-min(obtabli$tempmin[((segl*j)-segl+1):(segl*j)], na.rm=TRUE)
#         Compilesegl$tempdiff[maxn*(i-1)+j]<-max(obtabli$tempdiff[((segl*j)-segl+1):(segl*j)], na.rm=TRUE)
#         Compilesegl$avhum[maxn*(i-1)+j]<-mean(Compile$avhum[((segl*j)-segl+1):(segl*j)], na.rm=TRUE)
#         Compilesegl$avhumhigh[maxn*(i-1)+j]<-mean(Compile$avhumhigh[((segl*j)-segl+1):(segl*j)], na.rm=TRUE)
#         Compilesegl$avhumlow[maxn*(i-1)+j]<-mean(Compile$avhumlow[((segl*j)-segl+1):(segl*j)], na.rm=TRUE)
#         Compilesegl$hummax[maxn*(i-1)+j]<-max(obtabli$hummax[((segl*j)-segl+1):(segl*j)], na.rm=TRUE)
#         Compilesegl$hummin[maxn*(i-1)+j]<-min(obtabli$hummin[((segl*j)-segl+1):(segl*j)], na.rm=TRUE)
#         Compilesegl$humdiff[maxn*(i-1)+j]<-max(obtabli$humdiff[((segl*j)-segl+1):(segl*j)], na.rm=TRUE)
#     }
#   }
#   Compilesegl
# }
# 
# resegmentcompile(3)

# 6549/(177*2)
# div2 <- round(177, digits=0)
# div3 <- round(177*3, digits=0)
# 
# Compile$twoperiod<-c(rep(1:18, each=177*2),rep(19, each=177))
# Compile$threeperiod<-c(rep(1:12, each=177*3),rep(13, each=177))

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

###Calculating Confidence Intervals
#na's make this difficult
sdcim<-function(dataframe, rowin){
  sd(dataframe[,rowin], na.rm=TRUE)
}

findn<-function(dataframe, rowin){
  real<-which(is.na(dataframe[,rowin])==FALSE)
  length(real)
}

#put these functions into giant looping function for each infection status
colSummary<-function(treatment, dependentvariable){
    df<-cimfert[treatment,]
    sds<-sapply(X=dependentvariable, sdcim, dataframe=df)
    ns<-sapply(X=dependentvariable, findn, dataframe=df)
    xbar<-colMeans(df[,dependentvariable], na.rm= TRUE, dims=1)
    uCI<-xbar+(1.96*(sds/sqrt(ns)))
    lCI<-xbar-(1.96*(sds/sqrt(ns)))
    mtxdep <- as.matrix(cimfert[, dependentvariable])
    medians <- colMedians(mtxdep[treatment,], na.rm=TRUE)
    data.frame(sds, ns, xbar, medians, uCI, lCI)
} 
  
#now assemble tables accordingly
#assemble tables for the eggs laid
totegg<-colSummary(tot, postura)
infegg<-colSummary(infect, postura)
conegg<-colSummary(controls, postura)
cPLegg<-colSummary(ControlP, postura )
iPLegg<-colSummary(InfectP, postura)
cRAegg<-colSummary(ControlRA, postura)
iRAegg<-colSummary(InfectRA, postura)
cRBegg<-colSummary(ControlRB, postura)
iRBegg<-colSummary(InfectRB, postura)
#assemble tables fo the eggs hatched.
totvia<-colSummary(tot, viabilidad)
infvia<-colSummary(infect, viabilidad)
convia<-colSummary(controls, viabilidad)
cPLvia<-colSummary(ControlP, viabilidad)
iPLvia<-colSummary(InfectP, viabilidad)
cRAvia<-colSummary(ControlRA, viabilidad)
iRAvia<-colSummary(InfectRA, viabilidad)
cRBvia<-colSummary(ControlRB, viabilidad)
iRBvia<-colSummary(InfectRB, viabilidad)
#assemble tables for percent viability

###====================================================================================
##We can now plot this data for eggs
#all the repetitions pooled togehter.
par(mfrow=c(1,1))
#pdf(file="graphs/AvgEggsLaidBtwnAlvInfyContRepsCombobyWeek.pdf")
plot(infegg$xbar, type="o", main="Average Eggs Laid Between Alive Infected and Control Insects in all Replicates Combined",
     ylab="Number of eggs", xlab="Week in Study", ylim=c(0,8), col="darkorange1", pch=18)
      lines(conegg$xbar, type="o", pch=16, col="dodgerblue1")
      points(infegg$uCI, col="darkorange1") 
      points(infegg$lCI, col="darkorange1")
      points(conegg$uCI, col="dodgerblue1") 
      points(conegg$lCI, col="dodgerblue1")
legend("topright", c("infected","controls", "95% CI"), col=c("darkorange1", "dodgerblue1","black"), pch=c(18,16,1))
#dev.off()

#Por Pilot
#pdf(file="graphs/AvgEggsLaidBtwnAlvInfyCntrlPlbyWeek.pdf")
plot(iPLegg$xbar, type="o", main="Average Eggs Laid Between Alive Infected and Control Insects in Pilot",
     ylab="Number of eggs", xlab="Week in Study", col="darkorange1", pch=18)
  lines(cPLegg$xbar, type="o", pch=16, col="dodgerblue1")
  points(iPLegg$uCI, col="darkorange1") 
  points(iPLegg$lCI, col="darkorange1")
  points(cPLegg$uCI, col="dodgerblue1") 
  points(cPLegg$lCI, col="dodgerblue1")
legend("topright", c("infected","controls", "95% CI"), col=c("darkorange1", "dodgerblue1", "black"),
       pch=c(18,16))
#dev.off()

#Por Rep1
#pdf(file="graphs/AvgEggsLaidBtwnAlvInfyCntrlR1byWeek.pdf")
plot(iRAegg$xbar, type="o", main="Average Eggs Laid Between Alive Infected and Control Insects in Rep 1",
     ylab="Number of eggs", xlab="Week in Study", col="darkorange1", pch=18)
lines(cRAegg$xbar, type="o", pch=16, col="dodgerblue1")
  points(iRAegg$uCI, col="darkorange1") 
  points(iRAegg$lCI, col="darkorange1")
  points(cRAegg$uCI, col="dodgerblue1") 
  points(cRAegg$lCI, col="dodgerblue1")
legend("topright", c("infected","controls","95% CI"), col=c("darkorange1", "dodgerblue1", "black"), pch=c(18,16,1))
#dev.off()

#Por Rep2
#pdf("graphs/AvgEggsLaidBtwnAlvInfyCntrlR2byWeek.pdf")
plot(iRBegg$xbar, type="o", main="Average Eggs Laid Between Infected and Control Insects in Rep 2",
     ylab="Number of eggs", xlab="Week in Study", col="darkorange1", pch=18, ylim=c(1,9))
  points(iRBegg$uCI, col="darkorange1") 
  points(iRBegg$lCI, col="darkorange1")
  points(cRBegg$uCI, col="dodgerblue1") 
  points(cRBegg$lCI, col="dodgerblue1")
  lines(cRBegg$xbar, type="o", pch=16, col="dodgerblue1")
 legend("topright", c("infected","controls","95% CI"), col=c("darkorange1", "dodgerblue1","black"), pch=c(18,16,1))
#dev.off()

#Put them al on one graph
#pdf(file="graphs/AllRepsAvgEggLaidbtwnAlvbtwnInfContbyWeek.pdf")
plot(iPLegg$xbar, type="o", main="Average Eggs Laid Amoung Alive Insects by Infection Status",
     ylab="Number of eggs", xlab="Week in Study", col=2, pch=18, lty=1, ylim=c(0,10))
lines(cPLegg$xbar, type="o", pch=1, col=4, lty=1)
lines(iRAegg$xbar, type="o", pch=2, col=2, lty=2)
lines(cRAegg$xbar, type="o", pch=2, col=4, lty=2)
lines(iRBegg$xbar, type="o", pch=3, col=2, lty=3)
lines(cRBegg$xbar, type="o", pch=3, col=4, lty=3)
legend("topright", c("Pilot Infected","Pilot Controls", "Rep1 Infected", 
                     "Rep1 Controls", "Rep2 Infected", "Rep2 Controls"),
       col=c(2,4,2,4,2,4), pch=c(1,1,2,2,3,3), lty=c(1,1,2,2,3,3))
#dev.off()

##Do the same analysis por hatching
#all the repetitions pooled togehter.
#pdf(file="graphs/AvgHtchBtwnAlvInfCntrlbyWeek.pdf")
plot(infvia$xbar, type="o", main="Average Eggs Hatched by Week Between Alive Infected and Control Insects With All Reps Combined",
     ylab="Number of Hatched Insects", xlab="Week in Study", col="darkorange1", pch=18)
points(infvia$uCI, col="darkorange1") 
points(infvia$lCI, col="darkorange1")
points(convia$uCI, col="dodgerblue1") 
points(convia$lCI, col="dodgerblue1")
lines(convia$xbar, type="o", pch=16, col="dodgerblue1")
legend("topright", c("infected","controls", "95% CI"), 
       col=c("darkorange1", "dodgerblue1","black"), pch=c(18,16,1))
#dev.off()

#Por Pilot
#pdf(files="graphs/AvgHtchBtwnAlvInfyCntrlbyWeekPL.pdf")
plot(iPLvia$xbar, type="o", main="Average Hatched Eggs Between Alive Infected and Control Insects in Pilot",
     ylab="Number of Hatched Insects", xlab="Week in Study", col="darkorange1", pch=18, ylim=c(0,8))
lines(cPLvia$xbar, type="o", pch=16, col="dodgerblue1")
points(iPLvia$uCI, col="darkorange1") 
points(iPLvia$lCI, col="darkorange1")
points(cPLvia$uCI, col="dodgerblue1") 
points(cPLvia$lCI, col="dodgerblue1")
legend("topright", c("infected","controls","95% CI"), col=c("darkorange1", "dodgerblue1", "black"), pch=c(18,16,1))
#dev.off()

#Por Rep1
#pdf(files="graphs/R1AvgHtchBtwn.pdf")
plot(iRAvia$xbar, type="o", main="Average Hatched Eggs Between Alive Infected and Control Insects in Rep 1",
     ylab="Number of Hatched Insects", xlab="Week in Study", col="darkorange1", pch=18, ylim=c(0,9))
lines(cRAvia$xbar, type="o", pch=16, col="dodgerblue1")
legend("topright", c("infected","controls"), col=c("darkorange1", "dodgerblue1"), pch=c(18,16))
#dev.off()

#Por Rep2
#pdf(files="graphs/R2AvgHtchBtwnAlvInfCntrl.pdf")
plot(iRBvia$xbar, type="o", main="Average Hatched Eggs Between Alive Infected and Control Insects in Rep 2",
     ylab="Number of Hatched Insects", xlab="Week in Study", col="darkorange1", pch=18, ylim=c(0,9))
lines(cRBvia$xbar, type="o", pch=16, col="dodgerblue1")
legend("topright", c("infected","controls"), col=c("darkorange1", "dodgerblue1"), pch=c(18,16))
#dev.off()

#Put them all on one graph
#pdf(file="graphs/AllAvgHtchBtwnInfCntrl.pdf")
plot(iPLegg$xbar, type="o", main="Average Hatched Eggs Between Infected and Control Insects",
     ylab="Number of Hatched Insects", xlab="Week in Study", col=2, pch=18, lty=1, ylim=c(0,10))
lines(cPLvia$xbar, type="o", pch=1, col=4, lty=1)
lines(iRAvia$xbar, type="o", pch=2, col=2, lty=2)
lines(cRAvia$xbar, type="o", pch=2, col=4, lty=2)
lines(iRBvia$xbar, type="o", pch=3, col=2, lty=3)
lines(cRBvia$xbar, type="o", pch=3, col=4, lty=3)
legend("topright", c("Pilot Infected","Pilot Controls", "Rep1 Infected", 
                     "Rep1 Controls", "Rep2 Infected", "Rep2 Controls"),
       col=c(2,4,2,4,2,4), pch=c(1,1,2,2,3,3), lty=c(1,1,2,2,3,3))
#dev.off()

###plotting the medians
##lets make the table with the total medians and then medians with the mean
#Lets start with the median plots for eggs and such
par(mfrow=c(1,1))
#pdf()
plot(infegg$medians, type="o", main="Median Eggs Laid Between Infected and Control Insects",
     ylab="Number of eggs", xlab="Week in Study", col="darkorange1", pch=18)
lines(conegg$medians, type="o", pch=16, col="dodgerblue1")
legend("topright", c("infected","controls"), col=c("darkorange1", "dodgerblue1"), pch=c(18,16))

#dev.off()

plot(infvia$medians, type="o", main="Median Eggs Hatched Between Infected and Control Insects",
     ylab="Number of eggs", xlab="Week in Study", col="darkorange1", pch=18)
lines(convia$medians, type="o", pch=16, col="dodgerblue1")
legend("topright", c("infected","controls"), col=c("darkorange1", "dodgerblue1"), pch=c(18,16))

#Put them al on one graph
plot(iPLegg$medians, type="o", main="Median Eggs Laid Between Infected and Control Insects",
     ylab="Number of eggs", xlab="Week in Study", col=2, pch=18, lty=1, ylim=c(0,10))
lines(cPLegg$medians, type="o", pch=1, col=4, lty=1)
lines(iRAegg$medians, type="o", pch=2, col=2, lty=2)
lines(cRAegg$medians, type="o", pch=2, col=4, lty=2)
lines(iRBegg$medians, type="o", pch=3, col=2, lty=3)
lines(cRBegg$medians, type="o", pch=3, col=4, lty=3)
legend("topright", c("Pilot Infected","Pilot Controls", "Rep1 Infected", 
                     "Rep1 Controls", "Rep2 Infected", "Rep2 Controls"),
       col=c(2,4,2,4,2,4), pch=c(1,1,2,2,3,3), lty=c(1,1,2,2,3,3))

plot(iPLvia$medians, type="o", main="Median Eggs Hatched Between Infected and Control Insects",
     ylab="Number of eggs", xlab="Week in Study", col=2, pch=18, lty=1, ylim=c(0,10))
lines(cPLvia$medians, type="o", pch=1, col=4, lty=1)
lines(iRAvia$medians, type="o", pch=2, col=2, lty=2)
lines(cRAvia$medians, type="o", pch=2, col=4, lty=2)
lines(iRBvia$medians, type="o", pch=3, col=2, lty=3)
lines(cRBvia$medians, type="o", pch=3, col=4, lty=3)
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
#cimpilot<-which(cimfert$trial==0)
#cimfert <- cimfert[cimpilot,]
#rowna<-which(is.na(cimfert$ID)==TRUE)
#cimfert<-cimfert[-rowna,]

#par(mfrow=c(2,2))
eggweek<-lm(Compile$eggs ~Compile$week)
hatchweek<-lm(Compile$hatch ~Compile$week)
plot(Compile$week, Compile$eggs+rnorm(length(Compile$eggs), 0, 0.5), main="Eggs by Week")
abline(eggweek)
boxplot(Compile$eggs ~Compile$week, main="Eggs by Week")
plot(Compile$week, Compile$hatch+rnorm(length(Compile$hatch), 0, 0.5), main="Hatching by Week")
abline(hatchweek)
boxplot(Compile$hatch ~Compile$week, main="Hatching by Week")

#is this attempting to look at infection status at all>?
#pdf("")
#par(mfrow=c(2,4))
# plot(Compile$week, Compile$eggs, main="Eggs by Week")
# boxplot(Compile$eggs ~Compile$week, main="Eggs by Week")
# plot(Compile$week, Compile$hatch, main="Hatching by Week")
# boxplot(Compile$hatch ~Compile$week, main="Hatching by Week")
# plot(Compile$week, Compile$eggs, main="Eggs by Week")
# boxplot(Compile$eggs ~Compile$week, main="Eggs by Week")
# plot(Compile$week, Compile$hatch, main="Hatching by Week")
# boxplot(Compile$hatch ~Compile$week, main="Hatching by Week")
# #dev.off()
par(mfrow=c(1,1))
#pdf("graphs/NmbEggsByDateBox.pdf")
boxplot(Compile$eggs ~Compile$date, main="Eggs by Date")
#dev.off()

#pdf("graphs/NmbHtchByDateBox.pdf")
boxplot(Compile$hatch ~Compile$date, main="Hatching by Date")
#dev.off()

##lets makle a combined plot of the two groups together
enona<-which(is.na(Compile$eggs)==FALSE)
hnona<-which(is.na(Compile$hatch)==FALSE)
Compile$infected<-as.factor(Compile$infected)
Compile$week<-as.factor(Compile$week)

#the box plot for eggs laid
par(mfrow=c(1,1))
#pdf("graphs/NumEggsLaidByWeekYInfyCntlBox.pdf")
g<-ggplot(aes( y= eggs, x= week, fill = infected, na.rm=TRUE),
       data= Compile[enona,]) +geom_boxplot(data=Compile[enona,])
g<-g+ggtitle("Distribution of Number of Eggs Laid by Infection Status Each Week")
g<-g+scale_fill_manual(values=c("blue", "red"))
#g<-g+scale_x_continuous(breaks=seq(0, 38, 2))
#g<-g+scale_x_discrete(labels=c("", seq())
g

#ggsave(g, file="BoxPlotDistNumEggLaidbyInfectionStatusbyWeek.jpeg", units= "cm",
#   width = 26.4, height= 15.875)
      
#dev.off()

#hatch plot
#pdf("graphs/NumEggsHtchByWeekyInfyCntrlBox.pdf")
h<-ggplot(aes( y= hatch, x= week, fill = infected, na.rm=TRUE), 
       data= Compile[hnona,])+geom_boxplot(data=Compile[hnona,])
h<-h+ggtitle("Distribution of Number of Eggs Hatched by Infection Status")
h<-h+scale_fill_manual(values=c("blue", "red"))
h

ggsave(h, file="BoxPlotDistNumEggHtvhbyInfectionStatusbyWeek.jpeg", units= "cm",
       width = 26.4, height= 15.875)
#dev.off()

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

#pdf("graphs/NumInctsAlvByWeek.pdf")
plot(alivetotweek)
  points(aliveinfweek, col="red")
  points(aliveconweek, col="steelblue")
  legend("topright", c("All Bugs","infected","controls"), 
        col=c("black","red", "steelblue"), pch=c(1,1,1))
#dev.off()

#a quick check seeing if alivetotweek is the sum of the other two.  
#test<-aliveinfweek+aliveconweek
#testing<-data.frame(test, alivetotweek, aliveinfweek, aliveconweek)

#lets get a better view by looking at (percents)
 which(cimfert$infected==1)
 peraltotweek<-alivetotweek/(length(cimfert$ID))
  peralinfweek <-aliveinfweek/(length(which(cimfert$infected==1)))
  peralconweek <-aliveconweek/(length(which(cimfert$infected==0)))

#pdf("graphs/PerAlvInsctsByWeek.pdf")
plot(peraltotweek, col="black", xlab="Proportion Alive", ylab="Week", 
     main="Proportion Alive in Infected and Controls by Week")
  points(peralconweek, col="steelblue")
  points(peralinfweek, col="red")  
  legend("topright", c("All Bugs","infected","controls"), 
         col=c("black","red", "steelblue"), pch=c(1,1,1))
#dev.off()

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
  lines(peregeconweek, col="steelblue", type="o")
  legend("topright", c("infected","controls"), 
         col=c("red", "steelblue"), pch=c(1,1))

#now we need to see proportion of alive insects    
  proeggeinfweek<-eggeinfweek/aliveinfweek
  proeggeconweek<-eggeconweek/aliveconweek 
  
##see if we can add Confidence Intervals below
#pdf("PerAlvInsctsLaidEggsbyWeek")
  plot(proeggeinfweek, type="o", col="red", xlab="Week", ylab="Proportion of Alive Insects that Laid Eggs", 
       main="Proportion of Alive Insects that Laid in Infected and Controls by Week")
  lines(proeggeconweek, col="steelblue")
  legend("bottomleft", c("infected","controls"), 
         col=c("red", "steelblue"), pch=c(1,1))
# dev.off()
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
#  pdf(graphs/NmbEggsByAlvInsctbyWeekYInfyCont.pdf)
  #consider how to add CI's
  plot(proeggsinfweek, type="o", col="red", xlab="Week", ylab="Num Eggs Per Alive Insect", 
       main="Number of Eggs Per Alive Insect by Week and Treatment Group")
  lines(proeggsconweek, col="steelblue", type="o")
  legend("topright", c("infected","controls"), 
         col=c("red", "steelblue"), pch=c(1,1))
#  dev.off()
  
  
#Now lets observe percentage of insects hatching
  plot(Compile$week, Compile$perferc, na.rm=TRUE)
  
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
#summary(humeggs)

#eggs by average hum
egghum<-ggplot(aes( y= eggs, x= avhum, na.rm=TRUE), 
               data=Compile[enona,])+geom_point(data=Compile[enona,])
egghum<-egghum+ggtitle("Number of Eggs Laid by Average Humidity and Infection Status") 
egghum<-egghum+facet_grid(. ~infected)+geom_smooth(method= "lm")
egghum
eghum<-glm(eggs ~infected*avhum, data=Compile)

#eggs by average high hum
egghumavhigh<-ggplot(aes( y= eggs, x= avhumhigh, na.rm=TRUE), 
               data=Compile[enona,])+geom_point(data=Compile[enona,])
egghumavhigh<-egghumavhigh+ggtitle("Number of Eggs Laid by Average Max Humidity and Infection Status") 
egghumavhigh<-egghumavhigh+facet_grid(. ~infected)+geom_smooth(method= "lm")
egghumavhigh
#eggs by average low hum
egghumavlow<-ggplot(aes( y= eggs, x= avhumlow, na.rm=TRUE), 
                   data=Compile[enona,])+geom_point(data=Compile[enona,])
egghumavlow<-egghumavlow+ggtitle("Number of Eggs Laid by Average Low Humidity and Infection Status") 
egghumavlow<-egghumavlow+facet_grid(. ~infected)+geom_smooth(method= "lm")
egghumavlow

#eggs by week max high
egghumhigh<-ggplot(aes( y= eggs, x= hummax, na.rm=TRUE), 
                     data=Compile[enona,])+geom_point(data=Compile[enona,])
egghumhigh<-egghumhigh+ggtitle("Number of Eggs Laid by Max Humidity and Infection Status") 
egghumhigh<-egghumhigh+facet_grid(. ~infected)+geom_smooth(method= "lm")
egghumhigh
#eggs by week low high
egghumlow<-ggplot(aes( y= eggs, x= hummin, na.rm=TRUE), 
                   data=Compile[enona,])+geom_point(data=Compile[enona,])
egghumlow<-egghumlow+ggtitle("Number of Eggs Laid by Low Humidity and Infection Status") 
egghumlow<-egghumlow+facet_grid(. ~infected)+geom_smooth(method= "lm")
egghumlow
#eggs by hum diff
egghumdiff<-ggplot(aes( y= eggs, x= humdiff, na.rm=TRUE),
  data=Compile[enona,])+geom_point(data=Compile[enona,])
egghumdiff<-egghumdiff+ggtitle("Number of Eggs Laid by Largest Humidity Difference and Infection Status") 
egghumdiff<-egghumdiff+facet_grid(. ~infected)+geom_smooth(method= "lm")
egghumdiff
#lets compare which of these would be best for the model
#each show an interaction between hum and infection status. (maybe tempdiff less so)
mavhum<-glm(eggs~avhum*infected, data=Compile)#13910
mavhumhigh<-glm(eggs~avhumhigh*infected, data=Compile)#13910
mavhumlow<-glm(eggs~avhumlow*infected, data=Compile)#13910
mhummax<-glm(eggs~hummax*infected, data=Compile)#13930#we can kill this
mhummin<-glm(eggs~hummin*infected, data=Compile)#13910
mhumdiff<-glm(eggs~humdiff*infected, data=Compile)#13960 #we can kill this too
avhhumpdiff<-glm(eggs~avhum*+humdiff, data=Compile)#14020 (adding humdiff doesn't help)
avhumlm<-lm(eggs~avhum, data=Compile)
avhumhighlm<-lm(eggs~avhumhigh, data=Compile)
humminlm<-lm(eggs~hummin, data=Compile) #



################################################
#same for temp
#plot temperature by eggs
Compile$avtemp <- as.character(Compile$avtemp)
tempeggs<-lm(Compile$eggs ~Compile$avtemp)
plot(Compile$avtemp, Compile$eggs,
     main="Eggs by Temperature", ylab="Number of Eggs",
     xlab="Temperature(C)")
#eggs by average temp
eggtemp<-ggplot(aes( y= eggs, x= avtemp, na.rm=TRUE), 
               data=Compile[enona,])+geom_point(data=Compile[enona,])
eggtemp<-eggtemp+ggtitle("Number of Eggs Laid by Average Temperature and Infection Status") 
eggtemp<-eggtemp+facet_grid(. ~infected)+geom_smooth(method= "lm")
eggtemp

#eggs by average high hum
eggtempavhigh<-ggplot(aes( y= eggs, x= avtemphigh, na.rm=TRUE), 
                     data=Compile[enona,])+geom_point(data=Compile[enona,])
eggtempavhigh<-eggtempavhigh+ggtitle("Number of Eggs Laid by Avg High Temp and Infection Status") 
eggtempavhigh<-eggtempavhigh+facet_grid(. ~infected)+geom_smooth(method= "lm")
eggtempavhigh
#eggs by average low hum
eggtempavlow<-ggplot(aes( y= eggs, x= avtemplow, na.rm=TRUE), 
                    data=Compile[enona,])+geom_point(data=Compile[enona,])
eggtempavlow<-eggtempavlow+ggtitle("Number of Eggs Laid by Avg Low Temperature and Infection Status") 
eggtempavlow<-eggtempavlow+facet_grid(. ~infected)+geom_smooth(method= "lm")
eggtempavlow

#eggs by week max high
eggtemphigh<-ggplot(aes( y= eggs, x= tempmax, na.rm=TRUE), 
                   data=Compile[enona,])+geom_point(data=Compile[enona,])
eggtemphigh<-eggtemphigh+ggtitle("Number of Eggs Laid by Max Temperature and Infection Status") 
eggtemphigh<-eggtemphigh+facet_grid(. ~infected)+geom_smooth(method= "lm")
eggtemphigh
#eggs by week low high
eggtemplow<-ggplot(aes( y= eggs, x= tempmin, na.rm=TRUE), 
                  data=Compile[enona,])+geom_point(data=Compile[enona,])
eggtemplow<-egghumlow+ggtitle("Number of Eggs Laid by Low Temperature and Infection Status") 
eggtemplow<-egghumlow+facet_grid(. ~infected)+geom_smooth(method= "lm")
eggtemplow
#eggs by temp diff
eggtempdiff<-ggplot(aes( y= eggs, x= tempdiff, na.rm=TRUE),
                   data=Compile[enona,])+geom_point(data=Compile[enona,])
eggtempdiff<-eggtempdiff+ggtitle("Number of Eggs Laid by Largest Temperature Difference and Infection Status") 
eggtempdiff<-eggtempdiff+facet_grid(. ~infected)+geom_smooth(method= "lm")
eggtempdiff

Compile$avtemp <- as.numeric(Compile$avtemp)
eggtem<-ggplot(aes( y= eggs, x= avtemp, na.rm=TRUE), 
               data=Compile[enona,])+geom_point(data=Compile[enona,])
eggtem<-eggtem+ggtitle("Number of Eggs Laid by Temperature and Infection Status") 
eggtem<-eggtem+facet_grid(. ~infected)+geom_smooth(method= "lm")
eggtem
#lets compare to see what would be best for model
#these don't have as important interactions
lmavtemp<-lm(eggs~avtemp, data=Compile) #Rsq=0.017
lmavtemphigh<-lm(eggs~avtemphigh, data=Compile)#Rsq=0.02
lmavtemplow<-lm(eggs~avtemplow, data=Compile)#Rsq=0.010
lmavmin<-lm(eggs~tempmin, data=Compile)#Rsq=0.011
lmtempmax<-lm(eggs~tempmax, data=Compile)#Rsq=0.010
lmtempdiff<-lm(eggs~tempdiff, data=Compile)#Rsq=0.009

mavtemp<-glm(eggs~avtemp+infected, data=Compile) #AIC=13900
mavtemphigh<-glm(eggs~avtemphigh*infected, data=Compile)#AIC=13900
mavtemplow<-glm(eggs~avtemplow*infected, data=Compile)#AIC=13920
mavmin<-glm(eggs~tempmin*infected, data=Compile)
mtempmax<-glm(eggs~tempmax*infected, data=Compile)
mtempdiff<-glm(eggs~tempdiff*infected, data=Compile)


###################
#Now for hatching
#hatch by humidity
humhatch<-lm(Compile$hatch ~Compile$avhum)
plot(Compile$avhum, Compile$hatch,
     main="Hatching by Humidity", ylab="Number of Eggs",
     xlab="Humidity(%)")

#hatch by average hum
hchhum<-ggplot(aes( y= hatch, x= avhum, na.rm=TRUE), 
               data=Compile[hnona,])+geom_point(data=Compile[hnona,])
hchhum<-hchhum+ggtitle("Number of Eggs Hatched by Humidity and Infection Status") 
hchhum<-hchhum+facet_grid(. ~infected)+geom_smooth(method= "lm")
hchhum

#hatch average high hum
hchhumavhigh<-ggplot(aes( y= hatch, x= avhumhigh, na.rm=TRUE), 
                     data=Compile[hnona,])+geom_point(data=Compile[hnona,])
hchhumavhigh<-hchhumavhigh+ggtitle("Number of Eggs Hatched by Max Humidity and Infection Status") 
hchhumavhigh<-hchhumavhigh+facet_grid(. ~infected)+geom_smooth(method= "lm")
hchhumavhigh
#hatch by average low hum
hchhumavlow<-ggplot(aes( y= hatch, x= avhumlow, na.rm=TRUE), 
                    data=Compile[hnona,])+geom_point(data=Compile[hnona,])
hchhumavlow<-hchhumavlow+ggtitle("Number of Eggs Hatched by Average Low Humidity and Infection Status") 
hchhumavlow<-hchhumavlow+facet_grid(. ~infected)+geom_smooth(method= "lm")
hchhumavlow

#hatch by week max high
hchhumhigh<-ggplot(aes( y= hatch, x= hummax, na.rm=TRUE), 
                   data=Compile[hnona,])+geom_point(data=Compile[hnona,])
hchhumhigh<-hchhumhigh+ggtitle("Number of Eggs Hatched by Max Humidity and Infection Status") 
hchhumhigh<-hchhumhigh+facet_grid(. ~infected)+geom_smooth(method= "lm")
hchhumhigh
#hatch by week low high
hchhumlow<-ggplot(aes( y= hatch, x= hummin, na.rm=TRUE), 
                  data=Compile[hnona,])+geom_point(data=Compile[hnona,])
hchhumlow<-hchhumlow+ggtitle("Number of Eggs Hatched by Low Humidity and Infection Status") 
hchhumlow<-hchhumlow+facet_grid(. ~infected)+geom_smooth(method= "lm")
hchhumlow
#hatch by hum diff
hchhumdiff<-ggplot(aes( y= hatch, x= humdiff, na.rm=TRUE),
                   data=Compile[hnona,])+geom_point(data=Compile[hnona,])
hchhumdiff<-hchhumdiff+ggtitle("Number of Eggs Hatced by Largest Humidity Difference and Infection Status") 
hchhumdiff<-hchhumdiff+facet_grid(. ~infected)+geom_smooth(method= "lm")
hchhumdiff
############################
#hatch by temp
temphatch<-lm(Compile$hatch ~Compile$avtemp)
plot(Compile$avtemp, Compile$hatch,
     main="Hatching by Temperature", ylab="Number of Eggs",
     xlab="Temperature(C)")

#hatch by average temp
hchtemp<-ggplot(aes( y= hatch, x= avtemp, na.rm=TRUE), 
               data=Compile[hnona,])+geom_point(data=Compile[hnona,])
hchtemp<-hchtemp+ggtitle("Number of Eggs Hatched by Temp and Infection Status") 
hchtemp<-hchtemp+facet_grid(. ~infected)+geom_smooth(method= "lm")
hchtemp

#hatch average high temp
hchtempavhigh<-ggplot(aes( y= hatch, x= avtemphigh, na.rm=TRUE), 
                     data=Compile[hnona,])+geom_point(data=Compile[hnona,])
hchtempavhigh<-hchtempavhigh+ggtitle("Number of Eggs Hatched by Max Temp and Infection Status") 
hchtempavhigh<-hchtempavhigh+facet_grid(. ~infected)+geom_smooth(method= "lm")
hchtempavhigh
#hatch by average low temp
hchtempavlow<-ggplot(aes( y= hatch, x= avtemplow, na.rm=TRUE), 
                    data=Compile[hnona,])+geom_point(data=Compile[hnona,])
hchtempavlow<-hchtempavlow+ggtitle("Number of Eggs Hatched by Average Low Temp and Infection Status") 
hchtempavlow<-hchtempavlow+facet_grid(. ~infected)+geom_smooth(method= "lm")
hchtempavlow

#hatch by week max high
hchtemphigh<-ggplot(aes( y= hatch, x= tempmax, na.rm=TRUE), 
                   data=Compile[hnona,])+geom_point(data=Compile[hnona,])
hchtemphigh<-hchtemphigh+ggtitle("Number of Eggs Hatched by Max Temp and Infection Status") 
hchtemphigh<-hchtemphigh+facet_grid(. ~infected)+geom_smooth(method= "lm")
hchtemphigh
#hatch by week min low 
hchtemplow<-ggplot(aes( y= hatch, x= tempmin, na.rm=TRUE), 
                  data=Compile[hnona,])+geom_point(data=Compile[hnona,])
hchtemplow<-hchtemplow+ggtitle("Number of Eggs Hatched by Low Temp and Infection Status") 
hchtemplow<-hchtemplow+facet_grid(. ~infected)+geom_smooth(method= "lm")
hchtemplow
#hatch by hum diff
hchtempdiff<-ggplot(aes( y= hatch, x= tempdiff, na.rm=TRUE),
                   data=Compile[hnona,])+geom_point(data=Compile[hnona,])
hchtempdiff<-hchtempdiff+ggtitle("Number of Eggs Hatced by Largest Temp Difference and Infection Status") 
hchtempdiff<-hchtempdiff+facet_grid(. ~infected)+geom_smooth(method= "lm")
hchtempdiff
################################
# ##For each time line
# glm(cases~rhs(data$year,2003)+lhs(data$year,2003)+ offset(log(population)), data=data, 
#subset=28:36, family=poisson())

#Now lets make a graph of Eggs by week
infected<-which(Compile$infected==1)
controled<-which(Compile$infected==0)
uniquebugs<-unique(Compile$id)
plot(Compile$week[infected], Compile$eggs[infected]+rnorm(length(Compile$eggs[infected]), 0.5, 1), col="red")

#Now lets make a graphs o week and date by eggs of infected insects
infected<-which(Compile$infected==1)
controled<-which(Compile$infected==0)
uniquebugs<-unique(Compile$id)
plot(Compile$week[infected], Compile$eggs[infected], col="red")
plot(Compile$date[infected], Compile$eggs[infected], col="red")     

#Compare infected hatch and by week.
par(mfrow=c(2,4))
#infected
#pdf(file="graphs/EggsandHatchvsWeekboxanddotplots.pdf")
plot(Compile$week[infected], Compile$eggs[infected], main="Infected Eggs Laid by Week",
     ylab="Weeks", xlab="Eggs")
boxplot(Compile$eggs[infected] ~c(Compile$week[infected]), main="Infected Eggs Laid by Week",
        ylab="Weeks", xlab="Eggs")
plot(Compile$week[infected], Compile$hatch[infected], main="Infected Eggs Hatched by Week",
     ylab="Weeks", xlab="Eggs")
boxplot(Compile$hatch[infected] ~Compile$week[infected], main="Infected Eggs Hatched by Week",
        ylab="Weeks", xlab="Eggs")
#not infected
plot(Compile$week[controled], Compile$eggs[controled], main="Control Eggs Laid by Week",
     ylab="Weeks", xlab="Eggs")
boxplot(Compile$eggs[controled] ~Compile$week[controled], main="Control Eggs Laid by Week",
        ylab="Weeks", xlab="Eggs")
plot(Compile$week[controled], Compile$hatch[controled], main="Control Eggs Hatched by Week",
     ylab="Weeks", xlab="Eggs")
boxplot(Compile$hatch[controled] ~Compile$week[controled], main="Control Eggs Hatched by Week",
        ylab="Weeks", xlab="Eggs")

##create a table for total eggs and hatch for each female.
#make two rows to fill
Compile$eggs<-as.numeric(Compile$eggs)
Compile$egg_total<-Compile$eggs*NA
Compile$hatch_total<-Compile$eggs*NA

#create loop that does calculation
for (i in 1:max(Compile$idnum)){
  is<-which(Compile$idnum==i)
  toteggs<-sum(Compile$eggs[is], na.rm=TRUE)
  tothatches<-sum(Compile$hatch[is], na.rm=TRUE)
  Compile$egg_total[is]<-toteggs
  Compile$hatch_total[is]<-tothatches
}

#for histogram create unique
dayone <- which(Compile$week==1)

#par(mfrow=c(1,1))
 pdf("egg_total_hist.pdf")
hist(Compile$egg_total[dayone], breaks=max(Compile$egg_total[dayone]))
#dev.off()


#dev.off()
#create a factor id for mice for STATA
mice<-unique(Compile$mouse)
mouseidnum<-c(1:length(mice))
mousetable<-data.frame(mice,mouseidnum)
mousetable$mouseidnum<-as.factor(mousetable$mouseidnum)
Compile$mouseidnum<-Compile$trial*0

for(i in 1:length(mice)){
  micenumi<-which(mousetable$mouseidnum==i)
  micematch<-which(Compile$mouse==mousetable$mice[micenumi])
  Compile$mouseidnum[micematch]<-i   
}

##create total values for each insect
#create rows to fill by loop
#make two rows to fill
Compile$lifespan<-Compile$eggs*NA
Compile$avtemp_total<-Compile$eggs*NA
Compile$avlowtemp_total<-Compile$eggs*NA
Compile$avhightemp_total<-Compile$eggs*NA
Compile$avhum_total<-Compile$eggs*NA
Compile$avlowhum_total<-Compile$eggs*NA
Compile$avhighhum_total<-Compile$eggs*NA
Compile$hightemp_total<-Compile$eggs*NA
Compile$highhum_total<-Compile$eggs*NA
Compile$lowtemp_total<-Compile$eggs*NA
Compile$lowhum_total<-Compile$eggs*NA


#identify living observations so temperatures while the
#insect is dead does not have affect
lives <- which(Compile$alive==1) 

#loop over each insect and calculate average and max/min/diff
#add dfference
for (i in 1:max(Compile$idnum)) {
  ids<-which(Compile$idnum==i)
  is <- intersect(ids, lives)#that way you don't add dead times to observations
  #average temperatures
  avgt<-mean(Compile$avtemp[is], na.rm=TRUE)
  avght<-mean(Compile$avtemphigh[is], na.rm=TRUE)
  avglt<-mean(Compile$avtemplow[is], na.rm=TRUE)
  #average humidity
  avgh<-mean(Compile$avhum[is], na.rm=TRUE)
  avghh<-mean(Compile$avhumhigh[is], na.rm=TRUE)
  avglh<-mean(Compile$avhumlow[is], na.rm=TRUE)
  #Max and min Temp
  maxt <- max(Compile$tempmax[is], na.rm=TRUE)
  mint <- min(Compile$tempmin[is], na.rm=TRUE)
  #Max and min hum
  maxh <- max(Compile$hummax[is], na.rm=TRUE)
  minh <- min(Compile$hummin[is], na.rm=TRUE)
  
  #Longevity (The weeks alive)
  ls<-length(is)
 
  #Put the calculated items into the appropriate vectors of data frame
  Compile$lifespan[ids]<-ls
  Compile$avtemp_total[ids]<-avgt
  Compile$avlowtemp_total[ids]<-avglt
  Compile$avhightemp_total[ids]<-avght
  Compile$avhum_total[ids]<-avgh
  Compile$avlowhum_total[ids]<-avglh
  Compile$avhighhum_total[ids]<-avghh
  Compile$hightemp_total[ids]<-maxt
  Compile$highhum_total[ids]<-maxh
  Compile$lowtemp_total[ids]<-mint
  Compile$lowhum_total[ids]<-minh
}
write.csv(Compile,"CompiledFertilityData.csv")

###############################################################################
#==============================================================================
#ahora tenomos muchas graficas, podemos empezar haciendo el modelo
#########################################################
#reduce the data to just the first week
weekone<-which(Compile$week==1)
single<-Compile[weekone,]

##Now go through each temperature and humidity covariate by graphing
#see which variables will be used for temperature and humidity
pdf("Hum_Temp_covariates.pdf")
par(mfrow=(c(2,5)))
#eggs by average humidity
segghum<-ggplot(aes( y= eggs, x= avhum_total, na.rm=TRUE), 
               data=single)+geom_point(data=single)
segghum<-segghum+ggtitle("Number of Eggs Laid by Average Humidity and Infection Status") 
segghum<-segghum+facet_grid(. ~infected)+geom_smooth(method= "lm")
segghum
seghum<-glm(eggs ~infected*avhum_total, data=single)
seghum 

#egg avg high humidity
segghighhum <- ggplot(aes( y= eggs, x= avhighhum_total, na.rm=TRUE), 
                data=single)+geom_point(data=single)
segghighhum <- segghighhum+ggtitle("Number of Eggs Laid by Average High Humidity and Infection Status") 
segghighhum <- segghighhum+facet_grid(. ~infected)+geom_smooth(method= "lm")
segghighhum
seghighhum <- glm(eggs ~infected*avhighhum_total, data=single)
seghighhum 

#egg avg low humidity
segglowhum <- ggplot(aes( y= eggs, x= avlowhum_total, na.rm=TRUE), 
                      data=single)+geom_point(data=single)
segglowhum <- segglowhum+ggtitle("Number of Eggs Laid by Average Low Humidity and Infection Status") 
segglowhum <- segglowhum+facet_grid(. ~infected)+geom_smooth(method= "lm")
segglowhum
seglowhum <- glm.nb(eggs ~infected*avlowhum_total, data=single)
seglowhum 
seglowhum <- glm.nb(eggs ~avlowhum_total, data=single)
seglowhum 
summary(seglowhum)

#egg max humidity
seggmaxhum <- ggplot(aes( y= eggs, x= highhum_total, na.rm=TRUE), 
                     data=single)+geom_point(data=single)
seggmaxhum <- seggmaxhum+ggtitle("Number of Eggs Laid by Maximum Humidity and Infection Status") 
seggmaxhum <- seggmaxhum+facet_grid(. ~infected)+geom_smooth(method= "lm")
seggmaxhum
segmaxhum <- glm(eggs ~infected*highhum_total, data=single)
segmaxhum 

#egg min humidity
segglowhum <- ggplot(aes( y= eggs, x= lowhum_total, na.rm=TRUE), 
                     data=single)+geom_point(data=single)
segglowhum <- segglowhum+ggtitle("Number of Eggs Laid by Minimum Humidity and Infection Status") 
segglowhum <- segglowhum+facet_grid(. ~infected)+geom_smooth(method= "lm")
segglowhum
seglowhum <- glm(eggs ~infected*lowhum_total, data=single)
seglowhum 

##Temperature
#eggs by average temperature
seggtemp<-ggplot(aes( y= eggs, x= avtemp_total, na.rm=TRUE), 
                data=single)+geom_point(data=single)
seggtemp<-seggtemp+ggtitle("Number of Eggs Laid by Average Temperature and Infection Status") 
seggtemp<-seggtemp+facet_grid(. ~infected)+geom_smooth(method= "lm")
seggtemp
segtemp<-glm(eggs ~infected*avtemp_total, data=single)
segtemp

#egg avg high temperature
segghightemp<-ggplot(aes( y= eggs, x= avhightemp_total, na.rm=TRUE), 
                data=single)+geom_point(data=single)
segghightemp<-segghightemp+ggtitle("Number of Eggs Laid by Average High Temperature and Infection Status") 
segghightemp<-segghightemp+facet_grid(. ~infected)+geom_smooth(method= "lm")
segghightemp
seghightemp<-glm(eggs ~infected*avhightemp_total, data=single)
seghightemp 

#egg avg low termperatuer
segglowtemp<-ggplot(aes( y= eggs, x= avlowtemp_total, na.rm=TRUE), 
                 data=single)+geom_point(data=single)
segglowtemp<-segglowtemp+ggtitle("Number of Eggs Laid by Average Low Temperature and Infection Status") 
segglowtemp<-segglowtemp+facet_grid(. ~infected)+geom_smooth(method= "lm")
segglowtemp
seglowtemp<-glm(eggs ~infected*avlowtemp_total, data=single)
seglowtemp 

#egg max temperature
seggmaxtemp<-ggplot(aes( y= eggs, x= hightemp_total, na.rm=TRUE), 
                 data=single)+geom_point(data=single)
seggmaxtemp<-seggmaxtemp+ggtitle("Number of Eggs Laid by Maximum Temperature and Infection Status") 
seggmaxtemp<-seggmaxtemp+facet_grid(. ~infected)+geom_smooth(method= "lm")
seggmaxtemp
segmaxtemp<-glm(eggs ~infected*hightemp_total, data=single)
segmaxtemp 

#egg min temperature
seggmintemp<-ggplot(aes( y= eggs, x= lowtemp_total, na.rm=TRUE), 
                 data=single)+geom_point(data=single)
seggmintemp<-seggmintemp+ggtitle("Number of Eggs Laid by Minimum Temperature and Infection Status") 
seggmintemp<-seggmintemp+facet_grid(. ~infected)+geom_smooth(method= "lm")
seggmintemp
segmintemp<-glm(eggs ~infected*lowtemp_total, data=single)
segmintemp 

par(mfrow=c(1,1))
dev.off()

# Instead of doing this all again,lets make a loop
#varriates <- c("", "")

#el ejemplo de Ricardo
#glm(cases~rhs(data$year,2003)+lhs(data$year,2003)+ offset(log(population)), data=data, subset=28:36, family=poisson())
#fake code template

#Bring in Compiled data exported from code above.
Compile<-read.csv("CompiledFertilityData.csv")

#for box plots week was categorical.  This makes a numeric column.
Compile$weeknum<-as.numeric(Compile$week)

#I am also going to make a table with all egg and hatch are no longer NA but 0 
Compile$hatchdiff<-Compile$eggs-Compile$hatch
Compile$infectedf<-as.factor(Compile$infected)
Compile$hatchdiff<-as.numeric(Compile$hatchdiff)
hatchbox<-ggplot(aes( x= hatchdiff, fill = infectedf, na.rm=TRUE),
          data= Compile[enona,]) +geom_density(alpha=0.5, na.rm=TRUE)#geom_histogram(aes(y="..density.."),position="dodge")
hatchbox<-hatchbox+ggtitle("Relative denisty of differences between hatched and laid eggs by infection status")
hatchbox<-hatchbox+scale_fill_manual(values=c("blue", "red"))
ggsave(g, file="HatchDiffDensity.jpeg", units= "cm", width = 26.4, height= 15.875)
#g<-g+scale_x_continuous(breaks=seq(0, 38, 2))
#g<-g+scale_x_discrete(labels=c("", seq())
hatchbox

#Eggsave(hatchbox, file="HatchDiffDensityByInf.jpeg", units= "cm", width = 26.4, height= 15.875)

CompileNoNA<-Compile
eggna <- which(is.na(CompileNoNA$eggs)==TRUE)
hatchna <- which(is.na(CompileNoNA$hatch)==TRUE)
CompileNoNA$eggs[eggna]<-0
CompileNoNA$hatch[hatchna]<-0

#Make Compile where NA's eggs are removed. 
CompileRD<-Compile[-eggna,]

#One final change to the data frames, I would like to make a julian day vector.
#provides numeric value for possible seasonaility analysis from the initial observation. 
#careful this is not necessesary
dateRD<-julian.Date(CompileRD$date)
startDate<-julian.Date(CompileRD$start)
mindate<-min(startDate)
CompileRD$juliandate<-dateRD-startDate

#we found that we need to conduct a cox proportional hazaard test for time until first egg
#thus we need to first reverse the binary of egg event.
Compile$reveggevent<-1-Compile$eggevent
#http://socserv.socsci.mcmaster.ca/jfox/Books/Companion/appendix/Appendix-Cox-Regression.pdf
sevent<-Surv(time=CompileRD$weeknum, event=CompileRD$eggevent)
#need to get rid of the "+"

CoxEggLaid<-coxph(sevent~infected+cluster(idnum), data=CompileRD)
#summary(survfit(CoxEggLaid, newdata =))
#because events occur more than once its a mess because "+" are added.
hist(CoxEggLaid)

#The models remove the NA's so lets use CompileRD to make two simple graphs
hist(CompileRD$eggs, breaks=21)
mean(CompileRD$eggs)#3.94
var(CompileRD$eggs) #17.5
#This shows that poisson assumptions are not met. 
#earlier(under the poisson models) I took the variance of a mean by week...which gave me a smaller value of course.
#But yes, poisson values are not the same.

    

#lets combine the data to remove 0 inflation
#Use the Collapse to break into smaller groups

#function that takes in the desired number of weeks per periood
SummaryCompileRD<-function(segl){
  #Create an emty output vector
  CompileRD$period<-CompileRD$week*0
  #Calculate the appropriate variables: total number of weeks and the number of segments
  maxw<-max(Compile$week)
  nseg<-maxw/segl
  nseg<-ceiling(nseg)
  
  #assign each week into one of the segments. This starts with all the sets and 
  #adds 1 to the period vector
  #then it removes the first segment( or period) and adds another 1 to the remaining segments until
  #each week has the corresponding period designation.
  for(i in 1:nseg){
    adding<-which(CompileRD$week>((segl*i)-segl))
    CompileRD$period[adding]<-CompileRD$period[adding]+1
  }
#   #this is a test
#   CompileRD$periods<-CompileRD$week*0
#   addingq<-which(CompileRD$week>(0))
#   CompileRD$periods[addingq]<-CompileRD$periods[addingq]+1
#   d<-c(rep(1:3, each=10))
#   tree<-which(d>1)
#   d[tree]<-d[tree]+1
#   d
  
  
  #create unicode to merge tables later.
  CompileRD$periodid<-paste(CompileRD$idnum, CompileRD$period, sep="-")
  
  #redefine functions so na.rm is fed into the function
  #be aware that this can introduce 0's, but it is on CompileRD so that
  #should only happen for hatch when egg is already 0.
  sums<-function(row){
    sum(row, na.rm=TRUE)
  }
  maxs<-function(row){
    max(row, na.rm=TRUE)
  }
  mins<-function(row){
    min(row, na.rm=TRUE)
  }
  means<-function(row){
    mean(row, na.rm=TRUE)
  }
  
  #These functions perform a function on the desired outputs as it reduces the 
  #data from weeks to the desired period. 
  CompileRDsum<-summaryBy(eggs+hatch+alive~idnum+period+infected+
                mouse+trial+periodid, FUN=c(sums), 
                data=CompileRD, keep.names = TRUE) 
  CompileRDmax<-summaryBy(tempmax+tempdiff+hummax+humdiff~periodid, 
                FUN=c(maxs), data=CompileRD, keep.names = TRUE) 
  CompileRDmin<-summaryBy(tempmin+hummin~periodid, FUN=c(mins), data=CompileRD,
                          keep.names = TRUE) 
  CompileRDmean<-summaryBy(avtemphigh+avtemplow+avtemp+avhumhigh+avhumlow+avhum
                ~periodid, FUN=c(means), data=CompileRD, keep.names = TRUE) 

 a<-merge(CompileRDmax, CompileRDmean, by="periodid")
 b<-merge(a, CompileRDmin, by="periodid")
 final<-merge(b, CompileRDsum, by="periodid")
 final
}

#lets look at 2 and 3
C2week<-SummaryCompileRD(2)
C3week<-SummaryCompileRD(3)
C5week<-SummaryCompileRD(5)
C8week<-SummaryCompileRD(8)

#
fivedead<-which(C5week$alive<5)

#checking code
zeroeight<-which(C8week$eggs==0)
zerotable<-C8week[zeroeight,]
zeroids<-unique(zerotable$idnum)

removedeathday<-function(weektab){
  deads<-which(weektab$alive<max(weektab$alive))
  tablerd<-weektab[,-deads]
  tablerd
  }
C2weekRd<-removedeathday(C2week)
#10-2 is one with 0's
tens<-which(Compile$idnum==10)
tentable<-Compile[tens,]

#plot the 0's
  plot(1:37,1:37,col=0)	
   for (i in 1:20){
    zer<-which(Compile$idnum==zeroids[i])
    lines(Compile$week[zer], Compile$eggs[zer], col=i)
}
  plot(1:37,1:37,col=0)	
  for (i in 21:40){
    zer<-which(Compile$idnum==zeroids[i])
    lines(Compile$week[zer], Compile$eggs[zer], col=i)
  }
  
  plot(1:37,1:37,col=0)	
  for (i in 41:60){
    zer<-which(Compile$idnum==zeroids[i])
    lines(Compile$week[zer], Compile$eggs[zer], col=i)
  }  

  plot(1:37,1:37,col=0)	
  for (i in 61:80){
    zer<-which(Compile$idnum==zeroids[i])
    lines(Compile$week[zer], Compile$eggs[zer], col=i)
  }    

# pdf("compacted_egg_hist.pdf")
# par(mfrow=c(3,1))
# hist(CompileRD$eggs, breaks=compmax)
# hist(C2weeklive$eggs, breaks=twomax)
# hist(C3weeklive$eggs, breaks=threemax)
# dev.off()




#write.csv(CompileRD,"ReducedCompiledFertility Data.csv")
#write.csv( C2weeklive, "2weekGroupedFertilityData_simple.csv")
#write.csv(C3weeklive, "3weekGroupedFertilityData_simple.csv")

plot(CompileRD$week, CompileRD$eggs)
plot(log(CompileRD$week+1+rnorm(length(CompileRD$eggs), 0, 0.5)), CompileRD$eggs+rnorm(length(CompileRD$eggs), 0, 0.5))

#for box plots week was categorical.  This makes a numeric column.
Compile$weeknum<-as.numeric(Compile$week)

gmod1<-geem(eggs ~infected, id=idnum, data=Compile, family=poisson, corstr="exchangeable", Mv=1)

pmod1a<-glm(eggs ~ +(1|idnum), offset=log(visits+1), data = Compile, family ="poisson") 

pmod1b<-glm(eggs ~ infected, data = Compile, family = poisson) #17383
pmod1c<-glm(eggs ~ weeknum, data = Compile, family = poisson) #17187
pmod1d<-glm(eggs ~ mouse, data = Compile, family = poisson) #17113
pmod1e<-glm(eggs ~ trial, data = Compile, family = poisson) #17695
pmod1f<-glm(eggs ~ avtemphigh, data = Compile, family = poisson) #17695
pmod1g<-glm(eggs ~ avtemplow, data = Compile, family = poisson) #17322
pmod1h<-glm(eggs ~ avtemp, data = Compile, family = poisson) #17417
pmod1i<-glm(eggs ~ tempmax, data = Compile, family = poisson) #17356
pmod1j<-glm(eggs ~ tempmin, data = Compile, family = poisson) #17284
pmod1k<-glm(eggs ~ tempdiff, data = Compile, family = poisson) #17413
pmod1l<-glm(eggs ~ avhumhigh, data = Compile, family = poisson) #17419
pmod1m<-glm(eggs ~ avhumlow, data = Compile, family = poisson) #17440
pmod1n<-glm(eggs ~ avhum, data = Compile, family = poisson) #17450
pmod1o<-glm(eggs ~ hummax, data = Compile, family = poisson) #17442
pmod1p<-glm(eggs ~ hummin, data = Compile, family = poisson) #17482
pmod1q<-glm(eggs ~ humdiff, data = Compile, family = poisson) #17452


pmod2a2 <- glm(eggs ~ week + (1|idnum), data = Compile, family = poisson) #17383

pmod2a <- glm(eggs ~ infected + (1|idnum), data = Compile, family = poisson) #17383
pmod2al <- glm(eggs ~ infected+weeknum, data = Compile, family = poisson) 
pmod2am <- glm(eggs ~ mouse + (1|idnum), data = Compile, family = poisson) #17113
pmod2both <- glm(eggs ~ mouse + infected+ (1|idnum), data = Compile, family = poisson) #17113
pmod2b <- glm(eggs ~ infected+weeknum + (1|idnum), data = Compile, family = poisson) #16713
pmod2c <- glm(eggs ~ infected+weeknum+ avtemp + (1|idnum), data = Compile, family = poisson)#16545
pmod2d<- glm(eggs ~ infected+weeknum+ avtemp + avhum+ (1|idnum), data = Compile, family = poisson)#1647416
pmod2e<- glm(eggs ~ infected+weeknum+ avtemp + avhum+alive+ (1|idnum), data = Compile, family = poisson)#
#mod2f<- glm(eggs ~ infected+weeknum+ avtemp + avhum+alive+ (1|idnum), data = Compile, family = neg.bin())
pmod2fboth<- glm(eggs ~ infected+mouse+weeknum+ avtemp + avhum+alive+ (1|idnum), data = Compile, family =poisson)#16075


#plotting models onto the original data. use model$fitted on the NA removed dataframe
plot(CompileRD$week, CompileRD$eggs)
lines(CompileRD$week, pmod2a2$fitted)



#check assumptions for poisson.  This uses values from above so disregard.
#aveggweek<-mean(totegg$xbar, na.rm=T)  #2.61
#var(totegg$xbar, na.rm=T)   #3.48
#upperlimmean<-aveggweek+(1.96*(sd(totegg$xbar, na.rm=T)/sqrt(length(totegg$xbar)))) #3.21
#lowlimvar<-(length(totegg$xbar)-1)*(var(totegg$xbar, na.rm=T))/41.923 #2.98
#does not survive hypothesis test that they are 

#using MASS package, use the glm.nb for a negative binomial model
#run model and look at AICs, below there is a formula to interpret. But basically if AIC is >3

#############Negative Binomial###################
#at the end of the day we want to know if infection is responsible for change eggs
#when other envir. factors excluded and the change in lifespan.
#how I understand, not having interaction will only change intercept, not slope
nbmod1a<-glm.nb(eggs ~ weeknum+ (1|idnum), data=Compile) #AIC= 11999 (much better than any poisson)
nbmod1b<-glm.nb(eggs ~ infected+weeknum+ (1|idnum), data=Compile) #AIC 11917
nbmod1c<-glm.nb(eggs ~ infected*weeknum+ (1|idnum), data=Compile) #AIC 11911 (AIC of >=6 is sign.)

#now we have a base model, lets look at how humidity and temperature affect
nbmod2a<-glm.nb(eggs ~ infected*weeknum+avhum+ (1|idnum), data=Compile) #AIC=11858
nbmod2b<-glm.nb(eggs ~ infected*weeknum+avhum+avhum*infected+(1|idnum), data=Compile)# 11816
nbmod2c<-glm.nb(eggs ~ infected*weeknum+avhumhigh+(1|idnum), data=Compile)#11858
nbmod2d<-glm.nb(eggs ~ infected*weeknum+avhumhigh+avhumhigh*infected+(1|idnum), data=Compile)#11821
nbmod2e<-glm.nb(eggs ~ infected*weeknum+avhumlow+ (1|idnum), data=Compile)#11851
nbmod2f<-glm.nb(eggs ~ infected*weeknum+avhumlow+avhumlow*infected+(1|idnum), data=Compile)#11813
nbmod2g<-glm.nb(eggs ~ infected*weeknum+avtemp+(1|idnum), data=Compile)#11850
nbmod2h<-glm.nb(eggs ~ infected*weeknum+avtemp+avtemp*infected+(1|idnum), data=Compile)# 11846
nbmod2i<-glm.nb(eggs ~ infected*weeknum+avtemphigh+(1|idnum), data=Compile)# 11853
nbmod2j<-glm.nb(eggs ~ infected*weeknum+avtemphigh+avtemphigh*infected+(1|idnum), data=Compile)#11848
nbmod2k<-glm.nb(eggs ~ infected*weeknum+avtemplow+(1|idnum), data=Compile)#11850
nbmod2l<-glm.nb(eggs ~ infected*weeknum+avtemplow+avtemplow*infected+(1|idnum), data=Compile)#11848
nbmod2m<-glm.nb(eggs ~ infected*weeknum+hummax+(1|idnum), data=Compile)#11857
nbmod2n<-glm.nb(eggs ~ infected*weeknum+hummax+hummax*infected+(1|idnum), data=Compile)#11830
nbmod2o<-glm.nb(eggs ~ infected*weeknum+hummin+ (1|idnum), data=Compile) #11858
nbmod2p<-glm.nb(eggs ~ infected*weeknum+hummin+hummin*infected+(1|idnum), data=Compile) #11810
nbmod2q<-glm.nb(eggs ~ infected*weeknum+humdiff+(1|idnum), data=Compile)# 11859
nbmod2r<-glm.nb(eggs ~ infected*weeknum+humdiff+humdiff*infected+(1|idnum), data=Compile)#11860
nbmod2s<-glm.nb(eggs ~ infected*weeknum+tempmax+ (1|idnum), data=Compile)#11853
nbmod2t<-glm.nb(eggs ~ infected*weeknum+tempmax+tempmax*infected+(1|idnum), data=Compile)#11854
nbmod2u<-glm.nb(eggs ~ infected*weeknum+tempmin+(1|idnum), data=Compile)#11847
nbmod2v<-glm.nb(eggs ~ infected*weeknum+avhum+tempmin*infected+(1|idnum), data=Compile)#11841
nbmod2w<-glm.nb(eggs ~ infected*weeknum+avhum+(1|idnum), data=Compile)#11858
nbmod2z<-glm.nb(eggs ~ infected*weeknum+avhum+avhum*infected+(1|idnum), data=Compile)#11816

#Now we have strongest hum temp.  Lets add the missing pieces.

###OLD stuf
nbmod1<-glm.nb(eggs ~ infected+weeknum+avhum+ (1|idnum), data=Compile) #AIC 11867
nbmod2<-glm.nb(eggs ~ infected+weeknum+avtemp+ (1|idnum), data=Compile) #AIC 11858
nbmod3<-glm.nb(eggs ~ infected+weeknum+avhum+avtemp + (1|idnum), data=Compile) #AIC 11852
nbmod3a<-glm.nb(eggs ~ infected+weeknum+avhum*avtemp + (1|idnum) , data=Compile) #AIC 11852
nbmod4<-glm.nb(eggs ~ infected+weeknum+avhum+avtemphigh+(1|idnum), data=Compile)#11855
nbmod5<-glm.nb(eggs ~ infected+weeknum+avhum+mouse+ (1|idnum), data=Compile) #11824
nbmod6<-glm.nb(eggs ~ infected+weeknum+avhum+mouse+infected*mouse+ (1|idnum), data=Compile) #11824
#6 was the same as 5 because there interactions are themselves linearly related adding no new information.
nbmod7<-glm.nb(eggs ~ infected+weeknum+avhum+mouse+alive+ (1|idnum), data=Compile)#11824
nbmod8<-glm.nb(eggs ~ infected+weeknum+avhum+mouse+avtemp+(1|idnum), data=Compile) #11789
#now letsa add the difference.
nbmod9<-glm.nb(eggs ~ infected+weeknum+avhum+mouse+avtemp+humdiff+(1|idnum), data=Compile) #11791
nbmod10<-glm.nb(eggs ~ infected+weeknum+avhum+mouse+avtemp+tempdiff+(1|idnum), data=Compile) #11791
#adding temp/hum diff does not add significantly to the model
nbmod11<-glm.nb(eggs ~ infected+weeknum+avhum+mouse+avtemp+hummax+(1|idnum), data=Compile) #11791
nbmod12<-glm.nb(eggs ~ infected+weeknum+avhum+mouse+avtemp+tempmax+(1|idnum), data=Compile) #11791
#as seen in graphs above an infected insect may be affected differently by temp/hum affects.
#thus lets add an interaction term.
nbmod13<-glm.nb(eggs ~ infected+weeknum+avhum+mouse+avtemp+tempmax+infected*avhum+(1|idnum), data=Compile)#11741
nbmod14<-glm.nb(eggs ~ infected+weeknum+avhum+mouse+avtemp+tempmax+infected*avtemp+(1|idnum), data=Compile)#11777
nbmod15<-glm.nb(eggs ~ infected+weeknum+avhum+mouse+avtemp+tempmax+infected*avhum+infected*avtemp+(1|idnum), data=Compile)#11750
#hence an interaction with humidity plays a bigger role than temperature or both interactions.
nbmod16<-glm.nb(eggs ~ infected+weeknum+avhum+avtemp+infected*avhum+(1|idnum), data=Compile)
#lets look at humidity variations keeping all else constant.  ()
#lets see if they are some better predictor for hum other than avhum
nbmod13<-glm.nb(eggs ~ infected+weeknum+mouse+hummax+avtemp+(1|idnum), data=Compile)#11794

#lets look at the plot for the negative binomial version of the simplest model.
plot(CompileRD$week, CompileRD$eggs)
lines(CompileRD$week, nbmod0$fitted)
mean(nbmod0$fitted)
mean(CompileRD$eggs)


#Plotting Residuals and predicted values.
par(mfrow=c(2,4))
#pdf(file="Poisson vs Negative Binomial Model of eggs by infected+weeknum+ (1 idnum).pdf")
plot(nbmod0, ylim=4)
title(main="Negative Binomial")
plot(mod2b)
title(main="Poisson")
#dev.off()
#http://www.ats.ucla.edu/stat/r/dae/zinbreg.htm Describes a 0 inflated negative binomial
zinbmod1 <- zeroinfl(eggs ~ weeknum + infected | persons,
data = zinb, dist = "negbin", EM = TRUE)
summary(m1)


#alive is not helpful because NA's are already removed.
#lets plot to get a sense of where we're at
par(mfrow=c(1,1))
plot(nbmod13)
summary()

testplot<-ggplot(aes( y= eggs, x= weeknum, na.rm=TRUE), 
               data=Compile)+geom_point(data=Compile)
testplot<-testplot+ggtitle("Number of Eggs Laid by Week and Infection Status") 
testplot<-testplot+facet_grid(. ~infected)+geom_smooth(method= "lm")
testplot

plot(residuals(nbmod13))


#####MODEL FOR HATCH

#exp((AICmin-AICi)/2)=Probablity that i minimalizes information loss just as well as min.
#ln(prob)=AICmin-AICi/2
#2e^prob=difference between AICs.  
#if sign prob is 0.05 then
2*(log(0.05))
# thus a difference in AIC of 6 o greater is a significant similar probablity for now.
test1<-exp((11824-11852)/2)

plot(Compile$weeknum, Compile$eggs)


lrtest(nbmod1, nbmod2)
lrtest(nbmod2,nbmod3)


#attemp ggem
geem1<-geem(eggs ~ infected+weeknum, id=idnum, data=Compile)
geem2<-geem(eggs ~ infected+weeknum, id=idnum, data=Compile, family=negative.binomial(0.7279))
geem2<-geem(eggs ~ infected+weeknum, id=idnum, data=Compile, family=poisson)
geem3<-geem(eggs ~ infected+weeknum, id=idnum, data=Compile, corstr="ar1")
geem4<-geem(eggs ~ infected+weeknum, id=idnum, data=Compile, family=negative.binomial(0.7279),corstr="ar1")
geem5<-geem(eggs ~ infected+weeknum, id=idnum, data=Compile, family=negative.binomial(0.7279),corstr="ar2")
geem6<-geem(eggs ~ infected+weeknum, id=idnum, data=Compile, family=negative.binomial(0.7279),corstr="ar3")
#starting with most complex model
CompileRD$mouseidnum<-as.factor(CompileRD$mouseidnum)
geem7<-geem(eggs ~infected+weeknum+trial+mouseidnum+avhumhigh+avtemphigh, id=idnum, data=CompileRD, family=negative.binomial(0.7279), corstr="exchangeable")
geem8<-geem(eggs ~infected+weeknum+trial+avhumhigh+avtemphigh, id=idnum, data=CompileRD, family=negative.binomial(0.7279), corstr="exchangeable")
geem9<-geem(eggs ~infected+weeknum+avhumhigh+avtemphigh, id=idnum, data=CompileRD, family=negative.binomial(0.7279), corstr="exchangeable")
geem10<-geem(eggs ~infected+weeknum+avtemphigh, id=idnum, data=CompileRD, family=negative.binomial(0.7279), corstr="exchangeable")
geem11<-geem(eggs ~infected+weeknum, id=idnum, data=CompileRD, family=negative.binomial(0.7279), corstr="exchangeable")
geem12<-geem(eggs ~infected+weeknum+avtemphigh+avtemphigh*infected, id=idnum, data=CompileRD, family=negative.binomial(0.7279), corstr="exchangeable")






geemp10<-geem(eggs ~infected+weeknum+avtemphigh, id=idnum, data=CompileRD, negative.binomial(theta=2, link="log"), corstr="exchangeable")
geemp12<-geem(eggs ~infected+weeknum+avtemphigh+avtemphigh*infected, id=idnum, data=CompileRD, family=poisson, corstr="exchangeable")


geemnb<-geem(eggs ~ infected+weeknum, id=idnum, data=Compile, family=negative.binomial(1))
geemnb1<-geem(eggs ~ infected+weeknum, id=idnum, data=Compile, family=negative.binomial(1))

geemp<-geem(eggs ~ infected+weeknum, id=idnum, data=Compile, family=poisson)

# QIC in R custom http://www.r-bloggers.com/r-script-to-calculate-qic-for-generalized-estimating-equation-gee-model-selection/
# Zero Inflated Negative Binomial



#maybe try "nlme" package

#--------------
#--------------------------------------------------------------------------
# Code to extract coeff., 95CIs, and define final estimates with p-values
#--------------------------------------------------------------------------

# # use following commands for confident intervals and coefficients:
# confint()
# exp(coef())
# exp(confint())

# Disabling scientific notation
## Enable at the end with options(scipen = 0)!!! 
# 
# options(scipen = 999)
# library(lme4)
# 
# # Defining function to get the final estimates:
# # IRR, 95%CI, and p-value
# 
# estimates<-function(x, print = TRUE){
#   ul<-coef(summary(x))[,1] + qnorm(0.975)*coef(summary(x))[,2]
#   ll<-coef(summary(x))[,1] + qnorm(0.025)*coef(summary(x))[,2]
#   logIRR.x<-cbind(coef(summary(x))[,1], ll, ul)
#   result<-IRR.x<-cbind(exp(logIRR.x), coef(summary(x))[,4])
#   return(result)
# }  

