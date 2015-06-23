#libraries needed
library(plyr)  #para rbind.fill function
library(lubridate) #para extracting dates 
library(reshape) #para make the wide data into long data

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

####The Goal of this analysis is to see how bed bugs fertility may change 
####based on infection with chagas disease.
####To do so we must explore some confounds such as age of insect.
#Create data frame for eggs hatched to simplify loops and math later
# postura <-data.frame(cimfert$ID, cimfert$Procedencia, cimfert$trial, cimfert$s1_p,
#   cimfert$s2_p, cimfert$s3_p, cimfert$s4_p, cimfert$s5_p,
#   cimfert$s6_p, cimfert$s7_p, cimfert$s8_p, cimfert$s9_p,
#   cimfert$s10_p,cimfert$s11_p, cimfert$s12_p,cimfert$s13_p,
#   cimfert$s14_p, cimfert$s15_p, cimfert$s16_p,
#   cimfert$s17_p, cimfert$s18_p,cimfert$s19_p,
#   cimfert$s20_p, cimfert$s21_p, cimfert$s22_p,cimfert$s23_p, cimfert$s24_p,
#   cimfert$s25_p, cimfert$s26_p, cimfert$s27_p,
#   cimfert$s28_p,cimfert$s29_p, cimfert$s30_p)
# 
# #Create data frame for eggs hatched
# salieron <-data.frame(cimfert$ID, cimfert$Procedencia, 
#   cimfert$trial, cimfert$s1_v,
#   cimfert$s2_v, cimfert$s3_v, cimfert$s4_v, cimfert$s5_v,
#   cimfert$s6_v, cimfert$s7_v, cimfert$s8_v, cimfert$s9_v, 
#   cimfert$s10_v,cimfert$s11_v,cimfert$s12_v,cimfert$s13_v,
#   cimfert$s14_v, cimfert$s15_v, cimfert$s16_v,
#   cimfert$s17_v, cimfert$s18_v,cimfert$s19_v,
#   cimfert$s20_v, cimfert$s21_v, cimfert$s22_v,cimfert$s23_v, cimfert$s24_v,
#   cimfert$s25_v, cimfert$s26_v, cimfert$s27_v,
#   cimfert$s28_v,cimfert$s29_v, cimfert$s30_v)

###Create chart for eggs laid by treatment group.
##everthing together.
#make a vector that identifies(indexes) each treatment group on cimfert
ControlP <- which(cimfert$Procedencia=="CO")
InfectPA <- which(cimfert$Procedencia=="I-R1")
InfectPB <- which(cimfert$Procedencia=="I-R2")
ControlA <- which(cimfert$Procedencia=="CO-A")
ControlB <- which(cimfert$Procedencia=="CO-B")
InfectA <- which(cimfert$Procedencia=="I-A")
InfectB <- which(cimfert$Procedencia=="I-B")
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
 
##using a loop find the mean of eggs laid by each week.
#make blank input vectors
# AvPostC <- c(1:30)*NA
# AvPostI<- c(1:30)*NA
#   
# #controls
# 
# for(i in 1:30){
#     AvPostC[i] <- sum(postura[controls, (i+3)], na.rm = TRUE)/length(controls)
#   }
#   
# #infect
# for(i in 1:30){
#   AvPostI[i] <- sum(postura[infect, (i+3)], na.rm = TRUE)/length(infect)
# }


##instead of a loop, lets try to use apply or the colMeans function
#the issue now is defining our array or input given that later data
#may have different dimensions.

#solution: use "p" and "v" from substring of names to identify which are postura y viability columns
#be sure to check that no other columns of new data contain "s" first.
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

##We can now plot this data
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
plot(iPLeggmean, type="o", main="Average Eggs Laid Between Infected and Control Insects in Pilot",
     ylab="Number of eggs", xlab="Week in Study", col=2, pch=18, lty=1, ylim=c(0,10))
lines(cPLeggmean, type="o", pch=1, col=4, lty=1)
lines(iRAeggmean, type="o", pch=2, col=2, lty=2)
lines(cRAeggmean, type="o", pch=2, col=4, lty=2)
lines(iRBeggmean, type="o", pch=3, col=2, lty=3)
lines(cRBeggmean, type="o", pch=3, col=4, lty=3)
legend("topright", c("Pilot Infected","Pilot Controls", "Rep1 Infected", 
                    "Rep1 Controls", "Rep2 Infected", "Rep2 Controls"),
       col=c(2,4,2,4,2,4), pch=c(1,1,2,2,3,3), lty=c(1,1,2,2,3,3)

##for violin plots and lme4 analysis we need to make data frame by entry
#entry meaning every line represents week of egg)) and hatch from normal jar.

#We need to create outputs for the data.
blank <- (1:(30*length(cimfert$Procedencia))*0)
#
Compile <- data.frame(blank,0,0,0,0,0,0, 0, 0, 0, 0)


Compile <- rename(Compile, replace = c("blank"="id", "X0"="parents","X0.1"="infected","X0.2"="start",
       "X0.3"="week", "X0.4"="date", "X0.5"="eggs", "X0.6"="hatch", "X0.7"="rh",
       "X0.8"="temp", "X0.9"="procedencia"))
#make sure the cimfert columns are characters or they will not transfer.
cimfert$Nro_.pareja <- as.character(cimfert$Nro_.pareja)
cimfert$Procedencia <- as.character(cimfert$Procedencia)
cimfert$Fecha_Inicio_.Pareja <-as.character(cimfert$Fecha_Inicio_.Pareja)

#now we need to create a nested loop to get the data into a new data frame
for (d in 1:30) { 
  for (i in 1:(length(cimfert$s1_p))){ #i for each insect
     Compile$week[i+((d-1)*length(cimfert$ID))] <- d
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



#Now lets make somoe pretty pictures.
infected<-which(Compile$infected==1)
controled<-which(Compile$infected==0)
uniquebugs<-unique(Compile$id)
plot(Compile$week[infected], Compile$eggs[infected], col="red")
     

# #loop to get average temperature throughout the week.
# #its broken for now bc we don't have the data past 2015-01-31
#  for(i in 1:length(Compile$id)){
#    fechai<- which(tempRH$FECHA==Compile$date[i])
#    fechaSi<-which(tempRH$FECHA==(Compile$date[i]-6))
#    Compile$temp[i] <- ((sum(tempRH$TEMP.MAX..Â.C.[fechaSi:fechai], na.rm=TRUE) +
#      sum(tempRH$TEMP.MAX..Â.C.[fechaSi:fechai], na.rm=TRUE))) / 
#      (2*length(which(is.na(tempRH$TEMP.MAX..Â.C[fechaSi:fechai])==FALSE)))
#  }
#   

# #-----------------------------------------------------
# ###Figure out let us start by finding the base stats
# ###Longevity
# #identify groups
# controls <- which(cimfertpilot$Procedencia=="CO")
# infectA <- which(cimfertpilot$Procedencia=="I-R1")
# infectB <- which(cimfertpilot$Procedencia=="I-R2")
# infect <-c:(infectA, infectB)
# 
# c
# 
# 
# #find number in each cohort
# nmcontrols <- length(controls)
# nminfectA <- length(infectA)
# nminfectB <- length(infectB)
# nminfect <- length(infect)
# 
# #find the average 
# longcont <- sum(cimfertpilot$Longevidad[controls])/nmcontrols #16.78571
# longinfA <- sum(cimfertpilot$Longevidad[infectA])/nminfectA  #18.5
# longinfB <- sum(cimfertpilot$Longevidad[infectB])/nminfectB  #15.90909
# longinf <-sum(cimfertpilot$Longevidad[infect])/nminfect  #17.51724
# ------------------------------------------------------------------------------------
# ##For each time line
# glm(cases~rhs(data$year,2003)+lhs(data$year,2003)+ offset(log(population)), data=data, 
#subset=28:36, family=poisson())


