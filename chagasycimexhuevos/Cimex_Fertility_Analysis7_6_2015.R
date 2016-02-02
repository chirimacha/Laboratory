#packages needed/experimented with
#install.packages(c("lubridate","reshape2","vioplot", "matrixStats","ggplot",
#     "plyr", "geeM", "MASS", "lmtest", "survval", "doBy","stats"))

library(lubridate) #para extracting dates 
library(reshape2) #para make the wide data into long data
library(matrixStats)
library(ggplot2)
library(plyr)  #para rbind.fill function
library(geeM)
library(MASS)
library(lmtest)
#library(pscl)
library(survival)
library(doBy)
library(Rmisc) #summarySE command

#set directory and bring in files to be analyzed.
setwd("c:\\Users\\tradylan\\Documents\\Laboratory\\chagasycimexhuevos")
#setwd("/Users/mzlevy/Laboratory/chagasycimexhuevos")

#bring in hatching data
cimfertpilot <- read.csv("Cimex_FertP_update12_3.csv")
#Original Found https://docs.google.com/spreadsheets/d/1E-GRO1_Ybrgqj0wjgz5s9YHY1KwJUVPov0I2PwgC2CQ/edit

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
#group for trial

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

#This is assumes that if a number is entered the insect was alive during that week.\
Compile$eggs<-as.numeric(Compile$eggs)
dead<-which(is.na(Compile$eggs)==TRUE)
alive<-which(is.na(Compile$eggs)==FALSE)
Compile$alive[dead]<-0
Compile$alive[alive]<-1

#Now that table is made, make date so that humidity and temperature data can be easily entered.
Compile$start <- parse_date_time(Compile$start, "dmy", tz="EST")
Compile$start <- as.Date(Compile$start)
Compile$date <- (Compile$start+(Compile$week*7))


###add temperature and humidity values.
#make the tempRH also date format
tempRH$FECHA <- parse_date_time(tempRH$FECHA, "dmy", tz="EST")
tempRH$FECHA <- as.Date(tempRH$FECHA)

#identify which insects are in which repetition
pilot <- which(Compile$trial==0)
repone <- which(Compile$trial==1)
reptwo <- which(Compile$trial==2)

######################################################################
#=====================================================================
#Make Graphics Using Compile
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ##plot eggs and hatch by week.
# #create model to add regression line to line graphs
# eggweek<-lm(Compile$eggs ~Compile$week)
# hatchweek<-lm(Compile$hatch ~Compile$week)
# #line graph for eggs laid by week
# plot(Compile$week, Compile$eggs+rnorm(length(Compile$eggs), 0, 0.5), main="Eggs by Week")
# abline(eggweek)
# #box plot for eggs laid
# boxplot(Compile$eggs ~Compile$week, main="Eggs by Week")
# #Line graph for eggs laid by week
# plot(Compile$week, Compile$hatch+rnorm(length(Compile$hatch), 0, 0.5), main="Hatching by Week")
# abline(hatchweek)
# #box plot for eggs hatched
# boxplot(Compile$hatch ~Compile$week, main="Hatching by Week")
# 
# par(mfrow=c(1,1))
# #pdf("graphs/NmbEggsByDateBox.pdf")
# boxplot(Compile$eggs ~Compile$date, main="Eggs by Date")
# #dev.off()
# 
# #pdf("graphs/NmbHtchByDateBox.pdf")
# boxplot(Compile$hatch ~Compile$date, main="Hatching by Date")
# #dev.off()
# 
# ##lets makle a combined plot of the two groups together
# enona<-which(is.na(Compile$eggs)==FALSE)
# hnona<-which(is.na(Compile$hatch)==FALSE)
# Compile$infected<-as.factor(Compile$infected)
# Compile$week<-as.factor(Compile$week)
# 
# #the box plot for eggs laid
# par(mfrow=c(1,1))
# #pdf("graphs/NumEggsLaidByWeekYInfyCntlBox.pdf")
# g<-ggplot(aes( y= eggs, x= week, fill = infected, na.rm=TRUE),
#        data= Compile[enona,]) +geom_boxplot(data=Compile[enona,])
# g<-g+ggtitle("Distribution of Number of Eggs Laid by Infection Status Each Week")+
#   xlab("Week") +
#   ylab("Number of Eggs Laid")
# g<-g+scale_fill_manual(values=c("#00CCFF", "#990000"),name="Infection Status", labels=c("Controls", "Infected"))
# #g<-g+ scale_color_discrete(name="Infection Status", labels=c("Controls", "Infected"))
# #g<-g+scale_x_continuous(breaks=seq(0, 38, 2))
# #g<-g+scale_x_discrete(labels=c("", seq())
# g
# 
# 
# #ggsave(g, file="BoxPlotDistNumEggLaidbyInfectionStatusbyWeek.jpeg", units= "cm",
# #   width = 26.4, height= 15.875)
# #dev.off()
# 
# 
# #create graph that show average 
# weekinfsum<-summarySE(Compile, measurevar="eggs", groupvars=c("infected", "week"), na.rm=TRUE)
# 
# #Change working directory for easy access to draft writers
# setwd("c:\\Users\\tradylan\\Dropbox\\cruzi_on_lifetables\\paper_draft\\Figuras\\figura_5_AverageEggsLaid")
# #setwd("c:\\Users\\tradylan\\Documents\\Laboratory\\chagasycimexhuevos\\graphs")
# 
# #save as csv so figure can easily be recreated on its own.
# #write.csv(weekinfsum,"DataforFig5.csv")
# 
# ##to save as pdf
# #pdf("graphs/AvNumEggsLaidByWeekYInfyLineCI.pdf")
# pdf("AvNumEggsLaidByWeekYInfyLineCI.pdf")
# 
# #Code to create figure in ggplot
# g2 <- ggplot(weekinfsum, aes(x=week, y=eggs, color=infected)) +
#   geom_errorbar(aes(ymin=eggs-ci, ymax=eggs+ci, group=infected, color=infected),
#                 width=.1)+
#   geom_line(aes(group=infected)) +
#   ggtitle("Average Number of Eggs Laid from Living Insects By Week")+
#   xlab("Week") +
#   ylab("Average Number of Eggs Laid")+
#   scale_color_discrete(name="Infection Status", labels=c("Controls", "Infected"),h= c(250, 3))+
#   geom_point()+
#   theme(legend.position=c(0.9, 0.9))
# g2
# 
# dev.off()
# ##save as various file types
# #JPEG
# ggsave(g2, file="AvNumEggsLaidByWeekYInfyLineCI.jpeg", units= "cm",
#   width = 26.4, height= 15.875)
# 
# #BMP
# ggsave(g2, file="AvNumEggsLaidByWeekYInfyLineCI.bmp", units= "cm",
#  width = 26.4, height= 15.875)
# 
# #GIF
# #ggsave(g2, file="AvNumEggsLaidByWeekYInfyLineCI.gif", units= "cm",
# # width = 26.4, height= 15.875)
# 
# #TIFF
# ggsave(g2, file="AvNumEggsLaidByWeekYInfyLineCI.tiff", units= "cm",
#  width = 26.4, height= 15.875)
# 
# #EPS
# ggsave(g2, file="AvNumEggsLaidByWeekYInfyLineCI.eps", units= "cm",
#  width = 26.4, height= 15.875)
# 
# #SVG
# ggsave(g2, file="AvNumEggsLaidByWeekYInfyLineCI.svg", units= "cm",
#  width = 26.4, height= 15.875)
# 
# #png
# ggsave(g2, file="AvNumEggsLaidByWeekYInfyLineCI.png", units= "cm",
#  width = 26.4, height= 15.875)
# 
# 
# 
# #hatch plot
# #pdf("graphs/NumEggsHtchByWeekyInfyCntrlBox.pdf")
# h<-ggplot(aes( y= hatch, x= week, fill = infected, na.rm=TRUE), 
#        data= Compile[hnona,])+geom_boxplot(data=Compile[hnona,])
# h<-h+ggtitle("Distribution of Number of Eggs Hatched by Infection Status in Bugs that Laid Eggs")
# h<-h+scale_fill_manual(values=c("#00CCFF", "#990000"))
# h
# 
# #ggsave(h, file="BoxPlotDistNumEggHtvhbyInfectionStatusbyWeek.jpeg", units= "cm",
#        #width = 26.4, height= 15.875)
# #dev.off()
# 
# #h2: Hatchrate Plot
# #make a hatch rate 
# Compile$hatchrate<-Compile$hatch/Compile$eggs
# #Make a summary table
# hatchratesum<-summarySE(Compile, measurevar="hatchrate", groupvars=c("infected", "week"), na.rm=TRUE)
# #replace the Inf'swith NA's
# hatchratesum$hatchrate[which(is.infinite(hatchratesum$hatchrate)==TRUE)]<-NA
# #write.csv(hatchratesum,"DataforFig6.csv")
# 
# #Plot the average and 95% Confidence Interval
# #change working directory
# setwd("c:\\Users\\tradylan\\Dropbox\\cruzi_on_lifetables\\paper_draft\\Figuras\\figura_6_AverageHatchRate")
# 
# pdf("AvHatchrateByWeekYInfyLineCI.pdf")
# h2 <- ggplot(hatchratesum, aes(x=week, y=hatchrate, color=infected)) +
#   geom_errorbar(aes(ymin=hatchrate-ci, ymax=hatchrate+ci, group=infected, color=infected),
#                 width=.1)+
#   geom_line(aes(group=infected)) +
#   ggtitle("Average Percentage of Eggs that Hatched by Week")+
#   xlab("Week") +
#   ylab("Average Percentage of Eggs that Hatched")+
#   scale_color_discrete(name="Infection Status", labels=c("Controls", "Infected"), h= c(250, 3))+
#   geom_point()+
#   theme(legend.position=c(0.9, 0.9))+
#   scale_y_continuous(breaks=seq(-3, 3, 0.5))
# h2
# dev.off()
# ##save as other various file types.
# #JPEG
# ggsave(h2, file="AvHatchrateByWeekYInfyLineCI.jpeg", units= "cm",
#   width = 26.4, height= 15.875)
# 
# #BMP
# ggsave(h2, file="AvHatchrateByWeekYInfyLineCI.bmp", units= "cm",
#  width = 26.4, height= 15.875)
# 
# #TIFF
# ggsave(h2, file="AvHatchrateByWeekYInfyLineCI.tiff", units= "cm",
#  width = 26.4, height= 15.875)
# 
# #EPS
# ggsave(h2, file="AvHatchrateByWeekYInfyLineCI.eps", units= "cm",
#  width = 26.4, height= 15.875)
# 
# #SVG
# ggsave(h2, file="AvHatchrateByWeekYInfyLineCI.svg", units= "cm",
#  width = 26.4, height= 15.875)
# 
# #png
# ggsave(h2, file="AvHatchrateByWeekYInfyLineCI.png", units= "cm",
#  width = 26.4, height= 15.875)
# 
# 
# #h3: Hatchrate Plot by trial
# #Make a summary table
# hatchratetrial<-summarySE(Compile, measurevar="hatchrate", groupvars=c("infected", "week", "trial"), na.rm=TRUE)
# #replace the inf with NA
# hatchratetrial$hatchrate[which(is.infinite(hatchratetrial$hatchrate)==TRUE)]<-NA
# par(mfrow=c(3,1))
# pdf("graphs/AvHatchrateByWeekYInfyTrialLineCIReps.pdf")
# 
# #rep Pilot
# h3a <- ggplot(hatchratetrial[which(hatchratetrial$trial==0),], aes(x=week, y=hatchrate, color=infected)) +
#   geom_errorbar(aes(ymin=hatchrate-ci, ymax=hatchrate+ci, group=infected, color=infected),
#                 width=.1)+
#   geom_line(aes(group=c(infected))) +
#   ggtitle("Average Hatch Rate Each Week By Insects that Laid in Pilot")+
#   xlab("Week") +
#   ylab("Average Percentage of Eggs that Hatched")+
#   scale_color_discrete(name="Infection Status", labels=c("Controls", "Infected"), h= c(250, 3))+
#   geom_point()
# h3a
# #Rep 1
# h3b <- ggplot(hatchratetrial[which(hatchratetrial$trial==1),], aes(x=week, y=hatchrate, color=infected)) +
#   geom_errorbar(aes(ymin=hatchrate-ci, ymax=hatchrate+ci, group=infected, color=infected),
#                 width=.1)+
#   geom_line(aes(group=c(infected))) +
#   ggtitle("Average Hatch Rate Each Week By Insects that Laid in Rep 1")+
#   xlab("Week") +
#   ylab("Average Percentage of Eggs that Hatched")+
#   scale_color_discrete(name="Infection Status", labels=c("Controls", "Infected"), h= c(250, 3))+
#   geom_point()
# h3b
# #Rep 2
# h3c <- ggplot(hatchratetrial[which(hatchratetrial$trial==2),], aes(x=week, y=hatchrate, color=infected)) +
#   geom_errorbar(aes(ymin=hatchrate-ci, ymax=hatchrate+ci, group=infected, color=infected),
#                 width=.1)+
#   geom_line(aes(group=c(infected))) +
#   ggtitle("Average Hatch Rate Each Week By Insects that Laid in Rep 2")+
#   xlab("Week") +
#   ylab("Average Percentage of Eggs that Hatched")+
#   scale_color_discrete(name="Infection Status", labels=c("Controls", "Infected"), h= c(250, 3))+
#   geom_point()
# h3c
# 
# 
# dev.off()
# 
# par(mfrow=c(1,1))


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

# ##create total values for each insect
# #create rows to fill by loop
Compile$lifespan<-Compile$eggs*NA
Compile$egg_total<-Compile$eggs*NA
Compile$hatch_total<-Compile$eggs*NA
Compile$avmaxtemp<-Compile$eggs*NA
Compile$avmintemp<-Compile$eggs*NA
Compile$avmaxhum<-Compile$eggs*NA
Compile$avminhum<-Compile$eggs*NA
Compile$havmaxtemp<-Compile$eggs*NA
Compile$havmintemp<-Compile$eggs*NA
Compile$havmaxhum<-Compile$eggs*NA
Compile$havminhum<-Compile$eggs*NA

#make temperature and humidity values numeric
tempRH$TEMP.MAX..Â.C.<-as.numeric(tempRH$TEMP.MAX..Â.C.)
tempRH$TEMP.MIN..Â.C.<-as.numeric(tempRH$TEMP.MIN..Â.C.)
tempRH$HR.MAX....<-as.numeric(tempRH$HR.MAX....)
tempRH$HR.MIN....<-as.numeric(tempRH$HR.MIN....)


# #identify living observations so temperatures while the
# #insect is dead does not have affect
lives <- which(Compile$alive==1) 
#identify observations that have eggs laid
eggslaid <- which(Compile$eggs > 0)

# #loop over each insect and calculate lifespan and temperature variables
for (i in 1:max(Compile$idnum)) {
  ids<-which(Compile$idnum==i)
  #Sum eggs laid and hatched across life of bug
  Compile$egg_total[ids]<-sum(Compile$eggs[ids], na.rm=TRUE)
  Compile$hatch_total[ids]<-sum(Compile$hatch[ids], na.rm=TRUE)
  is <- intersect(ids, lives)#that way you don't add dead times to observations
  #the number of observations while alive == lifespan
  ls<-length(is)
  #identify the first and last observations in [is]
  fst<-is[1]
  lst<-is[ls]
  #Put the calculated items into the appropriate vectors of data frame
  Compile$lifespan[ids]<-ls
  #identify first and last date for each insect.
  begin<-(Compile$start[fst])
  end<-Compile$date[lst]
  a<-which(tempRH$FECHA==begin)
  b<-which(tempRH$FECHA==end)
  #Use dates to find values from temperature and humidity data
  #average over the life of the bug.
  avmaxtemp1<-mean(tempRH$TEMP.MAX..Â.C.[a:b], na.rm=TRUE)
  avmintemp1<-mean(tempRH$TEMP.MIN..Â.C.[a:b], na.rm=TRUE)
  avmaxhum1<-mean(tempRH$HR.MAX....[a:b], na.rm=TRUE)
  avminhum1<-mean(tempRH$HR.MIN....[a:b], na.rm=TRUE)
  #Now add these to Compile
  Compile$avmaxtemp[ids]<-avmaxtemp1
  Compile$avmintemp[ids]<-avmintemp1
  Compile$avmaxhum[ids]<-avmaxhum1
  Compile$avminhum[ids]<-avminhum1
}

#now make a similar calculation for humidity, only using weeks where eggs were laid
for (i in 1:max(Compile$idnum)) {
  ids<-which(Compile$idnum==i)
  leggs <- intersect(ids, eggslaid)
  if(length(leggs) > 0){
  #create a list of dates
    FECHA<-NA
    for (j in 1:length(leggs)){
      endings <- Compile$date[leggs[j]]
      beginings <- (Compile$date[leggs[j]])-6
      moredates<-(beginings:endings)
      FECHA<-c(FECHA, moredates)
    }
    num<-1:length(FECHA)
    datetable<-data.frame(num, FECHA)
  
    #merge temperature data onto datetable
    temptable<-merge(datetable, tempRH, by="FECHA", all.x=TRUE)
    #take the average of each of these columns
    havmaxtemp1 <- mean(temptable$TEMP.MAX..Â.C., na.rm=TRUE)
    havmintemp1 <- mean(temptable$TEMP.MIN..Â.C., na.rm=TRUE)
    havmaxhum1 <- mean(temptable$HR.MAX...., na.rm=TRUE)
    havminhum1 <- mean(temptable$HR.MIN...., na.rm=TRUE)
    #Now add these to Compile
    Compile$havmaxtemp[ids] <- havmaxtemp1
    Compile$havmintemp[ids] <- havmintemp1
    Compile$havmaxhum[ids] <- havmaxhum1
    Compile$havminhum[ids] <- havminhum1 
  }
}

#finally lets reduce the table down to only the lifespan data
weekone<-which(Compile$week==1)
FFdata<-Compile[weekone,]

#remove duplicate or week specific columns
FFdata$id<- NULL
FFdata$sdweek<- NULL
FFdata$procedencia<- NULL
FFdata$mouse<- NULL
FFdata$alive<- NULL
FFdata$date<- NULL
FFdata$week<- NULL
FFdata$start<- NULL
FFdata$id<- NULL
FFdata$parents<- NULL
FFdata$eggs<- NULL
FFdata$hatch<- NULL
#save as a csv
#write.csv("FertilityFecundityFinalData.csv")

###############################################################################
#==============================================================================
#ahora tenomos muchas graficas, podemos empezar haciendo el modelo
#########################################################
#getting model data
#[1]coefficient
#[2]residuals
#[3]fitted values
#[4]effects?
#[5]R
#[8]Link Function and NB value
#[10] deviance
#[11]AIC
#[14]Weights
#Model with infection and infection as covariates.
lifespanmod<-glm.nb(egg_total~infected+lifespan, data=FFdata)
  summary(lifespanmod)
  #AIC==
#Null Model without any temperature or humidity variables.
nullmodel<- glm.nb(egg_total~infected, data=FFdata, offset(lifespan))
 summary(nullmodel)  #AIC==26710   P=<2e-16 ***
 #AIC==
###Now just do a univariate analysis
maxtempmodel<- glm.nb(egg_total~avmaxtemp, data=FFdata, offset(lifespan))
mintempmodel<- glm.nb(egg_total~avmintemp, data=FFdata, offset(lifespan))
maxhummodel<- glm.nb(egg_total~avmaxhum, data=FFdata, offset(lifespan))
minhummodel<- glm.nb(egg_total~avminhum, data=FFdata, offset(lifespan))

#look at summary to check AIC and P value
summary(maxtempmodel)  #AIC==26605 
summary(mintempmodel) #AIC==26781  (null model is a better model) 
summary(maxhummodel) #AIC==26744  (null model is a better model)
summary(minhummodel) #AIC==26812   

#Model with Average Minimum Temperature and max humidity model
selectedmodel<-glm.nb(egg_total~infected+avmaxtemp+avminhum, data=FFdata, offset(lifespan))
summary(selectedmodel)  #AIC==26366   P(infection)==< 2e-16 *** (worse than full model)


#full models
fullmodel<-glm.nb(egg_total~infected+avmaxtemp+avminhum+avmaxhum+avmintemp, data=FFdata, offset(lifespan))
summary(fullmodel) #AIC==26040 (worse than full model)

#Alternative models
AltModelA<-glm.nb(egg_total~infected+avmintemp+avminhum, data=FFdata, offset(lifespan))
  summary(AltModelA) #AIC==26440
AltModelB<-glm.nb(egg_total~infected+avmaxtemp+avmaxhum, data=FFdata, offset(lifespan))
  summary(AltModelB) #AIC==26355
AltModelC<-glm.nb(egg_total~infected+avmintemp+avmaxhum, data=FFdata, offset(lifespan))
  summary(AltModelC) #AIC==26417

###############################################################################
  #Repeat for hatching models
#==============================================================================
#Model with lifespan, egg_total, and infection as covariates.
  hlifespanmod<-glm(hatch_total~infected+lifespan+egg_total, data=FFdata, family="poisson")
  summary(hlifespanmod)
  #AIC==
#Null Model without any temperature or humidity variables.
  nullhmodel<- glm(hatch_total~infected, data=FFdata, offset(egg_total),family="poisson")
  summary(nullhmodel)  #AIC==182496   
  
  #add lifespan to model to see if it is helpful
  nullhmodelB<- glm(hatch_total~infected, data=FFdata, offset(egg_total),family="poisson")
  summary(nullhmodel)
  #AIC==340734 same thus lifespan is not significantly beneficial
  #thus 
  ###Now just do a univariate analysis
  hmaxtempmodel<- glm(hatch_total~havmaxtemp, data=FFdata, offset(egg_total),family="poisson")
  hmintempmodel<- glm(hatch_total~havmintemp, data=FFdata, offset(egg_total),family="poisson")
  hmaxhummodel<- glm(hatch_total~havmaxhum, data=FFdata, offset(egg_total),family="poisson")
  hminhummodel<- glm(hatch_total~havminhum, data=FFdata, offset(egg_total),family="poisson")
  
  #look at summary to check AIC and P value
  summary(hmaxtempmodel)  #AIC==336158
  summary(hmintempmodel) #AIC==373140
  summary(hmaxhummodel) #AIC==307829
  summary(hminhummodel) #AIC==  304791
#Complete Model (all variants)
  fullmodel<- glm(hatch_total~infected+lifespan+havminhum+havmaxhum+havmintemp+havmaxtemp,data=FFdata, offset(egg_total),family="poisson")
    summary(fullmodel) #AIC==167222
    
#Model with Average Minimum Temperature and max humidity model
  hselectedmodel<-glm.nb(hatch_total~infected+lifespan+havmaxtemp+havminhum, data=FFdata, offset(egg_total))
  summary(hselectedmodel)  #AIC==93152   P(infection)==< 2e-16 ***
  
  #Alternative models
  AltModelA<-glm.nb(hatch_total~infected+lifespan+havmintemp+havminhum, data=FFdata, offset(egg_total))
  summary(AltModelA) #AIC==93113
  AltModelB<-glm.nb(hatch_total~infected+lifespan+havmaxtemp+havmaxhum, data=FFdata, offset(egg_total))
  summary(AltModelB) #AIC==93261
  AltModelC<-glm.nb(hatch_total~infected+lifespan+havmintemp+havmaxhum, data=FFdata, offset(egg_total))
  summary(AltModelC) #AIC== 93221

###############################################################################
#Principal Component Analysis
#------------------------------------------------------------------------------
#find the principal components across the entire bugs lifespan
pcad<-data.frame(FFdata$avmaxtemp,FFdata$avmintemp,FFdata$avmaxhum,FFdata$avminhum)
pcout <- princomp(pcad)
#the sixth subset is the matrix of raw components
pcm <- pcout[[6]]
#We need to add these componenets to original data set.
#create idnum variable to later merge.
prince<-data.frame(pcm)
prince$idnum<-c(1:length(prince$Comp.1))

##Repeat for hatch model variables  
#first create a data set without NA's Measurements
srnas<-which(is.na(FFdata$havmintemp)==FALSE)
rFFdata<-FFdata[srnas,]

Hpcad<-data.frame(rFFdata$havmaxtemp,rFFdata$havmintemp,rFFdata$havmaxhum,rFFdata$havminhum)
Hpcout<-princomp(Hpcad)
#the sixth subset is the matrix of raw components
Hpcm<-Hpcout[[6]]

#We need to add these componenets to original data set.
Hprince<-data.frame(Hpcm)
Hprince$idnum<-rFFdata$idnum
# Columns need to be renamed to be difirrent from prince
names(Hprince)<-c("HComp.1", "HComp.2", "HComp.3", "HComp.4", "idnum") 

#Merge the data sets
  single2<-merge(FFdata, prince, by="idnum")
  FFdataPC<-merge(Hprince, single2, by="idnum", all.y= TRUE)
write.csv(FFdataPC, "FertilityFecundityDataPC.csv")



#ploting examples of residuals and model 
testplot<-ggplot(aes( y= eggs, x= weeknum, na.rm=TRUE), 
               data=Compile)+geom_point(data=Compile)
testplot<-testplot+ggtitle("Number of Eggs Laid by Week and Infection Status") 
testplot<-testplot+facet_grid(. ~infected)+geom_smooth(method= "lm")
testplot

plot(residuals(nbmod13))


#####Likelihood ratio test

#exp((AICmin-AICi)/2)=Probablity that i minimalizes information loss just as well as min.
#ln(prob)=AICmin-AICi/2
#2e^prob=difference between AICs.  
#if sign prob is 0.05 then
2*(log(0.05))
# thus a difference in AIC of 6 o greater is a significant similar probablity for now.
test1<-exp((11824-11852)/2)

lrtest(nbmod1, nbmod2)
lrtest(nbmod2,nbmod3)

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
#
