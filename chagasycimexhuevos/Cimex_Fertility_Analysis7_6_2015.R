# #packages needed/experimented with
#install.packages(c("lubridate","reshape2", "matrixStats","ggplot2",
#     "plyr", "geeM", "MASS", "lmtest","survval", "doBy","stats", "stringi", 
#     "Rcpp", "pscl", "missMDA", "Rmisc", "lme4", "nlme"))
#call the packages
library(lubridate) #para extracting dates 
library(reshape2) #para make the wide data into long data
library(matrixStats)
library(ggplot2)
library(plyr)  #para rbind.fill function
library(geeM)
library(MASS)
library(lmtest)
library(pscl)
library(survival)
library(doBy)
library(missMDA)
library(Rmisc) #summarySE command
library(lme4)#linear mixed effects
library(nlme)

# #set directory and bring in files to be analyzed.
# #PC
setwd("c:\\Users\\tradylan\\Documents\\Laboratory\\chagasycimexhuevos")
#MAC
#setwd("/Users/mzlevy/Laboratory/chagasycimexhuevos")
# 
# #bring in hatching data from each rep (pilot, rep 1, and rep 2)
# cimfertpilot <- read.csv("Cimex_FertP_update12_3.csv")
# #Original Found https://docs.google.com/spreadsheets/d/1E-GRO1_Ybrgqj0wjgz5s9YHY1KwJUVPov0I2PwgC2CQ/edit
# cimfert1 <- read.csv("Cimex_FertR1_8_12.csv")
# ##https://docs.google.com/spreadsheets/d/1iDBITasgMrbmwGJJSwsPkcal1b7b3kdfmviRtw8wbqA/edit#gid=709304485
# cimfert2 <- read.csv("Cimex_FertR2_8_12.csv")
# #https://docs.google.com/spreadsheets/d/13RvsL-uZaKgJPN3RBf8nTlsR8BQ6U-BrEQ_7BmxYgsI/edit
# 
# #Bring in Mortality Data (currently derived from above tables.)
# #mortR1 <- read.csv("Cimex_Mortality_R1.csv")
# #mortR2 <- read.csv("Cimex_Mortality_R2.csv")
# #another tab in the two R1 and R2 Google docs above.
# 
# #bring in temperature and humidity data.
# #the pilot has the temp and RH data for all sections
# tempRH <- read.csv("TEMP_Y_RH.csv")
# #original from same google file as above hatch data.
# 
# ###Put all the data together.
# ##Create a table with all the insects.
# #first create marker so we can identify each trial.
# cimfertpilot$trial <- 0
# cimfert1$trial <- 1
# cimfert2$trial <- 2
# 
# #bring the tables together.".fill" allows different table lengths fill w/ NA
# cimfert<-rbind.fill(cimfert2, cimfert1, cimfertpilot)
# 
# #create unicode for insect pair
# cimfert$ID <- paste(cimfert$Nro_.pareja, cimfert$trial, sep="-")
# #create column that specifies specific mouse
# cimfert$raton <-paste(cimfert$Procedencia, cimfert$trial, sep="-")
# #first use Procedencia to get certain groups
# ControlP <- which(cimfert$Procedencia=="CO")
# InfectPA <- which(cimfert$Procedencia=="I-R1")
# InfectPB <- which(cimfert$Procedencia=="I-R2")
# ControlA <- which(cimfert$Procedencia=="CO-A")
# ControlB <- which(cimfert$Procedencia=="CO-B")
# InfectA <- which(cimfert$Procedencia=="I-A")
# InfectB <- which(cimfert$Procedencia=="I-B")
# 
# ##Now create some indicies using the mouse.
# #the R stands for which Rep it was in. RA is rep 1 and RB is rep2.
# InfectARA <-which(cimfert$raton=="I-A-1")
# InfectARB <-which(cimfert$raton=="I-A-2")
# InfectBRA <-which(cimfert$raton=="I-B-1")
# InfectBRB <-which(cimfert$raton=="I-B-2")
# ControlARA <- which(cimfert$raton=="CO-A-1")
# ControlARB <- which(cimfert$raton=="CO-A-2")
# ControlBRA <- which(cimfert$raton=="CO-B-1")
# ControlBRB <- which(cimfert$raton=="CO-B-2")
# #group the appropriate indicies for later usage(plots)
# tot<-c(1:length(cimfert$raton))
# controls <- c(ControlP, ControlA, ControlB) #all controls
# infect <- c(InfectPA, InfectPB, InfectA, InfectB) #all infected insects
# InfectP <- c(InfectPA, InfectPB) #both sets of infected mice in pilot
# InfectRA <- c(InfectARA, InfectBRA) #both infected mice in rep 1
# InfectRB <- c(InfectARB, InfectBRB)#both infected mice in rep 2
# ControlRA <- c(ControlARA, ControlBRA) #both controls rep 1
# ControlRB <- c(ControlARB, ControlBRB) #both controls rep 2
# #group for trial
# 
# #make column designating infected/control
# cimfert$infected[infect]<-1
# cimfert$infected[controls]<-0
# 
# #find which columns are hatch and which are eggs.
# postura<-which(substr(names(cimfert), nchar(names(cimfert)), nchar(names(cimfert)))=="p")
# viabilidad <-which(substr(names(cimfert), nchar(names(cimfert)), nchar(names(cimfert)))=="v")
# 
# #make id numbers for analysis(character string doesn't play friendly with other pkgs)
# cimfert$idnum<-c(1:length(cimfert$ID))
# 
# #We need to create outputs for the data to put it in long format 
# #each row represents a weekly observation for a particular insect
# #instead of all obsserations for that insect
# blank <- (1:(length(postura)*length(cimfert$Procedencia))*0)
# Compile <- data.frame(blank,0,0,0,0,0,0, 0, 0, 0, 0, 0, 0)
# #rename colums of new table.
# Compile <- rename(Compile, replace = c("blank"="id", "X0"="parents","X0.1"="infected","X0.2"="start",
#                                        "X0.3"="week", "X0.4"="date", "X0.5"="eggs", "X0.6"="hatch", "X0.7"="alive",
#                                        "X0.8"="mouse", "X0.9"="procedencia", "X0.10"="trial", "X0.11"="idnum"))
# #if adding colums, be sure that you capitalize the Xx
# 
# #make sure the cimfert columns are characters or they will not transfer.
# cimfert$Nro_.pareja <- as.character(cimfert$Nro_.pareja)
# cimfert$Procedencia <- as.character(cimfert$Procedencia)
# cimfert$Fecha_Inicio_.Pareja <-as.character(cimfert$Fecha_Inicio_.Pareja)
# 
# #now we need to create a nested loop to get the data into the  new data frame
# for (d in 1:(length(postura))){
#   for (i in 1:(length(cimfert$s1_p))){ #i for each insect
#     Compile$week[i+((d-1)*length(cimfert$ID))] <- d
#     Compile$trial[i+((d-1)*length(cimfert$ID))] <- cimfert$trial[i]
#     Compile$mouse[i+((d-1)*length(cimfert$ID))] <- cimfert$raton[i]
#     Compile$id[i+((d-1)*length(cimfert$ID))] <- cimfert$ID[i]
#     Compile$parents[i+((d-1)*length(cimfert$ID))] <- cimfert$Nro_.pareja[i]
#     Compile$procedencia[i+((d-1)*length(cimfert$ID))] <- cimfert$Procedencia[i]
#     Compile$infected[i+((d-1)*length(cimfert$ID))] <- cimfert$infected[i]
#     Compile$start[i+((d-1)*length(cimfert$ID))] <- cimfert$Fecha_Inicio_.Pareja[i]
#     Compile$hatch[i+((d-1)*length(cimfert$ID))] <-cimfert[i,(2*d+3)]
#     Compile$eggs[i+((d-1)*length(cimfert$ID))]<-cimfert[i,(2*d+2)]
#     Compile$idnum[i+((d-1)*length(cimfert$ID))] <- cimfert$idnum[i]
#   }
# }
# 
# #create a column that indicates if the mother is dead or alive
# #This is assumes that if a one is entered the insect was alive during that week.
# #this means the first week where the insect is dead has a 1 (it was alive at some point that week)
# 
# #make eggs a numeric vector
# Compile$eggs<-as.numeric(Compile$eggs)
# # iff an NA is placed and not a 0, the bug was dead
# #so identify NA
# dead<-which(is.na(Compile$eggs)==TRUE)
# alive<-which(is.na(Compile$eggs)==FALSE)
# #mark na's as dead, else alive.
# Compile$alive[dead]<-0
# Compile$alive[alive]<-1
# 
# #make date so that humidity and temperature data can be easily entered.
# Compile$start <- parse_date_time(Compile$start, "dmy", tz="EST")
# Compile$start <- as.Date(Compile$start)
# Compile$date <- (Compile$start+(Compile$week*7))
# 
# #make the tempRH also date format
# tempRH$FECHA <- parse_date_time(tempRH$FECHA, "dmy", tz="EST")
# tempRH$FECHA <- as.Date(tempRH$FECHA)
# 
# #identify which insects are in which repetition
# pilot <- which(Compile$trial==0)
# repone <- which(Compile$trial==1)
# reptwo <- which(Compile$trial==2)
# 
# ######################################################################
# #=====================================================================
# #Make Graphics Using Compile
# # #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# # 
# # ##plot eggs and hatch by week.
# # #create model to add regression line to line graphs
# # eggweek<-lm(Compile$eggs ~Compile$week)
# # hatchweek<-lm(Compile$hatch ~Compile$week)
# # #line graph for eggs laid by week
# # plot(Compile$week, Compile$eggs+rnorm(length(Compile$eggs), 0, 0.5), main="Eggs by Week")
# # abline(eggweek)
# # #box plot for eggs laid
# # boxplot(Compile$eggs ~Compile$week, main="Eggs by Week")
# # #Line graph for eggs laid by week
# # plot(Compile$week, Compile$hatch+rnorm(length(Compile$hatch), 0, 0.5), main="Hatching by Week")
# # abline(hatchweek)
# # #box plot for eggs hatched
# # boxplot(Compile$hatch ~Compile$week, main="Hatching by Week")
# # 
# # par(mfrow=c(1,1))
# # #pdf("graphs/NmbEggsByDateBox.pdf")
# # boxplot(Compile$eggs ~Compile$date, main="Eggs by Date")
# # #dev.off()
# # 
# # #pdf("graphs/NmbHtchByDateBox.pdf")
# # boxplot(Compile$hatch ~Compile$date, main="Hatching by Date")
# # #dev.off()
# # 
# # ##lets makle a combined plot of the two groups together
# # enona<-which(is.na(Compile$eggs)==FALSE)
# # hnona<-which(is.na(Compile$hatch)==FALSE)
# # Compile$infected<-as.factor(Compile$infected)
# # Compile$week<-as.factor(Compile$week)
# # 
# # #the box plot for eggs laid
# # par(mfrow=c(1,1))
# # #pdf("graphs/NumEggsLaidByWeekYInfyCntlBox.pdf")
# # g<-ggplot(aes( y= eggs, x= week, fill = infected, na.rm=TRUE),
# #        data= Compile[enona,]) +geom_boxplot(data=Compile[enona,])
# # g<-g+ggtitle("Distribution of Number of Eggs Laid by Infection Status Each Week")+
# #   xlab("Week") +
# #   ylab("Number of Eggs Laid")
# # g<-g+scale_fill_manual(values=c("#00CCFF", "#990000"),name="Infection Status", labels=c("Controls", "Infected"))
# # #g<-g+ scale_color_discrete(name="Infection Status", labels=c("Controls", "Infected"))
# # #g<-g+scale_x_continuous(breaks=seq(0, 38, 2))
# # #g<-g+scale_x_discrete(labels=c("", seq())
# # g
# # 
# # 
# # #ggsave(g, file="BoxPlotDistNumEggLaidbyInfectionStatusbyWeek.jpeg", units= "cm",
# # #   width = 26.4, height= 15.875)
# # #dev.off()
# # 
# # 
# # #create graph that show average 
# # weekinfsum<-summarySE(Compile, measurevar="eggs", groupvars=c("infected", "week"), na.rm=TRUE)
# # 
# # #Change working directory for easy access to draft writers
# # setwd("c:\\Users\\tradylan\\Dropbox\\cruzi_on_lifetables\\paper_draft\\Figuras\\figura_5_AverageEggsLaid")
# # #setwd("c:\\Users\\tradylan\\Documents\\Laboratory\\chagasycimexhuevos\\graphs")
# # 
# # #save as csv so figure can easily be recreated on its own.
# # #write.csv(weekinfsum,"DataforFig5.csv")
# # 
# # ##to save as pdf
# # #pdf("graphs/AvNumEggsLaidByWeekYInfyLineCI.pdf")
# # pdf("AvNumEggsLaidByWeekYInfyLineCI.pdf")
# # 
# # #Code to create figure in ggplot
# # g2 <- ggplot(weekinfsum, aes(x=week, y=eggs, color=infected)) +
# #   geom_errorbar(aes(ymin=eggs-ci, ymax=eggs+ci, group=infected, color=infected),
# #                 width=.1)+
# #   geom_line(aes(group=infected)) +
# #   ggtitle("Average Number of Eggs Laid from Living Insects By Week")+
# #   xlab("Week") +
# #   ylab("Average Number of Eggs Laid")+
# #   scale_color_discrete(name="Infection Status", labels=c("Controls", "Infected"),h= c(250, 3))+
# #   geom_point()+
# #   theme(legend.position=c(0.9, 0.9))
# # g2
# # 
# # dev.off()
# # ##save as various file types
# # #JPEG
# # ggsave(g2, file="AvNumEggsLaidByWeekYInfyLineCI.jpeg", units= "cm",
# #   width = 26.4, height= 15.875)
# # 
# # #BMP
# # ggsave(g2, file="AvNumEggsLaidByWeekYInfyLineCI.bmp", units= "cm",
# #  width = 26.4, height= 15.875)
# # 
# # #GIF
# # #ggsave(g2, file="AvNumEggsLaidByWeekYInfyLineCI.gif", units= "cm",
# # # width = 26.4, height= 15.875)
# # 
# # #TIFF
# # ggsave(g2, file="AvNumEggsLaidByWeekYInfyLineCI.tiff", units= "cm",
# #  width = 26.4, height= 15.875)
# # 
# # #EPS
# # ggsave(g2, file="AvNumEggsLaidByWeekYInfyLineCI.eps", units= "cm",
# #  width = 26.4, height= 15.875)
# # 
# # #SVG
# # ggsave(g2, file="AvNumEggsLaidByWeekYInfyLineCI.svg", units= "cm",
# #  width = 26.4, height= 15.875)
# # 
# # #png
# # ggsave(g2, file="AvNumEggsLaidByWeekYInfyLineCI.png", units= "cm",
# #  width = 26.4, height= 15.875)
# # 
# # 
# # 
# # #hatch plot
# # #pdf("graphs/NumEggsHtchByWeekyInfyCntrlBox.pdf")
# # h<-ggplot(aes( y= hatch, x= week, fill = infected, na.rm=TRUE), 
# #        data= Compile[hnona,])+geom_boxplot(data=Compile[hnona,])
# # h<-h+ggtitle("Distribution of Number of Eggs Hatched by Infection Status in Bugs that Laid Eggs")
# # h<-h+scale_fill_manual(values=c("#00CCFF", "#990000"))
# # h
# # 
# # #ggsave(h, file="BoxPlotDistNumEggHtvhbyInfectionStatusbyWeek.jpeg", units= "cm",
# #        #width = 26.4, height= 15.875)
# # #dev.off()
# # 
# # #h2: Hatchrate Plot
# # #make a hatch rate 
# # Compile$hatchrate<-Compile$hatch/Compile$eggs
# # #Make a summary table
# # hatchratesum<-summarySE(Compile, measurevar="hatchrate", groupvars=c("infected", "week"), na.rm=TRUE)
# # #replace the Inf'swith NA's
# # hatchratesum$hatchrate[which(is.infinite(hatchratesum$hatchrate)==TRUE)]<-NA
# # #write.csv(hatchratesum,"DataforFig6.csv")
# # 
# # #Plot the average and 95% Confidence Interval
# # #change working directory
# # setwd("c:\\Users\\tradylan\\Dropbox\\cruzi_on_lifetables\\paper_draft\\Figuras\\figura_6_AverageHatchRate")
# # 
# # pdf("AvHatchrateByWeekYInfyLineCI.pdf")
# # h2 <- ggplot(hatchratesum, aes(x=week, y=hatchrate, color=infected)) +
# #   geom_errorbar(aes(ymin=hatchrate-ci, ymax=hatchrate+ci, group=infected, color=infected),
# #                 width=.1)+
# #   geom_line(aes(group=infected)) +
# #   ggtitle("Average Percentage of Eggs that Hatched by Week")+
# #   xlab("Week") +
# #   ylab("Average Percentage of Eggs that Hatched")+
# #   scale_color_discrete(name="Infection Status", labels=c("Controls", "Infected"), h= c(250, 3))+
# #   geom_point()+
# #   theme(legend.position=c(0.9, 0.9))+
# #   scale_y_continuous(breaks=seq(-3, 3, 0.5))
# # h2
# # dev.off()
# # ##save as other various file types.
# # #JPEG
# # ggsave(h2, file="AvHatchrateByWeekYInfyLineCI.jpeg", units= "cm",
# #   width = 26.4, height= 15.875)
# # 
# # #BMP
# # ggsave(h2, file="AvHatchrateByWeekYInfyLineCI.bmp", units= "cm",
# #  width = 26.4, height= 15.875)
# # 
# # #TIFF
# # ggsave(h2, file="AvHatchrateByWeekYInfyLineCI.tiff", units= "cm",
# #  width = 26.4, height= 15.875)
# # 
# # #EPS
# # ggsave(h2, file="AvHatchrateByWeekYInfyLineCI.eps", units= "cm",
# #  width = 26.4, height= 15.875)
# # 
# # #SVG
# # ggsave(h2, file="AvHatchrateByWeekYInfyLineCI.svg", units= "cm",
# #  width = 26.4, height= 15.875)
# # 
# # #png
# # ggsave(h2, file="AvHatchrateByWeekYInfyLineCI.png", units= "cm",
# #  width = 26.4, height= 15.875)
# # 
# # 
# # #h3: Hatchrate Plot by trial
# # #Make a summary table
# # hatchratetrial<-summarySE(Compile, measurevar="hatchrate", groupvars=c("infected", "week", "trial"), na.rm=TRUE)
# # #replace the inf with NA
# # hatchratetrial$hatchrate[which(is.infinite(hatchratetrial$hatchrate)==TRUE)]<-NA
# # par(mfrow=c(3,1))
# # pdf("graphs/AvHatchrateByWeekYInfyTrialLineCIReps.pdf")
# # 
# # #rep Pilot
# # h3a <- ggplot(hatchratetrial[which(hatchratetrial$trial==0),], aes(x=week, y=hatchrate, color=infected)) +
# #   geom_errorbar(aes(ymin=hatchrate-ci, ymax=hatchrate+ci, group=infected, color=infected),
# #                 width=.1)+
# #   geom_line(aes(group=c(infected))) +
# #   ggtitle("Average Hatch Rate Each Week By Insects that Laid in Pilot")+
# #   xlab("Week") +
# #   ylab("Average Percentage of Eggs that Hatched")+
# #   scale_color_discrete(name="Infection Status", labels=c("Controls", "Infected"), h= c(250, 3))+
# #   geom_point()
# # h3a
# # #Rep 1
# # h3b <- ggplot(hatchratetrial[which(hatchratetrial$trial==1),], aes(x=week, y=hatchrate, color=infected)) +
# #   geom_errorbar(aes(ymin=hatchrate-ci, ymax=hatchrate+ci, group=infected, color=infected),
# #                 width=.1)+
# #   geom_line(aes(group=c(infected))) +
# #   ggtitle("Average Hatch Rate Each Week By Insects that Laid in Rep 1")+
# #   xlab("Week") +
# #   ylab("Average Percentage of Eggs that Hatched")+
# #   scale_color_discrete(name="Infection Status", labels=c("Controls", "Infected"), h= c(250, 3))+
# #   geom_point()
# # h3b
# # #Rep 2
# # h3c <- ggplot(hatchratetrial[which(hatchratetrial$trial==2),], aes(x=week, y=hatchrate, color=infected)) +
# #   geom_errorbar(aes(ymin=hatchrate-ci, ymax=hatchrate+ci, group=infected, color=infected),
# #                 width=.1)+
# #   geom_line(aes(group=c(infected))) +
# #   ggtitle("Average Hatch Rate Each Week By Insects that Laid in Rep 2")+
# #   xlab("Week") +
# #   ylab("Average Percentage of Eggs that Hatched")+
# #   scale_color_discrete(name="Infection Status", labels=c("Controls", "Infected"), h= c(250, 3))+
# #   geom_point()
# # h3c
# # 
# # 
# # dev.off()
# # 
# # par(mfrow=c(1,1))
# 
# 
# #create a factor id for mice
# mice<-unique(Compile$mouse)
# mouseidnum<-c(1:length(mice))
# mousetable<-data.frame(mice,mouseidnum)
# mousetable$mouseidnum<-as.factor(mousetable$mouseidnum)
# Compile$mouseidnum<-Compile$trial*0
# 
# for(i in 1:length(mice)){
#    micenumi<-which(mousetable$mouseidnum==i)
#    micematch<-which(Compile$mouse==mousetable$mice[micenumi])
#    Compile$mouseidnum[micematch]<-i   
# }
# 
# ###create values that summarize across insect's life
# # #create rows to fill by loop
# Compile$lifespan<-Compile$eggs*NA
# Compile$egg_total<-Compile$eggs*NA
# Compile$hatch_total<-Compile$eggs*NA
# Compile$avmaxtemp<-Compile$eggs*NA
# Compile$avmintemp<-Compile$eggs*NA
# Compile$avmaxhum<-Compile$eggs*NA
# Compile$avminhum<-Compile$eggs*NA
# Compile$havmaxtemp<-Compile$eggs*NA
# Compile$havmintemp<-Compile$eggs*NA
# Compile$havmaxhum<-Compile$eggs*NA
# Compile$havminhum<-Compile$eggs*NA
# 
# #in temperature dataset make temperature and humidity values numeric
# tempRH$TEMP.MAX..Â.C.<-as.numeric(tempRH$TEMP.MAX..Â.C.)
# tempRH$TEMP.MIN..Â.C.<-as.numeric(tempRH$TEMP.MIN..Â.C.)
# tempRH$HR.MAX....<-as.numeric(tempRH$HR.MAX....)
# tempRH$HR.MIN....<-as.numeric(tempRH$HR.MIN....)
# 
# # #identify living observations so temperatures while the
# # #insect is dead does not have affect
# lives <- which(Compile$alive==1) 
# #identify observations that have eggs laid
# eggslaid <- which(Compile$eggs > 0)
# 
# # #loop over each insect and calculate lifespan and temperature variables
# for (i in 1:max(Compile$idnum)) {
#   ids<-which(Compile$idnum==i)
#   #Sum eggs laid and hatched across life of bug
#   Compile$egg_total[ids]<-sum(Compile$eggs[ids], na.rm=TRUE)
#   Compile$hatch_total[ids]<-sum(Compile$hatch[ids], na.rm=TRUE)
#   is <- intersect(ids, lives)#that way you don't add dead times to observations
#   #the number of observations while alive == lifespan
#   ls<-length(is)
#   Compile$lifespan[ids]<-ls
#   #identify the first and last observations in [is]
#   fst<-is[1]
#   lst<-is[ls]
#   #identify first and last date for each insect.
#   begin<-(Compile$start[fst])
#   end<-Compile$date[lst]
#   a<-which(tempRH$FECHA==begin)
#   b<-which(tempRH$FECHA==end)
#   #Use dates to find values from temperature and humidity data
#   #average over the life of the bug.
#   avmaxtemp1<-mean(tempRH$TEMP.MAX..Â.C.[a:b], na.rm=TRUE)
#   avmintemp1<-mean(tempRH$TEMP.MIN..Â.C.[a:b], na.rm=TRUE)
#   avmaxhum1<-mean(tempRH$HR.MAX....[a:b], na.rm=TRUE)
#   avminhum1<-mean(tempRH$HR.MIN....[a:b], na.rm=TRUE)
#   #Now add summarized temperature values to Compile
#   Compile$avmaxtemp[ids]<-avmaxtemp1
#   Compile$avmintemp[ids]<-avmintemp1
#   Compile$avmaxhum[ids]<-avmaxhum1
#   Compile$avminhum[ids]<-avminhum1
# }
# 
# #now make a similar calculation for humidity, only using weeks where eggs were laid
# for (i in 1:max(Compile$idnum)) {
#   ids<-which(Compile$idnum==i)
#   leggs <- intersect(ids, eggslaid)
#   if(length(leggs) > 0){
#   #create a list of dates
#     FECHA<-NA
#     for (j in 1:length(leggs)){
#       endings <- Compile$date[leggs[j]]
#       beginings <- (Compile$date[leggs[j]])-6
#       moredates<-(beginings:endings)
#       FECHA<-c(FECHA, moredates)
#     }
#     num<-1:length(FECHA)
#     datetable<-data.frame(num, FECHA)
#   
#     #merge temperature data onto datetable
#     temptable<-merge(datetable, tempRH, by="FECHA", all.x=TRUE)
#     #take the average of each of these columns
#     havmaxtemp1 <- mean(temptable$TEMP.MAX..Â.C., na.rm=TRUE)
#     havmintemp1 <- mean(temptable$TEMP.MIN..Â.C., na.rm=TRUE)
#     havmaxhum1 <- mean(temptable$HR.MAX...., na.rm=TRUE)
#     havminhum1 <- mean(temptable$HR.MIN...., na.rm=TRUE)
#     #Now add these to Compile
#     Compile$havmaxtemp[ids] <- havmaxtemp1
#     Compile$havmintemp[ids] <- havmintemp1
#     Compile$havmaxhum[ids] <- havmaxhum1
#     Compile$havminhum[ids] <- havminhum1 
#   }
# }
# 
# #finally lets reduce the table down to only the lifespan data
# #this is the same as only taking the first week of dtaa
# weekone<-which(Compile$week==1)
# FFdata<-Compile[weekone,]
# #and then removing duplicate or week specific columns
# FFdata$id<- NULL
# FFdata$sdweek<- NULL
# FFdata$procedencia<- NULL
# FFdata$mouse<- NULL
# FFdata$alive<- NULL
# FFdata$date<- NULL
# FFdata$week<- NULL
# FFdata$start<- NULL
# FFdata$id<- NULL
# FFdata$parents<- NULL
# FFdata$eggs<- NULL
# FFdata$hatch<- NULL
# 
# # plot(FFdata$lifespan, FFdata$egg_total)
# 
# #find the principal components for Temp/Hum across the entire bugs lifespan
# pcad<-data.frame(FFdata$avmaxtemp,FFdata$avmintemp,FFdata$avmaxhum,
#                  FFdata$avminhum)
#   corm<-cor(pcad)
#   eigen(corm)  #First two components are greater than 1. 
# #pcout <- princomp(pcad)#R community recomends using prcomp over princomp.
# pcout2 <- prcomp(~FFdata.avmaxtemp+FFdata.avmintemp+FFdata.avmaxhum+
#                    FFdata.avminhum, data=pcad, scale=TRUE)
# #the sixth subset is the matrix of raw components
# pcm <- pcout2$x
# #We need to add these componenets to original data set.
# #create idnum variable to later merge.
# prince<-data.frame(pcm)
# prince$idnum<-c(1:length(prince$PC2))
# 
# #find the relative contribution of each variable to each component
# #fromhttp://stackoverflow.com/questions/12760108/principal-components-
# #analysis-how-to-get-the-contribution-of-each-paramete
# aload <- abs(pcout2$rotation)
# sweep(aload, 2, colSums(aload), "/")
# 
# ##Repeat for hatch model variables  
# #first create a data set without NA Measurements
# srnas<-which(is.na(FFdata$havmintemp)==FALSE)
# rFFdata<-FFdata[srnas,]
# Hpcad<-data.frame(rFFdata$havmaxtemp,rFFdata$havmintemp,rFFdata$havmaxhum,rFFdata$havminhum)
#   hcorm<-cor(Hpcad)
# #find eigen values
#   eigen(hcorm) #same as before, first two variales should be used.
# #Hpcout <-princomp(Hpcad)
# Hpcout2 <- prcomp(~rFFdata.havmaxtemp+rFFdata.havmintemp+rFFdata.havmaxhum+rFFdata.havminhum, data=Hpcad, scale=TRUE)
# #obtain raw components to be added back to data table
# Hpcm<-Hpcout2$x
# 
# #We need to add these componenets to original data set.
# Hprince<-data.frame(Hpcm)
# #attach idnum to merge on.
# Hprince$idnum<-rFFdata$idnum
# # Columns need to be renamed to be difirrent from non-hatch components
# names(Hprince)<-c("HComp.1", "HComp.2", "HComp.3", "HComp.4", "idnum") 
# 
# ###Merge the data sets
#   #eggs laid
#   single2<-merge(FFdata, prince, by="idnum")
#   #eggs hatched
#   FFdataPC<-merge(Hprince, single2, by="idnum", all.y= TRUE)
# write.csv(FFdataPC, "FertilityFecundityDataPC.csv")
 
##################################################################################
#start here by bringing in data from above.
 FFdataPC <- read.csv("FertilityFecundityDataPC.csv")

#to determine the number of principle components that could be used from PCA
#conduct Leave One Out Cross validation.  
#estimPCA<-estim_ncpPCA(pcad, ncp.min=0, ncp.max=4, method.cv="loo", method="EM")
#estim_ncpPCA(pcad, ncp.min=0, ncp.max=4, method.cv="loo")
#estim_ncpPCA(Hpcad, ncp.min=0, ncp.max=4, method.cv="loo")
#determines that the first 3 components could be used for each.

# ###############################################################################
# #==============================================================================
# #ahora tenomos muchas graficas, podemos empezar haciendo el modelo
# #########################################################
# #getting model data
# #[1]coefficient
# #[2]residuals
# #[3]fitted values
# #[4]effects?
# #[5]R
# #[8]Link Function and NB value
# #[10] deviance
# #[11]AIC
# #[14]Weights
# #Model with infection and lifespan as covariates.
lifespanmod<-glm.nb(egg_total~infected+lifespan, data=FFdataPC )
  summary(lifespanmod )
  #AIC==1636.7 P(infected)==0.107
#Null Model without any temperature or humidity variables.
nullmodel<- glm.nb(egg_total~infected, data=FFdataPC, offset(log(lifespan)))
 summary(nullmodel)  #AIC==26710   P=<2e-16 *** Theta is 1.13
#Check if poisson would be better
 poissonmod<-glm(egg_total~infected, offset(log(lifespan)), data=FFdataPC, family="poisson")
 #test from http://www.ats.ucla.edu/stat/r/dae/nbreg.htm
  X2 <- 2*(logLik(nullmodel)-logLik(poissonmod))
  pchisq(X2, df=1, lower.tail=FALSE)  #is the df=1 because they are the same, except for theta?
  lrtest(poissonmod, nullmodel) #this does the same thing, again shows that nb is better
 #This rules out poisson
#check for 0 inflation
nullzmod<- zeroinfl(egg_total~infected+offset(log(lifespan)), data=FFdataPC)
 vuong(nullmodel, nullzmod) #null model is better, so no zero inflation

#skip to to line 587 to show colinearity.
 
# ###Now just do a univariate analysis
# maxtempmodel<- glm.nb(egg_total~avmaxtemp, data=FFdata, offset(log(lifespan)))
# mintempmodel<- glm.nb(egg_total~avmintemp, data=FFdata, offset(log(lifespan)))
# maxhummodel<- glm.nb(egg_total~avmaxhum, data=FFdata, offset(log(lifespan)))
# minhummodel<- glm.nb(egg_total~avminhum, data=FFdata, (log(lifespan)))
# 
# #look at summary to check AIC and P value
# summary(maxtempmodel)  #AIC==
# summary(mintempmodel) #AIC==
# summary(maxhummodel) #AIC==
# summary(minhummodel) #AIC==  

#Model with Average Minimum Temperature and max humidity model
selectedmodel<-glm.nb(egg_total~infected+avmaxtemp+avminhum, data=FFdataPC, offset(log(lifespan)))
summary(selectedmodel)  

#full models
fullmodel<-glm.nb(egg_total~infected+avmaxtemp+avminhum+avmaxhum+avmintemp, data=FFdataPC, offset(lifespan))
summary(fullmodel) #AIC==
plot(FFdataPC$avmaxhum, FFdataPC$avminhum)
#Alternative models (2 variables)  #all of which the full model has a better AIC
AltModelA<-glm.nb(egg_total~infected+avmintemp+avminhum, data=FFdataPC, offset(log(lifespan)))
  summary(AltModelA) #AIC==
AltModelB<-glm.nb(egg_total~infected+avmaxtemp+avmaxhum, data=FFdataPC, offset(log(lifespan)))
  summary(AltModelB) #AIC==
AltModelC<-glm.nb(egg_total~infected+avmintemp+avmaxhum, data=FFdataPC, offset(log(lifespan)))
  summary(AltModelC) #AIC==
AltModelD<-glm.nb(egg_total~infected+avminhum+avmaxhum, data=FFdataPC, offset(log(lifespan)))
  summary(AltModelD) #AIC== 
AltModelE<-glm.nb(egg_total~infected+avmintemp+avmaxtemp, data=FFdataPC, offset(log(lifespan)))
  summary(AltModelE) #AIC==
  
#Alternative Models with 3 Variables
  AltModelF<-glm.nb(egg_total~infected+avmintemp+avmaxtemp+avminhum, data=FFdataPC, offset(log(lifespan)))
  summary(AltModelF) #AIC==
  AltModelG<-glm.nb(egg_total~infected+avmintemp+avmaxtemp+avmaxhum, data=FFdataPC, offset(log(lifespan)))
  summary(AltModelG) #AIC==
  AltModelH<-glm.nb(egg_total~infected+avmintemp+avmaxhum+avminhum, data=FFdataPC, offset(log(lifespan)))
  summary(AltModelH) #AIC==
  AltModelI<-glm.nb(egg_total~infected+avmaxhum+avmaxtemp+avminhum, data=FFdataPC, offset(log(lifespan)))
  summary(AltModelI) #AIC==

#it is likely that colinearity is inflating the significance of eachy variable.
  #thus try principle components and not each varriable by itself
  plot(FFdataPC$avmaxtemp, FFdataPC$avmintemp)
  plot(FFdataPC$avmaxtemp, FFdataPC$avmaxhum)
  plot(FFdataPC$avmaxtemp, FFdataPC$avminhum)
  plot(FFdataPC$avmintemp, FFdataPC$avmaxhum)
  plot(FFdataPC$avmintemp, FFdataPC$avminhum)
  plot(FFdataPC$avmaxhum, FFdataPC$avminhum)
#   
#   
# ###############################################################################
#   #Repeat for hatching models/ SKIP TO LINE 735 : COLINEARITY
# #==============================================================================
  
#First Create data set without 0 values(can't divide by 0)
  hasno<-which(FFdataPC$egg_total=="0")
  FFnonaPC<-FFdataPC[-hasno,] 
   
#Null Model without any temperature or humidity variables.
   pnullhmodel<- glm(hatch_total~infected, data=FFnonaPC, offset(log(egg_total)),family="poisson")
   nbnullhmodel<-glm.nb(hatch_total~infected, data=FFnonaPC, offset(log(egg_total)))
   lrtest(pnullhmodel, nbnullhmodel)
#    #This rules out poisson
# #now look at zero inflation  
   playzero <- zeroinfl(hatch_total~infected +offset(log(egg_total)), data=FFnonaPC, dist="negbin" )  
    vuong(playzero, nbnullhmodel)  #significantly positive, implies zero inflation is better
   
#add lifespan to model to see if it is helpful
   nullhmodelB<- glm(hatch_total~infected+lifespan, data=FFnonaPC, offset(egg_total),family="poisson")
   summary(nullhmodelB) #AIC==179486,and thus significantly beneficial
#    
# #   ###Now just do a univariate analysis
# #   hmaxtempmodel<- glm(hatch_total~havmaxtemp, data=FFnonaPC, offset(egg_total),family="poisson")
# #   hmintempmodel<- glm(hatch_total~havmintemp, data=FFnonaPC, offset(egg_total),family="poisson")
# #   hmaxhummodel<- glm(hatch_total~havmaxhum, data=FFnonaPC, offset(egg_total),family="poisson")
# #   hminhummodel<- glm(hatch_total~havminhum, data=FFnonaPC, offset(egg_total),family="poisson")
# #   
#McFadden's pseudo R-squared
  pRsq<-function(nulldev, residdev) {
      pseudoRsq<-1-(residdev/nulldev)
       pseudoRsq
   }
# #   
# #   #look at summary to check AIC and P value
# #   summary(hmaxtempmodel)  #AIC==336158
# #     pRsq(hmaxtempmodel$null.deviance, hmaxtempmodel$deviance)#0.194
# #     plot(hmaxtempmodel)
# #   summary(hmintempmodel) #AIC==373140
# #     pRsq(hmintempmodel$null.deviance, hmintempmodel$deviance)#0.087
# #   summary(hmaxhummodel) #AIC==307829
# #     pRsq(hmaxhummodel$null.deviance, hmaxhummodel$deviance)#0.275
# #   summary(hminhummodel) #AIC==  304791
# #     pRsq(hminhummodel$null.deviance, hminhummodel$deviance)#0.284
   
#Complete Model (all variants)
hfullmodel<- glm(hatch_total~infected+havminhum+havmaxhum+havmintemp+havmaxtemp,offset(egg_total),data=FFdataPC, offset(egg_total),family="poisson")
     summary(hfullmodel) #AIC==175426

# #Need to recalculate with poisson    
# #Model with Average Minimum Temperature and max humidity model (from univariate)
  hselectedmodel<-glm(hatch_total~infected+lifespan+havmaxtemp+havminhum, data=FFdataPC, offset(egg_total),family="poisson")
  summary(hselectedmodel)  #AIC==173490   P(infection)==< 2e-16 ***
  
  #Alternative models 2 Variables
  hAltModelA<-glm(hatch_total~infected+lifespan+havmintemp+havminhum, data=FFdataPC, offset(egg_total),family="poisson")
  summary(hAltModelA) #AIC==172242
  hAltModelB<-glm(hatch_total~infected+lifespan+havmaxtemp+havmaxhum, data=FFdataPC, offset(egg_total),family="poisson")
  summary(hAltModelB) #AIC==175427 (worse than full model)
  hAltModelC<-glm(hatch_total~infected+lifespan+havmintemp+havmaxhum, data=FFdataPC, offset(egg_total),family="poisson")
  summary(hAltModelC) #AIC== 173933
  hAltModelH<-glm(hatch_total~infected+lifespan+havminhum+havmaxhum, data=FFdataPC, offset(egg_total),family="poisson")
  summary(hAltModelH) #AIC== 167718
  hAltModelI<-glm(hatch_total~infected+lifespan+havmintemp+havmaxtemp, data=FFdataPC, offset(egg_total),family="poisson")
  summary(hAltModelI) #AIC==172095
  
  #Alternative models 3 variables
  hAltModelD<-glm(hatch_total~infected+lifespan+havmaxtemp+havmintemp+havminhum, data=FFdataPC, offset(egg_total),family="poisson")
  hAltModelE<-glm(hatch_total~infected+lifespan+havmaxtemp+havmintemp+havmaxhum, data=FFdataPC, offset(egg_total),family="poisson")
  hAltModelF<-glm(hatch_total~infected+lifespan+havmaxtemp+havmaxhum+havminhum, data=FFdataPC, offset(egg_total),family="poisson")
  hAltModelG<-glm(hatch_total~infected+lifespan+havmaxhum+havmintemp+havminhum, data=FFdataPC, offset(egg_total),family="poisson")
  
  summary(hAltModelD) #AIC==170664
  summary(hAltModelE)#AIC==171335
  summary(hAltModelF)#AIC==167568*** 
    pRsq(hAltModelF$null.deviance, hAltModelF$deviance) #0.6815045
  summary(hAltModelG)#AIC==167409***
    pRsq(hAltModelG$null.deviance, hAltModelG$deviance) #0.6819634
    
  #Alternative Models 1 variable. hc<- hatch complete  (2 variable models are better)
  hcmaxtempmodel<- glm(hatch_total~infected+lifespan+havmaxtemp, data=FFdataPC, offset(egg_total),family="poisson")
  hcmintempmodel<- glm(hatch_total~infected+lifespan+havmintemp, data=FFdataPC, offset(egg_total),family="poisson")
  hcmaxhummodel<- glm(hatch_total~infected+lifespan+havmaxhum, data=FFdataPC, offset(egg_total),family="poisson")
  hcminhummodel<- glm(hatch_total~infected+lifespan+havminhum, data=FFdataPC, offset(egg_total),family="poisson")
  
  summary(hcmaxtempmodel)  #AIC==179349
  summary(hcmintempmodel) #AIC==179063
  summary(hcmaxhummodel) #AIC==175945
  summary(hcminhummodel) #AIC==174323
  
#   lrtest(hAltModelF, hAltModelG)  #
#   lrtest(hAltModelF, hAltModelH)  #
#   lrtest(hAltModelG, hAltModelH)  #
#   lrtest(hAltModelF, hAltModelH)  #
#   lrtest(hAltModelF, hAltModelB)  #
#   lrtest(hAltModelF, hselectedmodel)#
#   

  
#taking a closer look at trial to see if there are signifcant differences.    
  ##it looks like trial with an interaction with infection would be beneficial
  FFdataPC$trial<-as.factor(FFdataPC$trial)  
  FFdataPC$mouseidnum<-as.factor(FFdataPC$mouseidnum)#163793  <-so we shoudl include trial
  trialmod<- glm(hatch_total~infected+lifespan+havmaxhum+havmintemp+havminhum+trial, data=FFdataPC, offset(egg_total),family="poisson")
  summary(trialmod) #AIC==145802
  pRsq(trialmod$null.deviance, trialmod$deviance) #0.744
  
  trialintmod<-glm(hatch_total~infected+lifespan+havmaxhum+havmintemp+havminhum+trial+infected*trial, data=FFdataPC, offset(egg_total),family="poisson")
  summary(trialintmod) #144487  <- even better than just trial 
  pRsq(trialintmod$null.deviance, trialintmod$deviance) #0.7482648

  trialintmodmxt<-glm(hatch_total~infected+lifespan+havmaxhum+havmintemp+havminhum+havmaxtemp+trial+infected*trial, data=FFdataPC, offset(egg_total),family="poisson")
  summary(trialintmodmxt) #143768  <-best model thus far, having all variables
  pRsq(trialintmodmxt$null.deviance, trialintmodmxt$deviance) #0.7503481

##################do the same but for egg laid model
completemodel<-glm.nb(hatch_total~infected+lifespan+avmaxhum+avmintemp+avminhum+avmaxtemp+trial+infected*trial, data=FFdataPC, offset(egg_total))
  summary(completemodel) #91904  <-best model thus far
  pRsq(completemodel$null.deviance, completemodel$deviance) #0.648106
completemodelni<-glm.nb(hatch_total~infected+lifespan+avmaxhum+avmintemp+avminhum+avmaxtemp+trial, data=FFdataPC, offset(egg_total))
summary(completemodelni) #91904  <-best model thus far


glm.nb(egg_total~infected+lifespan+avmaxhum+avmintemp+avminhum+avmaxtemp, data=FFdataPC, offset(lifespan))
#AIC==25058
glm.nb(egg_total~infected+lifespan+avmintemp+avminhum+avmaxtemp+trial, data=FFdataPC, offset(lifespan))
#AIC==25018 

glm.nb(egg_total~infected+lifespan+avmintemp+avminhum+avmaxtemp+trial, data=FFdataPC, offset(lifespan))
#AIC==25017 

glm.nb(egg_total~infected+lifespan+avmintemp+avminhum+avmaxtemp+trial*infected, data=FFdata, offset(lifespan))
#AIC==24960 

glm.nb(egg_total~infected+lifespan+avmintemp+avminhum+trial*infected, data=FFdataPC, offset(lifespan))
#AIC==24960 
#same, but all variables are signifcant

##############################################################################
#Principal Component MODELS
#------------------------------------------------------------------------------

###############################################################################
#==============================================================================
#Egg models
#==============================================================================

#Null model with no principle components
pceggmod0 <- glm.nb(egg_total~infected, data=FFdataPC, offset(log(lifespan)))
#Model with 1 PC
pceggmod1 <- glm.nb(egg_total~infected+PC1, data=FFdataPC, 
                    offset(log(lifespan)))
#Model with 2 PC
pceggmod2 <- glm.nb(egg_total~infected+PC1+PC2, data=FFdataPC, 
                    offset(log(lifespan)))
#Model with 2PC and trial
pceggmod2b <- glm.nb(egg_total~infected+PC1+PC2+factor(trial), data=FFdataPC,
                     offset(log(lifespan)))
#Model with trial and PC2 interaction
pceggmod2c <- glm.nb(egg_total~infected+PC1+PC2*factor(trial), data=FFdataPC,
                     offset(log(lifespan)))
#Model with trial and PC1 Interaction
pceggmod2d <- glm.nb(egg_total~infected+PC2+PC1*factor(trial), data=FFdataPC, 
                     offset(log(lifespan)))
#Model with trial interacting with both PC2 and PC1
pceggmod2e <- glm.nb(egg_total~infected+PC1*factor(trial)+PC2*factor(trial),
                     data=FFdataPC, offset(log(lifespan)))

#Model with 3 PC
pceggmod3 <- glm.nb(egg_total~infected+PC1+PC2+PC3, data=FFdataPC,
                    offset(log(lifespan)))
#Model without PC2 (more significant that 1,2, and 3 if no trial interactions)
pceggmod3b <- glm.nb(egg_total~infected+PC1+PC3, data=FFdataPC,
                     offset(log(lifespan)))
pceggmod3c <- glm.nb(egg_total~infected+PC1+PC2+PC3+factor(trial), 
                     data=FFdataPC, offset(log(lifespan)))
pceggmod3d <- glm.nb(egg_total~infected+PC1+PC3+factor(trial), data=FFdataPC,
                     offset(log(lifespan)))
#Full Model with all Principle Components #do not use, may overfit
pceggmod4 <- glm.nb(egg_total~infected+PC1+PC2+PC3+PC4, data=FFdataPC,
                    offset(log(lifespan)))

#look at AIC's
pceggmod0#4327
pceggmod1#4263
pceggmod2#4264
pceggmod2b#4199
pceggmod2c#4132
pceggmod2d#4129
pceggmod2e#4127***We will use this model

pceggmod3#4250
pceggmod3b#4248
pceggmod3c#4197.7
pceggmod3d#4239

pceggmod4#4215
#perform likelihood ratio test
lrtest(pceggmod4,pceggmod2)
#both test show pceggmod4 are significantly better than any other
lrtest(pceggmod4,pceggmod1)
lrtest(pceggmod3,pceggmod0)
lrtest(pceggmod3,pceggmod1)
lrtest(pceggmod3b,pceggmod3)

lrtest(pceggmod2,pceggmod1)

#random effect model
FFdataPC$idnum <- as.factor(FFdataPC$idnum)
meeggmod2a <- glmer.nb(egg_total~infected+PC1+PC2+(1|trial)+offset(log(FFdataPC$lifespan)), data=FFdataPC)
meeggmod2b <- glmer.nb(egg_total~infected+PC1+(1|trial)+offset(log(FFdataPC$lifespan)), data=FFdataPC)
meeggmod2c <- glmer.nb(egg_total~infected+PC2+(1|trial)+offset(log(FFdataPC$lifespan)), data=FFdataPC)

#

#==============================================================================
#Hatch models
#remove 0's again
pcnona<-which(FFdataPC$egg_total==0)
FFnonaPC<-FFdataPC[-pcnona,]
#Null model with no predictors
pchatchmodnull<-glm.nb(hatch_total~offset(log(egg_total)), data=FFnonaPC)
#Null Model without principal compoenents but with infection and lifespan
pchatchmod0<-glm.nb(hatch_total~infected+lifespan+offset(log(egg_total)),
                    data=FFnonaPC)
#AIC=1140
#Null Model without lifespan
pchatchmod0a<-glm.nb(hatch_total~infected+offset(log(egg_total)), data=FFnonaPC)
#1138.4 (lifespan can be removed.)
#No components, trial added.
pchatchmod0b<-glm.nb(hatch_total~infected+trial+offset(log(egg_total)),
                     data=FFnonaPC)

#Model with 1 PC
pchatchmod1<-glm.nb(hatch_total~infected+HComp.1+offset(log(egg_total)),
                    data=FFnonaPC) 
pchatchmod1b<-glm.nb(hatch_total~infected+factor(trial)+HComp.1+
                       offset(log(egg_total)), data=FFnonaPC) 
pchatchmod1c<-glm.nb(hatch_total~infected+factor(trial)*HComp.1+
                       offset(log(egg_total)), data=FFnonaPC) 
pchatchmod1d<-glm.nb(hatch_total~infected+lifespan+HComp.1+
                       offset(log(egg_total)), data=FFnonaPC) 
pchatchmod1e<-glm.nb(hatch_total~infected+lifespan+factor(trial)* 
                       HComp.1+offset(log(egg_total)), data=FFnonaPC) 

#Model with 2 PC
pchatchmod2<-glm.nb(hatch_total~infected+HComp.1+HComp.2+offset(log(egg_total)
                                                            ), data=FFnonaPC)
pchatchmod2b<-glm.nb(hatch_total~infected+factor(trial)+HComp.1+HComp.2+
                       offset(log(egg_total)), data=FFnonaPC) 
pchatchmod2c<-glm.nb(hatch_total~infected+HComp.1+HComp.2*factor(trial)+
                       offset(log(egg_total)), data=FFnonaPC) 
pchatchmod2d<-glm.nb(hatch_total~infected+lifespan+HComp.1+HComp.2+factor(trial)+
                       offset(log(egg_total)), data=FFnonaPC)
pchatchmod2e<-glm.nb(hatch_total~infected+HComp.2+HComp.1*factor(trial)+
                       lifespan+offset(log(egg_total)), data=FFnonaPC) 


#Model with 3 PC
pchatchmod3<-glm.nb(hatch_total~infected+HComp.1+HComp.2+HComp.3+
                      offset(log(egg_total)), data=FFnonaPC)
#Model with 3 PC
pchatchmod3a<-glm.nb(hatch_total~infected+HComp.1+HComp.2+HComp.3+factor(trial)
                     +offset(log(egg_total)), data=FFnonaPC)
pchatchmod3b<-glm.nb(hatch_total~infected+HComp.1+HComp.2+HComp.3+factor(trial)
                     +offset(log(egg_total)), data=FFnonaPC)
pchatchmod3c<-glm.nb(hatch_total~infected+HComp.1+HComp.2+HComp.3+lifespan+
                       offset(log(egg_total)), data=FFnonaPC)

#Model with 4 PC  #using 4 PC overfits PC 
pchatchmod4<-glm.nb(hatch_total~infected+HComp.1+HComp.2+HComp.3+HComp.4+
                      offset(log(egg_total)), data=FFnonaPC)

pchatchmod0#1140 SE inf=0.05
pchatchmod0a#1140
pchatchmod0b#1140 SE 0.49
pchatchmod1#1140
pchatchmod1b#1144
pchatchmod1c#1144
pchatchmod1d#1142
pchatchmod1e#1144 

pchatchmod2#1141.9
pchatchmod2b#1141.5
pchatchmod2c#1144
pchatchmod2d#1141
pchatchmod2e#1143

pchatchmod3#1143
pchatchmod3a#1142
pchatchmod3b#1142
pchatchmod3c#1145

pchatchmod4

#perform likelihood ratio test
lrtest(pchatchmod1,pchatchmod0)
#lrtest(pchatchmod4,pchatchmod2)#
#lrtest(pchatchmod4,pchatchmod1)
#lrtest(pchatchmod4,pchatchmod0)
#lrtest(pchatchmod2,pchatchmod1)

#adding random effects
mehatchmod <- glmer.nb(hatch_total~infected+PC1+(1|trial)+offset(log(FFnonaPC$egg_total)), data=FFnonaPC)

#look at realtion between HComp 1 and HComp=2
HCp<-ggplot(data=FFnonaPC, aes( y= HComp.2, x= HComp.1))+
       geom_point(aes(shape=factor(infected), color=factor(trial)))
HCp
# g<-g+ggtitle("Distribution of Number of Eggs Laid by Infection Status Each Week")+
#   xlab("Week") +
#   ylab("Number of Eggs Laid")
# g<-g+scale_fill_manual(values=c("#00CCFF", "#990000"),name="Infection Status", labels=c("Controls", "Infected"))
# #g<-g+ scale_color_discrete(name="Infection Status", labels=c("Controls", "Infected"))
# #g<-g+scale_x_continuous(breaks=seq(0, 38, 2))
# #g<-g+scale_x_discrete(labels=c("", seq())
# g
# 


# #ploting examples of residuals and model 
# testplot<-ggplot(aes( y= eggs, x= weeknum, na.rm=TRUE), 
#                data=Compile)+geom_point(data=Compile)
# testplot<-testplot+ggtitle("Number of Eggs Laid by Week and Infection Status") 
# testplot<-testplot+facet_grid(. ~infected)+geom_smooth(method= "lm")
# testplot
# 
# plot(residuals(nbmod13))


#####Likelihood ratio test example

#exp((AICmin-AICi)/2)=Probablity that i minimalizes information loss just as well as min.
#ln(prob)=AICmin-AICi/2
#2e^prob=difference between AICs.  
#if sign prob is 0.05 then
#2*(log(0.05) )
# thus a difference in AIC of 6 o greater is a significant similar probablity for now.
#test1<-exp((11824-11852)/2 )

#lrtest(nbmod1, nbmod2)
#lrtest(nbmod2,nbmod3)

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

#chisquare
#total of each
eggslaidtot<-sum(FFdataPC$egg_total, na.rm=TRUE)
sum(FFdataPC$hatch_total, na.rm=TRUE)
#total of infected
infecteds<-which(FFdataPC$infected==1)
infegg<-sum(FFdataPC$egg_total[infecteds], na.rm=TRUE)
infhat<-sum(FFdataPC$hatch_total[infecteds], na.rm=TRUE)
#total for controls
controls<-which(FFdataPC$infected==0)
cntegg<-sum(FFdataPC$egg_total[controls], na.rm=TRUE)
cnthat<-sum(FFdataPC$hatch_total[controls], na.rm=TRUE)
#eggs that did not hatch
infnhat<-infegg-infhat
cntnhat<-cntegg-cnthat 
#create table
infobs<-c(infhat, infnhat)
conobs<-c(cnthat, cntnhat)
conttab<-data.frame(infobs, conobs)
chisq.test(conttab)

#proportion test
#make conttab a table instead of data frame
conttab<-as.matrix(conttab)
#run the test
prop.test(conttab)


# use total number of hatched and not hatched respectively to calculate expected rates
rexphatch<-6670/9700
rexnohatch<-3030/9700
rexphatch+rexnohatch
#multiply the rates by the total number of infected bugs.  
iexhatch<-rexphatch*6958
iexnohatch<-rexnohatch*6958
cexhatch<-rexphatch*2742
cexnohatch<-rexnohatch*2742
(((4826-iexhatch)^2)/iexhatch)+(((2132-iexnohatch)^2)/iexnohatch)+
  (((1844-cexhatch)^2)/cexhatch)+(((898-cexnohatch)^2)/cexnohatch)

#chi square
infobs<-c(4826, 2132)
conobs<-c(1844, 898)
conttab<-data.frame(infobs, conobs)
chisq.test(conttab)
fisher.test(conttab)
