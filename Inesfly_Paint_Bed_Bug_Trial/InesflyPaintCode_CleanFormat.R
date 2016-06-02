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
#Surface for Dylan
#("C:/Users/tradylan/Documents/Laboratory/Inesfly_Paint_Bed_Bug_Trial")
#Envy for Dylan
setwd("C:/Users/dtrac/OneDrive/Documents/GitHub/Laboratory/Inesfly_Paint_Bed_Bug_Trial")
#MAC for Mike
#setwd("/Users/mzlevy/Laboratory/Inesfly_Paint_Bed_Bug_Trial")")


###############################################################################
#Tutorial on Cox Models from
# http://rstudio-pubs-static.s3.amazonaws.com/5896_8f0fed2ccbbd42489276e554a05af87e.html
#may also be helpful:
#https://stat.ethz.ch/education/semesters/ss2011/seminar/contents/presentation_3.pdf
data(package = "survival")

## Load lung data
data(lung)
## Show first 6 rows
head(lung)

#Each row is comprised of one insect, with the follow up duration (until death),
#and indicator if died or not (event=(status==2)
lung$SurvObj <- with(lung, Surv(time, status == 2))

?Surv


###############################################################################
###bring in data
###============================================================================

##Orignal Excel Document on Dropbox at 
##https://www.dropbox.com/s/2e1cibn7vls22cq/InesflyPilotData4_10_Updated_LM%20%281%29.xlsx?dl=0
##or ~Dropbox\Inesfly Paint for Bed Bugs\Data Entry\InesflyPilotData4_10_Updated_LM (1)
##data was saved as CSV then moved to repository.

#Pilot Data 
##Bring in Pilot Data
#Pilot1D <- read.csv("DATA/InesflyPilot_1D.csv")
#Pilot100D <- read.csv("DATA/InesflyPilot_100D.csv")
#250 Days <- read.csv("DATA/")
#1 Year <- read.csv("DATA/")

#Data For Full Study: Individual Data for 1 Month
#for exposure 1 day post paint with individual measurements
D1Ind <- read.csv("DATA/Inesfly_Ind_1D.csv")
#for exposure 90 day post paint with individual measurements
D90Ind <- read.csv("DATA/Inesfly_Ind_90DA.csv")
#for exposure 180 days post painting with indivual measurements
#D180Ind <- read.csv("DATA/Inesfly_Ind_180DA.csv")

#bring in paint data
PAINTDIST<- read.csv("DATA/InesflyPaintDistribution.csv")

#bring in temperature data
TEMPHUM<- read.csv("DATA/InesflyTempHum.csv")

###############################################################################
### Extract and Clean Individual Data and then Merge
###============================================================================

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
D1Jar$paint <- (c(1:length(D1Jar$Exposure))*0)
D1Jar$paint[igr] <- "5A"
D1Jar$paint[cloro] <- "CF"
D1Jar$paint[control] <- "CO"
#and by length of time
#make vector of indecies for each time
oneh <- grep("1",D1Jar$Exposure)
threeh <- grep("3", D1Jar$Exposure )
sixh <- grep("6", D1Jar$Exposure )
oned <- grep("24", D1Jar$Exposure )
#make a blank table
D1Jar$time <- c(1:length(D1Jar$Exposure))*0
#Insert the corresponding time into the table
D1Jar$time[oneh] <- 1
D1Jar$time[threeh] <- 3
D1Jar$time[sixh] <- 6
D1Jar$time[oned] <- 24

#Remove blank column
chop <- which(names(D1Ind)=="X")
D1Ind <- D1Ind[,-chop]

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
#lets make a copy of LD1Ind so that we have a clean and raw set
# Cl1Ind<-LD1Ind
# #If dead, then becomes knockdown mark as knock down.
# #create vector of unique days
# days<-unique(Cl1Ind$DAY)#made DAY below
# mday<-max(days)
# deadobs<-which(Cl1Ind$dead==1)
# nmax<-which(Cl1Ind$DAY < mday)
# testobs<-intersect(deadobs, nmax)
# Cl1Ind$CC<-Cl1Ind$STAGE*NA
# for(i in 1:length(testobs)){
#   d<-Cl1Ind$DAY[testobs[i]]
#   n<-which(days==d)
#   nx<-days[n+1]
#   ins<-Cl1Ind$INSECT[testobs[i]]
#   ains<-which(Cl1Ind$INSECT==ins)
#   anx<-which(Cl1Ind$DAY==nx)
#   nextobv<-intersect(ains, anx)
#   if(Cl1Ind$STATUS[testobs[i]] != Cl1Ind$STATUS[nextobv]){
#     Cl1Ind$STATUS[testobs[i]]<- Cl1Ind$STATUS[nextobv]
#     Cl1Ind$dead[testobs[i]]<- Cl1Ind$dead[nextobv]
#     Cl1Ind$alive[testobs[i]]<- Cl1Ind$alive[nextobv]
#     Cl1Ind$knockdown[testobs[i]]<- Cl1Ind$knockdown[nextobv]
#     Cl1Ind$unviable[testobs[i]]<- Cl1Ind$unviable[nextobv]
#     Cl1Ind$living[testobs[i]]<- Cl1Ind$living[nextobv]
#     Cl1Ind$CC[testobs[i]]<-"STATUS CHANGED IN CODE- Died later than originally recorded"
#   }
# }
# bug<-which(Cl1Ind$CC=="STATUS CHANGED IN CODE- Died later than originally recorded")
# Cl1Ind$INSECT[bug]
# #Check if these are data entry errors or other
# 
# 
# 
# Cl1Ind$INSECT[deadobs]
# if(Cl1Ind$DAY[deadobs]>mday){
#   if(Cl1$Ind)
# }


#also consider case where knock down went to alive.

#consider removing 2015-09-11 and 13 since data not available for 24hr.

#also add back collective data to 1H-5A 2015-09-2015

#==============================================================================

#Survival Analysis
#Create Kaplan Meier Curves
#Put date into numeric format
LD1Ind$julian<-julian(LD1Ind$DATE)
LD1Ind$DAY<-as.numeric(LD1Ind$julian-min(LD1Ind$julian))
#First create Survival object
Surv(time=LD1Ind$DAY, event=LD1Ind$dead)

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
sumpaint <- summaryBy(alive+dead+knockdown+unviable+living+totalbug~TREATMENT+DATE,
                      data=treatmentsum, FUN=sum,na.rm=TRUE, keep.names=TRUE)
#day
sumtdate <-summaryBy(alive+dead+knockdown+unviable+living+totalbug~DATE,
                     data=treatmentsum, FUN=sum,na.rm=TRUE, keep.names=TRUE)
##now add empty rows so you can join both tables together.
#treatment
sumpaint$EXPOSE<-sumpaint$alive*NA
#sumpaint$DATE<-sumpaint$alive*NA
#sumpaint$DATE<-as.Date(sumpaint$DATE, origin="1970-01-01")
sumpaint$TIME<-sumpaint$alive*NA
#DATE
sumtdate$EXPOSE<-sumtdate$alive*NA
sumtdate$TREATMENT<-sumtdate$alive*NA
sumtdate$TIME<-sumtdate$alive*NA

##join the three tables together
joined<-rbind(treatmentsum, sumtdate)
sumtab<-rbind(joined, sumpaint)

#make proportionality
treatmentsum$palive<-treatmentsum$alive/treatmentsum$totalbug
treatmentsum$pdead<-treatmentsum$dead/treatmentsum$totalbug
treatmentsum$pKD<-treatmentsum$knockdown/treatmentsum$totalbug
treatmentsum$pUV<- treatmentsum$unviable/treatmentsum$totalbug
treatmentsum$pliving<-treatmentsum$living/treatmentsum$totalbug

#==============================================================================
#Create Nice table with summary 

#==============================================================================
###Lets create curves showing the proportion of status seperated by treatment.
#Create indecies for each paint
fiveA <-which(treatmentsum$TREATMENT=="5A")
cnt <- which(treatmentsum$TREATMENT=="CO")
Clf <- which(treatmentsum$TREATMENT=="CF")

##Make plots
#pdf()
#5a
par(mfrow=c(1,3))
a<-ggplot(data= treatmentsum[fiveA,], aes( y=pdead , x= DATE, group= TIME, color=TIME, 
                                   na.rm=TRUE))+geom_line()+geom_point()
a<-a+ggtitle("5A-IGR")
a<-a+scale_fill_manual(values=c("blue", "red"))
a
#control
b<-ggplot(data= treatmentsum[cnt,], aes( y=pdead , x= DATE, group= TIME, color=TIME, 
                                   na.rm=TRUE))+geom_line()+geom_point()
b<-b+ggtitle("Control")
b<-b+scale_fill_manual(values=c("blue", "red"))
b
#cf
c<-ggplot(data= treatmentsum[Clf,], aes( y=pdead , x= DATE, group= TIME, color=TIME, 
                                   na.rm=TRUE))+geom_line()+geom_point()
c<-c+ggtitle("Clorofenapyr")
c<-c+scale_fill_manual(values=c("blue", "red"))
c
#dev.off()
#Clorofenapyr
par(mfrow=c(1,1))
##Lets plot death and unviable by time on each data set 
#DEATH
#pdf("TABLES_GRAPHS/DeathCurve_1DAY_PostPaint.pdf")
g<-ggplot(data= treatmentsum, aes( y=pdead , x= DATE, group= EXPOSE, color=TREATMENT,linetype= TIME, 
                                   na.rm=TRUE))+geom_line() +geom_point(aes(shape = TIME))
g<-g+ggtitle("Percentage Dead Over Time")+ylab("Percent Dead")
g<-g+scale_fill_manual(values=c("blue", "red"))
g
#dev.off()
#Unviable
#pdf("PerUV.pdf")
h<-ggplot(data= treatmentsum, aes(y = pUV , x = DATE, group = EXPOSE, color = TREATMENT, 
                             linetype = TIME,  na.rm=TRUE))+geom_line()+geom_point(aes(shape = TIME))
h<-h+ggtitle("Percentage Unviable Over Time")+ylab("Percentage Dead or Knockdown")
h<-h+scale_fill_manual(values=c("blue", "red"))
h
#dev.off()

pdf("TABLES_GRAPHS/SurvivalCurve_1DAY_PostPaint.pdf")
k<-ggplot(data= treatmentsum, aes( y=palive , x= DATE, group= EXPOSE, color=TREATMENT,linetype= TIME, 
                                   na.rm=TRUE))+geom_line() +geom_point(aes(shape = TIME))
k<-k+ggtitle("Percentage Alive Over Time")+ylab("Percent Alive")
k<-k+scale_fill_manual(values=c("blue", "red"))
k
#dev.off()
#Unviable
#pdf("PerUV.pdf")
l<-ggplot(data= treatmentsum, aes(y = pliving , x = DATE, group = EXPOSE, color = TREATMENT, 
                                  linetype = TIME,  na.rm=TRUE))+geom_line()+geom_point(aes(shape = TIME))
l<-l+ggtitle("Percentage Living Over Time")+ylab("Percentage Alive or Knockdown")
l<-l+scale_fill_manual(values=c("blue", "red"))
l
dev.off()

#==============================================================================
#90 Day Replicate
###bring in data
#for exposure 1 day post paint with individual measurements
D90Ind <- read.csv("DATA/Inesfly_Ind_90DA.csv")
#for exposure 1 day post paint with jar counts
#D90Jar <- read.csv("DATA/Inesfly_Jar_1D.csv")

#Split the unicode into relevant information for Individual observations
D90Ind$INSECT <- as.character(D90Ind$INSECT)
D90Ind$TIME <- substr(D90Ind$INSECT, 1, 3)
D90Ind$TREATMENT <- substr(D90Ind$INSECT, 5, 6)
D90Ind$QUAD <- substr(D90Ind$INSECT, 8, 8)
D90Ind$EXPOSE<-substr(D90Ind$INSECT, 1, 6)

#substring that character to split the exposure time by paint
igr <- grep("5A",D90Jar$Exposure)
cloro <- grep("CF", D90Jar$Exposure )
control <- grep("CO", D90Jar$Exposure )
D90Jar$paint <- (c(1:length(D90Jar$Exposure))*0)
D90Jar$paint[igr] <- "5A"
D90Jar$paint[cloro] <- "CF"
D90Jar$paint[control] <- "CO"
#and by length of time
#make vector of indecies for each time
oneh <- grep("1",D90Jar$Exposure)
threeh <- grep("3", D90Jar$Exposure )
sixh <- grep("6", D90Jar$Exposure )
oned <- grep("24", D90Jar$Exposure )
#make a blank table
D90Jar$time <- c(1:length(D1Jar$Exposure))*0
#Insert the corresponding time into the table
D90Jar$time[oneh] <- 1
D90Jar$time[threeh] <- 3
D90Jar$time[sixh] <- 6
D90Jar$time[oned] <- 24

#Remove blank column
#chop <- which(names(D90Ind)=="X")
#D90Ind <- D90Ind[,-chop]

LD90Ind<-melt(D90Ind, id=c("INSECT","STAGE_START","STAGE_END","TIME","EXPOSE", "QUAD", "NOTES",
                         "TREATMENT", "DAYS.SINCE.PAINT"))

#the variable needs to be turned into a date object
#so first make it a character
LD90Ind$variable<-as.character(LD90Ind$variable)
#remove the X's
LD90Ind$variable <- gsub("X","",LD90Ind$variable)
#replace the "." with "-"
LD90Ind$variable <- gsub("[.]","-", LD90Ind$variable)
#idk why dots are introduced in 90 but not 1, but lets remove the estras at ends
LD90Ind$variable<-substring(LD90Ind$variable, 2, 11)

LD90Ind$variable<-as.Date(LD90Ind$variable)

#to prevent confusion lets rename "variable" to "date"
chngname<-which(names(LD90Ind)=="variable")
names(LD90Ind)[chngname] <- "DATE"
#lets also rename "value" to "status"
chval<-which(names(LD90Ind)=="value")
names(LD90Ind)[chval] <- "STATUS"

#==============================================================================
###Now that we have the data in a usable table, lets split the status into binary
#find 
alive <- which(LD90Ind$STATUS== "A")
knockdown <- which(LD90Ind$STATUS=="K")
dead <- which(LD90Ind$STATUS=="D")
unviable <- c(dead, knockdown)
living <- c(alive, knockdown)

##create columns 
#make Quad numeric in order to create blank columns
LD90Ind$QUAD<-as.numeric(LD90Ind$QUAD)

#create blank columns for each status.
LD90Ind$alive <- LD90Ind$QUAD*0
LD90Ind$alive[alive] <- 1
LD90Ind$knockdown <- LD90Ind$QUAD*0
LD90Ind$knockdown[knockdown] <- 1
LD90Ind$dead <- LD90Ind$QUAD*0
LD90Ind$dead[dead] <- 1
LD90Ind$unviable <- LD90Ind$QUAD*0
LD90Ind$unviable[unviable] <- 1
LD90Ind$living <- LD90Ind$QUAD*0
LD90Ind$living[living] <- 1

#==============================================================================
##Lets clean up the data so that we get smooth transitions
#If dead, then becomes knockdown mark as knock down.

#also consider case where knock down went to alive.

#consider removing 2015-09-11 and 13 since data not available for 24hr.

#also add back collective data to 1H-5A 2015-09-2015

#==============================================================================
#lets do simple calculations finding the number and proportion by group

##Use the summary By funciton on Expose and Date to get counts
treatmentsum90<-summaryBy(alive+dead+knockdown+unviable+living~EXPOSE+DATE+
                          TIME+TREATMENT,data=LD90Ind, FUN=sum,na.rm=TRUE,
                        keep.names=TRUE)

#create column for total insect
treatmentsum90$totalbug<-(treatmentsum90$alive+treatmentsum90$unviable)

#remove rows with totalbug=0
nobug<-which(treatmentsum90$totalbug==0)
treatmentsum<-treatmentsum90[-nobug,]

##add rows for total on each treatment and the total for each day
#treatment
sumpaint90 <- summaryBy(alive+dead+knockdown+unviable+living+totalbug~TREATMENT+DATE,
                      data=treatmentsum90, FUN=sum,na.rm=TRUE, keep.names=TRUE)
#day
sumtdate90 <-summaryBy(alive+dead+knockdown+unviable+living+totalbug~DATE,
                     data=treatmentsum90, FUN=sum,na.rm=TRUE, keep.names=TRUE)
##now add empty rows so you can join both tables together.
#treatment
sumpaint90$EXPOSE<-sumpaint90$alive*NA
#sumpaint90$DATE<-sumpaint90$alive*NA
#sumpaint90$DATE<-as.Date(sumpaint90$DATE, origin="1970-01-01")
sumpaint90$TIME<-sumpaint90$alive*NA
#DATE
sumtdate90$EXPOSE<-sumtdate90$alive*NA
sumtdate90$TREATMENT<-sumtdate90$alive*NA
sumtdate90$TIME<-sumtdate90$alive*NA

##join the three tables together
joined90<-rbind(treatmentsum90, sumtdate90)
sumtab90<-rbind(joined90, sumpaint90)

#make proportionality
treatmentsum90$palive<-treatmentsum90$alive/treatmentsum90$totalbug
treatmentsum90$pdead<-treatmentsum90$dead/treatmentsum90$totalbug
treatmentsum90$pKD<-treatmentsum90$knockdown/treatmentsum90$totalbug
treatmentsum90$pUV<- treatmentsum90$unviable/treatmentsum90$totalbug
treatmentsum90$pliving<-treatmentsum90$living/treatmentsum90$totalbug

###Lets create curves showing the proportion of status seperated by treatment.
#Create indecies for each paint
fiveA <-which(treatmentsum90$TREATMENT=="5A")
cnt <- which(treatmentsum90$TREATMENT=="CO")
Clf <- which(treatmentsum90$TREATMENT=="CF")

##Make plots
#pdf()
#5a
par(mfrow=c(1,3))
m<-ggplot(data= treatmentsum90[fiveA,], aes( y=pdead , x= DATE, group= TIME, color=TIME, 
                                           na.rm=TRUE))+geom_line()+geom_point()
m<-m+ggtitle("5A-IGR")
m<-m+scale_fill_manual(values=c("blue", "red"))
m
#control
n<-ggplot(data= treatmentsum90[cnt,], aes( y=pdead , x= DATE, group= TIME, color=TIME, 
                                         na.rm=TRUE))+geom_line()+geom_point()
n<-n+ggtitle("Control")
n<-n+scale_fill_manual(values=c("blue", "red"))
n
#cf
o<-ggplot(data= treatmentsum90[Clf,], aes( y=pdead , x= DATE, group= TIME, color=TIME, 
                                         na.rm=TRUE))+geom_line()+geom_point()
o<-o+ggtitle("Clorofenapyr")
o<-o+scale_fill_manual(values=c("blue", "red"))
o
#dev.off()
#Clorofenapyr
par(mfrwo=c(1,1))
##Lets plot death and unviable by time on each data set 
#DEATH
pdf("TABLES_GRAPHS/DeathCurve_90DAYs_PostPaint.pdf")
p<-ggplot(data= treatmentsum90, aes( y=pdead , x= DATE, group= EXPOSE, color=TREATMENT,linetype= TIME, 
                                   na.rm=TRUE))+geom_line() +geom_point(aes(shape = TIME))
p<-p+ggtitle("Percentage Dead Over Time")+ylab("Percent Dead")
p<-p+scale_fill_manual(values=c("blue", "red"))
p
#dev.off()
#Unviable
#pdf("PerUV.pdf")
r<-ggplot(data= treatmentsum90, aes(y = pUV , x = DATE, group = EXPOSE, color = TREATMENT, 
                                  linetype = TIME,  na.rm=TRUE))+geom_line()+geom_point(aes(shape = TIME))
r<-r+ggtitle("Percentage Unviable Over Time")+ylab("Percentage Dead or Knockdown")
r<-r+scale_fill_manual(values=c("blue", "red"))
h
dev.off()

pdf("TABLES_GRAPHS/SurvivalCurve_90DAY_PostPaint.pdf")
s<-ggplot(data= treatmentsum90, aes( y=palive , x= DATE, group= EXPOSE, color=TREATMENT,linetype= TIME, 
                                   na.rm=TRUE))+geom_line() +geom_point(aes(shape = TIME))
s<-s+ggtitle("Percentage Alive Over Time")+ylab("Percent Alive")
s<-s+scale_fill_manual(values=c("blue", "red"))
s
#dev.off()
#Unviable
#pdf("PerUV.pdf")
t<-ggplot(data= treatmentsum90, aes(y = pliving , x = DATE, group = EXPOSE, color = TREATMENT, 
                                  linetype = TIME,  na.rm=TRUE))+geom_line()+geom_point(aes(shape = TIME))
t<-t+ggtitle("Percentage Living Over Time")+ylab("Percentage Alive or Knockdown")
t<-t+scale_fill_manual(values=c("blue", "red"))
t
dev.off()