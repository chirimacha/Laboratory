###This code cleans up the data for the bed bug inesfly paint study
## Review Read me at ____

###############################################################################
###GENERAL SET UP###
###============================================================================
##Install and load necessary packages
#Install packages
#install.packages(c("reshape","survival","tables", "doBy", "ggplot2"))
#
#load packages
library(reshape) #Used to change data between short and long formats.
library(tables)
library(doBy)#use summaryBy function
library(ggplot2)

##set up the working directory
#Surface for Dylan
#("C:/Users/tradylan/Documents/Laboratory/Inesfly_Paint_Bed_Bug_Trial")
#Envy for Dylan
setwd("C:/Users/dtracy198/Documents/GitHub/Laboratory/Inesfly_Paint_Bed_Bug_Trial")
#MAC for Mike
#setwd("/Users/mzlevy/Laboratory/Inesfly_Paint_Bed_Bug_Trial")

###############################################################################
###bring in data
###============================================================================

##Orignal Excel Document on Dropbox at 
##https://www.dropbox.com/s/2e1cibn7vls22cq/InesflyPilotData4_10_Updated_LM%20%281%29.xlsx?dl=0
##or ~Dropbox\Inesfly Paint for Bed Bugs\Data Entry\InesflyPilotData4_10_Updated_LM (1)
##data was saved as CSV then moved to repository.

#Data For Full Study: Individual Data for 1 Month (4 weeks)
#for exposure 1 day post paint with individual measurements
D1FSA <- read.csv("DATA/1DFSA.csv")
D1FSB <- read.csv("DATA/1DFSB.csv")

#for exposure 90 day post paint with individual measurements
D90FSA <- read.csv("DATA/90DFSA.csv")
D90FSB <- read.csv("DATA/90DFSB.csv")

#for exposure 180 days post painting with indivual measurements
D180FSA <- read.csv("DATA/180DFSA.csv")
D180FSB <- read.csv("DATA/180DFSB.csv")

#bring in paint data
PAINTDIST<- read.csv("DATA/InesflyPaintDistribution.csv")

#bring in temperature data
TEMPHUM<- read.csv("DATA/TempHumData.csv")

###Put data set names into a vector so they can be looped through
datasets <- c("D1FSA","D1FSB", "D90FSA", "D90FSB", "D180FSA", "D180FSB")

###investigate duplicate rows
#found 06H-CO-4-04 error during analysis
#error was a label issue
D1FSA$INSECT <- as.character(D1FSA$INSECT)
D1FSB$INSECT <- as.character(D1FSB$INSECT)

D1FSA$INSECT[276] <-"06H-CO-4-04"
D1FSB$INSECT[276] <-"06H-CO-4-04"

D1FSA$INSECT <- as.factor(D1FSA$INSECT)
D1FSB$INSECT <- as.factor(D1FSB$INSECT)

#Check for duplicates
CheckDupInsect<- function(raw.data){
  insects <- unique(raw.data$INSECT)
  dup <- 1:length(insects)*0
  dup.insects <- data.frame(insects, dup)
  for(i in 1:length(insects)){
    indexes <- which(raw.data$INSECT == insects[i])
    dup.insects$dup[i] <- length(indexes)
  }
  dups <- which(dup.insects$dup > 1)
  output <- dup.insects[dups,]
  output
}

CheckDupInsect(D1FSA)
CheckDupInsect(D90FSA)
CheckDupInsect(D180FSA)

###dimensions
#remove any unused rows
removeblank<-function(dataframe){
  nulls<-which(dataframe$INSECT=="")
  if( length(nulls) > 0) {
    override <- dataframe[-nulls,]
    dataframe<-override
  } 
  dataframe
}

#applie over all functions
for(i in 1:length(datasets)){
  reduced<-removeblank(get(datasets[i]))
  assign(datasets[i], reduced)
}

dimequal <-data.frame()

for(j in 1:2){
  if (dim(get(datasets[1]))[j] == dim(get(datasets[2]))[j]){print(TRUE)} else{print(FALSE)}
  if (dim(get(datasets[3]))[j] == dim(get(datasets[4]))[j]){print(TRUE)} else{print(FALSE)}
  if (dim(get(datasets[5]))[j] == dim(get(datasets[6]))[j]){print(TRUE)} else{print(FALSE)}
}

###4=False means that dataset 1 and 2 have different columns
names(get(datasets[1]))
names(get(datasets[2]))

###X1 should be "Days.Post.Painting"
dsp<-which(names(get(datasets[2]))=="X1")
names(D1FSB)[dsp] <- "Days.Post.Painting"

###X should be removed
xrm<-which(names(get(datasets[2]))=="X")
D1FSB<-D1FSB[,-xrm]

#################################################################
###Now that the big each have the right dimensions, lets check individual values

###Lets check Stage_start
D1FSA$STAGE_START<-as.character(D1FSA$STAGE_START)
D1FSB$STAGE_START<-as.character(D1FSB$STAGE_START)
D180FSA$STAGE_START<-as.character(D180FSA$STAGE_START)
D180FSB$STAGE_START<-as.character(D180FSB$STAGE_START)

errors1<-which(D1FSA$STAGE_START != D1FSB$STAGE_START)
errors2<-which(D90FSA$STAGE_START != D90FSB$STAGE_START)
errors3<-which(D180FSA$STAGE_START != D180FSB$STAGE_START)

###an AM. is introduced #error 3 below
pe<-which(D1FSB$STAGE_START=="AM.")
D1FSB$STAGE_START[pe]<-"AM"

###lets take a look at the error data

D1stageError<-data.frame(D1FSA$INSECT[errors1], D1FSA$STAGE_START[errors1], D1FSB$STAGE_START[errors1], 
                         D1FSA$STAGE_END[errors1], D1FSB$STAGE_END[errors1], D1FSA$NOTES[errors1], D1FSB$NOTES[errors1])

D90stageError<-data.frame(D90FSA$INSECT[errors2], D90FSA$STAGE_START[errors2], D90FSB$STAGE_START[errors2], 
                          D90FSA$STAGE_END[errors2], D90FSB$STAGE_END[errors2], D90FSA$NOTES[errors2], D90FSB$NOTES[errors2])

D180stageError<-data.frame(D180FSA$INSECT[errors3], D180FSA$STAGE_START[errors3], D180FSB$STAGE_START[errors3], 
                           D180FSA$STAGE_END[errors3], D180FSA$NOTES[errors3])

####Fixing D1 Stage Start Errors
#12 errors
#error 1:  5 not AF
sra <- errors1[1] 
D1FSA$STAGE_START[sra] <- "5"
D1FSA$STAGE_END[sra] <- "5"
#error 2: Molt 4 to 5
srb <- errors1[2] 
D1FSB$STAGE_START[srb] <- "4"
#error 3: Point added, fixed above
#error 4:
src <- errors1[4] 
D1FSA$STAGE_START[src] <- "5"
D1FSA$STAGE_END[src] <- "5"
#error 5: say comment that it laid eggs so assumed AF. 
#But note was ment for bug under it.
srd <- errors1[5] 
D1FSA$STAGE_START[srd] <- "5"
D1FSA$STAGE_END[srd] <- "5"
# D1FSA$NOTES[srd+1] <- D1FSA$NOTES[srd+1]
# D1FSA$NOTES[srd] <- ""
#error 6
sre<-errors1[6] 
D1FSA$STAGE_START[sre]<-"AF"
#error 7: Entered stages for quadrant 4 not 3
srf<-errors1[7] 
D1FSA$STAGE_START[srf]<-"4"
D1FSA$STAGE_END[srf]<-"4"
#error 8
srg<-errors1[8] 
D1FSA$STAGE_START[srg]<-"4"
D1FSA$STAGE_END[srg]<-"4"
#error 9
srh<-errors1[9] 
D1FSA$STAGE_START[srh]<-"AM"
D1FSA$STAGE_END[srh]<-"AM"
#error 10
sri<-errors1[10] 
D1FSA$STAGE_START[sri]<-"AM"
D1FSA$STAGE_END[sri]<-"AM"
#error 11
srj<-errors1[11] 
D1FSA$STAGE_START[srj]<-"5"
D1FSA$STAGE_END[srj]<-"5"
#error 12
srk<-errors1[12] 
D1FSA$STAGE_START[srk]<-"AM"
D1FSA$STAGE_END[srk]<-"AM"


####Fixing D90 Stage Start Errors
#8 errors
#error 1: did not molt
e1_90 <-errors2[1] 
D90FSA$STAGE_START[e1_90]<-"5"
#  #error 2:
e2_90 <-errors2[2] 
D90FSA$STAGE_START[e2_90] <- "4"
D90FSA$STAGE_END[e2_90] <- "4"
#error 3 Marked AM when note denotes that it molted
e3_90<-which(D90FSA$INSECT=="03H-CO-4-08")
D90FSA$STAGE_START[e3_90] <- "5"
#  #error 4: Died in Molt, so 4
e4_90 <-errors2[4] 
D90FSA$STAGE_START[e4_90] <- "4"
D90FSA$STAGE_END[e4_90] <- "4"
#  #error 5: Marked as AF on B when it molted
e5_90<-which(D90FSB$INSECT=="24H-CO-3-4")
D90FSB$STAGE_START[e5_90] <- "5"
#error 6:
e6_90<-which(D90FSB$INSECT=="24H-CO-3-6")
D90FSB$STAGE_START[e6_90] <- "5"
#error 7:switch molt with 7 and 8
e7_90<-errors2[7]
D90FSB$STAGE_START[e7_90] <- "AF"
#error 8:
e8_90<-which(D90FSB$INSECT=="24H-CO-4-4")
D90FSB$STAGE_START[e8_90] <- "5"

####Fixing D180 Stage Start Errors
#4 errors
#error 1: Molted, so should be 5 for FSB
eo<-errors3[1] 
D180FSB$STAGE_START[eo]<-"5"
#error 2: Entered incorrectly on FSA as AF
et<-errors3[2]  
D180FSA$STAGE_START[et] <- "AM"
D180FSA$STAGE_END[et] <- "AM"

#error 3: Molted 
bm<-errors3[3]  
D180FSB$STAGE_START[bm]<-"5"
#error 4: Just an extra space in D180FSB
dk<-errors3[4]  
D180FSB$STAGE_START[dk]<-"AM"


###Lets check to make sure all errors were corrected  
errors1a<-which(D1FSA$STAGE_START != D1FSB$STAGE_START) #3 not fixed (e2,4,5)
errors2a<-which(D90FSA$STAGE_START != D90FSB$STAGE_START) #good to go
errors3a<-which(D180FSA$STAGE_START != D180FSB$STAGE_START)

#############################################################################  
###for stage_end
D1FSA$STAGE_END<-as.character(D1FSA$STAGE_END)
D1FSB$STAGE_END<-as.character(D1FSB$STAGE_END)
D90FSA$STAGE_END<-as.character(D90FSA$STAGE_END)
D90FSB$STAGE_END<-as.character(D90FSB$STAGE_END)
D180FSA$STAGE_END<-as.character(D180FSA$STAGE_END)
D180FSB$STAGE_END<-as.character(D180FSB$STAGE_END)

eerrors1<-which(D1FSA$STAGE_END != D1FSB$STAGE_END)
eerrors2<-which(D90FSA$STAGE_END != D90FSB$STAGE_END)
eerrors3<-which(D180FSA$STAGE_END != D180FSB$STAGE_END)  

#Tables to visualize errors
D1stageErrorE<-data.frame(D1FSA$INSECT[eerrors1], D1FSA$STAGE_START[eerrors1], D1FSB$STAGE_START[eerrors1], 
                          D1FSA$STAGE_END[eerrors1], D1FSB$STAGE_END[eerrors1], D1FSA$NOTES[eerrors1], D1FSB$NOTES[eerrors1])

D90stageErrorE<-data.frame(D90FSA$INSECT[eerrors2], D90FSA$STAGE_START[eerrors2], D90FSB$STAGE_START[eerrors2], 
                           D90FSA$STAGE_END[eerrors2], D90FSB$STAGE_END[eerrors2], D90FSA$NOTES[eerrors2], D90FSB$NOTES[eerrors2])

D180stageErrorE<-data.frame(D180FSA$INSECT[eerrors3], D180FSA$STAGE_START[eerrors3], D180FSB$STAGE_START[eerrors3], 
                            D180FSA$STAGE_END[eerrors3],D180FSB$STAGE_END[eerrors3], D180FSA$NOTES[eerrors3])

###Errors for 1
## 3 errors
#error 1: Extra Space
D1FSB$STAGE_END[eerrors1[1]] <- "AM" 
#error 2: Comma introduced
D1FSB$STAGE_END[eerrors1[2]] <- "AM" 
#error 3: SM instead of Am
D1FSB$STAGE_END[eerrors1[3]] <- "AM" 

###Errors for 90
## 5 errors
#error 1: Died in molt so should be 5 for FSA
D90FSA$STAGE_END[eerrors2[1]]<-"5"
D90FSB$STAGE_END[eerrors2[c(4,5)]] <- "5"
#error 2: space?
D90FSA$STAGE_END[eerrors2[2]] <- "AM"
#error 3: Died in molth so record as 5
D90FSA$STAGE_END[eerrors2[3]] <- "5"
#errors 4 and 5 are missing values
D90FSA$STAGE_END[eerrors2[4]] <- "5"
D90FSB$STAGE_END[eerrors2[4]] <- "5"
D90FSB$STAGE_END[eerrors2[5]] <- "5"
D90FSA$STAGE_END[eerrors2[5]] <- "5"

###Errors for 180
## 10 errors
#error1: No entry. Stuck in molt?
D180FSB$STAGE_END[eerrors3[1]] <- "5"
#error2: No entry, died in molt
D180FSB$STAGE_END[eerrors3[2]] <- "5"
#error3: Died in molt
D180FSA$STAGE_END[eerrors3[3]] <- "5"
#error4: checked jar record, said most likely male
D180FSA$STAGE_END[eerrors3[4]] <- "AM"
D180FSB$STAGE_END[eerrors3[4]] <- "AM"
D180FSA$NOTES<-as.character(D180FSA$NOTES)
D180FSA$NOTES[eerrors3[4]] <- "Checked jar record; most likely male"
#error5: type error
D180FSB$STAGE_END[eerrors3[5]] <- "5"
#error6: added an extra A
D180FSB$STAGE_END[eerrors3[6]] <- "AM"
#error7: added 0 to 5
D180FSB$STAGE_END[eerrors3[7]] <- "5"
#error8:  #Unidentified(checked jar record)
D180FSA$STAGE_END[eerrors3[8]] <- "AU"
D180FSB$STAGE_END[eerrors3[8]] <- "AU"
D180FSA$NOTES[eerrors3[8]] <- "Adult; sex unidentified"
#error9: not entered
D180FSA$STAGE_END[eerrors3[9]] <- "5"
D180FSB$STAGE_END[eerrors3[9]] <- "5"

###Check that the no errors are occuring anymore
eerrors1a<-which(D1FSA$STAGE_END != D1FSB$STAGE_END)  # no errors
eerrors2a<-which(D90FSA$STAGE_END != D90FSB$STAGE_END) #3 errors
eerrors3a<-which(D180FSA$STAGE_END != D180FSB$STAGE_END)  

##############################################################################
###Now to clean up the Living Status data
##D1FS
inderrors1_1<-which(D1FSA$X2015.09.11 != D1FSB$X2015.09.11)
inderrors2_1<-which(D1FSA$X2015.09.12 != D1FSB$X2015.09.12)
inderrors3_1<-which(D1FSA$X2015.09.13 != D1FSB$X2015.09.13)
inderrors4_1<-which(D1FSA$X2015.09.18 != D1FSB$X2015.09.18)
inderrors5_1<-which(D1FSA$X2015.09.25 != D1FSB$X2015.09.25)
inderrors6_1<-which(D1FSA$X2015.10.02 != D1FSB$X2015.10.02)
inderrors7_1<-which(D1FSA$X2015.10.09 != D1FSB$X2015.10.09)


##9/11 errors 
#1 error  
error<-rbind(D1FSA[inderrors1_1,], D1FSB[inderrors1_1,])
#K should be A
D1FSA$X2015.09.11[inderrors1_1] <- "A"
##9/12 errors #mostly due to entering in 3's data again
error2<-rbind(D1FSA[inderrors2_1,], D1FSB[inderrors2_1,])
#4 errors
#error 1
D1FSA$X2015.09.12[inderrors2_1[1]] <- "K"
D1FSA$X2015.09.18[inderrors2_1[1]] <- "K"

#error 2
D1FSA$X2015.09.12[inderrors2_1[2]] <- "K"
D1FSA$X2015.09.13[inderrors2_1[2]] <- "K"

#error 3
D1FSA$X2015.09.12[inderrors2_1[3]] <- "K"
D1FSA$X2015.09.13[inderrors2_1[3]] <- "K"

#error 4
D1FSA$X2015.09.12[inderrors2_1[4]] <- "K"

##9/13 errors
error3<-rbind(D1FSA[inderrors3_1,], D1FSB[inderrors3_1,])
#5 errors #2 fixed above
#error1
D1FSB$X2015.09.13[inderrors3_1[1]] <- "K"
#error2
D1FSA$X2015.09.13[inderrors3_1[2]] <- "K"
#errors 3 and 4 are fixed above
#error 5
D1FSA$X2015.09.13[inderrors3_1[5]] <- "K"

##9/18 errors
#2 errors 364
error4<-rbind(D1FSA[inderrors4_1,], D1FSB[inderrors4_1,])
#error1 already corrected above
#error2:
D1FSA$X2015.09.18[inderrors4_1[2]] <- "D"

##9/25 errors#No errors :)
##10/2 errors
#1 error
D1FSB$X2015.10.02[inderrors6_1] <- "K"
D1FSB$X2015.10.09[inderrors6_1] <- "D"

##10/9 errors
#3 error #1 fixed above
error7<-rbind(D1FSA[inderrors7_1,], D1FSB[inderrors7_1,])
#error 1 
D1FSA$X2015.10.09[inderrors7_1[1]] <- "D"
#error 2
D1FSA$X2015.10.09[inderrors7_1[2]] <- "D"
#error 3 fixed above



inderrors1_1a<-which(D1FSA$X2015.09.11 != D1FSB$X2015.09.11)
inderrors2_1a<-which(D1FSA$X2015.09.12 != D1FSB$X2015.09.12)
inderrors3_1a<-which(D1FSA$X2015.09.13 != D1FSB$X2015.09.13) #
inderrors4_1a<-which(D1FSA$X2015.09.18 != D1FSB$X2015.09.18)
inderrors5_1a<-which(D1FSA$X2015.09.25 != D1FSB$X2015.09.25)
inderrors6_1a<-which(D1FSA$X2015.10.02 != D1FSB$X2015.10.02)
inderrors7_1a<-which(D1FSA$X2015.10.09 != D1FSB$X2015.10.09)

####  

##D90FS
inderrors1_90<-which(D90FSA$X.2015.12.10. != D90FSB$X.2015.12.10.)  #6 errors
#in FSA 242 is blank.
D90FSA$X.2015.12.16.[242] <- D90FSB$X.2015.12.16.[242]
D90FSA$X.2015.12.16. <- as.character(D90FSA$X.2015.12.16.)
D90FSB$X.2015.12.16. <- as.character(D90FSB$X.2015.12.16.)
inderrors2_90<-which(D90FSA$X.2015.12.16. != D90FSB$X.2015.12.16.)#7
#went home for Christmas so it was one day ahead of schedule. Should be 12/22
names(D90FSA)[6] <- "X.2015.12.22."
names(D90FSB)[6] <- "X.2015.12.22."
inderrors3_90<-which(D90FSA$X.2015.12.22. != D90FSB$X.2015.12.22.)#5
inderrors4_90<-which(D90FSA$X.2015.12.30. != D90FSB$X.2015.12.30.)#2
inderrors5_90<-which(D90FSA$X.2016.01.06. != D90FSB$X.2016.01.06.)#4

#errors on 12/10 (6 total)
#first error orignal data shows all observations are dead
D90FSA$X.2015.12.10.[inderrors1_90[1]] <- "D"
#error 2: same error as above
D90FSA$X.2015.12.10.[inderrors1_90[2]] <- "D"
#error 3: same mistake
D90FSA$X.2015.12.10.[inderrors1_90[3]] <- "D"
#error 4: Should be K
D90FSA$X.2015.12.10.[inderrors1_90[4]] <- "K"
#error 5: 
D90FSA$X.2015.12.10.[inderrors1_90[5]] <- "K"
D90FSA$X.2015.12.16.[inderrors1_90[5]] <- "K"
D90FSA$X.2015.12.22.[inderrors1_90[5]] <- "K"
#also found errors in 03-51-4-11
D90FSA$X.2015.12.16.[inderrors1_90[5]-1] <- "K"
D90FSA$X.2015.12.22.[inderrors1_90[5]-1] <- "K"
D90FSA$X.2015.12.30.[inderrors1_90[5]-1] <- "K"
#error 6: B should be K
D90FSB$X.2015.12.10.[inderrors1_90[6]] <- "K"

#12/16 Errors (5 and 6 already solved above)

#Error 1: Knock down from 12/10 to 12/22 on orignal.B is incorreect
D90FSB$X.2015.12.16.[inderrors2_90[1]] <- "K" 
D90FSB$X.2015.12.22.[inderrors2_90[1]] <- "K" 
#error 2: FSA should be K
D90FSA$X.2015.12.16.[inderrors2_90[2]] <- "K" 
#error 3:Only one knock down observation recorded
D90FSA$X.2015.12.16.[inderrors2_90[3]] <- "D" 
D90FSA$X.2015.12.22.[inderrors2_90[3]] <- "D" 
#error 4: No K observations
D90FSA$X.2015.12.16.[inderrors2_90[4]] <- "D" 
#error 7: Not all A as FSA shows
D90FSA$X.2015.12.16.[inderrors2_90[7]] <- "D" 
D90FSA$X.2015.12.22.[inderrors2_90[7]] <- "K" 
D90FSA$X.2015.12.30.[inderrors2_90[7]] <- "D" 
D90FSA$X.2016.01.06.[inderrors2_90[7]] <- "K" 

#12/22 and 12/30 Errors all fixed above
#1/06 errors(error 1 fixed above)
#error2: Dead on final observation
D90FSB$X.2016.01.06.[inderrors5_90[2]] <- "D" 
#error3
D90FSA$X.2016.01.06.[inderrors5_90[3]] <- "D" 
#error4
D90FSA$X.2016.01.06.[inderrors5_90[4]] <- "D" 


#recheck for errors
inderrors1_90a <- which(D90FSA$X.2015.12.10. != D90FSB$X.2015.12.10.)# 0 :)
inderrors2_90a <- which(D90FSA$X.2015.12.16. != D90FSB$X.2015.12.16.)#7
inderrors3_90a <- which(D90FSA$X.2015.12.22. != D90FSB$X.2015.12.22.)#5
inderrors4_90a <- which(D90FSA$X.2015.12.30. != D90FSB$X.2015.12.30.)#2
inderrors5_90a <- which(D90FSA$X.2016.01.06. != D90FSB$X.2016.01.06.)#4

####

##D180FS
names(D180FSA)[4:8] <- c("X.2016.03.09.","X.2016.03.15.", "X.2016.03.22.",
                       "X.2016.03.29.", "X.2016.04.05.")
names(D180FSB)[4:8] <- c("X.2016.03.09.","X.2016.03.15.", "X.2016.03.22.",
                       "X.2016.03.29.", "X.2016.04.05.")
inderrors1_180 <- which(D180FSA$X.2016.03.09. != D180FSB$X.2016.03.09.)#3 
inderrors2_180 <- which(D180FSA$X.2016.03.15. != D180FSB$X.2016.03.15.)#3 (81)
inderrors3_180 <- which(D180FSA$X.2016.03.22. != D180FSB$X.2016.03.22.)#4 (81,127)
inderrors4_180 <- which(D180FSA$X.2016.03.29. != D180FSB$X.2016.03.29.)#1 (311)
inderrors5_180 <- which(D180FSA$X.2016.04.05. != D180FSB$X.2016.04.05.)#1  ##D90FS

#errors on 3/9
D180FSA[inderrors1_180[1],] 
D180FSB[inderrors1_180[1],] 

#Error 1: all observations are dead
D180FSB$X.2016.03.09.[inderrors1_180[1]] <- "D" 
D180FSB$X.2016.03.15.[inderrors1_180[1]] <- "D" 
D180FSB$X.2016.03.22.[inderrors1_180[1]] <- "D" 
#Error 2: Should we K on FSA
D180FSA$X.2016.03.09.[inderrors1_180[2]] <- "K" 
#Error 3: Should be all dead
D180FSB$X.2016.03.09.[inderrors1_180[3]] <- "D" 

#Errors on 3/15 (Error 1 solved above)
#error 2: should be as FSA: 
D180FSB$X.2016.03.15.[inderrors2_180[2]] <- "D" 
D180FSB$X.2016.03.22.[inderrors2_180[2]] <- "D" 
#error 3
D180FSB$X.2016.03.15.[inderrors2_180[3]] <- "D" 
D180FSB$X.2016.03.22.[inderrors2_180[3]] <- "D" 
D180FSB$X.2016.03.29.[inderrors2_180[3]] <- "D" 

#Errors on 3/22 (Errors 1,3, and 4 solved above)
#error2:
D180FSB$X.2016.03.22.[inderrors3_180[2]] <- "D" 
#Errors from 3/29 all solved

#Errors from 4/5
D180FSA$X.2016.04.05.[inderrors5_180[1]] <- "A" 


inderrors1_180a <- which(D180FSA$X.2016.03.09. != D180FSB$X.2016.03.09.)#3 
inderrors2_180a <- which(D180FSA$X.2016.03.15. != D180FSB$X.2016.03.15.)#3 (81)
inderrors3_180a <- which(D180FSA$X.2016.03.22. != D180FSB$X.2016.03.22.)#81,127
inderrors4_180a <- which(D180FSA$X.2016.03.29. != D180FSB$X.2016.03.29.)#1(311)
inderrors5_180a <- which(D180FSA$X.2016.04.05. != D180FSB$X.2016.04.05.)#1  
###############################################################################
### Extract and Clean Individual Data and then Merge
###============================================================================
###Split the unicode into relevant information for Individual observations
#first make name for days since painting consistent
names(D1FSA)[12] <- "days.after.paint"
names(D90FSA)[10] <- "days.after.paint"
names(D180FSA)[10] <- "days.after.paint"

SplitUnicode <- function(df) {
df$INSECT <- as.character(df$INSECT)
df$exp.time <- substr(df$INSECT, 1, 3)
df$paint <- substr(df$INSECT, 5, 6)
df$quad <- substr(df$INSECT, 8, 8)
#df$NUM <- substr(df$INSECT, 10, 11)
df$exposure <- substr(df$INSECT, 1, 6)
df$treatment <- paste(substr(df$INSECT, 1, 6), df$days.after.paint, sep = "-")
return(df)
}

D1FSA <- SplitUnicode(D1FSA)
D90FSA <- SplitUnicode(D90FSA)
D180FSA <- SplitUnicode(D180FSA)

###Last cleaning checks
##If dead resurect 
#1 day
dead1.1 <- which(D1FSA$X2015.09.11 == "D")
livin2.1 <- which(D1FSA$X2015.09.12 != "D")
dead2.1 <- which(D1FSA$X2015.09.12 == "D")
livin3.1 <- which(D1FSA$X2015.09.13 != "D")
dead3.1 <- which(D1FSA$X2015.09.13 == "D")
livin4.1 <- which(D1FSA$X2015.09.18 != "D")
dead4.1<- which(D1FSA$X2015.09.18 == "D")
livin5.1 <- which(D1FSA$X2015.09.25 != "D")
dead5.1 <- which(D1FSA$X2015.09.25 == "D")
livin6.1 <- which(D1FSA$X2015.10.02 != "D")
dead6.1 <- which(D1FSA$X2015.10.02 == "D")
livin7.1 <- which(D1FSA$X2015.10.09 != "D")

resurect2.1 <- intersect(dead1.1, livin2.1)
resurect3.1 <- intersect(dead2.1, livin3.1)
resurect4.1 <- intersect(dead3.1, livin4.1)
resurect5.1 <- intersect(dead4.1, livin5.1)
resurect6.1 <- intersect(dead5.1, livin6.1)
resurect7.1 <- intersect(dead6.1, livin7.1)

D1FSA$X2015.09.11[resurect2.1] <- "K"
D1FSA$X2015.09.12[resurect3.1] <- "K"
D1FSA$X2015.09.13[resurect4.1] <- "K"
D1FSA$X2015.09.18[resurect5.1] <- "K"
D1FSA$X2015.09.25[resurect6.1] <- "K"

#Only 1 is K then D. Others for this date are not recorded
last <- length(D1FSA$X2015.10.02[resurect7.1])
D1FSA$X2015.10.02[resurect7.1[last]] <- "K"

#
#1 day
dead1.1 <- which(D1FSA$X2015.09.11 == "D")
livin2.1 <- which(D1FSA$X2015.09.12 != "D")
dead2.1 <- which(D1FSA$X2015.09.12 == "D")
livin3.1 <- which(D1FSA$X2015.09.13 != "D")
dead3.1 <- which(D1FSA$X2015.09.13 == "D")
livin4.1 <- which(D1FSA$X2015.09.18 != "D")
dead4.1<- which(D1FSA$X2015.09.18 == "D")
livin5.1 <- which(D1FSA$X2015.09.25 != "D")
dead5.1 <- which(D1FSA$X2015.09.25 == "D")
livin6.1 <- which(D1FSA$X2015.10.02 != "D")
dead6.1 <- which(D1FSA$X2015.10.02 == "D")
livin7.1 <- which(D1FSA$X2015.10.09 != "D")

resurect2.1 <- intersect(dead1.1, livin2.1)
resurect3.1 <- intersect(dead2.1, livin3.1)
resurect4.1 <- intersect(dead3.1, livin4.1)
resurect5.1 <- intersect(dead4.1, livin5.1)
resurect6.1 <- intersect(dead5.1, livin6.1)
resurect7.1 <- intersect(dead6.1, livin7.1)

D1FSA$X2015.09.11[resurect2.1] <- "K"
D1FSA$X2015.09.12[resurect3.1] <- "K"
D1FSA$X2015.09.13[resurect4.1] <- "K"
D1FSA$X2015.09.18[resurect5.1] <- "K"
D1FSA$X2015.09.25[resurect6.1] <- "K"

###90 Days
names(D90FSA)[4:8] <- c("X.2015.12.10","X.2015.12.16", "X.2015.12.22",
                         "X.2015.12.30", "X.2016.01.06")
names(D180FSA)[4:8] <- c("X.2016.03.09","X.2016.03.15", "X.2016.03.22",
                         "X.2016.03.29", "X.2016.04.05")
dead1.2 <- which(D90FSA$X.2015.12.10 == "D")
livin2.2 <- which(D90FSA$X.2015.12.16 != "D")
dead2.2 <- which(D90FSA$X.2015.12.16 == "D")
livin3.2 <- which(D90FSA$X.2015.12.22 != "D")
dead3.2 <- which(D90FSA$X.2015.12.22 == "D")
livin4.2 <- which(D90FSA$X.2015.12.30 != "D")
dead4.2 <- which(D90FSA$X.2015.12.30 == "D")
livin5.2 <- which(D90FSA$X.2016.01.06 != "D")

resurect2.2 <- intersect(dead1.2, livin2.2)
resurect3.2 <- intersect(dead2.2, livin3.2)
resurect4.2 <- intersect(dead3.2, livin4.2)
resurect5.2 <- intersect(dead4.2, livin5.2)

D90FSA$X.2015.12.10[resurect2.2] <- "K"
D90FSA$X.2015.12.16[resurect3.2] <- "K"
D90FSA$X.2015.12.22[resurect4.2] <- "K"
D90FSA$X.2015.12.30[resurect5.2] <- "K"

#180 Days
dead1.3 <- which(D180FSA$X.2016.03.09 == "D")
livin2.3 <- which(D180FSA$X.2016.03.15 != "D")
dead2.3 <- which(D180FSA$X.2016.03.15 == "D")
livin3.3 <- which(D180FSA$X.2016.03.22 != "D")
dead3.3 <- which(D180FSA$X.2016.03.22 == "D")
livin4.3 <- which(D180FSA$X.2016.03.29 != "D")
dead4.3 <- which(D180FSA$X.2016.04.05 == "D")
livin5.3 <- which(D180FSA$X.2016.04.05 != "D")

resurect2.3 <- intersect(dead1.3, livin2.3)
resurect3.3 <- intersect(dead2.3, livin3.3)
resurect4.3 <- intersect(dead3.3, livin4.3)
resurect5.3 <- intersect(dead4.3, livin5.3)

D180FSA$X.2016.03.09[resurect2.3] <- "K"
D180FSA$X.2016.03.15[resurect3.3] <- "K"
D180FSA$X.2016.03.22[resurect4.3] <- "K"
D180FSA$X.2016.03.29[resurect5.3] <- "K"

###############################################################################
###Missing data
#2 bugs escaped primary containment
escaped <- which(D1FSA$X2015.10.02 == "")
D1FSA <- D1FSA[-escaped, ]

escaped.2 <- which(D1FSA$X2015.09.13 == "")
escaped.3 <- which(D1FSA$X2015.09.18 == "")
escaped.4 <- which(D1FSA$X2015.09.25 == "")
escaped.5 <- which(D1FSA$X2015.10.02 == "")
escaped.6 <- which(D1FSA$X2015.10.09 == "")

escaped.2 <- which(D90FSA$X2015.12.10 == "")
escaped.3 <- which(D90FSA$X2015.12.16 == "")
escaped.4 <- which(D90FSA$X2015.12.22 == "")
escaped.5 <- which(D90FSA$X2015.12.30 == "")
escaped.6 <- which(D90FSA$X2016.01.06 == "")

escaped.2 <- which(D180FSA$X2016.03.09 == "")
escaped.3 <- which(D180FSA$X2016.03.15 == "")
escaped.4 <- which(D180FSA$X2016.03.22 == "")
escaped.5 <- which(D180FSA$X2016.03.29 == "")
escaped.6 <- which(D180FSA$X2016.04.05 == "")

#which(D1FSA == "")

###############################################################################
#See if we can infer missing date from Jar Data
# #Identify missing
# missing.jars <- which(D1FSA$X2015.10.09 == "")
# #assume if dead still dead
# dead.still.dead <- which(D1FSA$X2015.10.02 == "D")
# dd <- intersect(missing.jars, dead.still.dead)
# D1FSA$X2015.10.09[dd] <- "D"
# 
# #all remaining AF's survived to jar check on 10/16
# not.dead <- which(D1FSA$X2015.10.02 != "D")
# female <- which(D1FSA$STAGE_END == "AF")
# livin.f <- intersect(not.dead, female)
# liv.fm.miss <- intersect(livin.f, missing.jars)
# 
# length(liv.fm.miss)

######################
#In order to get to cox test, reshape the data.
D1.melt <- melt(D1FSA, id=c("INSECT", "STAGE_START", "STAGE_END", "NOTES", 
                         "paint", "days.after.paint", "quad", "exposure", 
                         "treatment", "exp.time"))

D90.melt <- melt(D90FSA, id=c("INSECT", "STAGE_START", "STAGE_END", "NOTES", 
                            "paint", "days.after.paint", "quad", "exposure", 
                            "treatment", "exp.time"))

D180.melt <- melt(D180FSA, id=c("INSECT", "STAGE_START", "STAGE_END", "NOTES", 
                            "paint", "days.after.paint", "quad", "exposure", 
                            "treatment", "exp.time"))

#as.character(substr(D90.melt$variable, 2,2))
#as.character(substr(D180.melt$variable, 2,2))


#View(D90.melt$variable)

#as.character(substr(D180.melt$variable, 1,1))
D90.melt$variable <- gsub("X.", "X", D90.melt$variable)
D180.melt$variable <- gsub("X.", "X", D180.melt$variable)

MakeBinary <-function(dtf){
#the variable needs to be turned into a date object
#so first make it a character
dtf$variable <- as.character(dtf$variable)
#remove the X's
dtf$variable <- gsub("X","",dtf$variable)
#replace the "." with "-"
dtf$variable <- gsub("[.]","-", dtf$variable)
dtf$variable <-as.Date(dtf$variable)

#to prevent confusion lets rename "variable" to "date"
chngname<-which(names(dtf)=="variable")
names(dtf)[chngname] <- "date"
#lets also rename "value" to "status"
chval<-which(names(dtf)=="value")
names(dtf)[chval] <- "status"

#==============================================================================
###Now that we have the data in a usable table, lets split the status into binary
#find 
alive <- which(dtf$status== "A")
knockdown <- which(dtf$status=="K")
dead <- which(dtf$status=="D")
unviable <- c(dead, knockdown)
living <- c(alive, knockdown)

#create blank columns for each status.
num <- as.numeric(dtf$quad)*0
dtf$alive <- num
dtf$alive[alive] <- 1
dtf$knockdown <- num
dtf$knockdown[knockdown] <- 1
dtf$dead <- num
dtf$dead[dead] <- 1
dtf$unviable <- num
dtf$unviable[unviable] <- 1
dtf$living <- num
dtf$living[living] <- 1
return(dtf)
}

D1.melt <- MakeBinary(D1.melt)
D90.melt <- MakeBinary(D90.melt)
D180.melt <- MakeBinary(D180.melt)

MakeJulianDate <- function(Data){
  Data$date <- as.Date(Data$date)
  Data$julian <- julian(Data$date)
  Data$day <- as.numeric(Data$julian-min(Data$julian))
  #1 day has observation the first day whereas 90 and 180 had first 24 hours later
  rep2y3 <- which(Data$days.after.paint > 80)
  Data$day[rep2y3] <- Data$day+1 
  return(Data)
}

D1.melt <- MakeJulianDate(D1.melt)
D90.melt <- MakeJulianDate(D90.melt)
D180.melt <- MakeJulianDate(D180.melt)

DataMelt<- rbind(D1.melt, D90.melt, D180.melt)

#write.csv(D1.melt, file = "DATA/D1_melt.csv")
#write.csv(D90.melt, file = "DATA/D90_melt.csv")
#write.csv(D180.melt, file = "DATA/D180_melt.csv")
write.csv(DataMelt, file = "DATA/DataMelt.csv")
#=============================================================================+

#also add back collective data to 1H-5A 2015-09-2015

#==============================================================================
