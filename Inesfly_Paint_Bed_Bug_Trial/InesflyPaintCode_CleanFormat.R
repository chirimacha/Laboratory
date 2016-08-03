###This code cleans up the data for the bed bug inesfly paint study
## Review Read me at

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
setwd("C:/Users/dtrac/OneDrive/Documents/GitHub/Laboratory/Inesfly_Paint_Bed_Bug_Trial")
#MAC for Mike
#setwd("/Users/mzlevy/Laboratory/Inesfly_Paint_Bed_Bug_Trial")")

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

########################################################################
###Compare A and B versions of each data set and see where errors were made.

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
  D90FSB$STAGE_END[eerrors2[c(4,5)]] <- "5"
  #error 2: space?
  D90FSA$STAGE_END[eerrors2[2]] <- "AM"
  #error 3: Died in molth so record as 5
  D90FSA$STAGE_END[eerrors2[3]] <- "5"
  #errors 4 and 5 are missing values
  D90FSB$STAGE_END[eerrors2[4]] <- "5"
  D90FSB$STAGE_END[eerrors2[5]] <- "5"
  
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
  D180FSB$STAGE_END[eerrors3[9]] <- "5"

###Check that the no errors are occuring anymore
  eerrors1a<-which(D1FSA$STAGE_END != D1FSB$STAGE_END)
  eerrors2a<-which(D90FSA$STAGE_END != D90FSB$STAGE_END)
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
