########################################
# Code to Compare total distance of chirimachas hour 1 
#written by Justin Sheen and Renzo Salazar abr 2018
########################################

### R packages being used:
# Use the commented command if the package is not installed

# install.packages("ggplot2")
#install.packages("reshape2")
# install.packages("RColorBrewer")
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/survminer")
#install.packages("gridExtra")
#install.packages("cowplot") #or
#devtools::install_github("wilkelab/cowplot")
#install.packages("data.table")
#install.packages("agricolae")
#install.packages("multcompView")
#install.packages("ggpmisc")

#load required packages
library("ggplot2")
library("reshape")
library("reshape2")
library("RColorBrewer")
library("plyr")
library("data.table")
library("agricolae")
library("knitr")
library("xtable")
library("gridExtra")
library("grid")
library("gtable")
library("plotrix")
library("multcompView")
library("ggpmisc")

# set working directory
setwd("D:/LABORATORIO/ENSAYOS/JUSTIN/fixed videos/all csv")
getwd()
# load csv files
d1c1 <- read.csv("DAY1_CAM1_1HR.csv")
d1c2 <- read.csv("DAY1_CAM2_1HR.csv")
d1c3 <- read.csv("DAY1_CAM3_1HR.csv")
d1c4 <- read.csv("DAY1_CAM4_1HR.csv")
d2c1 <- read.csv("DAY2_CAM1_1HR.csv")
d2c2 <- read.csv("DAY2_CAM2_1HR.csv")
d2c3 <- read.csv("DAY2_CAM3_1HR.csv")
d2c4 <- read.csv("DAY2_CAM4_1HR.csv")
d3c1 <- read.csv("DAY3_CAM1_1HR.csv")
d3c2 <- read.csv("DAY3_CAM2_1HR.csv")
d3c3 <- read.csv("DAY3_CAM3_1HR.csv")
d3c4 <- read.csv("DAY3_CAM4_1HR.csv")
d4c1 <- read.csv("DAY4_CAM1_1HR.csv")
d4c2 <- read.csv("DAY4_CAM2_1HR.csv")
d4c3 <- read.csv("DAY4_CAM3_1HR.csv")
d4c4 <- read.csv("DAY4_CAM4_1HR.csv")
d5c1 <- read.csv("DAY5_CAM1_1HR.csv")
d5c2 <- read.csv("DAY5_CAM2_1HR.csv")
d5c3 <- read.csv("DAY5_CAM3_1HR.csv")
d5c4 <- read.csv("DAY5_CAM4_1HR.csv")
d6c1 <- read.csv("DAY6_CAM1_1HR.csv")
d6c2 <- read.csv("DAY6_CAM2_1HR.csv")
d6c3 <- read.csv("DAY6_CAM3_1HR.csv")
d6c4 <- read.csv("DAY6_CAM4_1HR.csv")
d7c1 <- read.csv("DAY7_CAM1_1HR.csv")
d7c2 <- read.csv("DAY7_CAM2_1HR.csv")
d7c3 <- read.csv("DAY7_CAM3_1HR.csv")
d7c4 <- read.csv("DAY7_CAM4_1HR.csv")
d8c1 <- read.csv("DAY8_CAM1_1HR.csv")
d8c2 <- read.csv("DAY8_CAM2_1HR.csv")
d8c3 <- read.csv("DAY8_CAM3_1HR.csv")
d8c4 <- read.csv("DAY8_CAM4_1HR.csv")
d5c1.2 <- read.csv("DAY5_CAM1_2HR.csv")
d5c2.2 <- read.csv("DAY5_CAM2_2HR.csv")
d5c3.2 <- read.csv("DAY5_CAM3_2HR.csv")
d5c4.2 <- read.csv("DAY5_CAM4_2HR.csv")
d6c1.2 <- read.csv("DAY6_CAM1_2HR.csv")
d6c2.2 <- read.csv("DAY6_CAM2_2HR.csv")
d6c3.2 <- read.csv("DAY6_CAM3_2HR.csv")
d7c1.2 <- read.csv("DAY7_CAM1_2HR.csv")
d7c2.2 <- read.csv("DAY7_CAM2_2HR.csv")
d7c3.2 <- read.csv("DAY7_CAM3_2HR.csv")
d7c4.2 <- read.csv("DAY7_CAM4_2HR.csv")
d8c1.2 <- read.csv("DAY8_CAM1_2HR.csv")
d8c3.2 <- read.csv("DAY8_CAM3_2HR.csv")
d8c4.2 <- read.csv("DAY8_CAM4_2HR.csv")
d5c1.3 <- read.csv("DAY5_CAM1_3HR.csv")
d5c2.3 <- read.csv("DAY5_CAM2_3HR.csv")
d5c3.3 <- read.csv("DAY5_CAM3_3HR.csv")
d5c4.3 <- read.csv("DAY5_CAM4_3HR.csv")
d6c1.3 <- read.csv("DAY6_CAM1_3HR.csv")
d6c2.3 <- read.csv("DAY6_CAM2_3HR.csv")
d6c3.3 <- read.csv("DAY6_CAM3_3HR.csv")
d7c1.3 <- read.csv("DAY7_CAM1_3HR.csv")
d7c2.3 <- read.csv("DAY7_CAM2_3HR.csv")
d7c3.3 <- read.csv("DAY7_CAM3_3HR.csv")
d7c4.3 <- read.csv("DAY7_CAM4_3HR.csv")
d8c1.3 <- read.csv("DAY8_CAM1_3HR.csv")
d8c3.3 <- read.csv("DAY8_CAM3_3HR.csv")
d8c4.3 <- read.csv("DAY8_CAM4_3HR.csv")



#################################################
# formula to get total distance by hour and bug
# (Justin formula)
#################################################
# compare total distances function
getTotalDistance <- function(df) {
  
  # split into data frames for each of the chirimachas
  bugOne <- df[which(df$track == 1),]
  if (nrow(bugOne) == 0) {
    bugOne <- data.frame(matrix(ncol = 4, nrow = 1))
    colnames(bugOne) <- c("track", "frame", "x", "y")
    bugOne$track <- 1
    bugOne$frame <- 1
    bugOne$x <- 0
    bugOne$y <- 0
  }
  bugTwo <- df[which(df$track == 2),]
  if (nrow(bugTwo) == 0) {
    bugTwo <- data.frame(matrix(ncol = 4, nrow = 1))
    colnames(bugTwo) <- c("track", "frame", "x", "y")
    bugTwo$track <- 2
    bugTwo$frame <- 1
    bugTwo$x <- 0
    bugTwo$y <- 0
  }
  bugThree <- df[which(df$track == 3),]
  if (nrow(bugThree) == 0) {
    bugThree <- data.frame(matrix(ncol = 4, nrow = 1))
    colnames(bugThree) <- c("track", "frame", "x", "y")
    bugThree$track <- 3
    bugThree$frame <- 1
    bugThree$x <- 0
    bugThree$y <- 0
  }
  bugFour <- df[which(df$track == 4),]
  if (nrow(bugFour) == 0) {
    bugFour <- data.frame(matrix(ncol = 4, nrow = 1))
    colnames(bugFour) <- c("track", "frame", "x", "y")
    bugFour$track <- 4
    bugFour$frame <- 1
    bugFour$x <- 0
    bugFour$y <- 0
  }
  
  # first, make new data frames
  makeNewDataFrame <- function(bugRaw, trackN) {
    bugNew <- data.frame(matrix(ncol = 4, nrow = max(df$frame)))
    colnames(bugNew) <- c("track", "frame", "x", "y")
    bugNew$track <- trackN
    bugNew$frame <- seq.int(max(df$frame))
    for (i in 1:nrow(bugRaw)) {
      bugNew$x[which(bugNew$frame == bugRaw$frame[i])] <- bugRaw$x[i]
      bugNew$y[which(bugNew$frame == bugRaw$frame[i])] <- bugRaw$y[i]
    }
    
    # impute data if missing
    for (j in 2:nrow(bugNew)) {
      if (is.na(bugNew$x[j]) | is.na(bugNew$y[j])) {
        bugNew$x[j] <- bugNew$x[j - 1]
        bugNew$y[j] <- bugNew$y[j - 1]
      }
    }
    return(bugNew)
  }
  bugOne <- makeNewDataFrame(bugOne, 1)
  bugTwo <- makeNewDataFrame(bugTwo, 2)
  bugThree <- makeNewDataFrame(bugThree, 3)
  bugFour <- makeNewDataFrame(bugFour, 4)
  
  # add distance column
  addDistanceCol <- function(df) {
    df$distance <- 0
    for (k in 2:nrow(df)) {
      x1 <- df$x[k - 1]
      x2 <- df$x[k]
      y1 <- df$y[k - 1]
      y2 <- df$y[k]
      
      df$distance[k] <- sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
    }
    return(df)
  }
  bugOne <- addDistanceCol(bugOne)
  bugTwo <- addDistanceCol(bugTwo)
  bugThree <- addDistanceCol(bugThree)
  bugFour <- addDistanceCol(bugFour)
  
  # return a data table of the total distance of each insect
  toReturn<- data.frame(matrix(ncol = 2, nrow = 4))
  colnames(toReturn) <- c("track", "total_distance_traveled_(pixels)")
  toReturn$track <- seq.int(4)
  toReturn[1,2] <- sum(bugOne$distance)
  toReturn[2,2] <- sum(bugTwo$distance)
  toReturn[3,2] <- sum(bugThree$distance)
  toReturn[4,2] <- sum(bugFour$distance)
  
  return(toReturn)
}

############################
# Analysis hour ONE
############################
d1.1<-getTotalDistance(d1c1)
d1.2<-getTotalDistance(d1c2)
d1.3<-getTotalDistance(d1c3)
d1.4<-getTotalDistance(d1c4)
d2.1<-getTotalDistance(d2c1)
d2.2<-getTotalDistance(d2c2)
d2.3<-getTotalDistance(d2c3)
d2.4<-getTotalDistance(d2c4)
d3.1<-getTotalDistance(d3c1)
d3.2<-getTotalDistance(d3c2)
d3.3<-getTotalDistance(d3c3)
d3.4<-getTotalDistance(d3c4)
d4.1<-getTotalDistance(d4c1)
d4.2<-getTotalDistance(d4c2)
d4.3<-getTotalDistance(d4c3)
d4.4<-getTotalDistance(d4c4)
d5.1<-getTotalDistance(d5c1)
d5.2<-getTotalDistance(d5c2)
d5.3<-getTotalDistance(d5c3)
d5.4<-getTotalDistance(d5c4)
d6.1<-getTotalDistance(d6c1)
d6.2<-getTotalDistance(d6c2)
d6.3<-getTotalDistance(d6c3)
d6.4<-getTotalDistance(d6c4)
d7.1<-getTotalDistance(d7c1)
d7.2<-getTotalDistance(d7c2)
d7.3<-getTotalDistance(d7c3)
d7.4<-getTotalDistance(d7c4)
d8.1<-getTotalDistance(d8c1)
d8.2<-getTotalDistance(d8c2)
d8.3<-getTotalDistance(d8c3)
d8.4<-getTotalDistance(d8c4)

# create reference columns
day<-c(rep("day 1",16),rep("day 2",16),rep("day 3",16),rep("day 4",16),
       rep("day 5",16),rep("day 6",16),rep("day 7",16),rep("day 8",16))
camera<-c(rep(c(rep("cam 1",4), rep("cam 2",4), rep("cam 3",4),rep("cam 4",4)),8))
arena<-c(rep("arena 1",4), rep("arena 2",4), rep("arena 3",4),rep("arena 4",4),
         rep("arena 5",4), rep("arena 6",4), rep("arena 7",4),rep("arena 8",4),
         rep("arena 9",4), rep("arena 10",4), rep("arena 11",4),rep("arena 12",4),
         rep("arena 13",4), rep("arena 14",4), rep("arena 15",4),rep("arena 16",4),
         rep("arena 17",4), rep("arena 18",4), rep("arena 19",4),rep("arena 20",4),
         rep("arena 21",4), rep("arena 22",4), rep("arena 23",4),rep("arena 24",4),
         rep("arena 25",4), rep("arena 26",4), rep("arena 27",4),rep("arena 28",4),
         rep("arena 29",4), rep("arena 30",4), rep("arena 31",4),rep("arena 32",4))

infection<-c(rep(c("infected","infected","control","control"),32))

# Built a dataframe hour 1
df1<-data.frame(rbind(d1.1,d1.2,d1.3,d1.4,d2.1,d2.2,d2.3,d2.4,d3.1,d3.2,d3.3,d3.4,
                     d4.1,d4.2,d4.3,d4.4,d5.1,d5.2,d5.3,d5.4,d6.1,d6.2,d6.3,d6.4,
                     d7.1,d7.2,d7.3,d7.4,d8.1,d8.2,d8.3,d8.4))
dim(df1)
df1["day"]<-day
df1["camera"]<-camera
df1["arena"]<-arena
df1["infection.status"]<-infection

#####################################
# Analysis hour TWO
#####################################
d5.1.2<-getTotalDistance(d5c1.2)
d5.2.2<-getTotalDistance(d5c2.2)
d5.3.2<-getTotalDistance(d5c3.2)
d5.4.2<-getTotalDistance(d5c4.2)
d6.1.2<-getTotalDistance(d6c1.2)
d6.2.2<-getTotalDistance(d6c2.2)
d6.3.2<-getTotalDistance(d6c3.2)
d7.1.2<-getTotalDistance(d7c1.2)
d7.2.2<-getTotalDistance(d7c2.2)
d7.3.2<-getTotalDistance(d7c3.2)
d7.4.2<-getTotalDistance(d7c4.2)
d8.1.2<-getTotalDistance(d8c1.2)
d8.3.2<-getTotalDistance(d8c3.2)
d8.4.2<-getTotalDistance(d8c4.2)

# create reference columns
day2<-c(rep("day 5",16),rep("day 6",12),rep("day 7",16),rep("day 8",12))

camera2<-c(rep("cam 1",4), rep("cam 2",4), rep("cam 3",4),rep("cam 4",4),
          rep("cam 1",4), rep("cam 2",4), rep("cam 3",4),
          rep("cam 1",4), rep("cam 2",4), rep("cam 3",4),rep("cam 4",4),
          rep("cam 1",4), rep("cam 3",4),rep("cam 4",4))

arena2<-c(rep("arena 17",4), rep("arena 18",4), rep("arena 19",4),rep("arena 20",4),
         rep("arena 21",4), rep("arena 22",4), rep("arena 23",4),rep("arena 24",4),
         rep("arena 25",4), rep("arena 26",4), rep("arena 27",4),rep("arena 28",4),
         rep("arena 29",4), rep("arena 30",4))

infection2<-c(rep(c("infected","infected","control","control"),14))

# Built a dataframe hour2
df2<-data.frame(rbind(d5.1.2,d5.2.2,d5.3.2,d5.4.2,d6.1.2,d6.2.2,d6.3.2,
                     d7.1.2,d7.2.2,d7.3.2,d7.4.2,d8.1.2,d8.3.2,d8.4.2))
dim(df2)
df2["day"]<-day2
df2["camera"]<-camera2
df2["arena"]<-arena2
df2["infection.status"]<-infection2

###########################
# Analysis hour THREE
###########################
d5.1.3<-getTotalDistance(d5c1.3)
d5.2.3<-getTotalDistance(d5c2.3)
d5.3.3<-getTotalDistance(d5c3.3)
d5.4.3<-getTotalDistance(d5c4.3)
d6.1.3<-getTotalDistance(d6c1.3)
d6.2.3<-getTotalDistance(d6c2.3)
d6.3.3<-getTotalDistance(d6c3.3)
d7.1.3<-getTotalDistance(d7c1.3)
d7.2.3<-getTotalDistance(d7c2.3)
d7.3.3<-getTotalDistance(d7c3.3)
d7.4.3<-getTotalDistance(d7c4.3)
d8.1.3<-getTotalDistance(d8c1.3)
d8.3.3<-getTotalDistance(d8c3.3)
d8.4.3<-getTotalDistance(d8c4.3)

# create reference columns
day3<-c(rep("day 5",16),rep("day 6",12),rep("day 7",16),rep("day 8",12))

camera3<-c(rep("cam 1",4), rep("cam 2",4), rep("cam 3",4),rep("cam 4",4),
          rep("cam 1",4), rep("cam 2",4), rep("cam 3",4),
          rep("cam 1",4), rep("cam 2",4), rep("cam 3",4),rep("cam 4",4),
          rep("cam 1",4), rep("cam 3",4),rep("cam 4",4))

arena3<-c(rep("arena 17",4), rep("arena 18",4), rep("arena 19",4),rep("arena 20",4),
         rep("arena 21",4), rep("arena 22",4), rep("arena 23",4),rep("arena 24",4),
         rep("arena 25",4), rep("arena 26",4), rep("arena 27",4),rep("arena 28",4),
         rep("arena 29",4), rep("arena 30",4))

infection3<-c(rep(c("infected","infected","control","control"),14))

# Built a dataframe hour 3
df3<-data.frame(rbind(d5.1.3,d5.2.3,d5.3.3,d5.4.3,d6.1.3,d6.2.3,d6.3.3,
                     d7.1.3,d7.2.3,d7.3.3,d7.4.3,d8.1.3,d8.3.3,d8.4.3))
dim(df3)
df3["day"]<-day3
df3["camera"]<-camera3
df3["arena"]<-arena3
df3["infection.status"]<-infection3

#######################################################
#   built a unique data frame with hour 1,2 and 3     #
#######################################################
df<-data.frame(rbind(df1,df2,df3))
df["hour"]<-c(rep("hour 1",nrow(df1)),rep("hour 2",nrow(df2)),rep("hour 3",nrow(df3)))
df<-df[c(7,3,4,5,6,1,2)]

#replace NA by 0
df[is.na(df)]<-0

# convert pixels to cm ==> 1 pixel=0.026458333
df["tot.distance.cm"]<-round(df[,7]*0.026458333,2)

######################
#     plot data      #
######################

p<-ggplot(df,aes(x=infection.status, y=tot.distance.cm, fill=infection.status))+
  geom_boxplot(width=.5, alpha=0.4)+
  #geom_violin(width=.5, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=c(8), size=c(3),color=c("black"), position=position_dodge(0.8))+
  geom_point(aes(colour=infection.status),position = position_jitter(0.2),size=.9)+
  #geom_jitter(aes(colour=eggtype),position=position_jitter(0.2))+
  #geom_dotplot(alpha=0.2,binaxis='y', stackdir='center', position=position_dodge(0.8))+
  scale_fill_manual( labels = c("Non-infected","Infected"),values = c("blue","red"))+
  scale_color_manual(labels = c("Non-infected","Infected"), values = c("blue", "red"))+
  ylab("distance traveled (cm)")+
  xlab("treatment")+
  #facet_wrap(~day, ncol=3)+
  theme(legend.position="right",plot.title = element_text(hjust = 0.5),
        #axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p

p1<-ggplot(df,aes(x=infection.status, y=tot.distance.cm, fill=infection.status))+
  geom_boxplot(width=.5, alpha=0.4)+
  #geom_violin(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=c(8), size=c(3),color=c("black"), position=position_dodge(0.8))+
  geom_point(aes(colour=infection.status),position = position_jitter(0.2),size=.9)+
  #geom_jitter(aes(colour=eggtype),position=position_jitter(0.2))+
  #geom_dotplot(alpha=0.2,binaxis='y', stackdir='center', position=position_dodge(0.8))+
  scale_fill_manual( labels = c("Non-infected","Infected"),values = c("blue","red"))+
  scale_color_manual(labels = c("Non-infected","Infected"), values = c("blue", "red"))+
  ylab("distance traveled (cm)")+
  #xlab("treatment")+
  facet_grid(.~hour)+
  #facet_wrap(~day, ncol=3)+
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p1

# plot by day and hour (just days 5,6,7 and 8)
dfn<-df[65:nrow(df),]

p2<-ggplot(dfn,aes(x=infection.status, y=tot.distance.cm, fill=infection.status))+
  geom_boxplot(width=.5, alpha=0.4)+
  #geom_violin(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=c(8), size=c(3),color=c("black"), position=position_dodge(0.8))+
  geom_point(aes(colour=infection.status),position = position_jitter(0.2),size=.5)+
  #geom_jitter(aes(colour=eggtype),position=position_jitter(0.2))+
  #geom_dotplot(alpha=0.2,binaxis='y', stackdir='center', position=position_dodge(0.8))+
  scale_fill_manual( labels = c("Non-infected","Infected"),values = c("blue","red"))+
  scale_color_manual(labels = c("Non-infected","Infected"), values = c("blue", "red"))+
  ylab("distance traveled (cm)")+
 # xlab("treatment")+
  facet_grid(day~hour)+
 # facet_wrap(day~hour, ncol=3)+
  theme(legend.position="right",plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p2

#get pdf file
pdf(file = "videos analysis.pdf", width=6,height=4,paper='special')#, paper="A4r",)
p
p1
p2
dev.off()

