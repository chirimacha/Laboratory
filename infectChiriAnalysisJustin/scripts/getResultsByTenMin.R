#===================================================================================
# Code to Compare total distance, time and speed of chirimachas 
# infected and non infected but by ten minutes
# written by Justin Sheen and Renzo Salazar june 2018
#===================================================================================

#===================================================================================
# Part I: Dependencies, libraries
#===================================================================================

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
library("devtools")
library("sp")
library("videoplayR")
library("splancs")

#===================================================================================
# Part I: Load all data. This means that we load all the csv files for all of the hours,
#         for all of the cameras, for all of the days. This is a total of 96 files.
#===================================================================================

# Set working directory
# setwd("D:/LABORATORIO/ENSAYOS/JUSTIN/fixed videos/all csv")
#setwd("~/GITHUB/Levy_Research/Laboratory/Triatoma infected behavior_Video")
setwd("~/Desktop/Levy_Research/Laboratory/infectChiriAnalysis/data/dataByCam")
getwd()

# Load CSV files
day1cam1 <- read.csv("DAY1_CAM1.csv")
day1cam2 <- read.csv("DAY1_CAM2.csv")
day1cam3 <- read.csv("DAY1_CAM3.csv")
day1cam4 <- read.csv("DAY1_CAM4.csv")
day2cam1 <- read.csv("DAY2_CAM1.csv")
day2cam2 <- read.csv("DAY2_CAM2.csv")
day2cam3 <- read.csv("DAY2_CAM3.csv")
day2cam4 <- read.csv("DAY2_CAM4.csv")
day3cam1 <- read.csv("DAY3_CAM1.csv")
day3cam2 <- read.csv("DAY3_CAM2.csv")
day3cam3 <- read.csv("DAY3_CAM3.csv")
day3cam4 <- read.csv("DAY3_CAM4.csv")
day4cam1 <- read.csv("DAY4_CAM1.csv")
day4cam2 <- read.csv("DAY4_CAM2.csv")
day4cam3 <- read.csv("DAY4_CAM3.csv")
day4cam4 <- read.csv("DAY4_CAM4.csv")
day5cam1 <- read.csv("DAY5_CAM1.csv")
day5cam2 <- read.csv("DAY5_CAM2.csv")
day5cam3 <- read.csv("DAY5_CAM3.csv")
day5cam4 <- read.csv("DAY5_CAM4.csv")
day6cam1 <- read.csv("DAY6_CAM1.csv")
day6cam2 <- read.csv("DAY6_CAM2.csv")
day6cam3 <- read.csv("DAY6_CAM3.csv")
day7cam1 <- read.csv("DAY7_CAM1.csv")
day7cam2 <- read.csv("DAY7_CAM2.csv")
day7cam3 <- read.csv("DAY7_CAM3.csv")
day7cam4 <- read.csv("DAY7_CAM4.csv")
day8cam1 <- read.csv("DAY8_CAM1.csv")
day8cam3 <- read.csv("DAY8_CAM3.csv")
day8cam4 <- read.csv("DAY8_CAM4.csv")

#===================================================================================
# Part II: Load the following two functions which we are going to use to get data for analysis
#  IIa. Test that all polygons have been created correctly and get polygons
#  IIb. Total distance traveled by hour by the bugs
#  IIc. In an open area: total distance AND total distance traveled by hour by the bugs (two results)
#===================================================================================
#===================================================================================
# Part IIa: Test that all polygons have been created correctly and get polygons
#===================================================================================

setwd("~/Desktop/Levy_Research/Laboratory/infectChiriAnalysis/polygons")
polygons <- read.table("polygons.csv")
polygons[,1] <- as.character(polygons[,1])
polygons <- polygons[2:129,]
uniqueUsers <- unique(polygons[,1])

for (i in 1:length(uniqueUsers)) {
  indices <- which(polygons$V1 == uniqueUsers[i])
  maskInfo <- polygons[,2:3]
  maskInfo <- maskInfo[indices,]
  assign(uniqueUsers[i], maskInfo)
}

#===================================================================================
# Part IIb: Total distance traveled by hour by the bugs
#===================================================================================
# This function is meant to get the total distance that was traveled by the bugs within
# the replicate. You supply the data frame with the track information of the bugs, and it will
# output the total distance traveled by each bug in pixels.
getTotalDistance <- function(df) {
  # From the original data frame, we only need to keep the following variables
  toKeep <- c("frame", "track", "x", "y")
  df <- df[,toKeep]
  
  # Split the data frame into four different data frames, one for each bug
  bugOne <- df[which(df$track == 1),]
  bugTwo <- df[which(df$track == 2),]
  bugThree <- df[which(df$track == 3),]
  bugFour <- df[which(df$track == 4),]
  
  # We now add a distance column by simply substracting one frame from another
  addDistanceCol <- function(df) {
    df$distance <- 0
    for (k in 2:nrow(df)) {
      x1 <- df$x[k - 1]
      x2 <- df$x[k]
      y1 <- df$y[k - 1]
      y2 <- df$y[k]
      
      df$distance[k] <- sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
      
      # if the distance is NA because one of the coordinates is NA, this means that
      # the bug did not move in the NA slot, and thus, the distance traveled should
      # only be 0
      if (is.na(df$distance[k])) {
        df$distance[k] <- 0
      }
    }
    return(df)
  }
  bugOne <- addDistanceCol(bugOne)
  bugTwo <- addDistanceCol(bugTwo)
  bugThree <- addDistanceCol(bugThree)
  bugFour <- addDistanceCol(bugFour)
  
  # We now return a data frame that gets the distance every ten minutes
  # and adds up the previously described distance column
  toReturn<- data.frame(matrix(ncol = 19, nrow = 4))
  colnames(toReturn) <- c("track",
                          "t1_totDist","t2_totDist","t3_totDist","t4_totDist","t5_totDist","t6_totDist","t7_totDist","t8_totDist","t9_totDist",
                          "t10_totDist","t11_totDist","t12_totDist","t13_totDist","t14_totDist","t15_totDist","t16_totDist","t17_totDist","t18_totDist")
  toReturn$track <- seq.int(4)
  
  ans <- TRUE
  timePoint <- 2
  startFrame <- 1
  endFrame <- 2242
  while(ans) {
    
    if (endFrame >= 40356) {
      endFrame <- 40356
      ans <- FALSE
    } 
    
    toReturn[1,timePoint] <- sum(bugOne[startFrame:endFrame,]$distance)
    toReturn[2,timePoint] <- sum(bugTwo[startFrame:endFrame,]$distance)
    toReturn[3,timePoint] <- sum(bugThree[startFrame:endFrame,]$distance)
    toReturn[4,timePoint] <- sum(bugFour[startFrame:endFrame,]$distance)
    
    startFrame <- startFrame + 2242
    endFrame <- endFrame + 2242
    timePoint <- timePoint + 1
  }
  return(toReturn)
}

#===================================================================================
# Part IIc. In an open area: total distance AND total time traveled by hour by the bugs (two results)
#===================================================================================
# The purpose of this function is to do two things. The first is to get the total time spent
# within the open area, and the second is to get the total distance traversed within the
# open area.
TimeMoving <- function(df, inputPoly) {
  # Only keep columns we need
  toKeep <- c("frame", "track", "x", "y")
  df <- df[,toKeep]
  
  # Split into data frames for each of the chirimachas
  bugOne <- df[which(df$track == 1),]
  bugTwo <- df[which(df$track == 2),]
  bugThree <- df[which(df$track == 3),]
  bugFour <- df[which(df$track == 4),]
  
  # Now we have a function to get two pieces of information, (1) the total time spent
  # within the open area and (2) the distance traveled within the open area
  addCenterDistance <- function(bugRaw, poly=inputPoly) {
    # Here, we change the polygon mask so that it is smaller by 3cm (this smaller rectangle
    # constitutes the "open area.") Since one pixel is equal to 0.02645833 cm, and we want 3 
    # cm of distance from the walls, we need to add and subtract by 113.3858
    poly[1,1] <- poly[1,1] + 113.3858
    poly[1,2] <- poly[1,2] - 113.3858
    
    poly[2,1] <- poly[2,1] - 113.3858
    poly[2,2] <- poly[2,2] - 113.3858
    
    poly[3,1] <- poly[3,1] - 113.3858
    poly[3,2] <- poly[3,2] + 113.3858
    
    poly[4,1] <- poly[4,1] + 113.3858
    poly[4,2] <- poly[4,2] + 113.3858
    
    
    # Here, we check whether each frame is within the polygon or not. If it is in the polygon
    # then we list it as TRUE in the column "inPoly" otherwise we list it as FALSE
    bugRaw$inPoly <- FALSE
    for (k in 1:nrow(bugRaw)) {
      
      # Need to check whether the coordinates are NA or not
      if (is.na(bugRaw$x[k]) | is.na(bugRaw$y[k])) {
        
      } else {
        if (point.in.polygon(bugRaw$x[k], bugRaw$y[k], poly[,1], poly[,2])) {
          bugRaw$inPoly[k] <- TRUE
        }
      }
    }
    
    # Here we get the total distance traveled by seeing whether the focal frame and previous
    # frame are both within the polygon. If they are, then we get the distance between these
    # two frames
    bugRaw$abiertaDistancia <- 0
    for (m in 2:nrow(bugRaw)) {
      if (bugRaw$inPoly[m - 1] & bugRaw$inPoly[m]) {
        x1 <- bugRaw$x[m - 1]
        x2 <- bugRaw$x[m]
        y1 <- bugRaw$y[m - 1]
        y2 <- bugRaw$y[m]
        
        bugRaw$abiertaDistancia[m] <- sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
      }
    }
    return(bugRaw)
  }
  bugOne <- addCenterDistance(bugOne)
  bugTwo <- addCenterDistance(bugTwo)
  bugThree <- addCenterDistance(bugThree)
  bugFour <- addCenterDistance(bugFour)
  
  # Here, we create a new data frame called "toReturn" that we will use to return the two
  # variables we want to analyze: total time spent in the open area, and total distance
  # spent in the open area.
  toReturn<- data.frame(matrix(ncol = 37, nrow = 4))
  colnames(toReturn) <- c("track", 
                          "t1_time_spent_in_open", "t2_time_spent_in_open", "t3_time_spent_in_open",
                          "t4_time_spent_in_open", "t5_time_spent_in_open", "t6_time_spent_in_open",
                          "t7_time_spent_in_open", "t8_time_spent_in_open", "t9_time_spent_in_open",
                          "t10_time_spent_in_open", "t11_time_spent_in_open", "t12_time_spent_in_open",
                          "t13_time_spent_in_open", "t14_time_spent_in_open", "t15_time_spent_in_open",
                          "t16_time_spent_in_open", "t17_time_spent_in_open", "t18_time_spent_in_open",
                          "t1_distance_spent_in_open", "t2_distance_spent_in_open", "t3_distance_spent_in_open",
                          "t4_distance_spent_in_open", "t5_distance_spent_in_open", "t6_distance_spent_in_open",
                          "t7_distance_spent_in_open", "t8_distance_spent_in_open", "t9_distance_spent_in_open",
                          "t10_distance_spent_in_open", "t11_distance_spent_in_open", "t12_distance_spent_in_open",
                          "t13_distance_spent_in_open", "t14_distance_spent_in_open", "t15_distance_spent_in_open",
                          "t16_distance_spent_in_open", "t17_distance_spent_in_open", "t18_distance_spent_in_open")
  toReturn$track <- seq.int(4)
  
  # get time spent in open
  ans <- TRUE
  timePoint <- 2
  startFrame <- 1
  endFrame <- 2242
  while(ans) {
    if (endFrame >= 40356) {
      endFrame <- 40356
      ans <- FALSE
    } 
    
    toReturn[1,timePoint] <- sum(ifelse(bugOne[startFrame:endFrame,]$inPoly, 1, 0))
    toReturn[2,timePoint] <- sum(ifelse(bugTwo[startFrame:endFrame,]$inPoly, 1, 0))
    toReturn[3,timePoint] <- sum(ifelse(bugThree[startFrame:endFrame,]$inPoly, 1, 0))
    toReturn[4,timePoint] <- sum(ifelse(bugFour[startFrame:endFrame,]$inPoly, 1, 0))
    
    startFrame <- startFrame + 2242
    endFrame <- endFrame + 2242
    timePoint <- timePoint + 1
  }
  
  # get distance spent in open
  ans <- TRUE
  timePoint <- 20
  startFrame <- 1
  endFrame <- 2242
  while(ans) {
    if (endFrame >= 40356) {
      endFrame <- 40356
      ans <- FALSE
    }
    
    toReturn[1,timePoint] <- sum(bugOne[startFrame:endFrame,]$abiertaDistancia)
    toReturn[2,timePoint] <- sum(bugTwo[startFrame:endFrame,]$abiertaDistancia)
    toReturn[3,timePoint] <- sum(bugThree[startFrame:endFrame,]$abiertaDistancia)
    toReturn[4,timePoint] <- sum(bugFour[startFrame:endFrame,]$abiertaDistancia)
    
    startFrame <- startFrame + 2242
    endFrame <- endFrame + 2242
    timePoint <- timePoint + 1
  }
  
  # Here, we add a column that will get the total minutes that it was within the open area
  # and a column that will get the total minutes that it was not within the open area. We divide
  # by the 29.9 frames per second, 60 seconds per minute, and 8 because of the fast forward effect
  # that we used for the videos
  #toReturn$minutes.moving <- round(((toReturn[,2] / 29.9) / 60) * 8, digits = 1)
  #toReturn$minutes.staying <- round((((max(df$frame) - toReturn[,2]) / 29.9) / 60) * 8, digits = 1)
  
  return(toReturn)
}

# every 2242 frames is a datapoint for ten minutes (see written explanation on sheet of paper)
# there are 18 data points at most for each bug

#===================================================================================
# Part III: Analysis of all files (combining part I and part II)
#  IIIa. Analysis for all cameras, by ten minutes each
#===================================================================================
#===================================================================================
# Part IIIa. Analysis for all cameras, by ten minutes each
#===================================================================================
#===================================================================================
# Day 1, Cam 1
#===================================================================================
print("day1cam1")
d1c1.res<-as.data.frame(list(getTotalDistance(day1cam1),TimeMoving(day1cam1, POLYDAY1CAM1)[,2:37]))
#===================================================================================
# Day 1, Cam 2
#===================================================================================
print("day1cam2")
d1c2.res<-as.data.frame(list(getTotalDistance(day1cam2),TimeMoving(day1cam2, POLYDAY1CAM2)[,2:37]))
#===================================================================================
# Day 1, Cam 3
#===================================================================================
print("day1cam3")
d1c3.res<-as.data.frame(list(getTotalDistance(day1cam3),TimeMoving(day1cam3, POLYDAY1CAM3)[,2:37]))
#===================================================================================
# Day 1, Cam 4
#===================================================================================
print("day1cam4")
d1c4.res<-as.data.frame(list(getTotalDistance(day1cam4),TimeMoving(day1cam4, POLYDAY1CAM4)[,2:37]))
#===================================================================================
# Day 2, Cam 1
#===================================================================================
print("day2cam1")
d2c1.res<-as.data.frame(list(getTotalDistance(day2cam1),TimeMoving(day2cam1, POLYDAY2CAM1)[,2:37]))
#===================================================================================
# Day 2, Cam 2
#===================================================================================
print("day2cam2")
d2c2.res<-as.data.frame(list(getTotalDistance(day2cam2),TimeMoving(day2cam2, POLYDAY2CAM2)[,2:37]))
#===================================================================================
# Day 2, Cam 3
#===================================================================================
print("day2cam3")
d2c3.res<-as.data.frame(list(getTotalDistance(day2cam3),TimeMoving(day2cam3, POLYDAY2CAM3)[,2:37]))
#===================================================================================
# Day 2, Cam 4
#===================================================================================
print("day2cam4")
d2c4.res<-as.data.frame(list(getTotalDistance(day2cam4),TimeMoving(day2cam4, POLYDAY2CAM4)[,2:37]))
#===================================================================================
# Day 3, Cam 1
#===================================================================================
print("day3cam1")
d3c1.res<-as.data.frame(list(getTotalDistance(day3cam1),TimeMoving(day3cam1, POLYDAY3CAM1)[,2:37]))
#===================================================================================
# Day 3, Cam 2
#===================================================================================
print("day3cam2")
d3c2.res<-as.data.frame(list(getTotalDistance(day3cam2),TimeMoving(day3cam2, POLYDAY3CAM2)[,2:37]))
#===================================================================================
# Day 3, Cam 3
#===================================================================================
print("day3cam3")
d3c3.res<-as.data.frame(list(getTotalDistance(day3cam3),TimeMoving(day3cam3, POLYDAY3CAM3)[,2:37]))
#===================================================================================
# Day 3, Cam 4
#===================================================================================
print("day3cam4")
d3c4.res<-as.data.frame(list(getTotalDistance(day3cam4),TimeMoving(day3cam4, POLYDAY3CAM4)[,2:37]))
#===================================================================================
# Day 4, Cam 1
#===================================================================================
print("day4cam1")
d4c1.res<-as.data.frame(list(getTotalDistance(day4cam1),TimeMoving(day4cam1, POLYDAY4CAM1)[,2:37]))
#===================================================================================
# Day 4, Cam 2
#===================================================================================
print("day4cam2")
d4c2.res<-as.data.frame(list(getTotalDistance(day4cam2),TimeMoving(day4cam2, POLYDAY4CAM2)[,2:37]))
#===================================================================================
# Day 4, Cam 3
#===================================================================================
print("day4cam3")
d4c3.res<-as.data.frame(list(getTotalDistance(day4cam3),TimeMoving(day4cam3, POLYDAY4CAM3)[,2:37]))
#===================================================================================
# Day 4, Cam 4
#===================================================================================
print("day4cam4")
d4c4.res<-as.data.frame(list(getTotalDistance(day4cam4),TimeMoving(day4cam4, POLYDAY4CAM4)[,2:37]))
#===================================================================================
# Day 5, Cam 1
#===================================================================================
print("day5cam1")
d5c1.res<-as.data.frame(list(getTotalDistance(day5cam1),TimeMoving(day5cam1, POLYDAY5CAM1)[,2:37]))
#===================================================================================
# Day 5, Cam 2
#===================================================================================
print("day5cam2")
d5c2.res<-as.data.frame(list(getTotalDistance(day5cam2),TimeMoving(day5cam2, POLYDAY5CAM2)[,2:37]))
#===================================================================================
# Day 5, Cam 3
#===================================================================================
print("day5cam3")
d5c3.res<-as.data.frame(list(getTotalDistance(day5cam3),TimeMoving(day5cam3, POLYDAY5CAM3)[,2:37]))
#===================================================================================
# Day 5, Cam 4
#===================================================================================
print("day5cam4")
d5c4.res<-as.data.frame(list(getTotalDistance(day5cam4),TimeMoving(day5cam4, POLYDAY5CAM4)[,2:37]))
#===================================================================================
# Day 6, Cam 1
#===================================================================================
print("day6cam1")
d6c1.res<-as.data.frame(list(getTotalDistance(day6cam1),TimeMoving(day6cam1, POLYDAY6CAM1)[,2:37]))
#===================================================================================
# Day 6, Cam 2
#===================================================================================
print("day6cam2")
d6c2.res<-as.data.frame(list(getTotalDistance(day6cam2),TimeMoving(day6cam2, POLYDAY6CAM2)[,2:37]))
#===================================================================================
# Day 6, Cam 3
#===================================================================================
print("day6cam3")
d6c3.res<-as.data.frame(list(getTotalDistance(day6cam3),TimeMoving(day6cam3, POLYDAY6CAM3)[,2:37]))
#===================================================================================
# Day 7, Cam 1
#===================================================================================
print("day7cam1")
d7c1.res<-as.data.frame(list(getTotalDistance(day7cam1),TimeMoving(day7cam1, POLYDAY7CAM1)[,2:37]))
#===================================================================================
# Day 7, Cam 2
#===================================================================================
print("day7cam2")
d7c2.res<-as.data.frame(list(getTotalDistance(day7cam2),TimeMoving(day7cam2, POLYDAY7CAM2)[,2:37]))
#===================================================================================
# Day 7, Cam 3
#===================================================================================
print("day7cam3")
d7c3.res<-as.data.frame(list(getTotalDistance(day7cam3),TimeMoving(day7cam3, POLYDAY7CAM3)[,2:37]))
#===================================================================================
# Day 7, Cam 4
#===================================================================================
print("day7cam4")
d7c4.res<-as.data.frame(list(getTotalDistance(day7cam4),TimeMoving(day7cam4, POLYDAY7CAM4)[,2:37]))
#===================================================================================
# Day 8, Cam 1
#===================================================================================
print("day8cam1")
d8c1.res<-as.data.frame(list(getTotalDistance(day8cam1),TimeMoving(day8cam1, POLYDAY8CAM1)[,2:37]))
#===================================================================================
# Day 8, Cam 3
#===================================================================================
print("day8cam3")
d8c3.res<-as.data.frame(list(getTotalDistance(day8cam3),TimeMoving(day8cam3, POLYDAY8CAM3)[,2:37]))
#===================================================================================
# Day 8, Cam 4
#===================================================================================
print("day8cam4")
d8c4.res<-as.data.frame(list(getTotalDistance(day8cam4),TimeMoving(day8cam4, POLYDAY8CAM4)[,2:37]))

# create reference columns
day<-c(rep("day 1",16),rep("day 2",16),rep("day 3",16),rep("day 4",16),
       rep("day 5",16),rep("day 6",16),rep("day 7",16),rep("day 8",16))
camera<-c(rep(c(rep("cam 1",4), rep("cam 2",4), rep("cam 3",4),rep("cam 4",4)),8))
replicate<-c(rep("rep1",4), rep("rep2",4), rep("rep3",4),rep("rep4",4),
         rep("rep5",4), rep("rep6",4), rep("rep7",4),rep("rep8",4),
         rep("rep9",4), rep("rep10",4), rep("rep11",4),rep("rep12",4),
         rep("rep13",4), rep("rep14",4), rep("rep15",4),rep("rep16",4),
         rep("rep17",4), rep("rep18",4), rep("rep19",4),rep("rep20",4),
         rep("rep21",4), rep("rep22",4), rep("rep23",4),rep("rep24",4),
         rep("rep25",4), rep("rep26",4), rep("rep27",4),rep("rep28",4),
         rep("rep29",4), rep("rep30",4), rep("rep31",4),rep("rep32",4))

infection<-c(rep(c("infected","infected","control","control"),32))

id<-c(rep(c("I1","I2","C1","C2"),32))

# Get the day 6 cam 4, and day 8 cam 2 data frames (blank data frames)
d6c4.res <- data.frame(matrix(ncol = 55, nrow = 4))
colnames(d6c4.res) <- c("track",
                        "t1_totDist","t2_totDist","t3_totDist","t4_totDist","t5_totDist","t6_totDist","t7_totDist","t8_totDist","t9_totDist",
                        "t10_totDist","t11_totDist","t12_totDist","t13_totDist","t14_totDist","t15_totDist","t16_totDist","t17_totDist","t18_totDist",
                        "t1_time_spent_in_open", "t2_time_spent_in_open", "t3_time_spent_in_open",
                        "t4_time_spent_in_open", "t5_time_spent_in_open", "t6_time_spent_in_open",
                        "t7_time_spent_in_open", "t8_time_spent_in_open", "t9_time_spent_in_open",
                        "t10_time_spent_in_open", "t11_time_spent_in_open", "t12_time_spent_in_open",
                        "t13_time_spent_in_open", "t14_time_spent_in_open", "t15_time_spent_in_open",
                        "t16_time_spent_in_open", "t17_time_spent_in_open", "t18_time_spent_in_open",
                        "t1_distance_spent_in_open", "t2_distance_spent_in_open", "t3_distance_spent_in_open",
                        "t4_distance_spent_in_open", "t5_distance_spent_in_open", "t6_distance_spent_in_open",
                        "t7_distance_spent_in_open", "t8_distance_spent_in_open", "t9_distance_spent_in_open",
                        "t10_distance_spent_in_open", "t11_distance_spent_in_open", "t12_distance_spent_in_open",
                        "t13_distance_spent_in_open", "t14_distance_spent_in_open", "t15_distance_spent_in_open",
                        "t16_distance_spent_in_open", "t17_distance_spent_in_open", "t18_distance_spent_in_open")
d8c2.res <- data.frame(matrix(ncol = 55, nrow = 4))
colnames(d8c2.res) <- c("track",
                        "t1_totDist","t2_totDist","t3_totDist","t4_totDist","t5_totDist","t6_totDist","t7_totDist","t8_totDist","t9_totDist",
                        "t10_totDist","t11_totDist","t12_totDist","t13_totDist","t14_totDist","t15_totDist","t16_totDist","t17_totDist","t18_totDist",
                        "t1_time_spent_in_open", "t2_time_spent_in_open", "t3_time_spent_in_open",
                        "t4_time_spent_in_open", "t5_time_spent_in_open", "t6_time_spent_in_open",
                        "t7_time_spent_in_open", "t8_time_spent_in_open", "t9_time_spent_in_open",
                        "t10_time_spent_in_open", "t11_time_spent_in_open", "t12_time_spent_in_open",
                        "t13_time_spent_in_open", "t14_time_spent_in_open", "t15_time_spent_in_open",
                        "t16_time_spent_in_open", "t17_time_spent_in_open", "t18_time_spent_in_open",
                        "t1_distance_spent_in_open", "t2_distance_spent_in_open", "t3_distance_spent_in_open",
                        "t4_distance_spent_in_open", "t5_distance_spent_in_open", "t6_distance_spent_in_open",
                        "t7_distance_spent_in_open", "t8_distance_spent_in_open", "t9_distance_spent_in_open",
                        "t10_distance_spent_in_open", "t11_distance_spent_in_open", "t12_distance_spent_in_open",
                        "t13_distance_spent_in_open", "t14_distance_spent_in_open", "t15_distance_spent_in_open",
                        "t16_distance_spent_in_open", "t17_distance_spent_in_open", "t18_distance_spent_in_open")

# Build a dataframe
df.res<-data.frame(rbind(d1c1.res, d1c2.res, d1c3.res, d1c4.res,
                      d2c1.res, d2c2.res, d2c3.res, d2c4.res,
                      d3c1.res, d3c2.res, d3c3.res, d3c4.res,
                      d4c1.res, d4c2.res, d4c3.res, d4c4.res,
                      d5c1.res, d5c2.res, d5c3.res, d5c4.res,
                      d6c1.res, d6c2.res, d6c3.res, d6c4.res,
                      d7c1.res, d7c2.res, d7c3.res, d7c4.res,
                      d8c1.res, d8c2.res, d8c3.res, d8c4.res))

dim(df.res)
df.res["day"]<-day
df.res["camera"]<-camera
df.res["replicate"]<-replicate
df.res["treatment"]<-infection

#===================================================================================
# Part IIId. Build a unique data frame with hour 1,2 and 3
#===================================================================================

# Create new data frame just in case there are problems
df <- df.res

## Part I of analysis: total distance measures
# This calculates the total distance traveled from pixels to centimeters
df$t1_totDist <- round(df$t1_totDist * 0.02645833, 2)
df$t2_totDist <- round(df$t2_totDist * 0.02645833, 2)
df$t3_totDist <- round(df$t3_totDist * 0.02645833, 2)
df$t4_totDist <- round(df$t4_totDist * 0.02645833, 2)
df$t5_totDist <- round(df$t5_totDist * 0.02645833, 2)
df$t6_totDist <- round(df$t6_totDist * 0.02645833, 2)
df$t7_totDist <- round(df$t7_totDist * 0.02645833, 2)
df$t8_totDist <- round(df$t8_totDist * 0.02645833, 2)
df$t9_totDist <- round(df$t9_totDist * 0.02645833, 2)
df$t10_totDist <- round(df$t10_totDist * 0.02645833, 2)
df$t11_totDist <- round(df$t11_totDist * 0.02645833, 2)
df$t12_totDist <- round(df$t12_totDist * 0.02645833, 2)
df$t13_totDist <- round(df$t13_totDist * 0.02645833, 2)
df$t14_totDist <- round(df$t14_totDist * 0.02645833, 2)
df$t15_totDist <- round(df$t15_totDist * 0.02645833, 2)
df$t16_totDist <- round(df$t16_totDist * 0.02645833, 2)
df$t17_totDist <- round(df$t17_totDist * 0.02645833, 2)
df$t18_totDist <- round(df$t18_totDist * 0.02645833, 2)

# This calculates the total distance traveled from pixels to meters
# df["tot.distance.m"] <- df$tot.distance.cm / 100


## Part II of analysis: total distance in open area, and total time spent in open area measures
# This calculates the time spent in an open area from FRAMES to SECONDS
df$t1_time_spent_in_open <- df$t1_time_spent_in_open / 29.9 * 8
df$t2_time_spent_in_open <- df$t2_time_spent_in_open / 29.9 * 8
df$t3_time_spent_in_open <- df$t3_time_spent_in_open / 29.9 * 8
df$t4_time_spent_in_open <- df$t4_time_spent_in_open / 29.9 * 8
df$t5_time_spent_in_open <- df$t5_time_spent_in_open / 29.9 * 8
df$t6_time_spent_in_open <- df$t6_time_spent_in_open / 29.9 * 8
df$t7_time_spent_in_open <- df$t7_time_spent_in_open / 29.9 * 8
df$t8_time_spent_in_open <- df$t8_time_spent_in_open / 29.9 * 8
df$t9_time_spent_in_open <- df$t9_time_spent_in_open / 29.9 * 8
df$t10_time_spent_in_open <- df$t10_time_spent_in_open / 29.9 * 8
df$t11_time_spent_in_open <- df$t11_time_spent_in_open / 29.9 * 8
df$t12_time_spent_in_open <- df$t12_time_spent_in_open / 29.9 * 8
df$t13_time_spent_in_open <- df$t13_time_spent_in_open / 29.9 * 8
df$t14_time_spent_in_open <- df$t14_time_spent_in_open / 29.9 * 8
df$t15_time_spent_in_open <- df$t15_time_spent_in_open / 29.9 * 8
df$t16_time_spent_in_open <- df$t16_time_spent_in_open / 29.9 * 8
df$t17_time_spent_in_open <- df$t17_time_spent_in_open / 29.9 * 8
df$t18_time_spent_in_open <- df$t18_time_spent_in_open / 29.9 * 8

# This calculates the distance spent in an open area from pixels to centimeters
df$t1_distance_spent_in_open <- round(df$t1_distance_spent_in_open * 0.02645833, 2)
df$t2_distance_spent_in_open <- round(df$t2_distance_spent_in_open * 0.02645833, 2)
df$t3_distance_spent_in_open <- round(df$t3_distance_spent_in_open * 0.02645833, 2)
df$t4_distance_spent_in_open <- round(df$t4_distance_spent_in_open * 0.02645833, 2)
df$t5_distance_spent_in_open <- round(df$t5_distance_spent_in_open * 0.02645833, 2)
df$t6_distance_spent_in_open <- round(df$t6_distance_spent_in_open * 0.02645833, 2)
df$t7_distance_spent_in_open <- round(df$t7_distance_spent_in_open * 0.02645833, 2)
df$t8_distance_spent_in_open <- round(df$t8_distance_spent_in_open * 0.02645833, 2)
df$t9_distance_spent_in_open <- round(df$t9_distance_spent_in_open * 0.02645833, 2)
df$t10_distance_spent_in_open <- round(df$t10_distance_spent_in_open * 0.02645833, 2)
df$t11_distance_spent_in_open <- round(df$t11_distance_spent_in_open * 0.02645833, 2)
df$t12_distance_spent_in_open <- round(df$t12_distance_spent_in_open * 0.02645833, 2)
df$t13_distance_spent_in_open <- round(df$t13_distance_spent_in_open * 0.02645833, 2)
df$t14_distance_spent_in_open <- round(df$t14_distance_spent_in_open * 0.02645833, 2)
df$t15_distance_spent_in_open <- round(df$t15_distance_spent_in_open * 0.02645833, 2)
df$t16_distance_spent_in_open <- round(df$t16_distance_spent_in_open * 0.02645833, 2)
df$t17_distance_spent_in_open <- round(df$t17_distance_spent_in_open * 0.02645833, 2)
df$t18_distance_spent_in_open <- round(df$t18_distance_spent_in_open * 0.02645833, 2)

# This calculates the average speed within the open area
#df["open.speed.cmxmin"] <- round(df$open.distance.cm / df$minutes.moving, digits = 1)
#df$open.speed.cmxmin <- ifelse((is.nan(df$open.speed.cmxmin) | is.infinite(df$open.speed.cmxmin)), 0, df$open.speed.cmxmin)

## Part III of analysis: "total" columns
df$total_distance <- rowSums(df[,2:19], na.rm = TRUE)
df$total_opentime <- rowSums(df[,20:37], na.rm = TRUE)
df$total_open_distance <- rowSums(df[,38:55], na.rm = TRUE)

# change distance column names
colnames(df)[which(names(df) == "t1_totDist")] <- "t1_distance"
colnames(df)[which(names(df) == "t2_totDist")] <- "t2_distance"
colnames(df)[which(names(df) == "t3_totDist")] <- "t3_distance"
colnames(df)[which(names(df) == "t4_totDist")] <- "t4_distance"
colnames(df)[which(names(df) == "t5_totDist")] <- "t5_distance"
colnames(df)[which(names(df) == "t6_totDist")] <- "t6_distance"
colnames(df)[which(names(df) == "t7_totDist")] <- "t7_distance"
colnames(df)[which(names(df) == "t8_totDist")] <- "t8_distance"
colnames(df)[which(names(df) == "t9_totDist")] <- "t9_distance"
colnames(df)[which(names(df) == "t10_totDist")] <- "t10_distance"
colnames(df)[which(names(df) == "t11_totDist")] <- "t11_distance"
colnames(df)[which(names(df) == "t12_totDist")] <- "t12_distance"
colnames(df)[which(names(df) == "t13_totDist")] <- "t13_distance"
colnames(df)[which(names(df) == "t14_totDist")] <- "t14_distance"
colnames(df)[which(names(df) == "t15_totDist")] <- "t15_distance"
colnames(df)[which(names(df) == "t16_totDist")] <- "t16_distance"
colnames(df)[which(names(df) == "t17_totDist")] <- "t17_distance"
colnames(df)[which(names(df) == "t18_totDist")] <- "t18_distance"

# reorganize columns
df <- df[,c(56:59,2:19,60,20:37,61,38:55,62)]

# write all of the files
output_direc <- "~/Desktop/Levy_Research/Laboratory/infectChiriAnalysis/data"

# main data frame
filename <- file.path(output_direc, paste("rawData10022018.csv", sep=""))
write.csv(df, filename, row.names = F)

# separate the main data frame into the infected and control
setwd(output_direc)
main <- read.csv("rawData10022018.csv")
infected <- main[which(main$treatment == "infected"),]
control <- main[which(main$treatment == "control"),]

# write infected file
filename <- file.path(output_direc, paste("rawData10022018_infected.csv", sep=""))
write.csv(infected, filename, row.names = F)

# write infected file
filename <- file.path(output_direc, paste("rawData10022018_control.csv", sep=""))
write.csv(control, filename, row.names = F)

#===================================================================================
# Part IV: Plot all files
#  IVa. Plot total distance
#  IVb. Plot effective time spent in an open area
#  IVc. Plot distance traveled in an open area
#  IVe. Get PDF file
#===================================================================================
#===================================================================================
# Part IVa. Plot total distance
#===================================================================================
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
  ggtitle("Total distance traveled (cm)")+
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

# plot by day and hour (all days)

p2<-ggplot(df,aes(x=infection.status, y=tot.distance.cm, fill=infection.status))+
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

#===================================================================================
# Part IVb. Plot effective time spent in an open area
#===================================================================================
pt<-ggplot(df,aes(x=infection.status, y=minutes.moving, fill=infection.status))+
  geom_boxplot(width=.5, alpha=0.4)+
  #geom_violin(width=.5, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=c(8), size=c(3),color=c("black"), position=position_dodge(0.8))+
  geom_point(aes(colour=infection.status),position = position_jitter(0.2),size=.9)+
  #geom_jitter(aes(colour=eggtype),position=position_jitter(0.2))+
  #geom_dotplot(alpha=0.2,binaxis='y', stackdir='center', position=position_dodge(0.8))+
  scale_fill_manual( labels = c("Non-infected","Infected"),values = c("blue","red"))+
  scale_color_manual(labels = c("Non-infected","Infected"), values = c("blue", "red"))+
  ggtitle("Total time moving in an open area(minutes)")+
  ylab("time spent in an open area (minutes)")+
  xlab("treatment")+
  #facet_wrap(~day, ncol=3)+
  theme(legend.position="right",plot.title = element_text(hjust = 0.5),
        #axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

pt

pt1<-ggplot(df,aes(x=infection.status, y=minutes.moving, fill=infection.status))+
  geom_boxplot(width=.5, alpha=0.4)+
  #geom_violin(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=c(8), size=c(3),color=c("black"), position=position_dodge(0.8))+
  geom_point(aes(colour=infection.status),position = position_jitter(0.2),size=.9)+
  #geom_jitter(aes(colour=eggtype),position=position_jitter(0.2))+
  #geom_dotplot(alpha=0.2,binaxis='y', stackdir='center', position=position_dodge(0.8))+
  scale_fill_manual( labels = c("Non-infected","Infected"),values = c("blue","red"))+
  scale_color_manual(labels = c("Non-infected","Infected"), values = c("blue", "red"))+
  ylab("time spent in an open area (minutes)")+
  #xlab("treatment")+
  facet_grid(.~hour)+
  #facet_wrap(~day, ncol=3)+
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
pt1

# plot by day and hour
pt2<-ggplot(df,aes(x=infection.status, y=minutes.moving, fill=infection.status))+
  geom_boxplot(width=.5, alpha=0.4)+
  #geom_violin(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=c(8), size=c(3),color=c("black"), position=position_dodge(0.8))+
  geom_point(aes(colour=infection.status),position = position_jitter(0.2),size=.5)+
  #geom_jitter(aes(colour=eggtype),position=position_jitter(0.2))+
  #geom_dotplot(alpha=0.2,binaxis='y', stackdir='center', position=position_dodge(0.8))+
  scale_fill_manual( labels = c("Non-infected","Infected"),values = c("blue","red"))+
  scale_color_manual(labels = c("Non-infected","Infected"), values = c("blue", "red"))+
  ylab("time spent in an open area (minutes)")+
  # xlab("treatment")+
  facet_grid(day~hour)+
  # facet_wrap(day~hour, ncol=3)+
  theme(legend.position="right",plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

pt2

#===================================================================================
# Part IVc. Plot distance traveled in an open area
#===================================================================================
ps<-ggplot(df,aes(x=infection.status, y=open.distance.cm, fill=infection.status))+
  geom_boxplot(width=.5, alpha=0.4)+
  #geom_violin(width=.5, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=c(8), size=c(3),color=c("black"), position=position_dodge(0.8))+
  geom_point(aes(colour=infection.status),position = position_jitter(0.2),size=.9)+
  #geom_jitter(aes(colour=eggtype),position=position_jitter(0.2))+
  #geom_dotplot(alpha=0.2,binaxis='y', stackdir='center', position=position_dodge(0.8))+
  scale_fill_manual( labels = c("Non-infected","Infected"),values = c("blue","red"))+
  scale_color_manual(labels = c("Non-infected","Infected"), values = c("blue", "red"))+
  ggtitle("Distance traveled in open area (cm)")+
  ylab("distance traveled in open area (cm)")+
  xlab("treatment")+
  #facet_wrap(~day, ncol=3)+
  theme(legend.position="right",plot.title = element_text(hjust = 0.5),
        #axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ps

ps1<-ggplot(df,aes(x=infection.status, y=open.distance.cm, fill=infection.status))+
  geom_boxplot(width=.5, alpha=0.4)+
  #geom_violin(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=c(8), size=c(3),color=c("black"), position=position_dodge(0.8))+
  geom_point(aes(colour=infection.status),position = position_jitter(0.2),size=.9)+
  #geom_jitter(aes(colour=eggtype),position=position_jitter(0.2))+
  #geom_dotplot(alpha=0.2,binaxis='y', stackdir='center', position=position_dodge(0.8))+
  scale_fill_manual( labels = c("Non-infected","Infected"),values = c("blue","red"))+
  scale_color_manual(labels = c("Non-infected","Infected"), values = c("blue", "red"))+
  ylab("distance traveled in open area (cm)")+
  #xlab("treatment")+
  facet_grid(.~hour)+
  #facet_wrap(~day, ncol=3)+
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ps1

# plot by day and hour
ps2<-ggplot(df,aes(x=infection.status, y=open.distance.cm, fill=infection.status))+
  geom_boxplot(width=.5, alpha=0.4)+
  #geom_violin(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=c(8), size=c(3),color=c("black"), position=position_dodge(0.8))+
  geom_point(aes(colour=infection.status),position = position_jitter(0.2),size=.5)+
  #geom_jitter(aes(colour=eggtype),position=position_jitter(0.2))+
  #geom_dotplot(alpha=0.2,binaxis='y', stackdir='center', position=position_dodge(0.8))+
  scale_fill_manual( labels = c("Non-infected","Infected"),values = c("blue","red"))+
  scale_color_manual(labels = c("Non-infected","Infected"), values = c("blue", "red"))+
  ylab("distance traveled in open area (cm)")+
  # xlab("treatment")+
  facet_grid(day~hour)+
  # facet_wrap(day~hour, ncol=3)+
  theme(legend.position="right",plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ps2


#===================================================================================
# Part IVd. Get PDF file
#===================================================================================
pdf(file = "video analysis II.pdf", width=6,height=4,paper='special')#, paper="A4r",)
p
p1
#p2
pt
pt1
#pt2
ps
ps1
#ps2

dev.off()
