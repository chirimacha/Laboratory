#===================================================================================
# Code to Compare total distance, time and speed of chirimachas 
# infected and non infected 1,2 and 3 hours
# written by Justin Sheen and Renzo Salazar may 2018
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
#  Ia. Hour one
#  Ib. Hour two
#  Ic. Hour three
#===================================================================================

# Set working directory
# setwd("D:/LABORATORIO/ENSAYOS/JUSTIN/fixed videos/all csv")
#setwd("~/GITHUB/Laboratory/Triatoma infected behavior_Video")
setwd("~/Desktop/Laboratory/Triatoma infected behavior_Video")
getwd()

# Load CSV files
#===================================================================================
# Part Ia. Hour one
#===================================================================================
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

#===================================================================================
# Part Ib. Hour two
#===================================================================================
d1c1.2 <- read.csv("DAY1_CAM1_2HR.csv")
d1c2.2 <- read.csv("DAY1_CAM2_2HR.csv")
d1c3.2 <- read.csv("DAY1_CAM3_2HR.csv")
d1c4.2 <- read.csv("DAY1_CAM4_2HR.csv")
d2c1.2 <- read.csv("DAY2_CAM1_2HR.csv")
d2c2.2 <- read.csv("DAY2_CAM2_2HR.csv")
d2c3.2 <- read.csv("DAY2_CAM3_2HR.csv")
d2c4.2 <- read.csv("DAY2_CAM4_2HR.csv")
d3c1.2 <- read.csv("DAY3_CAM1_2HR.csv")
d3c2.2 <- read.csv("DAY3_CAM2_2HR.csv")
d3c3.2 <- read.csv("DAY3_CAM3_2HR.csv")
d3c4.2 <- read.csv("DAY3_CAM4_2HR.csv")
d4c1.2 <- read.csv("DAY4_CAM1_2HR.csv")
d4c2.2 <- read.csv("DAY4_CAM2_2HR.csv")
d4c3.2 <- read.csv("DAY4_CAM3_2HR.csv")
d4c4.2 <- read.csv("DAY4_CAM4_2HR.csv")
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

#===================================================================================
# Part Ic: Hour three
#===================================================================================
d1c1.3 <- read.csv("DAY1_CAM1_3HR.csv")
d1c2.3 <- read.csv("DAY1_CAM2_3HR.csv")
d1c3.3 <- read.csv("DAY1_CAM3_3HR.csv")
d1c4.3 <- read.csv("DAY1_CAM4_3HR.csv")
d2c1.3 <- read.csv("DAY2_CAM1_3HR.csv")
d2c2.3 <- read.csv("DAY2_CAM2_3HR.csv")
d2c3.3 <- read.csv("DAY2_CAM3_3HR.csv")
d2c4.3 <- read.csv("DAY2_CAM4_3HR.csv")
d3c1.3 <- read.csv("DAY3_CAM1_3HR.csv")
d3c2.3 <- read.csv("DAY3_CAM2_3HR.csv")
d3c3.3 <- read.csv("DAY3_CAM3_3HR.csv")
d3c4.3 <- read.csv("DAY3_CAM4_3HR.csv")
d4c1.3 <- read.csv("DAY4_CAM1_3HR.csv")
d4c2.3 <- read.csv("DAY4_CAM2_3HR.csv")
d4c3.3 <- read.csv("DAY4_CAM3_3HR.csv")
d4c4.3 <- read.csv("DAY4_CAM4_3HR.csv")
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


#===================================================================================
# Part II: Load the following two functions which we are going to use to get data for analysis
#  IIa. Test that all polygons have been created correctly and get polygons
#  IIb. Total distance traveled by hour by the bugs
#  IIc. In an open area: total distance AND total distance traveled by hour by the bugs (two results)
#===================================================================================
#===================================================================================
# Part IIa: Test that all polygons have been created correctly and get polygons
#===================================================================================

setwd("~/Desktop/Laboratory/infectChiriAnalysis")
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
# compare total distances function
getTotalDistance <- function(df) {
  # only keep columns we need
  toKeep <- c("frame", "track", "x", "y")
  df <- df[,toKeep]
  
  # split into data frames for each of the chirimachas
  bugOne <- df[which(df$track == 1),]
  bugTwo <- df[which(df$track == 2),]
  bugThree <- df[which(df$track == 3),]
  bugFour <- df[which(df$track == 4),]
  
  # impute missing data (part one)
  imputeMissing <- function(bugTable, inputTrack) {
    if (length(which(bugTable$frame == 1)) == 0) {
      if (nrow(bugTable) == 0) {
        bugTable <- data.frame(matrix(ncol=4, nrow=1))
        colnames(bugTable) <- c("track", "frame", "x", "y")
        bugTable$track <- as.numeric(inputTrack)
        bugTable$frame <- 1
        bugTable$x <- 0
        bugTable$y <- 0
      } else {
        # impute with the first place bug one was found
        toAddFrameOne <- data.frame(matrix(ncol = 4, nrow = 1))
        colnames(toAddFrameOne) <- c("track", "frame", "x", "y")
        toAddFrameOne$track[1] <- bugTable$track[which(bugTable$track == as.numeric(inputTrack))[1]]
        toAddFrameOne$frame[1] <- 1
        toAddFrameOne$x[1] <- bugTable$x[which(bugTable$track == as.numeric(inputTrack))[1]]
        toAddFrameOne$y[1] <- bugTable$y[which(bugTable$track == as.numeric(inputTrack))[1]]
        
        # rbind to previous table
        bugTable <- rbind(bugTable, toAddFrameOne)
      }
    }
    return(bugTable)
  }
  
  bugOne <- imputeMissing(bugOne, 1)
  bugTwo <- imputeMissing(bugTwo, 2)
  bugThree <- imputeMissing(bugThree, 3)
  bugFour <- imputeMissing(bugFour, 4)
  
  # stop the function
  stopifnot(length(which(bugOne$frame == 1)) != 0)
  stopifnot(length(which(bugTwo$frame == 1)) != 0)
  stopifnot(length(which(bugThree$frame == 1)) != 0)
  stopifnot(length(which(bugFour$frame == 1)) != 0)
  
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
    
    # impute data if missing (impute the immediately previous frame) (part two of missing data)
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
  colnames(toReturn) <- c("track", "total_distance_traveled_pixels")
  toReturn$track <- seq.int(4)
  toReturn[1,2] <- sum(bugOne$distance)
  toReturn[2,2] <- sum(bugTwo$distance)
  toReturn[3,2] <- sum(bugThree$distance)
  toReturn[4,2] <- sum(bugFour$distance)
  
  return(toReturn)
}

#===================================================================================
# Part IIc. In an open area: total distance AND total distance traveled by hour by the bugs (two results)
#===================================================================================
TimeMoving <- function(df, inputPoly) {
  # only keep columns we need
  toKeep <- c("frame", "track", "x", "y")
  df <- df[,toKeep]
  
  # split into data frames for each of the chirimachas
  bugOne <- df[which(df$track == 1),]
  bugTwo <- df[which(df$track == 2),]
  bugThree <- df[which(df$track == 3),]
  bugFour <- df[which(df$track == 4),]
  
  # impute missing data (part one)
  imputeMissing <- function(bugTable, inputTrack) {
    if (length(which(bugTable$frame == 1)) == 0) {
      if (nrow(bugTable) == 0) {
        bugTable <- data.frame(matrix(ncol=4, nrow=1))
        colnames(bugTable) <- c("track", "frame", "x", "y")
        bugTable$track <- as.numeric(inputTrack)
        bugTable$frame <- 1
        bugTable$x <- 0
        bugTable$y <- 0
      } else {
        # impute with the first place bug one was found
        toAddFrameOne <- data.frame(matrix(ncol = 4, nrow = 1))
        colnames(toAddFrameOne) <- c("track", "frame", "x", "y")
        toAddFrameOne$track[1] <- bugTable$track[which(bugTable$track == as.numeric(inputTrack))[1]]
        toAddFrameOne$frame[1] <- 1
        toAddFrameOne$x[1] <- bugTable$x[which(bugTable$track == as.numeric(inputTrack))[1]]
        toAddFrameOne$y[1] <- bugTable$y[which(bugTable$track == as.numeric(inputTrack))[1]]
        
        # rbind to previous table
        bugTable <- rbind(bugTable, toAddFrameOne)
      }
    }
    return(bugTable)
  }
  
  bugOne <- imputeMissing(bugOne, 1)
  bugTwo <- imputeMissing(bugTwo, 2)
  bugThree <- imputeMissing(bugThree, 3)
  bugFour <- imputeMissing(bugFour, 4)
  
  # stop the function
  stopifnot(length(which(bugOne$frame == 1)) != 0)
  stopifnot(length(which(bugTwo$frame == 1)) != 0)
  stopifnot(length(which(bugThree$frame == 1)) != 0)
  stopifnot(length(which(bugFour$frame == 1)) != 0)
  
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
    
    # impute data if missing (part two)
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
  
  # next, have function to get distance traveled within the polygon
  addCenterDistance <- function(bugRaw, poly=inputPoly) {
    # change polygon so that it is smaller
    poly[1,1] <- poly[1,1] + 50
    poly[1,2] <- poly[1,2] - 50
    
    poly[2,1] <- poly[2,1] - 50
    poly[2,2] <- poly[2,2] - 50
    
    poly[3,1] <- poly[3,1] - 50
    poly[3,2] <- poly[3,2] + 50
    
    poly[4,1] <- poly[4,1] + 50
    poly[4,2] <- poly[4,2] + 50
    
    
    # get whether each frame was in the polygon or not
    bugRaw$inPoly <- FALSE
    for (k in 1:nrow(bugRaw)) {
      if (point.in.polygon(bugRaw$x[k], bugRaw$y[k], poly[,1], poly[,2])) {
        bugRaw$inPoly[k] <- TRUE
      }
    }
    
    # get the distance traveled
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
  
  
  toReturn<- data.frame(matrix(ncol = 3, nrow = 4))
  colnames(toReturn) <- c("track", "time_spent_in_open", "distance_spent_in_open")
  toReturn$track <- seq.int(4)
  toReturn[1,2] <- sum(ifelse(bugOne$inPoly, 1, 0))
  toReturn[2,2] <- sum(ifelse(bugTwo$inPoly, 1, 0))
  toReturn[3,2] <- sum(ifelse(bugThree$inPoly, 1, 0))
  toReturn[4,2] <- sum(ifelse(bugFour$inPoly, 1, 0))
  
  toReturn[1,3] <- sum(bugOne$abiertaDistancia)
  toReturn[2,3] <- sum(bugTwo$abiertaDistancia)
  toReturn[3,3] <- sum(bugThree$abiertaDistancia)
  toReturn[4,3] <- sum(bugFour$abiertaDistancia)
  
  # add columns to time in minutes (divide between 29.9 frames) and / 60 seconds
  toReturn$minutes.moving <- round(((toReturn[,2] / 29.9) / 60) * 8, digits = 1)
  toReturn$minutes.staying <- round((((max(df$frame) - toReturn[,2]) / 29.9) / 60) * 8, digits = 1)
  
  return(toReturn)
}

#===================================================================================
# Part III: Analysis of all files (combining part I and part II)
#  IIIa. Analysis hour one
#  IIIb. Analysis hour two
#  IIIc. Analysis hour three
#  IIId. Build a unique data frame with hour 1,2 and 3
#  IIIe. Plot total distance
#  IIIf. Plot effective time moving
#  IIIg. Plot speed
#  IIIh. Get PDF file
#===================================================================================
#===================================================================================
# Part IIIa. Analysis hour one
#===================================================================================
d1.1<-as.data.frame(list(getTotalDistance(d1c1),TimeMoving(d1c1, POLYDAY1CAM1)[,2:5]))
d1.2<-as.data.frame(list(getTotalDistance(d1c2),TimeMoving(d1c2, POLYDAY1CAM2)[,2:5]))
d1.3<-as.data.frame(list(getTotalDistance(d1c3),TimeMoving(d1c3, POLYDAY1CAM3)[,2:5]))
d1.4<-as.data.frame(list(getTotalDistance(d1c4),TimeMoving(d1c4, POLYDAY1CAM4)[,2:5]))
d2.1<-as.data.frame(list(getTotalDistance(d2c1),TimeMoving(d2c1, POLYDAY2CAM1)[,2:5]))
d2.2<-as.data.frame(list(getTotalDistance(d2c2),TimeMoving(d2c2, POLYDAY2CAM2)[,2:5]))
d2.3<-as.data.frame(list(getTotalDistance(d2c3),TimeMoving(d2c3, POLYDAY2CAM3)[,2:5]))
d2.4<-as.data.frame(list(getTotalDistance(d2c4),TimeMoving(d2c4, POLYDAY2CAM4)[,2:5]))
d3.1<-as.data.frame(list(getTotalDistance(d3c1),TimeMoving(d3c1, POLYDAY3CAM1)[,2:5]))
d3.2<-as.data.frame(list(getTotalDistance(d3c2),TimeMoving(d3c2, POLYDAY3CAM2)[,2:5]))
d3.3<-as.data.frame(list(getTotalDistance(d3c3),TimeMoving(d3c3, POLYDAY3CAM3)[,2:5]))
d3.4<-as.data.frame(list(getTotalDistance(d3c4),TimeMoving(d3c4, POLYDAY3CAM4)[,2:5]))
d4.1<-as.data.frame(list(getTotalDistance(d4c1),TimeMoving(d4c1, POLYDAY4CAM1)[,2:5]))
d4.2<-as.data.frame(list(getTotalDistance(d4c2),TimeMoving(d4c2, POLYDAY4CAM2)[,2:5]))
d4.3<-as.data.frame(list(getTotalDistance(d4c3),TimeMoving(d4c3, POLYDAY4CAM3)[,2:5]))
d4.4<-as.data.frame(list(getTotalDistance(d4c4),TimeMoving(d4c4, POLYDAY4CAM4)[,2:5]))
d5.1<-as.data.frame(list(getTotalDistance(d5c1),TimeMoving(d5c1, POLYDAY5CAM1)[,2:5]))
d5.2<-as.data.frame(list(getTotalDistance(d5c2),TimeMoving(d5c2, POLYDAY5CAM2)[,2:5]))
d5.3<-as.data.frame(list(getTotalDistance(d5c3),TimeMoving(d5c3, POLYDAY5CAM3)[,2:5]))
d5.4<-as.data.frame(list(getTotalDistance(d5c4),TimeMoving(d5c4, POLYDAY5CAM4)[,2:5]))
d6.1<-as.data.frame(list(getTotalDistance(d6c1),TimeMoving(d6c1, POLYDAY6CAM1)[,2:5]))
d6.2<-as.data.frame(list(getTotalDistance(d6c2),TimeMoving(d6c2, POLYDAY6CAM2)[,2:5]))
d6.3<-as.data.frame(list(getTotalDistance(d6c3),TimeMoving(d6c3, POLYDAY6CAM3)[,2:5]))
d6.4<-as.data.frame(list(getTotalDistance(d6c4),TimeMoving(d6c4, POLYDAY6CAM4)[,2:5]))
d7.1<-as.data.frame(list(getTotalDistance(d7c1),TimeMoving(d7c1, POLYDAY7CAM1)[,2:5]))
d7.2<-as.data.frame(list(getTotalDistance(d7c2),TimeMoving(d7c2, POLYDAY7CAM2)[,2:5]))
d7.3<-as.data.frame(list(getTotalDistance(d7c3),TimeMoving(d7c3, POLYDAY7CAM3)[,2:5]))
d7.4<-as.data.frame(list(getTotalDistance(d7c4),TimeMoving(d7c4, POLYDAY7CAM4)[,2:5]))
d8.1<-as.data.frame(list(getTotalDistance(d8c1),TimeMoving(d8c1, POLYDAY8CAM1)[,2:5]))
d8.2<-as.data.frame(list(getTotalDistance(d8c2),TimeMoving(d8c2, POLYDAY8CAM2)[,2:5]))
d8.3<-as.data.frame(list(getTotalDistance(d8c3),TimeMoving(d8c3, POLYDAY8CAM3)[,2:5]))
d8.4<-as.data.frame(list(getTotalDistance(d8c4),TimeMoving(d8c4, POLYDAY8CAM4)[,2:5]))

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

# Build a dataframe hour 1
df1<-data.frame(rbind(d1.1,d1.2,d1.3,d1.4,d2.1,d2.2,d2.3,d2.4,d3.1,d3.2,d3.3,d3.4,
                      d4.1,d4.2,d4.3,d4.4,d5.1,d5.2,d5.3,d5.4,d6.1,d6.2,d6.3,d6.4,
                      d7.1,d7.2,d7.3,d7.4,d8.1,d8.2,d8.3,d8.4))

dim(df1)
df1["day"]<-day
df1["camera"]<-camera
df1["arena"]<-arena
df1["infection.status"]<-infection

#===================================================================================
# Part IIIb. Analysis hour two
#===================================================================================
d1.1.2<-as.data.frame(list(getTotalDistance(d1c1.2),TimeMoving(d1c1.2, POLYDAY1CAM1)[,2:5]))
d1.2.2<-as.data.frame(list(getTotalDistance(d1c2.2),TimeMoving(d1c2.2, POLYDAY1CAM2)[,2:5]))
d1.3.2<-as.data.frame(list(getTotalDistance(d1c3.2),TimeMoving(d1c3.2, POLYDAY1CAM3)[,2:5]))
d1.4.2<-as.data.frame(list(getTotalDistance(d1c4.2),TimeMoving(d1c4.2, POLYDAY1CAM4)[,2:5]))
d2.1.2<-as.data.frame(list(getTotalDistance(d2c1.2),TimeMoving(d2c1.2, POLYDAY2CAM1)[,2:5]))
d2.2.2<-as.data.frame(list(getTotalDistance(d2c2.2),TimeMoving(d2c2.2, POLYDAY2CAM2)[,2:5]))
d2.3.2<-as.data.frame(list(getTotalDistance(d2c3.2),TimeMoving(d2c3.2, POLYDAY2CAM3)[,2:5]))
d2.4.2<-as.data.frame(list(getTotalDistance(d2c4.2),TimeMoving(d2c4.2, POLYDAY2CAM4)[,2:5]))
d3.1.2<-as.data.frame(list(getTotalDistance(d3c1.2),TimeMoving(d3c1.2, POLYDAY3CAM1)[,2:5]))
d3.2.2<-as.data.frame(list(getTotalDistance(d3c2.2),TimeMoving(d3c2.2, POLYDAY3CAM2)[,2:5]))
d3.3.2<-as.data.frame(list(getTotalDistance(d3c3.2),TimeMoving(d3c3.2, POLYDAY3CAM3)[,2:5]))
d3.4.2<-as.data.frame(list(getTotalDistance(d3c4.2),TimeMoving(d3c4.2, POLYDAY3CAM4)[,2:5]))
d4.1.2<-as.data.frame(list(getTotalDistance(d4c1.2),TimeMoving(d4c1.2, POLYDAY4CAM1)[,2:5]))
d4.2.2<-as.data.frame(list(getTotalDistance(d4c2.2),TimeMoving(d4c2.2, POLYDAY4CAM2)[,2:5]))
d4.3.2<-as.data.frame(list(getTotalDistance(d4c3.2),TimeMoving(d4c3.2, POLYDAY4CAM3)[,2:5]))
d4.4.2<-as.data.frame(list(getTotalDistance(d4c4.2),TimeMoving(d4c4.2, POLYDAY4CAM4)[,2:5]))
d5.1.2<-as.data.frame(list(getTotalDistance(d5c1.2),TimeMoving(d5c1.2, POLYDAY5CAM1)[,2:5]))
d5.2.2<-as.data.frame(list(getTotalDistance(d5c2.2),TimeMoving(d5c2.2, POLYDAY5CAM2)[,2:5]))
d5.3.2<-as.data.frame(list(getTotalDistance(d5c3.2),TimeMoving(d5c3.2, POLYDAY5CAM3)[,2:5]))
d5.4.2<-as.data.frame(list(getTotalDistance(d5c4.2),TimeMoving(d5c4.2, POLYDAY5CAM4)[,2:5]))
d6.1.2<-as.data.frame(list(getTotalDistance(d6c1.2),TimeMoving(d6c1.2, POLYDAY6CAM1)[,2:5]))
d6.2.2<-as.data.frame(list(getTotalDistance(d6c2.2),TimeMoving(d6c2.2, POLYDAY6CAM2)[,2:5]))
d6.3.2<-as.data.frame(list(getTotalDistance(d6c3.2),TimeMoving(d6c3.2, POLYDAY6CAM3)[,2:5]))
#d6.4 ==>there is no video
d7.1.2<-as.data.frame(list(getTotalDistance(d7c1.2),TimeMoving(d7c1.2, POLYDAY7CAM1)[,2:5]))
d7.2.2<-as.data.frame(list(getTotalDistance(d7c2.2),TimeMoving(d7c2.2, POLYDAY7CAM2)[,2:5]))
d7.3.2<-as.data.frame(list(getTotalDistance(d7c3.2),TimeMoving(d7c3.2, POLYDAY7CAM3)[,2:5]))
d7.4.2<-as.data.frame(list(getTotalDistance(d7c4.2),TimeMoving(d7c4.2, POLYDAY7CAM4)[,2:5]))
d8.1.2<-as.data.frame(list(getTotalDistance(d8c1.2),TimeMoving(d8c1.2, POLYDAY8CAM1)[,2:5]))
#d8.2 ==> there is no video
d8.3.2<-as.data.frame(list(getTotalDistance(d8c3.2),TimeMoving(d8c3.2, POLYDAY8CAM3)[,2:5]))
d8.4.2<-as.data.frame(list(getTotalDistance(d8c4.2),TimeMoving(d8c4.2, POLYDAY8CAM4)[,2:5]))


# create reference columns

day2<-c(rep("day 1",16),rep("day 2",16),rep("day 3",16),rep("day 4",16),
        rep("day 5",16),rep("day 6",12),rep("day 7",16),rep("day 8",12))
camera2<-c(rep(c(rep("cam 1",4), rep("cam 2",4), rep("cam 3",4),rep("cam 4",4)),4),
           rep("cam 1",4), rep("cam 2",4), rep("cam 3",4),rep("cam 4",4),
           rep("cam 1",4), rep("cam 2",4), rep("cam 3",4),
           rep("cam 1",4), rep("cam 2",4), rep("cam 3",4),rep("cam 4",4),
           rep("cam 1",4), rep("cam 3",4),rep("cam 4",4))
arena2<-c(rep("arena 1",4), rep("arena 2",4), rep("arena 3",4),rep("arena 4",4),
          rep("arena 5",4), rep("arena 6",4), rep("arena 7",4),rep("arena 8",4),
          rep("arena 9",4), rep("arena 10",4), rep("arena 11",4),rep("arena 12",4),
          rep("arena 13",4), rep("arena 14",4), rep("arena 15",4),rep("arena 16",4),
          rep("arena 17",4), rep("arena 18",4), rep("arena 19",4),rep("arena 20",4),
          rep("arena 21",4), rep("arena 22",4), rep("arena 23",4),#rep("arena 24",4),
          rep("arena 25",4), rep("arena 26",4), rep("arena 27",4),rep("arena 28",4),
          rep("arena 29",4), #rep("arena 30",4), 
          rep("arena 31",4),rep("arena 32",4))

infection2<-c(rep(c("infected","infected","control","control"),30))

# Built a dataframe hour2
df2<-data.frame(rbind(d1.1.2,d1.2.2,d1.3.2,d1.4.2,d2.1.2,d2.2.2,d2.3.2,d2.4.2,
                      d3.1.2,d3.2.2,d3.3.2,d3.4.2,d4.1.2,d4.2.2,d4.3.2,d4.4.2,
                      d5.1.2,d5.2.2,d5.3.2,d5.4.2,d6.1.2,d6.2.2,d6.3.2,
                      d7.1.2,d7.2.2,d7.3.2,d7.4.2,d8.1.2,d8.3.2,d8.4.2))
dim(df2)
df2["day"]<-day2
df2["camera"]<-camera2
df2["arena"]<-arena2
df2["infection.status"]<-infection2

#===================================================================================
# Part IIIc. Analysis hour three
#===================================================================================
d1.1.3<-as.data.frame(list(getTotalDistance(d1c1.3),TimeMoving(d1c1.3, POLYDAY1CAM1)[,2:5]))
d1.2.3<-as.data.frame(list(getTotalDistance(d1c2.3),TimeMoving(d1c2.3, POLYDAY1CAM2)[,2:5]))
d1.3.3<-as.data.frame(list(getTotalDistance(d1c3.3),TimeMoving(d1c3.3, POLYDAY1CAM3)[,2:5]))
d1.4.3<-as.data.frame(list(getTotalDistance(d1c4.3),TimeMoving(d1c4.3, POLYDAY1CAM4)[,2:5]))
d2.1.3<-as.data.frame(list(getTotalDistance(d2c1.3),TimeMoving(d2c1.3, POLYDAY2CAM1)[,2:5]))
d2.2.3<-as.data.frame(list(getTotalDistance(d2c2.3),TimeMoving(d2c2.3, POLYDAY2CAM2)[,2:5]))
d2.3.3<-as.data.frame(list(getTotalDistance(d2c3.3),TimeMoving(d2c3.3, POLYDAY2CAM3)[,2:5]))
d2.4.3<-as.data.frame(list(getTotalDistance(d2c4.3),TimeMoving(d2c4.3, POLYDAY2CAM4)[,2:5]))
d3.1.3<-as.data.frame(list(getTotalDistance(d3c1.3),TimeMoving(d3c1.3, POLYDAY3CAM1)[,2:5]))
d3.2.3<-as.data.frame(list(getTotalDistance(d3c2.3),TimeMoving(d3c2.3, POLYDAY3CAM2)[,2:5]))
d3.3.3<-as.data.frame(list(getTotalDistance(d3c3.3),TimeMoving(d3c3.3, POLYDAY3CAM3)[,2:5]))
d3.4.3<-as.data.frame(list(getTotalDistance(d3c4.3),TimeMoving(d3c4.3, POLYDAY3CAM4)[,2:5]))
d4.1.3<-as.data.frame(list(getTotalDistance(d4c1.3),TimeMoving(d4c1.3, POLYDAY4CAM1)[,2:5]))
d4.2.3<-as.data.frame(list(getTotalDistance(d4c2.3),TimeMoving(d4c2.3, POLYDAY4CAM2)[,2:5]))
d4.3.3<-as.data.frame(list(getTotalDistance(d4c3.3),TimeMoving(d4c3.3, POLYDAY4CAM3)[,2:5]))
d4.4.3<-as.data.frame(list(getTotalDistance(d4c4.3),TimeMoving(d4c4.3, POLYDAY4CAM4)[,2:5]))
d5.1.3<-as.data.frame(list(getTotalDistance(d5c1.3),TimeMoving(d5c1.3, POLYDAY5CAM1)[,2:5]))
d5.2.3<-as.data.frame(list(getTotalDistance(d5c2.3),TimeMoving(d5c2.3, POLYDAY5CAM2)[,2:5]))
d5.3.3<-as.data.frame(list(getTotalDistance(d5c3.3),TimeMoving(d5c3.3, POLYDAY5CAM3)[,2:5]))
d5.4.3<-as.data.frame(list(getTotalDistance(d5c4.3),TimeMoving(d5c4.3, POLYDAY5CAM4)[,2:5]))
d6.1.3<-as.data.frame(list(getTotalDistance(d6c1.3),TimeMoving(d6c1.3, POLYDAY6CAM1)[,2:5]))
d6.2.3<-as.data.frame(list(getTotalDistance(d6c2.3),TimeMoving(d6c2.3, POLYDAY6CAM2)[,2:5]))
d6.3.3<-as.data.frame(list(getTotalDistance(d6c3.3),TimeMoving(d6c3.3, POLYDAY6CAM3)[,2:5]))
#d6.4.3 ==> there is no video
d7.1.3<-as.data.frame(list(getTotalDistance(d7c1.3),TimeMoving(d7c1.3, POLYDAY7CAM1)[,2:5]))
d7.2.3<-as.data.frame(list(getTotalDistance(d7c2.3),TimeMoving(d7c2.3, POLYDAY7CAM2)[,2:5]))
d7.3.3<-as.data.frame(list(getTotalDistance(d7c3.3),TimeMoving(d7c3.3, POLYDAY7CAM3)[,2:5]))
d7.4.3<-as.data.frame(list(getTotalDistance(d7c4.3),TimeMoving(d7c4.3, POLYDAY7CAM4)[,2:5]))
d8.1.3<-as.data.frame(list(getTotalDistance(d8c1.3),TimeMoving(d8c1.3, POLYDAY8CAM1)[,2:5]))
#d8.2.3 ==> there is no video
d8.3.3<-as.data.frame(list(getTotalDistance(d8c3.3),TimeMoving(d8c3.3, POLYDAY8CAM3)[,2:5]))
d8.4.3<-as.data.frame(list(getTotalDistance(d8c4.3),TimeMoving(d8c4.3, POLYDAY8CAM4)[,2:5]))

# create reference columns
day3<-c(rep("day 1",16),rep("day 2",16),rep("day 3",16),rep("day 4",16),
        rep("day 5",16),rep("day 6",12),rep("day 7",16),rep("day 8",12))
camera3<-c(rep(c(rep("cam 1",4), rep("cam 2",4), rep("cam 3",4),rep("cam 4",4)),4),
           rep("cam 1",4), rep("cam 2",4), rep("cam 3",4),rep("cam 4",4),
           rep("cam 1",4), rep("cam 2",4), rep("cam 3",4),
           rep("cam 1",4), rep("cam 2",4), rep("cam 3",4),rep("cam 4",4),
           rep("cam 1",4), rep("cam 3",4),rep("cam 4",4))
arena3<-c(rep("arena 1",4), rep("arena 2",4), rep("arena 3",4),rep("arena 4",4),
          rep("arena 5",4), rep("arena 6",4), rep("arena 7",4),rep("arena 8",4),
          rep("arena 9",4), rep("arena 10",4), rep("arena 11",4),rep("arena 12",4),
          rep("arena 13",4), rep("arena 14",4), rep("arena 15",4),rep("arena 16",4),
          rep("arena 17",4), rep("arena 18",4), rep("arena 19",4),rep("arena 20",4),
          rep("arena 21",4), rep("arena 22",4), rep("arena 23",4),#rep("arena 24",4),
          rep("arena 25",4), rep("arena 26",4), rep("arena 27",4),rep("arena 28",4),
          rep("arena 29",4), #rep("arena 30",4), 
          rep("arena 31",4),rep("arena 32",4))

infection3<-c(rep(c("infected","infected","control","control"),30))

# Built a dataframe hour 3
df3<-data.frame(rbind(d1.1.3,d1.2.3,d1.3.3,d1.4.3,d2.1.3,d2.2.3,d2.3.3,d2.4.3,
                      d3.1.3,d3.2.3,d3.3.3,d3.4.3,d4.1.3,d4.2.3,d4.3.3,d4.4.3,
                      d5.1.3,d5.2.3,d5.3.3,d5.4.3,d6.1.3,d6.2.3,d6.3.3,
                      d7.1.3,d7.2.3,d7.3.3,d7.4.3,d8.1.3,d8.3.3,d8.4.3))
dim(df3)
df3["day"]<-day3
df3["camera"]<-camera3
df3["arena"]<-arena3
df3["infection.status"]<-infection3

#===================================================================================
# Part IIId. Build a unique data frame with hour 1,2 and 3
#===================================================================================
df<-data.frame(rbind(df1, df2, df3))
df["hour"]<-c(rep("hour 1",nrow(df1)),rep("hour 2",nrow(df2)),rep("hour 3",nrow(df3)))
colnames(df)
df <- df[,c(7,8,11,9,10,1:6)]
df["speed.cmxmin"] <- round(df$tot.distance.cm / df$minutes.moving, digits = 1)
df["tot.distance.m"] <- df$tot.distance.cm / 100
df["seconds.moving"] <- df$frames / 29.9 * 8
df["speed.mxsec"] <- round(df$tot.distance.m / df$seconds.moving, digits = 2)
df["speed.cmxsec"] <- round(df$tot.distance.cm / df$seconds.moving, digits = 2)


#===================================================================================
# Part IIIe. Plot total distance
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
# Part IIIf. Plot effective time moving
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
  ggtitle("Total time moving (minutes)")+
  ylab("effective time moving (minutes)")+
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
  ylab("effective time moving (minutes)")+
  #xlab("treatment")+
  facet_grid(.~hour)+
  #facet_wrap(~day, ncol=3)+
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
pt1

# plot by day and hour (just days 5,6,7 and 8)
pt2<-ggplot(df,aes(x=infection.status, y=minutes.moving, fill=infection.status))+
  geom_boxplot(width=.5, alpha=0.4)+
  #geom_violin(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=c(8), size=c(3),color=c("black"), position=position_dodge(0.8))+
  geom_point(aes(colour=infection.status),position = position_jitter(0.2),size=.5)+
  #geom_jitter(aes(colour=eggtype),position=position_jitter(0.2))+
  #geom_dotplot(alpha=0.2,binaxis='y', stackdir='center', position=position_dodge(0.8))+
  scale_fill_manual( labels = c("Non-infected","Infected"),values = c("blue","red"))+
  scale_color_manual(labels = c("Non-infected","Infected"), values = c("blue", "red"))+
  ylab("effective time moving (minutes)")+
  # xlab("treatment")+
  facet_grid(day~hour)+
  # facet_wrap(day~hour, ncol=3)+
  theme(legend.position="right",plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

pt2

#===================================================================================
# Part IIIg. Plot speed
#===================================================================================
ps<-ggplot(df,aes(x=infection.status, y=speed.cmxsec, fill=infection.status))+
  geom_boxplot(width=.5, alpha=0.4)+
  #geom_violin(width=.5, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=c(8), size=c(3),color=c("black"), position=position_dodge(0.8))+
  geom_point(aes(colour=infection.status),position = position_jitter(0.2),size=.9)+
  #geom_jitter(aes(colour=eggtype),position=position_jitter(0.2))+
  #geom_dotplot(alpha=0.2,binaxis='y', stackdir='center', position=position_dodge(0.8))+
  scale_fill_manual( labels = c("Non-infected","Infected"),values = c("blue","red"))+
  scale_color_manual(labels = c("Non-infected","Infected"), values = c("blue", "red"))+
  ggtitle("Mean speed (cm / sec)")+
  ylab("speed (cm / sec)")+
  xlab("treatment")+
  #facet_wrap(~day, ncol=3)+
  theme(legend.position="right",plot.title = element_text(hjust = 0.5),
        #axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ps

ps1<-ggplot(df,aes(x=infection.status, y=speed.cmxsec, fill=infection.status))+
  geom_boxplot(width=.5, alpha=0.4)+
  #geom_violin(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=c(8), size=c(3),color=c("black"), position=position_dodge(0.8))+
  geom_point(aes(colour=infection.status),position = position_jitter(0.2),size=.9)+
  #geom_jitter(aes(colour=eggtype),position=position_jitter(0.2))+
  #geom_dotplot(alpha=0.2,binaxis='y', stackdir='center', position=position_dodge(0.8))+
  scale_fill_manual( labels = c("Non-infected","Infected"),values = c("blue","red"))+
  scale_color_manual(labels = c("Non-infected","Infected"), values = c("blue", "red"))+
  ylab("speed (cm / sec)")+
  #xlab("treatment")+
  facet_grid(.~hour)+
  #facet_wrap(~day, ncol=3)+
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ps1

# plot by day and hour (just days 5,6,7 and 8)
ps2<-ggplot(df,aes(x=infection.status, y=speed.cmxsec, fill=infection.status))+
  geom_boxplot(width=.5, alpha=0.4)+
  #geom_violin(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=c(8), size=c(3),color=c("black"), position=position_dodge(0.8))+
  geom_point(aes(colour=infection.status),position = position_jitter(0.2),size=.5)+
  #geom_jitter(aes(colour=eggtype),position=position_jitter(0.2))+
  #geom_dotplot(alpha=0.2,binaxis='y', stackdir='center', position=position_dodge(0.8))+
  scale_fill_manual( labels = c("Non-infected","Infected"),values = c("blue","red"))+
  scale_color_manual(labels = c("Non-infected","Infected"), values = c("blue", "red"))+
  ylab("speed (cm / sec)")+
  # xlab("treatment")+
  facet_grid(day~hour)+
  # facet_wrap(day~hour, ncol=3)+
  theme(legend.position="right",plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ps2

#===================================================================================
# Part IIIh. Get PDF file
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