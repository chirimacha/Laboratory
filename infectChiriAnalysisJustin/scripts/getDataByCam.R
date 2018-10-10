#===================================================================================
# Code to get the final data sets of each camera for each day
# infected and non infected but by ten minutes
# written by Justin june 2018
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
#setwd("~/GITHUB/Levy_Research/Laboratory/Triatoma infected behavior_Video")
setwd("~/Desktop/Levy_Research/Laboratory/Triatoma infected behavior_Video")
getwd()

# Load CSV files
for (i in 1:8) { # day
  for (j in 1:4) { # cam
    for (k in 1:3) { # hour
      # Creates the variable name we want for the hour
      varName <- paste0("d", i, "c", j, ".", k)
      
      # Creates the filename we will pull from with the "read.csv" command
      fileName <- paste0("DAY", i, "_CAM", j, "_", k, "HR.csv")
      
      # The "if" condition references those files that do not have information for
      # the second and third hours
      if (fileName != "DAY6_CAM4_2HR.csv" & fileName != "DAY6_CAM4_3HR.csv" &
          fileName != "DAY8_CAM2_2HR.csv" & fileName != "DAY8_CAM2_3HR.csv") {
        
        # Use the "read.csv" command in order to assign a variable
        file <- read.csv(fileName)
        
        if (all(file[,1] == file[,2])) {
          file[,1] <- NULL
        }
        
        # Creates the new variable
        assign(varName, file)
      }
    }
  }
}

#===================================================================================
# Part II: Create the final data frames to use
#  IIa. Combine the three hours together
#  IIb. Impute missing frames function (comes in two parts)
#  IIc. Run the imputing of missing frames
#  IId. Write the new files
#===================================================================================
#===================================================================================
# Part IIa. Combine the three hours together for each day and camera
#===================================================================================
for (i in 1:8) { # day
  for (j in 1:4) { # cam
    print(i)
    print(j)
    
    if (paste0("d", i, "c", j, ".", 2) != "d6c4.2" & paste0("d", i, "c", j, ".", 3) != "d6c4.3" &
        paste0("d", i, "c", j, ".", 2) != "d8c2.2" & paste0("d", i, "c", j, ".", 3) != "d8c2.3") {
      # Get the "hour" variables for each camera
      h1 <- get(paste0("d", i, "c", j, ".", 1))
      h2 <- get(paste0("d", i, "c", j, ".", 2))
      h3 <- get(paste0("d", i, "c", j, ".", 3))
      
      # Get the maximum frame for hour 1, add this to all frame numbers for hour 2
      maxFh1 <- max(h1$frame)
      h2$frame <- h2$frame + maxFh1
      
      # Get the maximum frame for hour 2, add this to all frame numbers for hour 3
      maxFh2 <- max(h2$frame)
      h3$frame <- h3$frame + maxFh2
      
      # Now we rbind the three hours together, and assign it to the variable name
      # for the specific day and camera
      varName <- paste0("day", i, "cam", j)
      joinedFrames <- rbind(h1, h2, h3)
      assign(varName, joinedFrames)
    }
  }
}

#===================================================================================
# Part IIb: Impute missing frames function(comes in two parts)
#===================================================================================
imputeMissing <- function(df) {
  # From the original data frame, we only need to keep the following variables
  toKeep <- c("frame", "track", "x", "y")
  df <- df[,toKeep]
  
  # Split the data frame into four different data frames, one for each bug
  bugOne <- df[which(df$track == 1),]
  bugTwo <- df[which(df$track == 2),]
  bugThree <- df[which(df$track == 3),]
  bugFour <- df[which(df$track == 4),]
  
  # PART I: Here we have a function which will test whether the data frame has information
  # for frame 1. If it does not, and there is no proceeding information (i.e. it did
  # not move for the entire video) then a new row is created that just has the x and y
  # coordinates equal to 0. If it does have information in subsequent frames, we find 
  # the first instance of information and impute this data for frame 1.
  imputeMissingSingleBug <- function(bugTable, inputTrack) {
    if (length(which(bugTable$frame == 1)) == 0) {
      if (nrow(bugTable) == 0) {
        bugTable <- data.frame(matrix(ncol=4, nrow=1))
        colnames(bugTable) <- c("track", "frame", "x", "y")
        bugTable$track <- as.numeric(inputTrack)
        bugTable$frame <- 1
        bugTable$x <- 0
        bugTable$y <- 0
      } else {
        # impute with the first place bug of inputTrack was found
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
  bugOne <- imputeMissingSingleBug(bugOne, 1)
  bugTwo <- imputeMissingSingleBug(bugTwo, 2)
  bugThree <- imputeMissingSingleBug(bugThree, 3)
  bugFour <- imputeMissingSingleBug(bugFour, 4)
  
  # We stop the function if somehow, we still do not have information for frame 1
  stopifnot(length(which(bugOne$frame == 1)) != 0)
  stopifnot(length(which(bugTwo$frame == 1)) != 0)
  stopifnot(length(which(bugThree$frame == 1)) != 0)
  stopifnot(length(which(bugFour$frame == 1)) != 0)
  
  # PART II: We now have a function that will first, create all of the frames that we need
  # for each bug (getting the max number of frames of the entire data frame) as 
  # well as imputing data if it is missing. How this is done is that if a frame does
  # not have data, then the data from the frame immediately previous will be imputed
  # as the data for the frame.
  makeNewDataFrame <- function(bugRaw, trackN) {
    bugNew <- data.frame(matrix(ncol = 4, nrow = max(df$frame)))
    colnames(bugNew) <- c("track", "frame", "x", "y")
    bugNew$track <- trackN
    bugNew$frame <- seq.int(max(df$frame))
    for (i in 1:nrow(bugRaw)) {
      bugNew$x[which(bugNew$frame == bugRaw$frame[i])] <- bugRaw$x[i]
      bugNew$y[which(bugNew$frame == bugRaw$frame[i])] <- bugRaw$y[i]
    }
    
    # impute data if missing (impute the immediately previous frame)
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
  
  # Return an rbinded version of the data frame, order by the frame number
  toReturn <- rbind(bugOne, bugTwo, bugThree, bugFour)
  toReturn <- toReturn[order(toReturn$frame),]
  return(toReturn)
}

#===================================================================================
# Part IIc. Run the imputing of missing frames
#===================================================================================
for (i in 1:8) { # day
  for (j in 1:4) { # cam
    print(paste0("day", i))
    print(paste0("cam", j))
    
    # Creates the variable name we want for the day, camera, and hour
    varName <- paste0("day", i, "cam", j)
    
    if (varName != "day6cam4" & varName != "day8cam2") {
      # Impute the missing information for the variable name
      temp <- imputeMissing(get(varName))
      
      # Assign the data (temp) to the variable name (overwrite it essentially)
      assign(varName, temp)
    }
  }
}



#===================================================================================
# Part IId. Write the new files
#===================================================================================
output_direc <- "~/Desktop/Levy_Research/Laboratory/infectChiriAnalysis/data/dataByCam"
for (i in 1:8) { # day
  for (j in 1:4) { # cam
    # Get the variable name that we will write
    varName <- paste0("day", i, "cam", j)
    
    if (varName != "day6cam4" & varName != "day8cam2") {
      # Create the name of the file we will write
      fileName <- paste0("DAY", i, "_CAM", j, ".csv")
      
      # Write the csv file we need
      toWrite <- file.path(output_direc, fileName)
      write.csv(get(varName), toWrite, row.names = F)
    }
  }
}
