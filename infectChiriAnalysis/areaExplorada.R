# area explorada por cada insecto, y tambien el tiempo explorado en la area explorada
# mayo 2018 Justin y Renzo

#if (!require(devtools)) {
#  install.packages("devtools")
#}
#devtools::install_github("swarm-lab/videoplayR")
library(devtools)
library(sp)
library(videoplayR)
library(splancs)

getAreaExplorada <- function(df, inputPoly) {
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

  return(toReturn)
}

# get all polygons
# dia 5
# cam 1
setwd("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day5/cam1")
vid  <- readVid("DAY5_CAM1_1HR.mp4")
imshow(getFrame(vid, 20))
POLYDAY5CAM1 <- getpoly(quiet=FALSE)

# cam 2
setwd("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day5/cam2")
vid  <- readVid("DAY5_CAM2_1HR.mp4")
imshow(getFrame(vid, 20))
POLYDAY5CAM2 <- getpoly(quiet=FALSE)

# cam 3
setwd("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day5/cam3")
vid  <- readVid("DAY5_CAM3_1HR.mp4")
imshow(getFrame(vid, 20))
POLYDAY5CAM3 <- getpoly(quiet=FALSE)

# cam 4
setwd("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day5/cam4")
vid  <- readVid("DAY5_CAM4_1HR.mp4")
imshow(getFrame(vid, 20))
POLYDAY5CAM4 <- getpoly(quiet=FALSE)

# day 6
# cam 1
setwd("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day6/cam1")
vid  <- readVid("DAY6_CAM1_1HR.mp4")
imshow(getFrame(vid, 20))
POLYDAY6CAM1 <- getpoly(quiet=FALSE)

# cam 2
setwd("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day6/cam2")
vid  <- readVid("DAY6_CAM2_1HR.mp4")
imshow(getFrame(vid, 20))
POLYDAY6CAM2 <- getpoly(quiet=FALSE)

# cam 3
setwd("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day6/cam3")
vid  <- readVid("DAY6_CAM3_1HR.mp4")
imshow(getFrame(vid, 20))
POLYDAY6CAM3 <- getpoly(quiet=FALSE)

# cam 4
setwd("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day6/cam4")
vid  <- readVid("DAY6_CAM4_1HR.mp4")
imshow(getFrame(vid, 20))
POLYDAY6CAM4 <- getpoly(quiet=FALSE)

# day 7
# cam 1
setwd("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day7/cam1")
vid  <- readVid("DAY7_CAM1_1HR.mp4")
imshow(getFrame(vid, 20))
POLYDAY7CAM1 <- getpoly(quiet=FALSE)

# cam 2
setwd("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day7/cam2")
vid  <- readVid("DAY7_CAM2_1HR.mp4")
imshow(getFrame(vid, 20))
POLYDAY7CAM2 <- getpoly(quiet=FALSE)

# cam 3
setwd("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day7/cam3")
vid  <- readVid("DAY7_CAM3_1HR.mp4")
imshow(getFrame(vid, 20))
POLYDAY7CAM3 <- getpoly(quiet=FALSE)

# cam 4
setwd("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day7/cam4")
vid  <- readVid("DAY7_CAM4_1HR.mp4")
imshow(getFrame(vid, 20))
POLYDAY7CAM4 <- getpoly(quiet=FALSE)

# day 8
# cam 1
setwd("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day8/cam1")
vid  <- readVid("DAY8_CAM1_1HR.mp4")
imshow(getFrame(vid, 20))
POLYDAY8CAM1 <- getpoly(quiet=FALSE)

# cam 2
setwd("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day8/cam2")
vid  <- readVid("DAY8_CAM2_1HR.mp4")
imshow(getFrame(vid, 20))
POLYDAY8CAM2 <- getpoly(quiet=FALSE)

# cam 3
setwd("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day8/cam3")
vid  <- readVid("DAY8_CAM3_1HR.mp4")
imshow(getFrame(vid, 20))
POLYDAY8CAM3 <- getpoly(quiet=FALSE)

# cam 4
setwd("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day8/cam4")
vid  <- readVid("DAY8_CAM4_1HR.mp4")
imshow(getFrame(vid, 20))
POLYDAY8CAM4 <- getpoly(quiet=FALSE)

# helper function to combine the three hours of information
combineAreaExplorada <-function(h1, h2, h3) {
  toReturn<- data.frame(matrix(ncol = 3, nrow = 4))
  colnames(toReturn) <- c("track", "time_spent_in_open", "distance_spent_in_open")
  toReturn$track <- seq.int(4)
  
  # bug one
  toReturn[1,2] <- sum(h1[1,2], h2[1,2], h3[1,2])
  toReturn[1,3] <- sum(h1[1,3], h2[1,3], h3[1,3])
  
  # bug two
  toReturn[2,2] <- sum(h1[2,2], h2[2,2], h3[2,2])
  toReturn[2,3] <- sum(h1[2,3], h2[2,3], h3[2,3])
  
  # bug three
  toReturn[3,2] <- sum(h1[3,2], h2[3,2], h3[3,2])
  toReturn[3,3] <- sum(h1[3,3], h2[3,3], h3[3,3])
  
  # bug four
  toReturn[4,2] <- sum(h1[4,2], h2[4,2], h3[4,2])
  toReturn[4,3] <- sum(h1[4,3], h2[4,3], h3[4,3])
  
  return(toReturn)
}

# big loop for running everything
for (i in 8:8) { # day
  for (j in 3:4) { # cam
    ext1 <- paste0("day", i)
    ext2 <- paste0("cam", j)
    wd <- paste("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs", ext1, ext2, sep="/")
    setwd(wd)
    
    areaExplorada1 <- NA
    areaExplorada2 <- NA
    areaExplorada3 <- NA
    polyName <- NA
    for (k in 1:3) { # hour
      name1 <- paste0("DAY", i)
      name2 <- paste0("CAM", j)
      name3 <- paste0(k, "HR")
      CSVname <- paste(name1, name2, name3, sep="_")
      CSVname <- paste0(CSVname, ".csv")
      
      polyName <- paste0("POLY", name1, name2)
      
      assign(paste0("areaExplorada", k), getAreaExplorada(read.csv(CSVname), eval(parse(text = polyName))  ))
    }
    combinedAreaExplorada <- combineAreaExplorada(areaExplorada1, areaExplorada2, areaExplorada3)
    
    # to return object
    returnResultAndPoly <- list(combinedAreaExplorada, eval(parse(text = polyName)))
    
    filename <- file.path("/Users/Justin/Levy_Research/Laboratory/basicStatsInfectChiri", paste("areaExplorada_", name1, "_", name2, "_50pixels.txt", sep=""))
    capture.output(returnResultAndPoly, file = filename)
  }
}
