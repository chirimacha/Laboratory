# Compare total distance of chirimachas
# mayo 2018 Justin y Renzo

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
  colnames(toReturn) <- c("track", "total_distance_traveled_(pixels)")
  toReturn$track <- seq.int(4)
  toReturn[1,2] <- sum(bugOne$distance)
  toReturn[2,2] <- sum(bugTwo$distance)
  toReturn[3,2] <- sum(bugThree$distance)
  toReturn[4,2] <- sum(bugFour$distance)
  
  return(toReturn)
}

combineDist <- function(h1, h2, h3) {
  toReturn <- data.frame(matrix(ncol = 2, nrow = 4))
  colnames(toReturn) <- c("track", "total_distance_traveled_(pixels)")
  toReturn$track <- seq.int(4)
  toReturn[1,2] <- sum(h1[1,2], h2[1,2], h3[1,2])
  toReturn[2,2] <- sum(h1[2,2], h2[2,2], h3[2,2])
  toReturn[3,2] <- sum(h1[3,2], h2[3,2], h3[3,2])
  toReturn[4,2] <- sum(h1[4,2], h2[4,2], h3[4,2])
  return(toReturn)
}

# big for loop to automatically run everything
for (i in 5:8) { # day
  for (j in 1:4) { # cam
    ext1 <- paste0("day", i)
    ext2 <- paste0("cam", j)
    wd <- paste("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs", ext1, ext2, sep="/")
    setwd(wd)
    
    dist1 <- NA
    dist2 <- NA
    dist3 <- NA
    for (k in 1:3) { # hour
      name1 <- paste0("DAY", i)
      name2 <- paste0("CAM", j)
      name3 <- paste0(k, "HR")
      CSVname <- paste(name1, name2, name3, sep="_")
      CSVname <- paste0(CSVname, ".csv")
      
      assign(paste0("dist", k), getTotalDistance(read.csv(CSVname)))
    }
    combinedDistances <- combineDist(dist1, dist2, dist3)

    filename <- file.path("/Users/Justin/Levy_Research/Laboratory/basicStatsInfectChiri", paste("distance_", name1, "_", name2, ".txt", sep=""))
    capture.output(combinedDistances, file = filename)
  }
}