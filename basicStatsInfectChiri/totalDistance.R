# Compare total distance of chirimachas (dia6)
# Justin Sheen January 2018

# set working directory
setwd("/Users/Justin/Desktop/dia6")

# load csv files
hourOne <- read.csv("DAY6_CAM2_1HR.csv")
hourTwo <- read.csv("DAY6_CAM2_2HR.csv")
hourThree <- read.csv("DAY6_CAM2_3HR.csv")

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
