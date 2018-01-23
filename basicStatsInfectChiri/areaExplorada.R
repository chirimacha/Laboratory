# area explorada por cada insecto, y tambien el tiempo explorado en la area explorada
# Justin Sheen enero 2018

#if (!require(devtools)) {
#  install.packages("devtools")
#}
#devtools::install_github("swarm-lab/videoplayR")
library(sp)
library(devtools)
library(videoplayR)

getAreaExplorada <- function(df) {
  
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
  
  # next, have function to get distance traveled within the polygon
  addCenterDistance <- function(bugRaw) {
    # get whether each frame was in the polygon or not
    bugRaw$inPoly <- FALSE
    for (k in 1:nrow(bugRaw)) {
      if (point.in.polygon(bugRaw$x[k], bugRaw$y[k], poly.x, poly.y)) {
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
  colnames(toReturn) <- c("track", "time spent in open", "distance spent in open")
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

# load csv files
# dia 5
# cam 1
d5c1h1 <- read.csv()
d5c1h2 <- read.csv()
d5c1h3 <- read.csv()

setwd("")
vid  <- readVid("")
imshow(getFrame(vid, 20))
poly <- getpoly(quiet=FALSE)

getAreaExplorada(d5c1h1)
getAreaExplorada(d5c1h2)
getAreaExplorada(d5c1h3)

# cam 2
d5c2h1 <- read.csv()
d5c2h2 <- read.csv()
d5c2h3 <- read.csv()
getAreaExplorada(d5c2h1)
getAreaExplorada(d5c2h2)
getAreaExplorada(d5c2h3)

# cam 3
d5c3h1 <- read.csv()
d5c3h2 <- read.csv()
d5c3h3 <- read.csv()
getAreaExplorada(d5c3h1)
getAreaExplorada(d5c3h2)
getAreaExplorada(d5c3h3)

# cam 4
d5c4h1 <- read.csv()
d5c4h2 <- read.csv()
d5c4h3 <- read.csv()
getAreaExplorada(d5c4h1)
getAreaExplorada(d5c4h2)
getAreaExplorada(d5c4h3)


# dia 6
# cam 1
d6c1h1 <- read.csv()
d6c1h2 <- read.csv()
d6c1h3 <- read.csv()

# cam 2
d6c2h1 <- read.csv()
d6c2h2 <- read.csv()
d6c2h3 <- read.csv()

# cam 3
d6c3h1 <- read.csv()
d6c3h2 <- read.csv()
d6c3h3 <- read.csv()

# cam 4
d6c4h1 <- read.csv()
d6c4h2 <- read.csv()
d6c4h3 <- read.csv()

# dia 7
# cam 1
d7c1h1 <- read.csv()
d7c1h2 <- read.csv()
d7c1h3 <- read.csv()

# cam 2
d7c2h1 <- read.csv()
d7c2h2 <- read.csv()
d7c2h3 <- read.csv()

# cam 3
d7c3h1 <- read.csv()
d7c3h2 <- read.csv()
d7c3h3 <- read.csv()

# cam 4
d7c4h1 <- read.csv()
d7c4h2 <- read.csv()
d7c4h3 <- read.csv()

# dia 8
# cam 1
d8c1h1 <- read.csv()
d8c1h2 <- read.csv()
d8c1h3 <- read.csv()

# cam 2
d8c2h1 <- read.csv()
d8c2h2 <- read.csv()
d8c2h3 <- read.csv()

# cam 3
d8c3h1 <- read.csv()
d8c3h2 <- read.csv()
d8c3h3 <- read.csv()

# cam 4
d8c4h1 <- read.csv()
d8c4h2 <- read.csv()
d8c4h3 <- read.csv()
