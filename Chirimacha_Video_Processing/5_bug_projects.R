###Lets get video processing going for film on 5 insects in the same petri dish
#WARNING: DO NOT CHANGE QUARTZ WINDOW DIMENSIONS DURING ANALYSIS

###Code for video tracking
#To determine if bed bugs can detect pesticides.

###Install Packages and open libraries.
#Install VideoPlayR
# if (!require(devtools)) {
#   install.packages("devtools")
# }
# 
# devtools::install_github("swarm-lab/videoplayR")
# 
# #install Other packages:
# install.packages("dplyr")
# install.packages("clue")
# install.packages("shiny")
# install.packages("splancs")
# install.packages("grid")
## Open Libraries
library(videoplayR)
library(dplyr)
library(clue)
library(shiny)
library(splancs)
library(grid)
library(tictoc)
library(compiler)
library(splancs)
library(trackR)

## Simple Tracker
pdiff <- function(a, b) {
  nr <- length(a)
  nc <- length(b)
  
  ma <- matrix(a, nrow = nr, ncol = nc)
  mb <- matrix(b, nrow = nr, ncol = nc, byrow = TRUE)
  
  ma - mb
}

simpleTracker <- function(current, past, lookBack = 30, maxDist = 20) { 
  if (nrow(past) == 0) {
    current$track <- 1:nrow(current)
    return(current)
  }
  
  i <- current$frame[1]
  trackCounter <- max(past$track)
  # past <- dplyr::filter(past, frame > (i - lookBack), frame < i)
  
  mat <- abs(pdiff(current$x, past$x)) + abs(pdiff(current$y, past$y))
  maxMat <- matrix(maxDist * (i - past$frame), nrow = nrow(current), ncol = nrow(past), byrow = TRUE)
  validMat <- mat <= maxMat
  
  h <- as.vector(clue::solve_LSAP(mat))
  tracks <- past$track[h]
  
  valid <- validMat[(h - 1) * nrow(mat) + 1:nrow(mat)]
  if (any(!valid)) {
    nNew <- sum(!valid)
    tracks[!valid] <- trackCounter + 1:nNew
    trackCounter <- trackCounter + nNew
  }
  
  dup <- duplicated(tracks) | duplicated(tracks, fromLast = TRUE)
  safe <- !dup
  h[safe] <- 0
  while (any(dup)) {
    safe[which.max(h)] <- TRUE
    h[safe] <- 0
    
    newCurrent <- current[!safe, ]
    newPast <- dplyr::filter(past, !(track %in% tracks[safe]))
    
    if (nrow(newCurrent) > nrow(newPast)) {
      n <- nrow(newCurrent) - nrow(newPast)
      newPast <- rbind(newPast, data.frame(id = NA, x = rep(-9999, n), y = rep(-9999, n),
                                           alpha = NA, major = NA, minor = NA, area = NA,
                                           frame = newCurrent$frame[1], track = NA))
    }
    
    mat <- abs(pdiff(newCurrent$x, newPast$x)) + abs(pdiff(newCurrent$y, newPast$y))
    maxMat <- matrix(maxDist * (i - newPast$frame), nrow = nrow(newCurrent),
                     ncol = nrow(newPast), byrow = TRUE)
    validMat <- mat <= maxMat
    
    newH <- as.vector(clue::solve_LSAP(mat))
    newTracks <- newPast$track[newH]
    
    valid <- validMat[(newH - 1) * nrow(mat) + 1:nrow(mat)]
    if (any(!valid)) {
      nNew <- sum(!valid)
      newTracks[!valid] <- trackCounter + 1:nNew
      trackCounter <- trackCounter + nNew
    }
    
    tracks[!safe] <- newTracks
    
    dup <- duplicated(tracks) | duplicated(tracks, fromLast = TRUE)
    safe <- !dup
    h[safe] <- 0
  }
  
  current$track <- tracks
  current
}

pipeline <- function(video, begin, end, background, mask = NULL,
                     threshold, minSize, lookBack, maxDist, progress = FALSE) {
  
  n <- (end - begin + 1)
  tracks <- data.frame(id = numeric(n), x = numeric(n), y = numeric(n),
                       alpha = numeric(n), major = numeric(n),
                       minor = numeric(n),
                       area = numeric(n), frame = numeric(n) - 2 * lookBack,
                       track = numeric(n))
  pos <- 0
  
  if (progress) {
    pb <- shiny::Progress$new()
    on.exit(pb$close())
    pb$set(message = "Computing tracks", value = 0, detail = "0%")
    nFrames <- length(begin:end)
    old <- 0
    oldFrame <- begin
    oldTime <- Sys.time()
  }
  
  for (i in begin:end) {
    past <- dplyr::filter(tracks, frame > (i - lookBack) & frame < i)
    
    tmp <- getFrame(video, i) %>%
      ddd2d() %>%
      blend(background, ., "-") %>%
{if (is.null(mask)) . else blend(., mask, "*")} %>%
  thresholding(threshold, "binary") %>%
  blobDetector() %>%
  dplyr::filter(area >= minSize) %>%
  dplyr::mutate(frame = i, track = NA) %>%
{if (length(.$x) > 0)
  trackR::simpleTracker(., past, lookBack = lookBack, maxDist = maxDist)
 else . }

nRows <- nrow(tmp)
if (nRows > 0) {
  if ((pos + nRows) > n) {
    tracks[(n + 1):(2 * n + nRows), ] <- 0
    n <- nrow(tracks)
  }
  tracks[(pos + 1):(pos + nRows), ] <- tmp
  pos <- pos + nRows
}

if (progress) {
  new <- floor(100 * (i - begin + 1) / nFrames)
  if (new > old) {
    newTime <- Sys.time()
    fps <- (i - oldFrame + 1) / as.numeric(difftime(newTime, oldTime,
                                                    unit = "secs"))
    old <- new
    oldFrame <- i
    oldTime <- newTime
    pb$set(value = old / 100, detail = paste0(old, "% - ",
                                              round(fps, digits = 2), "fps"))
  }
}
  }
tracks[1:pos, ]
}

##########################
# Set Working Directory
setwd("/Users/Justin/Desktop/one_hour_video")
# Bring in videos
# Files are too large for Github - mp4 and avi can be found on Google Drive
# https://drive.google.com/open?id=0BymPutRx4sc2Yjh3YXJXVXV6QzA

##########################
# Create the background; serves as comparison or "bugless" control
bg <- backgrounder(test_two, n=150, method="median", color= FALSE)

# Mask creation (must switch out video name)
# Run imshow(getFrame(vid, 20)
# Run "poly <- getpoly(quiet=FALSE)"
maskCreation <- function(videoname, width, height) {
poly.list <- vector('list', width)
for (i in 1:width) {
  x <- rep(i, height)
  pts <- cbind(x, height:1)
  output <- inout(pts, poly, bound=NULL, quiet=TRUE)
  poly.list[[i]] <- output
}
comb.output <- do.call('cbind', poly.list)
false.vector <- which(comb.output == "FALSE")
true.vector <- which(comb.output == "TRUE")
comb.output[false.vector] <- 0 
comb.output[true.vector] <- 1
mask <- d2ddd(r2img(comb.output))
imshow(blend(getFrame(videoname, 20), mask, "*"))
return(mask)
}
white.mat <- matrix(1, nrow = 480, ncol = 854)
white.img <- r2img(white.mat)
imshow(white.img)
# Save white image with correct dimensions onto desktop
white.img <- readImg("white.png")
imshow(blend(white.img, mask, "*"))
# Save mask image with correct dimensions onto desktop

# Show lines
showLines <- function(video, tracksDF, highTrack) {
  plot.new()
  imshow(getFrame(video, 20))
  for (i in 1:highTrack){
    insect <- which(tracksDF$track == i)
    lines(x = c(tracksDF$x[insect]), y = c(tracksDF$y[insect]), col = i) 
  }
}

showSingleLine <- function(video, tracksDF, num.track) {
  plot.new()
  imshow(getFrame(video, 20))
  for (i in num.track:num.track){
    insect <- which(tracksDF$track == i)
    lines(x = c(tracksDF$x[insect]), y = c(tracksDF$y[insect]), col = i) 
  }
}

showUpToLines <- function(video, tracksDF, highTrack, frame) {
  plot.new()
  imshow(getFrame(video, frame))
  for (i in 1:highTrack){
    insect <- which(tracksDF$track == i)
    x_insect = c(tracksDF$x[insect])
    y_insect = c(tracksDF$y[insect])
    lines(x = x_insect[1:frame], y = y_insect[1:frame], col = i) 
  }
}

showUpToSingleLine <- function(video, tracksDF, num.track, frame) {
  plot.new()
  imshow(getFrame(video, frame))
  for (i in num.track:num.track){
    insect <- which(tracksDF$track == i) #maybe try to look within a frame
    x_insect = c(tracksDF$x[insect])
    y_insect = c(tracksDF$y[insect])
    
    # Note that below, the frame here is an approximation, as it is only
    # the first, for example, 10,000 indices for x_insect, as with
    # the video it shows the actual 10,000th frame
    
    # This is evident in: showUpToSingleLine(test_two, tracks, 1, 16851)
    # with a print statement:
    # print(length(x_insect))
    print(length(x_insect))
    lines(x = x_insect[1:frame], y = y_insect[1:frame], col = i) 
  }
}

# Stitching
testing2_tracksdup$track[testing2_tracksdup$track == 6] <- 1

testing2_tracksdup$track[testing2_tracksdup$track == 11] <- 2

testing2_tracksdup$track[testing2_tracksdup$track == 7] <- 4
testing2_tracksdup$track[testing2_tracksdup$track == 8] <- 4
testing2_tracksdup$track[testing2_tracksdup$track == 9] <- 4

testing2_tracksdup$track[testing2_tracksdup$track == 10] <- 5
testing2_tracksdup$track[testing2_tracksdup$track == 12] <- 5

# Stitching second part
testing_secondpart2tracks$track[testing_secondpart2tracks$track == 7] <- 2
testing_secondpart2tracks$track[testing_secondpart2tracks$track == 8] <- 4

