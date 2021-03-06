## Code for video tracking to determine if bed bugs can detect pesticides.
## WARNING: DO NOT CHANGE QUARTZ WINDOW DIMENSIONS DURING ANALYSIS

## Install Packages and open libraries
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
# Open Libraries
library(videoplayR)
library(dplyr)
library(clue)
library(shiny)
library(splancs)
library(grid)

## Set Working Directory
# Lab computer
# setwd("/Users/mzlevy/Laboratory/
#     Inesfly_Paint_Bed_Bug_Trial/Pesticide_Detection")
# Justin's Computer
setwd("/Users/Justin/Desktop/Levy_Research/Laboratory/
      Inesfly_Paint_Bed_Bug_Trial/Pesticide_Detection")
# Gian Franco's
# setwd(".../Laboratory/Inesfly_Paint_Bed_Bug_Trial/Pesticide_Detection")

## Simple Tracker (package not available for new R)
pdiff <- function(a, b) {
  nr <- length(a)
  nc <- length(b)
  
  ma <- matrix(a, nrow = nr, ncol = nc)
  mb <- matrix(b, nrow = nr, ncol = nc, byrow = TRUE)
  
  ma - mb
}

simpleTracker <- function(current, past, lookBack = 30, maxDist = 10) {
  if (nrow(past) == 0) {
    current$track <- 1:nrow(current)
    return(current)
  }
  
  i <- current$frame[1]
  trackCounter <- max(past$track)
  # past <- dplyr::filter(past, frame > (i - lookBack), frame < i)
  
  mat <- abs(pdiff(current$x, past$x)) + abs(pdiff(current$y, past$y))
  maxMat <- matrix(maxDist * (i - past$frame), nrow = nrow(current), 
                   ncol = nrow(past), byrow = TRUE)
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
      newPast <- rbind(newPast, data.frame(id = NA, x = rep(-9999, n), 
                                           y = rep(-9999, n),
                                           alpha = NA, major = NA, minor = NA,
                                           area = NA,
                                           frame = newCurrent$frame[1],
                                           track = NA))
    }
    
    mat <- abs(pdiff(newCurrent$x, newPast$x)) + abs(pdiff(newCurrent$y,
                                                           newPast$y))
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

## Bring in videos and TrayPlace
# Import Videos for First Repetition
# Repetition 1 (Recorded on "2016-04-21")
# Trial One
R1T1C1<- readVid("Trial1Cam1.mp4")
R1T1C2<- readVid("Trial1Cam2.mp4")
# Trial Two
R1T2C1<- readVid("Trial2Cam1.mp4") #<-why is video only 913 frames
R1T2C2<- readVid("Trial2Cam2.mp4")
# Trial Three
R1T3C1<- readVid("Trial3Cam1.mp4")
R1T3C2<- readVid("Trial3Cam2.mp4")
# Trial Four
R1T4C1<- readVid("Trial4Cam1.mp4")
R1T4C2<- readVid("Trial4Cam2.mp4")
# Trial Five
R1T5C1<- readVid("Trial5Cam1.mp4")
R1T5C2<- readVid("Trial5Cam2.mp4")
# Trial Six
R1T6C1<- readVid("Trial6Cam1.mp4")
R1T6C2<- readVid("Trial6Cam2.mp4")

# Repetition 2 (Recorded "2016-05-12")
# Trial One
R2T1C1<- readVid("R2T1C1.mp4")
R2T1C2<- readVid("R2T1C2.mp4")
# Trial Two
R2T2C1<- readVid("R2T2C1.mp4")
R2T2C2<- readVid("R2T2C2.mp4")
# Trial Three
R2T3C1<- readVid("R2T3C1.mp4")
R2T3C2<- readVid("R2T3C2.mp4")
# Trial Four
R2T4C1<- readVid("R2T4C1.mp4")
R2T4C2<- readVid("R2T4C2.mp4")
# Trial Five
R2T5C1<- readVid("R2T5C1.mp4")
R2T5C2<- readVid("R2T5C2.mp4")
# Trial Six
R2T6C1<- readVid("R2T6C1.mp4")
R2T6C2<- readVid("R2T6C2.mp4")

## Input Data Tables with times, dates, humidity as TrayPlace and quadrant
## assignments.
TrayPlace<- read.csv("TraysRep1y2.csv")

###############################################################################
## See what Simon Garnier's loop is doing by taking only 1 frame
#  rev<-getFrame(pilotvidr1, 5)
#  grscl<-ddd2d(rev)
#  mask<-blend(grscl, pmaska, "*")
#  neg<-blend(nbga, mask, "-") #the order matters
#  #mult<-blend(neg1, neg1, "*")
#  ths<-thresholding(neg, 60, "binary")
#  imshow(ths)
#  bugloc<-blobDetector(ths)
#  bcoutputx<-mutate(bugloc, frame=5, track=NA)
#  stoutx<-simpleTracker(bcoutputx, past=bugpos, maxDist= 10)
#  bugpos<- rbind(bugpos, stoutx)

###############################################################################
## Full Videos
# Get a frames from each video in order to find coordinates
# Cam 1 for Repetition 1 videos has recording error 
# Error with duplicate and skipped frames

# #Repetition 1 Frame 5
# FR1T1C1 <- getFrame(R1T1C1, 5)
# FR1T1C2 <- getFrame(R1T1C2, 5)
# FR1T2C1 <- getFrame(R1T2C1, 5)
# FR1T2C2 <- getFrame(R1T2C2, 5)
# FR1T3C1 <- getFrame(R1T3C1, 5)
# FR1T3C2 <- getFrame(R1T3C2, 5)
# FR1T4C1 <- getFrame(R1T4C1, 5)
# FR1T4C2 <- getFrame(R1T4C2, 5)
# FR1T5C1 <- getFrame(R1T5C1, 5)
# FR1T5C2 <- getFrame(R1T5C2, 5)
# FR1T6C1 <- getFrame(R1T6C1, 5)
# FR1T6C2 <- getFrame(R1T6C2, 5)

# #Repetition 2 Frame 5
# FR2T1C1 <- getFrame(R2T1C1, 5)
# FR2T1C2 <- getFrame(R2T1C2, 5)
# FR2T2C1 <- getFrame(R2T2C1, 5)
# FR2T2C2 <- getFrame(R2T2C2, 5)
# FR2T3C1 <- getFrame(R2T3C1, 5)
# FR2T3C2 <- getFrame(R2T3C2, 5)
# FR2T4C1 <- getFrame(R2T4C1, 5)
# FR2T4C2 <- getFrame(R2T4C2, 5)
# FR2T5C1 <- getFrame(R2T5C1, 5)
# FR2T5C2 <- getFrame(R2T5C2, 5)
# FR2T6C1 <- getFrame(R2T6C1, 5)
# FR2T6C2 <- getFrame(R2T6C2, 5)

#Function uses frame above to click and obtain coordinates. 
#DO NOT CHANGE QUARTZ SIZE!!!

# getpoint<-function(frame){
#   rto <- frame$dim[1]/frame$dim[2]
#   print(rto)
#   ht<-rto*6
#   quartz(width=6, height=ht)
#   imshow(frame)
#   a<-grid.locator(unit = "npc")
#   gcx<-as.numeric(a$x)
#   gcy<-as.numeric(a$y)
#   X <- ceiling(gcx*frame$dim[2])
#   Y <- ceiling(gcy*frame$dim[1])
#   imshow(frame)
#   points(x=c(X), y=c(Y), col="red", pch=19, cex = 0.1 )
#   coord<-c(X,Y)
#   output=coord
#   }
# 
# #Repeat the above code to find the points and 
# #manually enter them in2the dataframes below.
# tester<-getpoint(FR1T2C1)
# tester

#Video FR1T1C2
#tray number
Tray<-c(1,2,3,4,5,6)
#Left Matrix limit for mask
bMXL <-c(168, 351, 521, 178, 349, 517)
#right Matrix limit for mask
bMXR <-c(351, 521, 703, 349, 517, 703)
#Top matrix limit
bMYT <-c(427, 427, 427, 238, 238, 238)
#bottom matrix limit
bMYB <-c(238, 238, 238,  59,  59,  59)

#Top point of vertical line (X-Coord)
bTPX <-c(258, 434, 608, 267, 439, 596)
#Top point of vertical line (Y-Coord)
bTPY <-c(404, 406, 410, 224, 224, 219)
#Bottom Point
bBPX <-c(284, 439, 604, 276, 438, 601)
bBPY <-c(254, 255, 252,  87,  82,  77)
#Left Point of horizontal line
bLPX <-c(199, 363, 532, 198, 362, 527)
bLPY <-c(316, 329, 330, 153, 155, 150)
#Right Point of Horizontal line
bRPX <-c(338, 511, 685, 337, 515, 678)
bRPY <-c(338, 331, 333, 158, 154, 155)
#c(,,,,,)
#create a coordinate table
CoTbR1T1C2<-data.frame(Tray, bMXL, bMXR, bMYT, bMYB, bBPX, bBPY, bTPX, bTPY, 
                       bRPX, bRPY, bLPX, bLPY)
#rename so values are easily called in loop or function
names(CoTbR1T1C2)<-c("Tray","MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX",
                     "TPY", "RPX", "RPY", "LPX", "LPY")
CoTbR1T1C2
imshow(FR1T1C2)
points(CoTbR1T1C2$TPX,CoTbR1T1C2$TPY)

#########################
# visualize<-function(CD, frame){
#   imshow(frame)
#   for(i in 1:6){
#     lines(x = c(CD$MXR[i], CD$MXL[i]), y = c(CD$MYT[i], CD$MYT[i]), col=i) 
#     lines(x = c(CD$MXR[i], CD$MXL[i]), y = c(CD$MYB[i], CD$MYB[i]),col=i) 
#     lines(x = c(CD$MXR[i], CD$MXR[i]), y = c(CD$MYT[i], CD$MYB[i]),col=i) 
#     lines(x = c(CD$MXL[i], CD$MXL[i]), y = c(CD$MYT[i], CD$MYB[i]),col=i) 
#     lines(x = c(CD$TPX[i], CD$BPX[i]), y = c(CD$TPY[i], CD$BPY[i]),col=i)
#     lines(x = c(CD$LPX[i], CD$RPX[i]), y = c(CD$LPY[i], CD$RPY[i]),col=i)
#   }
# }

## Bring in Frames coordinate tables
# Repetition 1
CoTbR1T1C1 <- read.csv("CoTbR1T1C1.csv")
CoTbR1T1C2 <- read.csv("CoTbR1T1C2.csv")
CoTbR1T2C1 <- read.csv("CoTbR1T2C1.csv")

# Repetition 2
CoTbR2T1C1 <- read.csv("CoTbR2T1C1.csv")
CoTbR2T1C2 <- read.csv("CoTbR2T1C2.csv")
CoTbR2T2C1 <- read.csv("CoTbR2T2C1.csv")
CoTbR2T2C2 <- read.csv("CoTbR2T2C2.csv")
CoTbR2T3C1 <- read.csv("CoTbR2T3C1.csv")
CoTbR2T3C2 <- read.csv("CoTbR2T3C2.csv")
CoTbR2T4C1 <- read.csv("CoTbR2T4C1.csv")
CoTbR2T4C2 <- read.csv("CoTbR2T4C2.csv")
CoTbR2T5C1 <- read.csv("CoTbR2T5C1.csv")
CoTbR2T5C2 <- read.csv("CoTbR2T5C2.csv")
CoTbR2T6C1 <- read.csv("CoTbR2T6C1.csv")
CoTbR2T6C2 <- read.csv("CoTbR2T6C2.csv")

## Create background. Do this step only once.
bga <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
bgb <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# bgc <- backgrounder(R1T2C1, n = 1600, method = "mean", color = FALSE) 
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)

## Repetition 2
# Trial 1
bgaRB <- backgrounder(R2T1C1, n = 1800, method = "mean", color = FALSE)
bgbRB <- backgrounder(R2T1C2, n = 1800, method = "mean", color = FALSE)
# Trial 2
bgcRB <- backgrounder(R2T2C1, n = 1800, method = "mean", color = FALSE)
bgdRB <- backgrounder(R2T2C2, n = 1800, method = "mean", color = FALSE)
# Trial 3
bgeRB <- backgrounder(R2T3C1, n = 1800, method = "mean", color = FALSE)
bgfRB <- backgrounder(R2T3C2, n = 1800, method = "mean", color = FALSE)
# # Trial 4
# bggRB <- backgrounder(R2T4C1, n = 1800, method = "mean", color = FALSE)
# bghRB <- backgrounder(R2T4C2, n = 1800, method = "mean", color = FALSE)
# #T5
# bgiRB <- backgrounder(R2T5C1, n = 1800, method = "mean", color = FALSE)
# bgjRB <- backgrounder(R2T5C2, n = 1800, method = "mean", color = FALSE)
# #T6
# bgkRB <- backgrounder(R2T6C1, n = 1800, method = "mean", color = FALSE)
# bglRB <- backgrounder(R2T6C2, n = 1800, method = "mean", color = FALSE)

# Takes in the video and coordinate table to 
# output the coordinates of the insect in each frame for all 6 bugs
VidAnalysis<-function(video, bg, coordtab, thresholda, maxDistb, cam, rep,
                      trial) {
  #create the background
  #bg <- backgrounder(video, n = 1800, method = "mean", color = FALSE)
  
  # Creates black masks over each petri dish, giving black 
  mat1 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  mat2 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  mat3 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  mat4 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  mat5 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  mat6 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  
  # Create hole for each petri dish in each mask
  # The matrix works left to right, BUT top to bottom.  
  # Graphing works bottom to top so we need correction
  mat1[((bg$dim[1])-coordtab$MYT[1]):((bg$dim[1])-coordtab$MYB[1]),
       coordtab$MXL[1]:coordtab$MXR[1]] <- 1
  mat2[((bg$dim[1])-coordtab$MYT[2]):((bg$dim[1])-coordtab$MYB[2]),
       coordtab$MXL[2]:coordtab$MXR[2]] <- 1
  mat3[((bg$dim[1])-coordtab$MYT[3]):((bg$dim[1])-coordtab$MYB[3]), 
       coordtab$MXL[3]:coordtab$MXR[3]] <- 1
  mat4[((bg$dim[1])-coordtab$MYT[4]):((bg$dim[1])-coordtab$MYB[4]), 
       coordtab$MXL[4]:coordtab$MXR[4]] <- 1
  mat5[((bg$dim[1])-coordtab$MYT[5]):((bg$dim[1])-coordtab$MYB[5]), 
       coordtab$MXL[5]:coordtab$MXR[5]] <- 1
  mat6[((bg$dim[1])-coordtab$MYT[6]):((bg$dim[1])-coordtab$MYB[6]), 
       coordtab$MXL[6]:coordtab$MXR[6]] <- 1
  
  # Make Mask Matrix into an image
  pmaska <- (r2img(mat1))
  pmaskb <- (r2img(mat2))
  pmaskc <- (r2img(mat3))
  pmaskd <- (r2img(mat4))
  pmaske <- (r2img(mat5))
  pmaskf <- (r2img(mat6))
  
  # Now bring the mask and the background together
  nbga1<-blend(bg, pmaska, "*")
  nbga2<-blend(bg, pmaskb, "*")
  nbga3<-blend(bg, pmaskc, "*")
  nbga4<-blend(bg, pmaskd, "*")
  nbga5<-blend(bg, pmaske, "*")
  nbga6<-blend(bg, pmaskf, "*")
  
  #Coords finds the coordinate of the insect in each quadrant in each frame
  Coords<-function(video, pmask, nbga, coordtaba, tn, threshold, maxDista, rep,
                   cam){
    #determine loop length
    #     if (video$length<1800) {
    #       fr <- video$length
    #     } else {
    #       fr<-1800
    #     }
    
    # Temporarily set fr to 20 to speed up code while debugging.
    fr <- 20
    
    # Reset bugpos to blank data frame
    bugpos<-data.frame()
    
    # Looks at each video frame and finds the coordinates of each blob
    for (i in 1:fr){
      #extract individual frames
      res <- getFrame(video, i) 
      #put frame into grey scale.
      gryscl <- ddd2d(res) 
      #mask other petri dishes
      mask <- blend(gryscl, pmask, "*")
      #subtract background from the mask (previous image). Only movement shows
      sub <- blend(nbga, mask, "-") 
      #set a threshold difference to remove changes due to glare/noise
      bw <- thresholding(sub, threshold, "binary")
      #detect the white blobs that are created. Get coordinates
      bugcords <- blobDetector(bw)
      
      # add track # to data frame only if a change is detected
      if (nrow(bugcords) > 0) {
        bugcords<-mutate(bugcords, frame = i, track = NA) 
        # determines what points are linked. Optimally each insect given 1 track 
        # each because there is only one object, we can max out maxDist. 
        stout<-simpleTracker(past = bugpos, current = bugcords, 
                             maxDist = maxDista) 
        # combine tables previous in the loop.
        bugpos<- rbind(bugpos, stout)
      }
    }
    
    # Defines the lines of the quadrants on the petri dish
    ya <- c(coordtaba$BPY[tn],coordtaba$TPY[tn])
    xa <- c(coordtaba$BPX[tn],coordtaba$TPX[tn])   
    yb <- c(coordtaba$LPY[tn],coordtab$RPY[tn])
    xb <- c(coordtaba$LPX[tn],coordtab$RPX[tn])  
    line1a <- lm(ya~xa)
    line1b <- lm(yb~xb)
    
    ## Gives y value of line for given x of bug.
    # Thus, we can see whether bug is above or below line.
    newsa <- data.frame(xa = bugpos$x)
    newsb <- data.frame(xb = bugpos$x)
    bugpos$pred1 <- predict(line1a, newsa, na.rm=TRUE)
    bugpos$pred2 <- predict(line1b, newsb, na.rm=TRUE)
    
    # Finding whether the vertical line has positive or negative slope
    bugpos$TPX <- coordtaba$TPX[tn]
    bugpos$BPX <- coordtaba$BPX[tn]
    
    # Indicate the tray in data table.
    bugpos$trayn <- tn
    
    # Return the data table.
    return(bugpos)   
  }
  
  # Now run this subfunction over the 6 dishes   
  # e.g. pdt1 = bugpos for tray 1, location, regardless of camera 1 or camera 2
  pdt1 <-Coords(video, pmaska, nbga1, coordtaba=coordtab, tn=1, 
                threshold=thresholda, maxDista=maxDistb)
  pdt2 <-Coords(video, pmaskb, nbga2, coordtaba=coordtab, tn=2, 
                threshold=thresholda, maxDista=maxDistb)
  pdt3 <-Coords(video, pmaskc, nbga3, coordtaba=coordtab, tn=3, 
                threshold=thresholda, maxDista=maxDistb)
  pdt4 <-Coords(video, pmaskd, nbga4, coordtaba=coordtab, tn=4, 
                threshold=thresholda, maxDista=maxDistb)
  pdt5 <-Coords(video, pmaske, nbga5, coordtaba=coordtab, tn=5, 
                threshold=thresholda, maxDista=maxDistb)
  pdt6 <-Coords(video, pmaskf, nbga6, coordtaba=coordtab, tn=6, 
                threshold=thresholda, maxDista=maxDistb)
  
  # Bind All the tables
  MasterTab<-rbind(pdt1, pdt2, pdt3, pdt4, pdt5, pdt6)
  
  # Indicate which camera this is
  MasterTab$camera <- cam
  # Indicate which repetition
  MasterTab$rep <- rep
  MasterTab$trial <- trial
  MasterTab$position <- (MasterTab$trayn)+(6*(cam-1))
  
  #Output as single data table
  return(MasterTab)
}

######################################################
######## Running VidAnalysis for all videos ##########

#### Repetition 1
DR1T1C1 <- VidAnalysis(video=R1T1C1, bg= bga, coordtab=CoTbR1T1C1, 
                       thresholda=25, maxDistb=1000, cam=1, rep=1, trial=1)
#write.csv(DR1T1C1, "Rep1Trial1Cam1RawData.csv")

DR1T1C2 <- VidAnalysis(video=R1T1C2, bg= bgb, coordtab=CoTbR1T1C2, 
                       thresholda=30, maxDistb=1000, cam=2, rep=1, trial=1)
#write.csv(DR1T1C2, "Rep1Trial1Cam2RawData.csv")

DR1T2C1 <- VidAnalysis(video=R1T2C1, bg= bgc, coordtab=CoTbR1T2C1,
                       thresholda=25, maxDistb=1000, cam=1, rep=1, trial=2)
#write.csv(DR1T2C1, "Rep1Trial2Cam1RawData.csv")

## Repetition 2
DR2T1C1 <- VidAnalysis(video=R2T1C1, bg= bgaRB, coordtab=CoTbR2T1C1, 
                       thresholda=25, maxDistb=1000, cam=1, rep=2, trial=1)
write.csv(DR2T1C1, "Rep2Trial1Cam1RawData.csv")

DR2T1C2 <- VidAnalysis(video=R2T1C2, bg= bgbRB, coordtab=CoTbR2T1C2, 
                       thresholda=30, maxDistb=1000, cam=2, rep=2, trial=1)
write.csv(DR2T1C2, "Rep2Trial1Cam2RawData.csv")

DR2T2C1 <- VidAnalysis(video=R2T2C1, bg= bgcRB, coordtab=CoTbR2T2C1, 
                       thresholda=25, maxDistb=1000, cam=1, rep=2, trial=2)
write.csv(DR2T2C1, "Rep2Trial2Cam1RawData.csv")

DR2T2C2 <- VidAnalysis(video=R2T2C2, bg= bgdRB, coordtab=CoTbR2T2C2,
                       thresholda=30, maxDistb=1000, cam=2, rep=2, trial=2)
write.csv(DR2T2C2, "Rep2Trial2Cam2RawData.csv")

DR2T3C1 <- VidAnalysis(video=R2T3C1, bg= bgeRB, coordtab=CoTbR2T3C1, 
                       thresholda=25, maxDistb=1000, cam=1, rep=2, trial=3)
write.csv(DR2T3C1, "Rep2Trial3Cam1RawData.csv")

DR2T3C2 <- VidAnalysis(video=R2T3C2, bg= bgfRB, coordtab=CoTbR2T3C2, 
                       thresholda=30, maxDistb=1000, cam=2, rep=2, trial=3)
write.csv(DR2T3C2, "Rep2Trial3Cam2RawData.csv")

# DR2T4C1 <- VidAnalysis(video=R2T4C1, bg= bgeRB, coordtab=CoTbR2T4C1, thresholda=50, 
#                        maxDistb=1000, cam=1, rep=2, trial=4)
#            write.csv(DR2T4C1, "Rep2Trial4Cam1RawData.csv")
# 
# DR2T4C2 <- VidAnalysis(video=R2T4C1, bg= bgfRB, coordtab=CoTbR2T4C2, thresholda=50, 
#                        maxDistb=1000, cam=2, rep=2, trial=4)
#            write.csv(DR2T4C2, "Rep2Trial4Cam2RawData.csv")
# 
# DR2T5C1 <- VidAnalysis(video=R2T5C1, bg= bgeRB, coordtab=CoTbR2T5C1, thresholda=50, 
#                        maxDistb=1000, cam=1, rep=2, trial=5)
#            write.csv(DR2T5C1, "Rep2Trial5Cam1RawData.csv")
# 
# DR2T5C2 <- VidAnalysis(video=R2T5C1, bg= bgfRB, coordtab=CoTbR2T5C2, thresholda=50, 
#                        maxDistb=1000, cam=2, rep=2, trial=5)
#            write.csv(DR2T5C2, "Rep2Trial5Cam2RawData.csv")
# 
# DR2T6C1 <- VidAnalysis(video=R2T6C1, bg= bgeRB, coordtab=CoTbR2T6C1, thresholda=50, 
#                        maxDistb=1000, cam=1, rep=2, trial=6)
#           write.csv(DR2T6C1, "Rep2Trial6Cam1RawData.csv")
# 
# DR2T6C2 <- VidAnalysis(video=R2T6C1, bg= bgfRB, coordtab=CoTbR2T6C2, thresholda=50, 
#                        maxDistb=1000, cam=2, rep=2, trial=6)
#            write.csv(DR2T6C2, "Rep2Trial6Cam2RawData.csv")

######################################################################
###The Code below should be able to be used on a PC

#If running from PC run these codes
# DR2T1C1 <- read.csv("Rep2Trial1Cam1RawData.csv")
# DR2T1C2 <- read.csv("Rep2Trial1Cam2RawData.csv")
# DR2T2C1 <- read.csv("Rep2Trial2Cam1RawData.csv")
# DR2T2C2 <- read.csv("Rep2Trial2Cam2RawData.csv")
# DR2T3C1 <- read.csv("Rep2Trial3Cam1RawData.csv")
# DR2T3C2 <- read.csv("Rep2Trial3Cam2RawData.csv")

# DR2T4C1 <- read.csv("Rep2Trial4Cam1RawData.csv")
# DR2T4C2 <- read.csv("Rep2Trial4Cam2RawData.csv")
# DR2T5C1 <- read.csv("Rep2Trial5Cam1RawData.csv")
# DR2T5C2 <- read.csv("Rep2Trial5Cam2RawData.csv")
# DR2T6C1 <- read.csv("Rep2Trial6Cam1RawData.csv")
# DR2T6C2 <- read.csv("Rep2Trial6Cam2RawData.csv")

######################################################################

#Combing ALL THE DATA TABLES?
CompVidRep2 <- rbind(DR2T1C1, DR2T1C2, DR2T2C1, DR2T2C2, DR2T3C1, DR2T3C2)
#  DR2T4C1, DR2T4C2, DR2T5C1, DR2T5C2, DR2T6C1, DR2T6C2)

#write.csv( CompVid,"CompiledRawVideoData.csv")
####create function that takes in data set and adds quadrant assignments
#determine if bug is above or below line (differnet from predicted y)
# Assign<-function(VidData, CoordData, trayData){    
#     belowa <- which((VidData$y) <  (VidData$pred1))
#     abovea <- which((VidData$y) >= (VidData$pred1))
#     belowb <- which((VidData$y) <  (VidData$pred2))
#     aboveb <- which((VidData$y) >= (VidData$pred2))
#     
#     NegSlope <- which(CoordData$TPX <  CoordData$BPX )
#     PosSlope <- which(CoordData$TPX >= CoordData$BPX )
#     
#     negs<-which(is.na(match(NegSlope, VidData$trayn))==FALSE)
#     poss<-which(is.na(match(PosSlope, VidData$trayn))==FALSE)
#     
# # Determine Quadrants #change depending on slope of verticle line
# # In cases of positive slopes
# VidData$quad<-NA
# VidData$quad[intersect( poss, (intersect(belowa,aboveb)))]<-1
# VidData$quad[intersect( poss, (intersect(abovea,aboveb)))]<-4
# VidData$quad[intersect( poss, (intersect(belowa,belowb)))]<-2
# VidData$quad[intersect( poss, (intersect(abovea,belowb)))]<-3
# VidData$quad[intersect( negs, (intersect(abovea,aboveb)))]<-1
# VidData$quad[intersect( negs, (intersect(belowa,aboveb)))]<-4
# VidData$quad[intersect( negs, (intersect(abovea,belowb)))]<-2
# VidData$quad[intersect( negs, (intersect(belowa,belowb)))]<-3
#       
# ###Create function that determines which quadrants have pesticide
# for (i in 1:length(VidData$quad)){
#    r <- which(trayData$Repetition==VidData$rep[i])
#    t <- which(trayData$Trial==VidData$trial[i])
#    p <- which(trayData$Position==VidData$position[i])
#    id <- intersect( p, intersect(r, t))
# 
#     VidData$DishID[i] <- trayData$DishID[id]
#     VidData$Orientation[i] <- trayData$Orientation[id]
#   
#   #there has to be a better way to do this
#     one   <- c(1,2,3,4)
#     two   <- c(2,3,4,1)
#     three <- c(3,4,1,2)
#     four  <- c(4,1,2,3)
#     OTab  <- data.frame(one, two, three, four)
#     Or    <- which(OTab$one==VidData$Orientation[i])
#   VidData$PQuad[i] <- OTab[Or, VidData$quad[i]]
#   #I'm pretty sure the above 7 lines could be two.
#   }
# 
#   uno <- which(VidData$PQuad==1)  
#   dos <- which(VidData$PQuad==2)  
#   tres <- which(VidData$PQuad==3)  
#   cuatro <- which(VidData$PQuad==4)  
#   PTrays <- which(VidData$DishID <=6)
# 
#   VidData$Pesticide <- 0
#   VidData$Pesticide[intersect( PTrays, dos)] <- 1
#   VidData$Pesticide[intersect( PTrays, cuatro)] <- 1
#   
#   return(VidData)
# 
# }


# =============

# a = vertical
# b = horizontal

belowa <- which((CompVidRep2$y) <  (CompVidRep2$pred1))
abovea <- which((CompVidRep2$y) >= (CompVidRep2$pred1))
belowb <- which((CompVidRep2$y) <  (CompVidRep2$pred2))
aboveb <- which((CompVidRep2$y) >= (CompVidRep2$pred2))

NegSlope <- which(CompVidRep2$TPX <  CompVidRep2$BPX )
PosSlope <- which(CompVidRep2$TPX >= CompVidRep2$BPX )

# Determine Quadrants change depending on slope of verticle line
# In cases of positive slopes

# Instead of counter-clockwise numbering of quadrants (from the perspective
# of the video, not considering pesticide), quadrants were labeled clockwise
# starting form the top right as 1
CompVidRep2$quad <- 0
CompVidRep2$quad[intersect( PosSlope, (intersect(belowa,aboveb)))]<-1
CompVidRep2$quad[intersect( PosSlope, (intersect(abovea,aboveb)))]<-2
CompVidRep2$quad[intersect( PosSlope, (intersect(belowa,belowb)))]<-4
CompVidRep2$quad[intersect( PosSlope, (intersect(abovea,belowb)))]<-3
CompVidRep2$quad[intersect( NegSlope, (intersect(abovea,aboveb)))]<-1
CompVidRep2$quad[intersect( NegSlope, (intersect(belowa,aboveb)))]<-2
CompVidRep2$quad[intersect( NegSlope, (intersect(abovea,belowb)))]<-4
CompVidRep2$quad[intersect( NegSlope, (intersect(belowa,belowb)))]<-3

# Create function that determines which quadrants have pesticide
CompVidRep2$PQuad <- 0
CompVidRep2$DishID <- 0
CompVidRep2$Orientation <- 0

#Table to determine the painted quadrants given orientation
one   <- c(1,2,3,4)
two   <- c(2,3,4,1)
three <- c(3,4,1,2)
four  <- c(4,1,2,3)
OTab  <- data.frame(one, two, three, four)

# Input data from TrayPlace into CompVidRep2
for (i in 1:length(CompVidRep2$quad)) {
  # r, t and p are the INDICES within TrayPlace
  # by themselves, r, t and p are vectors but we then find the intersection
  # of all three to arrive at the id
  r <- which(TrayPlace$Repetition == CompVidRep2$rep[i]) 
  t <- which(TrayPlace$Trial == CompVidRep2$trial[i])
  p <- which(TrayPlace$Position == CompVidRep2$position[i])
  id <- intersect(p, intersect(r, t))
  
  CompVidRep2$DishID[i] <- TrayPlace$DishID[id]
  CompVidRep2$Orientation[i] <- TrayPlace$Orientation[id]
  
  # Setting up orientations
  CompVidRep2$PQuad[i] <- OTab[CompVidRep2$Orientation[i], 
                               CompVidRep2$quad[i]]
}

uno <- which(CompVidRep2$PQuad == 1)  
dos <- which(CompVidRep2$PQuad == 2)  
tres <- which(CompVidRep2$PQuad == 3)  
cuatro <- which(CompVidRep2$PQuad == 4)  
PTrays<- which(CompVidRep2$DishID <= 6)

CompVidRep2$Pesticide <- 0  
CompVidRep2$Pesticide[uno] <- 0
CompVidRep2$Pesticide[intersect( PTrays, dos)] <- 1
CompVidRep2$Pesticide[tres] <- 0
CompVidRep2$Pesticide[intersect( PTrays, cuatro)] <- 1
