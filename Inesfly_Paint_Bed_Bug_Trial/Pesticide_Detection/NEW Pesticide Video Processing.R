## Code for video tracking to determine if bed bugs can detect pesticides
## Levy Lab 2016
## WARNING: Do not change Quartz window dimensions during getPoint analysis

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
# install.packages("tictoc")
# install.packages("reshape2")
# install.packages("vioplot")
# install.packages("scales")
# install.packages("tictoc")

# Open Libraries
library(videoplayR)
library(dplyr)
library(clue)
library(shiny)
library(splancs)
library(grid)
library(tictoc)
library(reshape2)
library(vioplot)
library(scales)
library(tictoc)

## Set Working Directory
#Dylan's PC
setwd("/Users/dtrac/OneDrive/Documents/GitHub/Laboratory/Inesfly_Paint_Bed_Bug_Trial/Pesticide_Detection")

# Lab computer
 setwd("/Users/mzlevy/Laboratory/Inesfly_Paint_Bed_Bug_Trial/Pesticide_Detection")
# Justin's Computer
#setwd(file.path("/Users/Justin/Desktop/Levy_Research/Laboratory/",
                "Inesfly_Paint_Bed_Bug_Trial/Pesticide_Detection"))
# Gian Franco's
# setwd(".../Laboratory/Inesfly_Paint_Bed_Bug_Trial/Pesticide_Detection")

## Set number of repetitions, trials, cameras
# WARNING: as of May 25, 2016 only using repetition 2 trials
repetition <- 2
trial <- 6
camera <- 2

## Bring in videos, coordinate tables (frames), TrayPlace
# Repetition 1 recorded on 2016-04-21; repetition 2 recorded 2016-05-12
# CoTb = coordinate table; R1 = rep 1; T1 = trial 1; C1 = camera 1
TrayPlace<- read.csv("TraysRep1y2.csv") # times, dates, humidity quadrant 
#if using PC, run this loop instead of the one below
for (i in 2:repetition) { 
  for (j in 1:trial) {
    for (k in 1:camera) {
      temp_name2 <- paste("CoTbR", i, "T", j, "C", k, sep = "")
      csv_name <- paste("CoTbR", i, "T", j, "C", k, ".csv", sep = "")
      assign(temp_name2, read.csv(csv_name))
    }
  }
}



# assignments as TrayPlace
# for (i in 2:repetition) { 
#   for (j in 1:trial) {
#     for (k in 1:camera) {
#       temp_name1 <- paste("vidR", i, "T", j, "C", k, sep = "")
#       video_name <- paste("R", i, "T", j, "C", k, ".mp4", sep = "")
#       assign(temp_name1, readVid(video_name))
#       
#       temp_name2 <- paste("CoTbR", i, "T", j, "C", k, sep = "")
#       csv_name <- paste("CoTbR", i, "T", j, "C", k, ".csv", sep = "")
#       assign(temp_name2, read.csv(csv_name))
#     }
#   }
# }

## Create background (this will take awhile, ~30-40 minutes)
# WARNING: Do this step only once
for (i in 2:repetition) { 
  for (j in 1:trial) {
    for (k in 1:camera) {
      temp_name <- paste("bgR", i, "T", j, "C", k, sep = "")
      vid_name <- paste("vidR", i, "T", j, "C", k, sep = "")
      assign(temp_name, backgrounder(get(vid_name),  n = 1800, 
                                     method = "median", color = FALSE))
    }
  }
}

## Get a frame from each video in order to find coordinates
staticFrame = 5
for (i in 2:repetition) { 
  for (j in 1:trial) {
    for (k in 1:camera) {
      temp_name <- paste("FR", i, "T", j, "C", k, sep = "")
      vid_name <- paste("vidR", i, "T", j, "C", k, sep = "")
      assign(temp_name, getFrame(get(vid_name), staticFrame))
    }
  }
}

# ##getpoint<-function(frame) { # Do not change Quartz size
#   rto <- frame$dim[1]/frame$dim[2]
#   print(rto)
#   ht<-rto*6
#   quartz(width=6, height=ht)
#
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
# # Repeat the above code to find points. Manually enter them in data frames.
# tester <- getpoint(FR1T2C1)
# tester

visualize<-function(CD, frame){
  imshow(frame)
  for(i in 1:6){
    lines(x = c(CD$MXR[i], CD$MXL[i]), y = c(CD$MYT[i], CD$MYT[i]), col=i) 
    lines(x = c(CD$MXR[i], CD$MXL[i]), y = c(CD$MYB[i], CD$MYB[i]),col=i) 
    lines(x = c(CD$MXR[i], CD$MXR[i]), y = c(CD$MYT[i], CD$MYB[i]),col=i) 
    lines(x = c(CD$MXL[i], CD$MXL[i]), y = c(CD$MYT[i], CD$MYB[i]),col=i) 
    lines(x = c(CD$TPX[i], CD$BPX[i]), y = c(CD$TPY[i], CD$BPY[i]),col=i)
    lines(x = c(CD$LPX[i], CD$RPX[i]), y = c(CD$LPY[i], CD$RPY[i]),col=i)
  }
}

## Simple Tracker (package not available for new R)
pdiff <- function(a, b) { # helper function
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

# Coords helper function finds which quadrant 
# each of the blobs are in for each frame
Coords <- function(video, imask, maskBG, coordtaba, tn, threshold, maxDista) {
 if (video$length < 1800) {  # determine loop length
   fr <- video$length
   } else {
   fr <- 1800
   }
#    fr <- 200
  
  # Looks at each video frame and finds the coordinates of each blob
  bugpos <- data.frame()
  for (l in 1:fr){
    res <- getFrame(video, l) # extract individual frames
    gryscl <- ddd2d(res) # put frame into grey scale.
    mask <- blend(gryscl, imask, "*") # mask other petri dishes
    sub <- blend(maskBG, mask, "-") # subtract background from the mask 
    # (previous image). *Only movement shows* (difference b/w average and
    # frame location)
    bw <- thresholding(sub, threshold, "binary") # set a threshold difference 
    # to remove changes due to glare/noise. Converts to 0's and 1's.
    bugcords <- blobDetector(bw) 
    # 1. Detects the white (1's) blob that is created. 
    # 2. Gets coordinates. 
    # 3. Adds track # to data frame only if a change is detected.
    
    if (nrow(bugcords) > 0) {
      bugcords <- mutate(bugcords, frame = l, track = NA) # add frame and track
      # determines what points are linked. Optimally each insect given 1 track 
      # each because there is only one object, we can max out maxDist. 
      stout <- simpleTracker(past = bugpos, current = bugcords, 
                             maxDist = maxDista) 
      # combine tables previous in the loop.
      bugpos <- rbind(bugpos, stout)
      # bugpos <- bugcords
    }
    else if (identical(nrow(bugcords), 0)) {
      bugcords <- mutate(bugcords, frame = NA, track = NA) # add frame and track
      # determines what points are linked. Optimally each insect given 1 track 
      # each because there is only one object, we can max out maxDist. 
      stout <- simpleTracker(past = bugpos, current = bugcords, 
                             maxDist = maxDista) 
      # combine tables previous in the loop.
      bugpos <- rbind(bugpos, stout)
      # bugpos <- bugcords
    }
  }
  # Defines the lines of the quadrants on the petri dish
  ya <- c(coordtaba$BPY[tn],coordtaba$TPY[tn])
  xa <- c(coordtaba$BPX[tn],coordtaba$TPX[tn])   
  yb <- c(coordtaba$LPY[tn],coordtaba$RPY[tn])
  xb <- c(coordtaba$LPX[tn],coordtaba$RPX[tn])  
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
  bugpos$trayn <- tn # Indicate the tray in data table.
  return(bugpos) # Return the data table.
}

# VidAnalysis takes in the video and coordinate table to output the coordinates
# of the insect in each frame for all 6 bugs.
VidAnalysis <- function(video, bg, coordtab, thresholda,
                      maxDistb, cam, rep, trial) {
  # Creates black masks over each petri dish, giving black rectangle
  for (i in 1:6) {
    temp_mat <- paste("mat", i, sep = "")
    assign(temp_mat, matrix(0, nrow = bg$dim[1], ncol = bg$dim[2]))
  }
  
  # Create white hole for each petri dish in complete mask. The matrix works 
  # left to right, BUT top to bottom. Graph works bottom to top so we need 
  # correction.
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
  
  # Make mask matrix into an image
  for (j in 1:6) {
    temp_imask <- paste("imask", j, sep = "")
    assign(temp_imask, r2img(get(paste("mat", j, sep = ""))))
  }
  # Bring the mask and the background together
  for (k in 1:6) {
    temp_maskBG <- paste("maskBG", k, sep = "")
    assign(temp_maskBG, blend(bg, get(paste("imask", k, sep = "")), "*"))
  }
  # Run Coords helper function over the 6 dishes   
  # e.g. pdt1 = bugpos for tray 1, location, regardless of camera 1 or camera 2
  for (m in 1:6) {
    temp_pdt <- paste("pdt", m, sep = "")
    assign(temp_pdt, Coords(video, imask=get(paste("imask", m, sep = "")),
                            maskBG=get(paste("maskBG", m, sep = "")), 
                            coordtaba = coordtab, tn = m, 
                            threshold = thresholda, maxDista = maxDistb))
  }
  # Binds into master table
  MasterTab<-rbind(pdt1, pdt2, pdt3, pdt4, pdt5, pdt6)
  MasterTab$camera <- cam
  MasterTab$rep <- rep
  MasterTab$trial <- trial
  MasterTab$position <- (MasterTab$trayn) + (6 * (cam - 1))
  return(MasterTab)
}

# VidAnalysis used for all videos
user_thresh = 30
user_max = 1000
for (i in 2:repetition) { 
  for (j in 1:trial) {
    for (k in 1:camera) {
      tic(msg = NULL, quiet = TRUE, func.tic = NULL)
      temp_name <- paste("DR", i, "T", j, "C", k, sep = "")
      vid_name <- paste("vidR", i, "T", j, "C", k, sep = "")
      bg_name <- paste("bgR", i, "T", j, "C", k, sep = "")
      coord_name <- paste("CoTbR", i, "T", j, "C", k, sep = "")
      assign(temp_name, VidAnalysis(video <- get(vid_name), bg <- get(bg_name), 
                                    coordtab <- get(coord_name), 
                                    thresholda <- user_thresh,
                                    maxDistb <- user_max,
                                    cam <- k, rep <- i, trial <- j))
      write.csv(temp_name, file = paste("DR", i, "T", j, "C", k, ".csv", sep = ""))
      print(paste(temp_name,"....Completed!", sep = ""))
      toc(log = FALSE, quiet = FALSE, func.toc = toc.outmsg)
    }
  }
}

###
#Bring Data in for 
CompVidRep2<- read.csv("CompVidRep2.csv")


# Combining all data tables of repetition 2
# CompVidRep2 <- rbind(DR2T1C1, DR2T1C2, DR2T2C1, DR2T2C2, DR2T3C1, DR2T3C2,
#                      DR2T4C1, DR2T4C2, DR2T5C1, DR2T5C2, DR2T6C1, DR2T6C2)


# cnt_dup <- 0
# for (i in (1:nrow(CompVidRep2))) {
#   if (identical(CompVidRep2$id[i], 2)) {
#     if (identical(CompVidRep2$id[i - 2], 2)) {
#       cnt_dup <- cnt_dup + 1
#     }
#   }
# }
# cnt_dup
# 
# # Duplicate correction
# for (i in (1:nrow(CompVidRep2))) {
#   if (identical(CompVidRep2$id[i], 2)) {
#     fir_x_diff <- abs(CompVidRep2$x[i - 1] - CompVidRep2$x[i - 2])
#     fir_y_diff <- abs(CompVidRep2$y[i - 1] - CompVidRep2$y[i - 2])
#     sec_x_diff <- abs(CompVidRep2$x[i] - CompVidRep2$x[i - 2])
#     sec_y_diff <- abs(CompVidRep2$y[i] - CompVidRep2$y[i - 2])
#     
#     fir_diff <- (fir_x_diff + fir_y_diff)
#     sec_diff <- (sec_x_diff + sec_y_diff)
#     
#     if (fir_diff > sec_diff) {
#       CompVidRep2$id[i] <- 1
#       CompVidRep2 <- CompVidRep2[-(i - 1),]
#     }
#     else if ((fir_diff < sec_diff) || identical(fir_diff,sec_diff)) {
#       CompVidRep2 <- CompVidRep2[-i,]
#     }
#   }
#   else if (identical(DR2T1C1$id[i], 3)) {
#     fir_x_diff <- abs(CompVidRep2$x[i - 1] - CompVidRep2$x[i - 2])
#     fir_y_diff <- abs(CompVidRep2$y[i - 1] - CompVidRep2$y[i - 2])
#     sec_x_diff <- abs(CompVidRep2$x[i] - CompVidRep2$x[i - 2])
#     sec_y_diff <- abs(CompVidRep2$y[i] - CompVidRep2$y[i - 2])
#     
#     fir_diff <- (fir_x_diff + fir_y_diff)
#     sec_diff <- (sec_x_diff + sec_y_diff)
#     
#     if (fir_diff > sec_diff) {
#       CompVidRep2$id[i] <- 1
#       CompVidRep2 <- CompVidRep2[-(i - 1),]
#     }
#     else if ((fir_diff < sec_diff) || identical(fir_diff,sec_diff)) {
#       CompVidRep2 <- CompVidRep2[-i,]
#     }
#   }
#   else if (identical(CompVidRep2$id[i], 4)) {
#     fir_x_diff <- abs(CompVidRep2$x[i - 1] - CompVidRep2$x[i - 2])
#     fir_y_diff <- abs(CompVidRep2$y[i - 1] - CompVidRep2$y[i - 2])
#     sec_x_diff <- abs(CompVidRep2$x[i] - CompVidRep2$x[i - 2])
#     sec_y_diff <- abs(CompVidRep2$y[i] - CompVidRep2$y[i - 2])
#     
#     fir_diff <- (fir_x_diff + fir_y_diff)
#     sec_diff <- (sec_x_diff + sec_y_diff)
#     
#     if (fir_diff > sec_diff) {
#       CompVidRep2$id[i] <- 1
#       CompVidRep2 <- CompVidRep2[-(i - 1),]
#     }
#     else if ((fir_diff < sec_diff) || identical(fir_diff,sec_diff)) {
#       CompVidRep2 <- CompVidRep2[-i,]
#     }
#   }
# }

## 2.7% of missing data

## Finding quadrants
# a = vertical
# b = horizontal

belowa <- which((CompVidRep2$y) <  (CompVidRep2$pred1))
abovea <- which((CompVidRep2$y) >= (CompVidRep2$pred1))
belowb <- which((CompVidRep2$y) <  (CompVidRep2$pred2))
aboveb <- which((CompVidRep2$y) >= (CompVidRep2$pred2))

NegSlope <- which(CompVidRep2$TPX <  CompVidRep2$BPX )
PosSlope <- which(CompVidRep2$TPX >= CompVidRep2$BPX )

# Determine Quadrants change depending on slope of vertical line
# In cases of positive slopes

# Instead of counter-clockwise numbering of quadrants (from the perspective
# of the video, not considering pesticide), quadrants were labeled clockwise
# starting form the top right as 1

###
#Bring Data in for 
#CompVidRep2<- read.csv("CompVidRep2.csv")


CompVidRep2$quad <- 0
CompVidRep2$quad[intersect( PosSlope, (intersect(belowa,aboveb)))] <- 1
CompVidRep2$quad[intersect( PosSlope, (intersect(abovea,aboveb)))] <- 2
CompVidRep2$quad[intersect( PosSlope, (intersect(belowa,belowb)))] <- 4
CompVidRep2$quad[intersect( PosSlope, (intersect(abovea,belowb)))] <- 3
CompVidRep2$quad[intersect( NegSlope, (intersect(abovea,aboveb)))] <- 1
CompVidRep2$quad[intersect( NegSlope, (intersect(belowa,aboveb)))] <- 2
CompVidRep2$quad[intersect( NegSlope, (intersect(abovea,belowb)))] <- 4
CompVidRep2$quad[intersect( NegSlope, (intersect(belowa,belowb)))] <- 3

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

# ############## Dylan's code 6.30.16
# 
# ## Duplicate correction
# Dup_Correct <- Function(VData){
#   #For every value in the Video Data frame
#   for (i in (1:nrow(VData))) {
#     #and every possible track larger than one
#     for( j in 2:max(VData$ID)){
#       #Identify if the value is larger than one
#       if (identical(VData$ID[i], j)) {
#         #if so determine the previous frame, the first frame and the duplicate
#         frame_num <- VData$frame[i]
#         share_frame <- which(VData$frame == frame_num)
#         org_frames <- which(Vdata$ID == 1)
#         prev <- which(VData$frame == (frame_num-1))
#         
#         first_frame <- intersect(share_frames, org_frames)
#         prev_frame <- intersect(prev, org_frames)
#         
#         fir_x_diff <- abs(VData$x[first_frame] - VData$x[prev_frame])
#         fir_y_diff <- abs(VData$y[first_frame] - VData$y[prev_frame])
#         sec_x_diff <- abs(VData$x[i] - VData$x[prev_frame])
#         sec_y_diff <- abs(VData$y[i] - VData$y[prev_frame])
#         
#         fir_diff <- (fir_x_diff + fir_y_diff)
#         sec_diff <- (sec_x_diff + sec_y_diff)
#         
#         if (fir_diff > sec_diff) {
#           VData$id[i] <- 1
#           VData <- VData[-(first_frame),]
#         }
#         else if ((fir_diff < sec_diff) || identical(fir_diff,sec_diff)) {
#           VData <- VData[-i,]
#         }
#       }
#     }
#   }
#   return(VData)  
# }  
# 
# 
# ###Run Dup_Correct then Compile the Videos together
# CompiledData <- data.frame()  
# for (i in 2:repetition) { 
#   for (j in 1:trial) {
#     for (k in 1:camera) {
#       temp_name <- paste("DR", i, "T", j, "C", k, sep = "")
#       Fixed <-Dup_Correct(get(temp_name))
#       CompileData<-rbind(CompiledData, Fixed)
#     }
#   }
# }

CompiledData <- CompVidRep2

## Finding quadrants
# a = vertical
# b = horizontal

belowa <- which((CompiledData$y) <  (CompiledData$pred1))
abovea <- which((CompiledData$y) >= (CompiledData$pred1))
belowb <- which((CompiledData$y) <  (CompiledData$pred2))
aboveb <- which((CompiledData$y) >= (CompiledData$pred2))

NegSlope <- which(CompiledData$TPX <  CompiledData$BPX )
PosSlope <- which(CompiledData$TPX >= CompiledData$BPX )

# Determine Quadrants change depending on slope of vertical line
# In cases of positive slopes

# Instead of counter-clockwise numbering of quadrants (from the perspective
# of the video, not considering pesticide), quadrants were labeled clockwise
# starting form the top right as 1
CompiledData$quad <- 0
CompiledData$quad[intersect( PosSlope, (intersect(belowa,aboveb)))] <- 1
CompiledData$quad[intersect( PosSlope, (intersect(abovea,aboveb)))] <- 2
CompiledData$quad[intersect( PosSlope, (intersect(belowa,belowb)))] <- 4
CompiledData$quad[intersect( PosSlope, (intersect(abovea,belowb)))] <- 3
CompiledData$quad[intersect( NegSlope, (intersect(abovea,aboveb)))] <- 1
CompiledData$quad[intersect( NegSlope, (intersect(belowa,aboveb)))] <- 2
CompiledData$quad[intersect( NegSlope, (intersect(abovea,belowb)))] <- 4
CompiledData$quad[intersect( NegSlope, (intersect(belowa,belowb)))] <- 3

# Create function that determines which quadrants have pesticide
CompiledData$PQuad <- 0
CompiledData$DishID <- 0
CompiledData$Orientation <- 0

#Table to determine the painted quadrants given orientation
one   <- c(1,2,3,4)
two   <- c(2,3,4,1)
three <- c(3,4,1,2)
four  <- c(4,1,2,3)
OTab  <- data.frame(one, two, three, four)

# Input data from TrayPlace into CompVidRep2
for (i in 1:nrow(CompiledData)) {
  # r, t and p are the INDICES within TrayPlace
  # by themselves, r, t and p are vectors but we then find the intersection
  # of all three to arrive at the id
  r <- which(TrayPlace$Repetition == CompiledData$rep[i]) 
  t <- which(TrayPlace$Trial == CompiledData$trial[i])
  p <- which(TrayPlace$Position == CompiledData$position[i])
  id <- intersect(p, intersect(r, t))
  
  CompiledData$DishID[i] <- TrayPlace$DishID[id]
  CompiledData$Orientation[i] <- TrayPlace$Orientation[id]
  
  # Setting up orientations
  CompiledData$PQuad[i] <- OTab[CompiledData$Orientation[i], 
                                CompiledData$quad[i]]
}

uno <- which(CompiledData$PQuad == 1)  
dos <- which(CompiledData$PQuad == 2)  
tres <- which(CompiledData$PQuad == 3)  
cuatro <- which(CompiledData$PQuad == 4)  
PTrays<- which(CompiledData$DishID <= 6)

CompiledData$PTray <- CompiledData$PQuad*0
CompiledData$PTray[PTrays] <- 1

CompiledData$Pesticide[intersect( PTrays, dos)] <- 1
CompiledData$Pesticide[intersect( PTrays, cuatro)] <- 1

CompiledData$Treat_Quad <- 0  
CompiledData$Treat_Quad[union(dos, cuatro)] <- 1

CompiledData$Result <- paste(CompiledData$Treat_Quad,
                             CompiledData$PTray, sep="-")
CN <- length(which(CompiledData$Result=="0-0"))
CP <- length(which(CompiledData$Result=="0-1"))
TN <- length(which(CompiledData$Result=="1-0"))
TP <- length(which(CompiledData$Result=="1-1"))
sum(CN, CP, TN, TP)
dim(CompiledData)
Result_Mat<-matrix(data=c(CN,CP,TN,TP), nrow = 2, ncol = 2,  byrow = FALSE)

chisq.test(Result_Mat, correct = TRUE)

###
#lets look at speed (distance traveled from previous frame)
CompiledData$speed<-CompiledData$PQuad*0
for(i in 2:2){
  ii <- which(CompiledData$rep==i)
  for(j in 1:6){
    ji <- which(CompiledData$trial==j)
    ij<-intersect(ii, ji)
    for(k in 1:2){
      ki <- which(CompiledData$camera==k)
      ijk<-intersect(ij, ki)
      for(l in 1:6){
        li<-which(CompiledData$trayn==l)
        ijkl<-intersect(ijk, li)
        rev_frames<-CompiledData$frame[ijkl]
        print(length(rev_frames))
for(f in 2:length(rev_frames)){
  frmn<-rev_frames[f]
  cfr<-which(CompiledData==frmn)
  lessframes<-which(rev_frames < frmn)
  pfmn <- max(rev_frames[lessframes])
  pfr<-which(CompiledData==pfmn)
  cf <- intersect(cfr, ijkl)
  pf <- intersect(pfr, ijkl)
  print(paste(i,j,k,l, sep=""))
  print(cf)

#CompiledData$speed[cf]<-sqrt((CompiledData$x[cf]-CompiledData$x[pf])^2+(CompiledData$y[cf]-CompiledData$y[pf])^2)/(cf-pf)
        }
      }
    }
  }
}

CompiledData$speed<-CompiledData$PQuad*0
iia <- which(CompiledData$rep==2)
    jia <- which(CompiledData$trial==1)
    ija<-intersect(iia, jia)
      kia <- which(CompiledData$camera==1)
      ijka<-intersect(ija, kia)
        lia<-which(CompiledData$trayn==1)
        ijkla<-intersect(ijka, lia)
        rev_frames<-CompiledData$frame[ijkla]
          frmna<-rev_frames[2]
          cfra<-which(CompiledData==frmna)
          pfmna <- rev_frames[2-1]
          pfra<-which(CompiledData==pfmna)
          cfa <- intersect(cfra, ijkla)
          pfa <- intersect(pfra, ijkla)
          CompiledData$speed[cfa]<-sqrt((CompiledData$x[cfa]-CompiledData$x[pfa])^2+(CompiledData$y[cfa]-CompiledData$y[pfa])^2)/(cfa-pfa)


###############################################################################
#Make plots that track the bugs across time
#==============================================================================
###
pointtype<-c(18,20)
sf<-1
fl<-300
plot(x=CompiledData$x, y=CompiledData$y, type="n")
c_two<-which(TrayPlace$Position>6)
c_one<-which(TrayPlace$Position<=6)
TrayPlace$camera[c_two]<-2
TrayPlace$camera[c_one]<-1

for(i in 2:2){
  ii <- which(CompiledData$rep==i)
  ti <- which(TrapPlace$Repetition==i)
  for(j in 1:6){
    ji <- which(CompiledData$trial==j)
    ij<-intersect(ii, ji)
    tj<- which(TrayPlace$Trial==j)
    tij<-intersect(ti, tj)
    for(k in 1:2){
      ki <- which(CompiledData$camera==k)
      ijk<-intersect(ij, ki)
      tk <- which(TrayPlace$camera == k)
      tijk<-intersect(tij, tk)
      temp_name <- paste("trackplot", i, j, k,".pdf", sep="")
      pdf(file=temp_name)
      plot(x=CompiledData$x, y=CompiledData$y, type="n")
      points(x =( 1:length(sf:fl)/3)+200, y = rep(120, times=length(sf:fl)), 
               col= alpha(topo.colors(n=length(sf:fl)),0.2))
      Ctname<-paste("CoTbR", i, "T", j, "C", k, sep = "")
      for(l in 1:6){
        li<-which(CompiledData$trayn==l)
        ijkl<-intersect(ijk, li)
        tl<-which(CompiledData$trayn==l)
        tijkl<-intersect(tijk, tl)
        lines(x=c(get(Ctname)$BPX[l],get(Ctname)$TPX[l]), y=c(get(Ctname)$BPY[l],get(Ctname)$TPY[l]), col=6)
        lines(x=c(get(Ctname)$RPX[l],get(Ctname)$LPX[l]), y=c(get(Ctname)$RPY[l],get(Ctname)$LPY[l]), col=6)
        points()
        for(f in sf:fl){
          #tic()
          fi <- which(CompiledData$frame==f)
          fijkl<-intersect(fi, ijkl)
          points(x = CompiledData$x[fijkl], y = CompiledData$y[fijkl], 
            col = alpha(topo.colors(n=(fl-sf))[f],0.2),
            pch=pointtype[CompiledData$PTray[fijkl]+1])

         # points(x =( 1:length(frame_limit)/5)+200, y = rep(80, times=length(frame_limit)), 
          #  col= alpha(topo.colors(n=length(fijkl)),0.2))
          #toc()
        }
      }
      dev.off()
}
}
}

for(i in 1:6){
  lines(x = c(CD$MXR[i], CD$MXL[i]), y = c(CD$MYT[i], CD$MYT[i]),col=i) 
  lines(x = c(CD$MXR[i], CD$MXL[i]), y = c(CD$MYB[i], CD$MYB[i]),col=i) 
  lines(x = c(CD$MXR[i], CD$MXR[i]), y = c(CD$MYT[i], CD$MYB[i]),col=i) 
  lines(x = c(CD$MXL[i], CD$MXL[i]), y = c(CD$MYT[i], CD$MYB[i]),col=i) 
  lines(x = c(CD$TPX[i], CD$BPX[i]), y = c(CD$TPY[i], CD$BPY[i]),col=i)
  lines(x = c(CD$LPX[i], CD$RPX[i]), y = c(CD$LPY[i], CD$RPY[i]),col=i)
}

## Create insect id for new data table aggregating data by insect.
CompiledData$iid<- paste(CompiledData$rep, CompiledData$trial, CompiledData$camera, CompiledData$DishID, sep="-")

iids<-unique(CompiledData$iid)

#create insect based data frame
insectdata <- data.frame(iids)
##create blank colomns for loop 
#The percentage of treatment frame
insectdata$Perc_Treatment_Frames <- c(1:length(insectdata$iids)*NA)
#Whether or not the tray is exposed to pesticide
insectdata$Pesticide_Tray <- c(1:length(insectdata$iids)*NA)

#run a loop to create data table to fill
for( i in 1:length(iids)){
  insect<-which(CompiledData$iid==insectdata$iids[i])
  treat_quads<-sum(CompiledData$Treat_Quad[insect])
  insectdata$Perc_Treatment_Frames[i] <- treat_quads/length(CompiledData$Treat_Quad[insect])
  insectdata$Pesticide_Tray[i] <- CompiledData$PTray[min(insect)]
  insectdata$tray_number[i] <- CompiledData$trayn[min(insect)]
  insectdata$trial[i] <- CompiledData$trial[min(insect)]
  insectdata$tray_position[i] <- CompiledData$position[min(insect)]
}

pesticide <- which(insectdata$Pesticide_Tray==1)
control <- which(insectdata$Pesticide_Tray==0)

jpeg(filename= "PercFramesonPesticideByExposure_Scatter.jpeg")
plot(x=(insectdata$Pesticide_Tray+rnorm(n=length(insectdata$Pesticide_Tray), mean=0, sd=0.15)),
     y=insectdata$Perc_Treatment_Frames, col = insectdata$Pesticide_Tray+1, 
    ylab= "Proportion of Time Spent on Treatment Quadrants",
    xlab= "Treatment Status (Black = Controls, Red= Exposed to Pesticide)")
dev.off()

jpeg("PercFramesonPesticideByExposure_Violin.jpeg")
plot(-3,-3,type="n",xlim=c(.5, 2.5 ),ylim=c(0,1),axes=FALSE,ann=FALSE)
vioplot(insectdata$Perc_Treatment_Frames[control], insectdata$Perc_Treatment_Frames[pesticide], add = TRUE)
        axis(side= 1, labels=c("Only Control Paint", "With Pesticidal Paint"), at=1:2)
        axis(side= 2, labels=paste(seq(0, 100, by =10), "%", sep=""), at=seq(0, 1, by =0.1))
dev.off()   
#now look at trays as a covariate
        
plot(x=as.factor(insectdata$Pesticide_Tray), y=insectdata$Perc_Treatment_Frames, col = insectdata$tray_number)
plot(-3,-3,type="n",xlim=c(.5, 2.5 ),ylim=c(0,1),axes=FALSE,ann=FALSE)

points(x=as.factor(insectdata$Pesticide_Tray), y=insectdata$Perc_Treatment_Frames, col= insectdata$tray_number)

plot(x=insectdata$Pesticide_Tray, y=insectdata$Perc_Treatment_Frames, col = insectdata$trial)
plot(x=insectdata$Pesticide_Tray, y=insectdata$Perc_Treatment_Frames, col = insectdata$tray_position)


t.test(insectdata$Perc_Treatment_Frames[pesticide], insectdata$Perc_Treatment_Frames[control])



