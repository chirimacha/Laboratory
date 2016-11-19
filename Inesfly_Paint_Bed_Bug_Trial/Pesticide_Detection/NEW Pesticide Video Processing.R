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
install.packages("dplyr")
install.packages("clue")
install.packages("shiny")
install.packages("splancs")
install.packages("tictoc")
install.packages("reshape2")
install.packages("vioplot")
install.packages("scales")
install.packages("tictoc")

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
# wd <- paste("/Users/dtracy198/Documents/GitHub/Laboratory/",
#             "Inesfly_Paint_Bed_Bug_Trial/Pesticide_Detection", sep = "")
# setwd(wd)

# Lab computer
wd <- paste("/Users/mzlevy/Laboratory/Inesfly_Paint_Bed_Bug_Trial/",
      "Pesticide_Detection", sep = "")
setwd(wd)

#Justin's Computer
# setwd(file.path("/Users/Justin/Desktop/Levy_Research/Laboratory/",
#                "Inesfly_Paint_Bed_Bug_Trial/Pesticide_Detection"))
#setwd("/Users/Justin/Desktop/")

## Set number of repetitions, trials, cameras
repetition <- 4 #(max number, rep 1 not used)
trial <- 6
camera <- 2

### Bring in data: videos, coordinate tables (frames), TrayPlace
# Repetition 1 recorded on 2016-04-21(not used); repetition 2 recorded (3 weeks)
# Repetition 3 and 4 were recorded on 2016-07-14(12 weeks and 1 day, resp.)

# CoTb = coordinate table; R1 = rep 1; T1 = trial 1; C1 = camera 1
TrayPlace <- read.csv("TraysRep1y2y3y4.csv") # times, dates, humidity quadrant 

#a couple errors were detected. Slight tint to paint allows us to see 
#R2T2P2 and R3T3P9 orientation should be 1 or 3 have pesticide
TrayPlace$tray.id <-paste(TrayPlace$Repetition, TrayPlace$Trial, 
                          TrayPlace$Position, sep = "-")
tpca <- which(TrayPlace$tray.id == "2-2-2")
tpcb <- which(TrayPlace$tray.id == "3-3-9")
TrayPlace$Orientation[tpca] <- 1 
TrayPlace$Orientation[tpcb] <- 1

#I suspect R4T4C2 and R4T6C2 to also possible be errors that we cannot validify

#Bring in Videos
#videos are outside of GitHub due to space constraints.
#set wd to local video location
vid.fold <- c("/Users/mzlevy/Desktop/Videos")
setwd(vid.fold) 

#bring in videos with loop
#loop through i reps, j trials, and k cameras
for (i in 2:4) { 
  for (j in 1:6) {
    for (k in 1:2) {
      temp_name1 <- paste("vidR", i, "T", j, "C", k, sep = "")
      video_name <- paste("R", i, "T", j, "C", k, ".mp4", sep = "")
      assign(temp_name1, readVid(video_name))
    }
  }
}
setwd(wd)
 
# Brings in Coordinate Tables
for (i in 2:repetition) { 
  for (j in 1:trial) {
    for (k in 1:camera) {
     temp_name2 <- paste("CoTbR", i, "T", j, "C", k, sep = "")
      csv_name <- paste("Coordinate_Tables/CoTbR", i, "T", j, "C", k,
                        ".csv", sep = "")
      assign(temp_name2, read.csv(csv_name))
    }
  }
}

## Create background (this will take awhile, ~30-40 minutes)
# WARNING: Do this step only once
#originally loops together, but not efficient so divided into 3.
#bgR4T2C2 <- backgrounder(vidR4T2C2, n=1800, method = "median", color =FALSE)

for (i in 2:2) { 
  for (j in 1:trial) {
    for (k in 1:camera) {
      temp_name <- paste("bgR", i, "T", j, "C", k, sep = "")
      vid_name <- paste("vidR", i, "T", j, "C", k, sep = "")
      assign(temp_name, backgrounder(get(vid_name),  n = 1800, 
                                     method = "median", color = FALSE))
    }
  }
}
for (i in 3:3) { 
  for (j in 1:trial) {
    for (k in 1:camera) {
      tic(msg = NULL, quiet = TRUE, func.tic = NULL)
      temp_name <- paste("bgR", i, "T", j, "C", k, sep = "")
      vid_name <- paste("vidR", i, "T", j, "C", k, sep = "")
      assign(temp_name, backgrounder(get(vid_name),  n = 1700, 
                                     method = "median", color = FALSE))
      toc(log = FALSE, quiet = FALSE, func.toc = toc.outmsg)
    }
  }
}
for (i in 4:4) { 
  for (j in 1:trial) {
    for (k in 1:camera) {
      tic(msg = NULL, quiet = TRUE, func.tic = NULL)
      temp_name <- paste("bgR", i, "T", j, "C", k, sep = "")
      vid_name <- paste("vidR", i, "T", j, "C", k, sep = "")
      assign(temp_name, backgrounder(get(vid_name),  n = 1700, 
                                     method = "median", color = FALSE))
      toc(log = FALSE, quiet = FALSE, func.toc = toc.outmsg)
    }
  }
}

### See MaskMatrixTablesRX.R in Cordinate_Tables folder to see how 
#masks were created
#the get point function was necessary and may be helpful for further analysis
getpoint<-function(frame){
  rto <- frame$dim[1]/frame$dim[2]
  print(rto)
  ht<-rto*6
  quartz(width=6, height=ht)
  imshow(frame)
  a<-grid.locator(unit = "npc")
  gcx<-as.numeric(a$x)
  gcy<-as.numeric(a$y)
  X <- ceiling(gcx*frame$dim[2])
  Y <- ceiling(gcy*frame$dim[1])
  imshow(frame)
  points(x=c(X), y=c(Y), col="red", pch=19, cex = 0.1 )
  coord<-c(X,Y)
  output=coord
}

#Function used to show which areas are shown with each mask
visualize <- function(CD, frame){
  imshow(frame)
  for(i in 1:6){
    lines(x = c(CD$MXR[i], CD$MXL[i]), y = c(CD$MYT[i], CD$MYT[i]), col=i) 
    lines(x = c(CD$MXR[i], CD$MXL[i]), y = c(CD$MYB[i], CD$MYB[i]), col=i) 
    lines(x = c(CD$MXR[i], CD$MXR[i]), y = c(CD$MYT[i], CD$MYB[i]), col=i) 
    lines(x = c(CD$MXL[i], CD$MXL[i]), y = c(CD$MYT[i], CD$MYB[i]), col=i) 
    lines(x = c(CD$TPX[i], CD$BPX[i]), y = c(CD$TPY[i], CD$BPY[i]), col=i)
    lines(x = c(CD$LPX[i], CD$RPX[i]), y = c(CD$LPY[i], CD$RPY[i]), col=i)
  }
}

## Simple Tracker (package not available on cran for new R (3.2.4+))
#if older, simply run code below to line ~312. 
pdiff <- function(a, b) { # helper function
  nr <- length(a)
  nc <- length(b)
  
  ma <- matrix(a, nrow = nr, ncol = nc)
  mb <- matrix(b, nrow = nr, ncol = nc, byrow = TRUE)
  
  ma - mb
}

simpleTracker <- function(current, past, lookBack = 60, maxDist = 1000) {
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
   #   fr <- 200
  
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
      bugcords <- mutate(bugcords, frame = NA, track = NA) #add frame and track
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
      #write.csv(temp_name, file = paste("DR", i, "T", j, "C", k, ".csv", 
         #sep = ""))
      print(paste(temp_name,"....Completed!", sep = ""))
      toc(log = FALSE, quiet = FALSE, func.toc = toc.outmsg)
    }
  }
}

#lets do Vid analysis for just the fixed video
# DR4T2C2 <- VidAnalysis(video <- vidR4T2C2, bg <- bgR4T2C2, 
#             coordtab <- CoTbR4T2C2, 
#             thresholda <- user_thresh,
#             maxDistb <- user_max,
#             cam <- 2, rep <- 4, trial <- 2)
#write.csv(DR4T2C2, file = "DR4T2C2.csv")
###

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

#If running from PC run these codes
#rep2
DR2T1C1 <- read.csv("Video_Data/Rep2/Rep2Trial1Cam1RawData.csv")
DR2T1C2 <- read.csv("Video_Data/Rep2/Rep2Trial1Cam2RawData.csv")
DR2T2C1 <- read.csv("Video_Data/Rep2/Rep2Trial2Cam1RawData.csv")
DR2T2C2 <- read.csv("Video_Data/Rep2/Rep2Trial2Cam2RawData.csv")
DR2T3C1 <- read.csv("Video_Data/Rep2/Rep2Trial3Cam1RawData.csv")
DR2T3C2 <- read.csv("Video_Data/Rep2/Rep2Trial3Cam2RawData.csv")
DR2T4C1 <- read.csv("Video_Data/Rep2/Rep2Trial4Cam1RawData.csv")
DR2T4C2 <- read.csv("Video_Data/Rep2/Rep2Trial4Cam2RawData.csv")
DR2T5C1 <- read.csv("Video_Data/Rep2/Rep2Trial5Cam1RawData.csv")
DR2T5C2 <- read.csv("Video_Data/Rep2/Rep2Trial5Cam2RawData.csv")
DR2T6C1 <- read.csv("Video_Data/Rep2/Rep2Trial6Cam1RawData.csv")
DR2T6C2 <- read.csv("Video_Data/Rep2/Rep2Trial6Cam2RawData.csv")
#rep3
DR3T1C1 <- read.csv("Video_Data/Rep3/Rep3Trial1Cam1RawData.csv")
DR3T1C2 <- read.csv("Video_Data/Rep3/Rep3Trial1Cam2RawData.csv")
DR3T2C1 <- read.csv("Video_Data/Rep3/Rep3Trial2Cam1RawData.csv")
DR3T2C2 <- read.csv("Video_Data/Rep3/Rep3Trial2Cam2RawData.csv")
DR3T3C1 <- read.csv("Video_Data/Rep3/Rep3Trial3Cam1RawData.csv")
DR3T3C2 <- read.csv("Video_Data/Rep3/Rep3Trial3Cam2RawData.csv")
DR3T4C1 <- read.csv("Video_Data/Rep3/Rep3Trial4Cam1RawData.csv")
DR3T4C2 <- read.csv("Video_Data/Rep3/Rep3Trial4Cam2RawData.csv")
DR3T5C1 <- read.csv("Video_Data/Rep3/Rep3Trial5Cam1RawData.csv")
DR3T5C2 <- read.csv("Video_Data/Rep3/Rep3Trial5Cam2RawData.csv")
DR3T6C1 <- read.csv("Video_Data/Rep3/Rep3Trial6Cam1RawData.csv")
DR3T6C2 <- read.csv("Video_Data/Rep3/Rep3Trial6Cam2RawData.csv")
#rep4
DR4T1C1 <- read.csv("Video_Data/Rep4/Rep4Trial1Cam1RawData.csv")
DR4T1C2 <- read.csv("Video_Data/Rep4/Rep4Trial1Cam2RawData.csv")
DR4T2C1 <- read.csv("Video_Data/Rep4/Rep4Trial2Cam1RawData.csv")
DR4T2C2 <- read.csv("Video_Data/Rep4/Rep4Trial2Cam2RawData.csv")
DR4T3C1 <- read.csv("Video_Data/Rep4/Rep4Trial3Cam1RawData.csv")
DR4T3C2 <- read.csv("Video_Data/Rep4/Rep4Trial3Cam2RawData.csv")
DR4T4C1 <- read.csv("Video_Data/Rep4/Rep4Trial4Cam1RawData.csv")
DR4T4C2 <- read.csv("Video_Data/Rep4/Rep4Trial4Cam2RawData.csv")
DR4T5C1 <- read.csv("Video_Data/Rep4/Rep4Trial5Cam1RawData.csv")
DR4T5C2 <- read.csv("Video_Data/Rep4/Rep4Trial5Cam2RawData.csv")
DR4T6C1 <- read.csv("Video_Data/Rep4/Rep4Trial6Cam1RawData.csv")
DR4T6C2 <- read.csv("Video_Data/Rep4/Rep4Trial6Cam2RawData.csv")

CompVidRep2 <- rbind(DR2T1C1, DR2T1C2, DR2T2C1, DR2T2C2, DR2T3C1, DR2T3C2,
                     DR2T4C1, DR2T4C2, DR2T5C1, DR2T5C2, DR2T6C1, DR2T6C2)

CompVidRep3 <- rbind(DR3T1C1, DR3T1C2, DR3T2C1, DR3T2C2, DR3T3C1, DR3T3C2,
                     DR3T4C1, DR3T4C2, DR3T5C1, DR3T5C2, DR3T6C1, DR3T6C2)

CompVidRep4 <- rbind(DR4T1C1, DR4T1C2, DR4T2C1, DR4T2C2, DR4T3C1, DR4T3C2,
                     DR4T4C1, DR4T4C2, DR4T5C1, DR4T5C2, DR4T6C1, DR4T6C2)


###########
###Temp Read.csv
CompVidRep2 <- read.csv("CompVidRep2.csv")
CompVidRep3 <- read.csv("CompVidRep3.csv")
CompVidRep4 <- read.csv("CompVidRep4.csv")

#create a unicode by combining rep, trial, and position
CompVidRep2$insect.id <- paste(CompVidRep2$rep, CompVidRep2$trial, 
                               CompVidRep2$position, sep = "-")
CompVidRep3$insect.id <- paste(CompVidRep3$rep, CompVidRep3$trial,
                               CompVidRep3$position, sep = "-")
CompVidRep4$insect.id <- paste(CompVidRep4$rep, CompVidRep4$trial, 
                               CompVidRep4$position, sep = "-")

#########################Duplicate Correction##################################
###function can't deal with frame 1 with id >1 and currently if there is no 1
#check to make sure there is the correct number
ones.rtwo<- which(CompVidRep2$frame == 1)
ones.rthree<- which(CompVidRep3$frame == 1)
ones.rfour<- which(CompVidRep4$frame == 1)
#they should each ideallly have 72(6 trialsx12 bugs per trial)
length(ones.rtwo) #70
length(ones.rthree) #68
length(ones.rfour) #77

#fortunately only CompVidRep4 has multiple 1's
twomany<- which(CompVidRep4$id == 2)
waytoomany <- which(CompVidRep4$id > 1)
firstfr<- which(CompVidRep4$frame == 1)
one.twos<- intersect(twomany, firstfr)
first.twomany<- intersect(waytoomany, firstfr)
#fortunately they are only id=2 

###Remove ones with area of 0
noarea <- which(CompVidRep4$area[one.twos] == 0)
to.delete <- one.twos[noarea]

find.id <- which(CompVidRep4$insect.id == CompVidRep4$insect.id[first.twomany[1]])
found.it <- intersect(find.id, firstfr)
no.area <- which(CompVidRep4$area[found.it] == 0)
todelete <- found.it[no.area]
deletions <- union(to.delete, todelete)
CompVidRep4$id[first.twomany] <- 1
CompVidRep4 <- CompVidRep4[-deletions,]

###Duplication correction for all other frames
DupCorrect <- function(df){
  #create a column as an indicator for later deletion
  df$delete <- df$x*0
  #Identify duplicates (id>1)
  dups <- which(df$id > 1)
  twos <- which(df$id == 2)
  #identify the frames with duplicates
  d.fr <- df$frame[dups]
  ud.fr <- unique(d.fr)
  ud.fr <- ud.fr[order(unique(ud.fr))]
  #loop through each duplicate frame
  for(i in 1:length(ud.fr)){
    #identify the individual error (mal.fr includes non-duplicate obs)
    mal.fr <- which(df$frame == ud.fr[i])
    mtwo.fr <- intersect(mal.fr, twos) 
    #mtwo.fr identifies how many duplication insect obs are on this frame
    #so loop through each observation
    for(j in 1:length(mtwo.fr)){
      #identify the particular insect and frame number for this duplicate
      jid <- which(df$insect.id == df$insect.id[mtwo.fr[j]] )
      jf <- which(df$frame == df$frame[mtwo.fr[j]] )
      #find the previous frame for this specific observation
      less.fr <- which(df$frame < df$frame[mtwo.fr[j]])
      less.ifr <- intersect(less.fr, jid)
      if(length(less.ifr) < 1){print(paste("no prior frame", "i", i, "j", j))}
      ##the max arguement is likely producing errors if no previous frame
      #obtain the maximum frame that is less than the error frame 
      lcf <- which(df$frame == max(df$frame[less.ifr]))
      #lcf is all with that frame.Intersect with less.ifr for insect specific
      ilcf<- intersect(lcf, less.ifr)
      #remove duplicates, this assumes that prev. are already corrected
      non.dup <- which(df$delete == 0)
      rev.p.obv <- intersect(ilcf, non.dup)
      ndjf <- intersect(jf, non.dup)
      rev.obvs <- intersect(jid, ndjf)
      px <- df$x[rev.p.obv]
      py <- df$y[rev.p.obv]
      if(length(px) > 1){print(paste("I", i, "j", j, length(py), sep = " "))}
      dif.output <- rev.obvs*0
      for(k in 1:length(rev.obvs)){
        kx <- df$x[rev.obvs[k]]
        if(length(kx) > 1){ print(paste("K", k, length(kx), sep = " "))}    
        ky <- df$y[rev.obvs[k]]
        dif.output[k] <- sqrt((kx-px)^2+(ky-py)^2)
      }
      mini <- which(dif.output == min(dif.output))
      del.ob <- which(dif.output != min(dif.output))
      df$id[rev.obvs[mini]] <- 1
      df$delete[rev.obvs[del.ob]] <- 1
    }
  }
  duplicates <- which(df$delete == 1)
  df <- df[-duplicates,]
  return(df)
}

# #Identify duplicates (id>1)
# dups.t <- which(CompVidRep4$id > 1)
# twos.t <- which(CompVidRep4$id == 2)
# #identify the frames with duplicates
# d.fr.t <- CompVidRep4$frame[dups.t]
# ud.fr.t <- unique(d.fr.t)
# ud.fr.t <- ud.fr.t[order(unique(ud.fr.t))]
# 
# 
# remnant <- which(CompVidRep2$frame == 1104)
# prev <- which(CompVidRep2$frame == 1103)
# #vprev <- which(CompVidRep2$frame == 1501)
# dos.test <- which(CompVidRep2$insect.id == "2-3-2")
# now <- intersect(dos.test, remnant)          
# pri <- intersect(dos.test, prev) 
# #vpri <- intersect(dos.test, prev) 
# View(CompVidRep2a[now,])
# View(CompVidRep2a[pri,])
# #View(CompVidRep2a[vpri,])


#still 1 observation creating two warnings.
CompVidRep2 <- DupCorrect(CompVidRep2) 

CompVidRep3 <- DupCorrect(CompVidRep3) 
CompVidRep4 <- DupCorrect(CompVidRep4) #only this works?

#save as csv
#write.csv( CompVidRep2, file = "CompVidRep2.csv")
#write.csv( CompVidRep3, file = "CompVidRep3.csv")
#write.csv( CompVidRep4, file = "CompVidRep4.csv")

addOrientation <- function(CompiledData) {
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
CompiledData$quad <- CompiledData$x * 0
CompiledData$quad[intersect( PosSlope, (intersect(belowa,aboveb)))] <- 1
CompiledData$quad[intersect( PosSlope, (intersect(belowa,belowb)))] <- 2
CompiledData$quad[intersect( PosSlope, (intersect(abovea,belowb)))] <- 3
CompiledData$quad[intersect( PosSlope, (intersect(abovea,aboveb)))] <- 4

CompiledData$quad[intersect( NegSlope, (intersect(abovea,aboveb)))] <- 1
CompiledData$quad[intersect( NegSlope, (intersect(abovea,belowb)))] <- 2
CompiledData$quad[intersect( NegSlope, (intersect(belowa,belowb)))] <- 3
CompiledData$quad[intersect( NegSlope, (intersect(belowa,aboveb)))] <- 4


# Create function that determines which quadrants have pesticide
CompiledData$PQuad <- CompiledData$x * 0
CompiledData$DishID <- CompiledData$x * 0
CompiledData$Orientation <- CompiledData$x * 0

#Table to determine the painted quadrants given orientation
one   <- c(1,4,3,2)
two   <- c(2,1,4,3)
three <- c(3,2,1,4)
four  <- c(4,3,2,1)
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
  CompiledData$PQuad[i] <- OTab[CompiledData$quad[i], 
                                CompiledData$Orientation[i]]
}

uno <- which(CompiledData$PQuad == 1)  
dos <- which(CompiledData$PQuad == 2)  
tres <- which(CompiledData$PQuad == 3)  
cuatro <- which(CompiledData$PQuad == 4)  
PTrays <- which(CompiledData$DishID <= 6)

CompiledData$PTray <- CompiledData$PQuad * 0
CompiledData$PTray[PTrays] <- 1

CompiledData$Pesticide <- CompiledData$x * 0
CompiledData$Pesticide[intersect( PTrays, uno)] <- 1
CompiledData$Pesticide[intersect( PTrays, tres)] <- 1

CompiledData$Treat_Quad <- CompiledData$x * 0  
#quadrants 1 and 3 have pesticide on them.
CompiledData$Treat_Quad[union(uno, tres)] <- 1

CompiledData$Result <- paste(CompiledData$PTray,
                             CompiledData$Treat_Quad, sep="-")

return(CompiledData)
}

CompVidRep2<- addOrientation(CompVidRep2)
CompVidRep3<- addOrientation(CompVidRep3)
CompVidRep4<- addOrientation(CompVidRep4)

resultMat <- function(CompVidRep) {
  CN <- length(which(CompVidRep$Result == "0-0"))
  CP <- length(which(CompVidRep$Result == "0-1"))
  TN <- length(which(CompVidRep$Result == "1-0"))
  TP <- length(which(CompVidRep$Result == "1-1"))
  
  Result_Mat <- matrix(data = c(CN,CP,TN,TP), nrow = 2, ncol = 2,  
                       byrow = FALSE)
  return(Result_Mat)
}

###############################################################################
###DO NOT DELETE: Read.csv to bring in Data tables from computer without 
###data held as objects

# write.csv( CompVidRep2, file = "CompVidRep2.csv")
# write.csv( CompVidRep3, file = "CompVidRep3.csv")
# write.csv( CompVidRep4, file = "CompVidRep4.csv")

CompVidRep2 <- read.csv("CompVidRep2.csv")
CompVidRep3 <- read.csv("CompVidRep3.csv")
CompVidRep4 <- read.csv("CompVidRep4.csv")
###############################################################################
################################ Plottting #################################### 
####Tracks
###Create functions that plot 
#where d.frm is the compiled video data, lower is the lowest frame of interest
#and upper is the highest frame of interest (0 - 500 for example)
trackplot <- function(d.frm, lower, upper){
  ##first need to plot corresponding frames for tracking
  #First identify repetition. Should all be the same in df. so just take
  #the first observations frame
  rep.v <- d.frm$rep[1]
  #use this to find the corresponding repetition in Trayplace
  rep.tp<- which(TrayPlace$Repetition == rep.v)
  #identify the frames to be used in this analysis
  up.lim <- which(d.frm$frame <= upper)
  low.lim <- which(d.frm$frame >= lower)
  limits <- intersect(up.lim, low.lim)
  #Now move trial, looping through each (1-6)
  for (j in 1:trial) {
    #find trial in d.frm
    trial.v<- which(d.frm$trial == j)
    #find corresponding values in tray place
    trial.tp <- which(TrayPlace$Trial == j)
    video.tp <- intersect(rep.tp, trial.tp)
    #now move to camera
    for(k in 1:camera){
      #create temp_name for output
      temp_name <- paste("End.FR", rep.v, "T", j, "C", k, sep = "")
      #find specific video
      vid_name <- paste("vidR", rep.v, "T", j, "C", k, sep = "")
      #find specific coordinate table
      Ctname<-paste("CoTbR", rep.v, "T", j, "C", k, sep = "")
      ##identify the last frame, either 1800 or frame closest to upper range
      if(get(vid_name)$length < upper){
        uppers <- get(vid_name)$length 
      } else {uppers <- upper}
      assign(temp_name, getFrame(get(vid_name), uppers))
      #now plot last frame
      imshow(get(temp_name))
      #pdf("example.pdf")
      #once again, find corresponding cameras in d.frm
      cam.v <- which(d.frm$camera == k)
      #for our current trial
      src <- intersect(trial.v, cam.v)
      #within our frame limits
      relv <- intersect(src, limits)
      #plot points
      points(d.frm$x[relv], d.frm$y[relv], pch = 20,
             col = alpha(topo.colors(n=(upper-lower))[d.frm$frame[relv]-lower], 0.2))
      #scale for color gradient
      points(x = ( (1:(upper-lower)/3)+175), y = (rep(((get(Ctname)$TPY[1])+40),
                                                      times = (upper-lower))), 
             col = alpha(topo.colors(n=(upper-lower)),0.2), pch = 20)
      #Now need to indicate which quadrants have pesticide
      for(i in 1:6){
        position.tp <- which(TrayPlace$Position == (i+6*(k-1)))
        tray.ct <-which(get(Ctname)$Tray == i)
        id.tp <- intersect(position.tp, video.tp)
        off <- 100
        if(TrayPlace$DishID[id.tp] <= 6){
          if(TrayPlace$Orientation[id.tp] == 1){
            points(x = (get(Ctname)$RPX[tray.ct]) - off, 
                   y = (get(Ctname)$TPY[tray.ct]) - off,
                   pch = "*", col = "red")
            points(x = (get(Ctname)$LPX[tray.ct]) + off, 
                   y = (get(Ctname)$BPY[tray.ct]) + off,
                   pch = "*", col = "red")
          }
          if(TrayPlace$Orientation[id.tp] == 3){
            points(x = (get(Ctname)$RPX[tray.ct]) - off, 
                   y = (get(Ctname)$TPY[tray.ct]) - off,
                   pch = "*", col = "red")
            points(x = (get(Ctname)$LPX[tray.ct]) + off, 
                   y = (get(Ctname)$BPY[tray.ct]) + off,
                   pch = "*", col = "red")
          }
          if(TrayPlace$Orientation[id.tp] == 2){
            points(x = (get(Ctname)$RPX[tray.ct]) - off, 
                      y = (get(Ctname)$BPY[tray.ct]) + off,
                      pch = "*", col = "red")
            points(x = (get(Ctname)$LPX[tray.ct]) + off, 
                      y = (get(Ctname)$TPY[tray.ct]) - off,
                      pch = "*", col = "red")
          }
          if(TrayPlace$Orientation[id.tp] == 4){
            points(x = (get(Ctname)$RPX[tray.ct]) - off, 
                   y = (get(Ctname)$BPY[tray.ct]) + off,
                   pch = "*", col = "red")
            points(x = (get(Ctname)$LPX[tray.ct]) + off, 
                   y = (get(Ctname)$TPY[tray.ct]) - off,
                   pch = "*", col = "red")
          }
        }
      }
      
    #dev.off()
    }
  }
}

#Run Function on First 5 min.
#pdf("TrackPlots/firstfive/TrackPlotR2_fst.pdf")
trackplot(CompVidRep2, 1, 300)
#dev.off()
#pdf("TrackPlots/firstfive/TrackPlotR3_fst.pdf")
trackplot(CompVidRep3, 1, 300)
#dev.off()
#pdf("TrackPlots/firstfive/TrackPlotR4_fst.pdf")
trackplot(CompVidRep4, 1, 300)
#dev.off()

#Run Function on Last 5 min
#pdf("TrackPlots/lastfive/TrackPlotR2_lfm.pdf")
trackplot(CompVidRep2, 1500, 1800)
#dev.off()
#pdf("TrackPlots/lastfive/TrackPlotR3_lfm.pdf")
trackplot(CompVidRep3, 1500, 1800) #error says requested frame does not exist
#dev.off()

#pdf("TrackPlots/lastfive/TrackPlotR4_lfm.pdf")
trackplot(CompVidRep4, 1500, 1800)
#dev.off()

  insect.num <- unique(d.frm$insect.id)
  id.table <- cbind(1:length(insect.num), insect.num)
  for(i in 1: length(insect.num)){
      focal.i <- which(d.frm$insect.id == id.table$insect.num[i])
      #now we have the insect
  }


##Last 5 Minutes



################################ Averageing ###################################
###Function to create plot for instantanious proportion on pesticide
#Treatment
af <- function(d.f, length) {
  my.list <- vector('list', length)
  for (i in 1:length) {
    frame.num <- which(d.f$frame == i)
    one.zero <- which(d.f$Result == "1-0")
    one.one <- which(d.f$Result == "1-1")
    treatment.fr <- intersect(frame.num, union(one.zero, one.one))
    pesticide.fr <- intersect(frame.num, one.one)
    my.list[[i]] <- length(pesticide.fr)/length(treatment.fr)
  }
  final.df <- do.call('rbind', my.list)
  return(final.df)
}

# Control
af.control <- function(d.f, length) {
  my.list <- vector('list', length)
  for (i in 1:length) {
    frame.num <- which(d.f$frame == i)
    zero.zero <- which(d.f$Result == "0-0")
    zero.one <- which(d.f$Result == "0-1")
    control.fr <- intersect(frame.num, union(zero.zero, zero.one))
    pesticide.fr <- intersect(frame.num, zero.one)
    
    my.list[[i]] <- length(pesticide.fr)/length(control.fr)
  }
  final.df <- do.call('rbind', my.list)
  return(final.df) 
}

#Run the Function of Rep 2
af.CompVidRep2 <- af(CompVidRep2, 1800)
af.control.CompVidRep2 <- af.control(CompVidRep2, 1800)

aCVR2SD<- sd(af.CompVidRep2)
aCVR2SE<- aCVR2SD/sqrt(length(af.CompVidRep2))
acCVR2SD<- sd(af.control.CompVidRep2)
acCVR2SE<- acCVR2SD/sqrt(length(af.control.CompVidRep2))
mean(af.control.CompVidRep2)
acCVR2SD
acCVR2SE

acCVR3SD<- sd(af.control.CompVidRep3)
acCVR3SE<- acCVR2SD/sqrt(length(af.control.CompVidRep3))
mean(af.control.CompVidRep3)
acCVR3SD
acCVR3SE

mean(af.CompVidRep2)
aCVR2SD
aCVR2SE

#90 days
t.test(af.CompVidRep2, af.control.CompVidRep2, "two.sided")
#180 days
t.test(af.CompVidRep3, af.control.CompVidRep3, "two.sided")
#day 1
t.test(af.CompVidRep4, af.control.CompVidRep4, "two.sided")


#Run the Function of Rep 2
af.CompVidRep3 <- af(CompVidRep3, 1800)
af.control.CompVidRep3 <- af.control(CompVidRep3, 1800)

#Run the Function of Rep 2
af.CompVidRep4 <- af(CompVidRep4, 1800)
af.control.CompVidRep4 <- af.control(CompVidRep4, 1800)

###Functions to make a running average of proportion of bugs on pesticide
#Treatment
ma <- function(d.f, length) {
  my.list <- vector('list', length)
  pesticide.fr.past <- 0
  treatment.fr.past <- 0
  for (i in 1:length) {
    frame.num <- which(d.f$frame == i)
    one.zero <- which(d.f$Result == "1-0")
    one.one <- which(d.f$Result == "1-1")
    treatment.fr <- intersect(frame.num, union(one.zero, one.one))
    pesticide.fr <- intersect(frame.num, one.one)
    
    pesticide.fr.rt <- (length(pesticide.fr) + pesticide.fr.past)
    treatment.fr.rt <- (length(treatment.fr) + treatment.fr.past)
    my.list[[i]] <- pesticide.fr.rt/treatment.fr.rt
    
    pesticide.fr.past <- pesticide.fr.rt
    treatment.fr.past <- treatment.fr.rt
  }
  final.df <- do.call('rbind', my.list)
  return(final.df) 
}

ma.control <- function(d.f, length) {
  my.list <- vector('list', length)
  pesticide.fr.past <- 0
  control.fr.past <- 0
  for (i in 1:length) {
    frame.num <- which(d.f$frame == i)
    zero.zero <- which(d.f$Result == "0-0")
    zero.one <- which(d.f$Result == "0-1")
    control.fr <- intersect(frame.num, union(zero.zero, zero.one))
    pesticide.fr <- intersect(frame.num, zero.one)
    
    pesticide.fr.rt <- (length(pesticide.fr) + pesticide.fr.past)
    control.fr.rt <- (length(control.fr) + control.fr.past)
    my.list[[i]] <- pesticide.fr.rt/control.fr.rt
    
    pesticide.fr.past <- pesticide.fr.rt
    control.fr.past <- control.fr.rt
  }
  final.df <- do.call('rbind', my.list)
  return(final.df) 
}

#Run Function over Rep 2
ma.CompVidRep2 <- ma(CompVidRep2, 1800)
ma.control.CompVidRep2 <- ma.control(CompVidRep2, 1800)

#Run Function over Rep 2
ma.CompVidRep3 <- ma(CompVidRep3, 1800)
ma.control.CompVidRep3 <- ma.control(CompVidRep3, 1800)

#Run Function over Rep 2
ma.CompVidRep4 <- ma(CompVidRep4, 1800)
ma.control.CompVidRep4 <- ma.control(CompVidRep4, 1800)

###############################################################################
#### Running Average for Individual Insects ####
#make an insect identifier
ima <- function(d.f, length) {
  insects <- unique(d.f$insect.id)
  my.df <- data.frame(matrix(nrow = length(insects), ncol = length+2))
  my.df[,1] <- unique(d.f$insect.id)
  zero <- which(d.f$Treat_Quad == 0)
  one <- which(d.f$Treat_Quad == 1)
  for(j in 1:length(unique(d.f$insect.id))){
    pesticide.fr.past <- 0
    treatment.fr.past <- 0
    insect.fr <- which(d.f$insect.id == unique(d.f$insect.id)[j])
    my.df[j,2] <- d.f$PTray[min(insect.fr)]
    denom <- intersect(union(zero, one), insect.fr)
    neum <- intersect(one, insect.fr)
    for (i in 1:length) {
      frame.num <- which(d.f$frame == i)
      treatment.fr <- intersect(frame.num, denom)
      pesticide.fr <- intersect(frame.num, neum)
      pesticide.fr.rt <- (length(pesticide.fr) + pesticide.fr.past)
      treatment.fr.rt <- (length(treatment.fr) + treatment.fr.past)
      my.df[[j,i+2]] <- pesticide.fr.rt/treatment.fr.rt
      pesticide.fr.past <- pesticide.fr.rt
      treatment.fr.past <- treatment.fr.rt
    }
  }
  return(my.df)
}

#Run Function over Rep 2
ima.CVR2 <- ima(CompVidRep2, 1800)
ima.CVR3 <- ima(CompVidRep3, 1800)
ima.CVR4 <- ima(CompVidRep4, 1800)



###############################################################################
#### Buckets ####

###############################################################################
#### Instantaneous Speed ####
#lets look at speed (distance traveled from previous frame)
ClockSpeed <-function(df){
  insects <- unique(df$insect.id)
  my.df <- data.frame(matrix(nrow = length(insects), ncol = (max(df$frame)+1)))
  my.df[,1] <- unique(df$insect.id)
  insects <- unique(df$insect.id)
  for(i in 1:length(insects)){
    ins <- which(df$insect.id == insects[i])
    rev_frames <- df$frame[ins]
    my.df[i,2] <- df$PTray[min(ins)]
    for(f in 2:length(rev_frames)){
      frmn <- rev_frames[f]
      pfrmn <-rev_frames[f-1]
      p.cf <- which(df$frame == frmn)
      p.pf <- which(df$frame == pfrmn)
      cf <- intersect(p.cf, ins)
      pf <- intersect(p.pf, ins)
    my.df[i,f+1]<-sqrt((df$x[cf]-df$x[pf])^2+(df$y[cf]-df$y[pf])^2)/(frmn-pfrmn)
    }
  }
  return(my.df)
}

  insects <- unique(CompVidRep3$insect.id)
  my.df <- data.frame(matrix(nrow = length(insects), 
                             ncol = (max(CompVidRep3$frame)+1)))
  my.df[,1] <- unique(CompVidRep3$insect.id)
  insects <- unique(CompVidRep3$insect.id)
  for(i in 1){
    ins <- which(CompVidRep3$insect.id == insects[1])
    rev_frames <- CompVidRep3$frame[ins]
    my.df[i,2] <- CompVidRep3$PTray[min(ins)]
    for(f in 2:length(rev_frames)){
      frmn <- rev_frames[f]
      pfrmn <-rev_frames[f-1]
      p.cf <- which(CompVidRep3$frame == frmn)
      p.pf <- which(CompVidRep3$frame == pfrmn)
      cf <- intersect(p.cf, ins)
      pf <- intersect(p.pf, ins)
      my.df[i,f+1]<-sqrt((CompVidRep3$x[cf]-CompVidRep3$x[pf])^2 + 
                        (CompVidRep3$y[cf]-CompVidRep3$y[pf])^2)/(frmn-pfrmn)
    }
  }

csCVR2<-ClockSpeed(CompVidRep2)
csCVR3<-ClockSpeed(CompVidRep3)
csCVR4<-ClockSpeed(CompVidRep4)

#insect average speed
InsectAvSpeed <- function(csCVR){
  csCVR$AvSpeed <- csCVR4[,3]*0
  a <- apply(csCVR[,3:1801], 1:2, as.numeric)
  means<- function(x){mean(x, na.rm = TRUE)}
  b  <- apply(a, 1, means)
  csCVR$AvSpeed <- b
  return(csCVR)
}

csCVR2 <- InsectAvSpeed(csCVR2)
csCVR3 <- InsectAvSpeed(csCVR3)
csCVR4 <- InsectAvSpeed(csCVR4)

exposed2 <- which(csCVR2[,2] == 1)
exposed3 <- which(csCVR3[,2] == 1)
exposed4 <- which(csCVR4[,2] == 1)
control2 <- which(csCVR2[,2] == 0)
control3 <- which(csCVR3[,2] == 0)
control4 <- which(csCVR4[,2] == 0)

ExAS2 <- mean(csCVR2$AvSpeed[exposed2])
ExAS3 <- mean(csCVR3$AvSpeed[exposed3])
ExAS4 <- mean(csCVR4$AvSpeed[exposed4])
CoAS2 <- mean(csCVR2$AvSpeed[control2])
CoAS3 <- mean(csCVR3$AvSpeed[control3])
CoAS4 <- mean(csCVR4$AvSpeed[control4])

###############################################################################
#### Average Speed within Bins ####

###############################################################################
############################## Plotting Averages ##############################
###CompVidRep2 (3 weeks post painting)
#Instantaneous proportion of bugs on pesticide 
#pdf("TABLES_GRAPHS/PropBugTreatmentPerSec.pdf", height = 9, width= 3)
jpeg("TABLES_GRAPHS/PropBugTreatmentPerSec.jpeg", height = 9, width= 3, 
     units = "in", res = 800)
par(mfrow = c(3, 1), oma = c(1,1,2,1))
plot(x = c(0,1800), y = c(0.15, 0.85), type ="n", col = 0, xlab = "Time (seconds)",
     ylab = "Proportion of Bugs", main = "1 Day", xaxt = 'n', yaxt = 'n')
axis( 2, at = c(0:5 / 5), las = 2,
      labels = as.character(c(0:5 / 5)))
axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
points(x = 1:1800, y = af.CompVidRep4, pch = 20, col = "red")
points(x = 1:1800, y = af.control.CompVidRep4, pch = 20)
abline(h = 0.5, lty = 2)


plot(x = c(0,1800), y = c(0.15, 0.85), type ="n", col = 0, xlab = "Time (seconds)",
     ylab = "Proportion of Bugs", main = "3 Weeks", xaxt = 'n', yaxt = 'n')
axis( 2, at = c(0:5 / 5), las = 2,
      labels = as.character(c(0:5 / 5)))
axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
points(x = 1:1800, y = af.CompVidRep2, pch = 20, col = "red")
points(x = 1:1800, y = af.control.CompVidRep2, pch = 20)
abline(h = 0.5, lty = 2)
#dev.off()

### CompVidRep3 (12 weeks post painting)
#Instantaneous proportion
ttl <- c("12 Weeks")
#pdf("InstPropCvT_12Weeks.pdf")
plot(x = c(0,1800), y = c(0.15, 0.85), type ="n", col = 0, xlab = "Time (seconds)",
     ylab = "Proportion of Bugs", main = ttl, xaxt = 'n', yaxt = 'n')
axis( 2, at = c(0:5 / 5), las = 2,
      labels = as.character(c(0:5 / 5)))
axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
points(x = 1:1800, y = af.CompVidRep3, pch = 20, col = "red")
points(x = 1:1800, y = af.control.CompVidRep3, pch = 20)
abline(h = 0.5, lty = 2)

mtext("Proportion of Bugs on Treatment", side = 3, 
      line = 0.3, outer = TRUE, cex = 1)
mtext("Quadrants Every Second", side = 3, 
      line = -1, outer = TRUE, cex = 1)

dev.off()

# #Running Average
# #pdf("RunAvgComp_3Weeks.pdf")
# plot(x = c(0,1800), y = c(0,1), type ="n", col = 0, xlab = "Time(sec)",
#      ylab = "Average Proportion of Bugs on Pesticide Quadrants",
#      main = "Running Average Proportion of Bugs on Pesticide: 3 Weeks")
# points(x = 1:1800, y = ma.CompVidRep2, pch = 20, col = "red")
# points(x = 1:1800, y = ma.control.CompVidRep2, pch = 20)
# abline(h = 0.5, lty = 2)
# #dev.off()
# 
# ### CompVidRep3 (12 weeks post painting)
# #Instantaneous proportion
# ttl <- c("Instantaneous Proportion of Bugs on Pesticide Over Time: 12 Weeks")
# #pdf("InstPropCvT_12Weeks.pdf")
# plot(x = c(0,1800), y = c(0, 1), type ="n", col = 0, xlab = "Time(sec)",
#      ylab = "Proportion of Bugs on Pesticide Quadrants",
#      main = ttl)
# points(x = 1:1800, y = af.CompVidRep3, pch = 20, col = "red")
# points(x = 1:1800, y = af.control.CompVidRep3, pch = 20)
# abline(h = 0.5, lty = 2)
# #dev.off()
# 
# #Running Average
# #pdf("RunAvgComp_12Weeks.pdf")
# plot(x = c(0,1800), y = c(0,1), type ="n", col = 0, xlab = "Time(sec)",
#      ylab = "Average Proportion of Bugs on Pesticide Quadrants",
#      main = "Running Average Proportion of Bugs on Pesticide: 12 Weeks")
# points(x = 1:1800, y = ma.CompVidRep3, pch = 20, col = "red")
# points(x = 1:1800, y = ma.control.CompVidRep3, pch = 20)
# abline(h = 0.5, lty = 2)
# #dev.off()
# 
# ### CompVidRep4 (1 day post painting)
# #Instantaneous proportion
# #pdf("InstPropCvT_1Day.pdf")
# plot(x = c(0,1800), y = c(0.3,0.6), type ="n", col = 0, xlab = "Time(sec)",
#      ylab = "Proportion of Bugs on Pesticide Quadrants",
#      main = "Instantaneous Proportion of Bugs on Pesticide Over Time: 1 Day")
# points(x = 1:1800, y = af.CompVidRep4, pch = 20, col = "red")
# points(x = 1:1800, y = af.control.CompVidRep4, pch = 20)
# abline(h = 0.5, lty = 2)
# 
# #Running Average
# #pdf("RunAvgComp_1Day.pdf")
# plot(x = c(0,1800), y = c(0,1), type ="n", col = 0, xlab = "Time(sec)",
#      ylab = "Average Proportion of Bugs on Pesticide Quadrants",
#      main = "Running Average Proportion of Bugs on Pesticide: 1 Day")
# points(x = 1:1800, y = ma.CompVidRep4, pch = 20, col = "red")
# points(x = 1:1800, y = ma.control.CompVidRep4, pch = 20)
# abline(h = 0.5, lty = 2)
# #dev.off()

#######################Plot Individual Running Averages########################
#pdf("TABLES_GRAPHS/IndRrunning_avg.pdf", height = 9, width = 6)
#jpeg("TABLES_GRAPHS/IndRrunning_avg.jpeg", height = 9, width = 6, res = 300, 
#     units = "in")

par(mfrow = c(3, 2), oma = c(1,1,2,1))

# CompVidRep4
plot(x = c(1, 1800), y = c(0, 1), type = "n", xlab = "Time (seconds)",
     ylab = "Proportion of Time on Pesticide")
PlotSpeed(csCVR4, 1)
PlotSpeed(csCVR2, 90)
PlotSpeed(csCVR3, 180)
mtext(paste("Speed of Insects Between Each Frame", sep=" "), side = 3, 
      line = -1.5, outer = TRUE, cex = 1.2,main = "1 Day Exposed", xaxt = 'n', 
      yaxt = 'n')
axis( 2, at = c(0:5 / 5), las = 2,
      labels = as.character(c(0:5 / 5)))
axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
filter <- which(ima.CVR4[,2] == 1)
for(i in 1:length(CompVidRep4[filter,1])){
  lines(x = 1:1800, y = ima.CVR4[filter[i], 3:1802], 
        col = i, lty = 3)
}#(ima.CompVidRep2[filter[i],2]+1)
lines(x = 1:1800, y = ma.CompVidRep4, lty = 1, lwd = 1.5, col = 1)

plot(x = c(1, 1800), y = c(0, 1), type = "n", xlab = "Time (seconds)",
     ylab = "Proportion of Time",
     main = "1 Day Control", xaxt = 'n', yaxt = 'n')
axis( 2, at = c(0:5 / 5), las = 2,
      labels = as.character(c(0:5 / 5)))
axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
filter <- which(ima.CVR4[,2] == 0)
for(i in 1:length(CompVidRep4[filter,1])){
  lines(x = 1:1800, y = ima.CVR4[filter[i], 3:1802], col = i, lty = 3)
}
lines(x = 1:1800, y = ma.control.CompVidRep4, lty = 1, lwd = 1.5, col = 1)

# CompVidRep2
plot(x = c(1, 1800), y = c(0, 1), type = "n", xlab = "Time (seconds)",
     ylab = "Proportion of Time",
     main = "3 Weeks Exposed", xaxt = 'n', yaxt = 'n')
axis( 2, at = c(0:5 / 5), las = 2,
      labels = as.character(c(0:5 / 5)))
axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
filter <- which(ima.CVR2[,2] == 1)
for(i in 1:length(CompVidRep2[filter,1])){
  lines(x = 1:1800, y = ima.CVR2[filter[i], 3:1802], 
        col = i, lty = 3)
}#(ima.CompVidRep2[filter[i],2]+1)
lines(x = 1:1800, y = ma.CompVidRep2, lty = 1, lwd = 1.5, col = 1)

plot(x = c(1, 1800), y = c(0, 1), type = "n", xlab = "Time (seconds)",
     ylab = "Proportion of Time",
     main = "3 Weeks Control", xaxt = 'n', yaxt = 'n')
axis( 2, at = c(0:5 / 5), las = 2,
      labels = as.character(c(0:5 / 5)))
axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
filter <- which(ima.CVR2[,2] == 0)
for(i in 1:length(CompVidRep2[filter,1])){
  lines(x = 1:1800, y = ima.CVR2[filter[i], 3:1802], col = i, lty = 3)
}
lines(x = 1:1800, y = ma.control.CompVidRep2, lty = 1, lwd = 1.5, col = 1)

# CompVidRep3
plot(x = c(1, 1800), y = c(0, 1), type = "n", xlab = "Time (seconds)",
     ylab = "Proportion of Time",
     main = "12 Weeks Exposed", xaxt = 'n', yaxt = 'n')
axis( 2, at = c(0:5 / 5), las = 2,
      labels = as.character(c(0:5 / 5)))
axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
filter <- which(ima.CVR3[,2] == 1)
for(i in 1:length(CompVidRep3[filter,1])){
  lines(x = 1:1800, y = ima.CVR3[filter[i], 3:1802], 
        col = i, lty = 3)
}#(ima.CompVidRep2[filter[i],2]+1)
lines(x = 1:1800, y = ma.CompVidRep3, lty = 1, lwd = 1.5, col = 1)

plot(x = c(1, 1800), y = c(0, 1), type = "n", xlab = "Time (seconds)",
     ylab = "Proportion of Time",
     main = "12 Weeks Control", xaxt = 'n', yaxt = 'n')
axis( 2, at = c(0:5 / 5), las = 2,
      labels = as.character(c(0:5 / 5)))
axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
filter <- which(ima.CVR3[,2] == 0)
for(i in 1:length(CompVidRep3[filter,1])){
  lines(x = 1:1800, y = ima.CVR3[filter[i], 3:1802], col = i, lty = 3)
}
lines(x = 1:1800, y = ma.control.CompVidRep3, lty = 1, lwd = 1.5, col = 1)


mtext("Running Average of Proportion of Time that Individual ", side = 3, 
      line = 0.3, outer = TRUE, cex = 1.2)
mtext("Insects Spent on Treatment Quadrants", side = 3, 
      line = -1.2, outer = TRUE, cex = 1.2)

dev.off()

###############################Ind. Speed Graph################################
PlotSpeed <- function(csTest, num){
  filter <- which(csTest[,2] == 1)
  ConFilt <- which(csTest[,2] == 0)
  MeanSpeedPest <- csTest[1,]
  MeanSpeedCont <- csTest[1,]
  MeanSpeedPest[1] <- "MeanSpeedPest"
  MeanSpeedCont[1] <- "MeanSpeedCont"
  MeanSpeedPest[2] <- 1
  MeanSpeedCont[2] <- 0
  for(i in 3:1801){
    MeanSpeedPest[i] <- mean(csTest[filter, i], na.rm = T)
    MeanSpeedCont[i] <- mean(csTest[ConFilt, i], na.rm = T)
  }
  if(num == 1){ 
    d <- "Day -"
  } else {d <- "Weeks -"}
  mn.tile.t <- paste(num, d, "Exposed")
  mn.tile.c <- paste(num, d, "Control")
  #pdf("InstSpeed_3weeks")
  plot(x = c(1, 1800), y = c(0, 40), type = "n", 
     main = mn.tile.t, ylab = "Speed (pixels/sec)",
     xlab = "Time (seconds)", xaxt = 'n', yaxt = 'n')
  axis( 2, at = c(0:4 * 10), las = 2,
        labels = as.character(c(0:4 * 10)))
  axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
  for(i in 1:length(filter)){
    lines(x = 2:1800, y = csTest[ filter[i], 3:1801], 
          col = alpha(i, 0.5), lty = 3)
  }
  lines(x = 2:1800, y = MeanSpeedPest[3:1801], 
       col = 1, lty = 1)
  plot(x = c(1, 1800), y = c(0, 40), type = "n", 
       main = mn.tile.c, ylab = "Speed (pixels/sec)",
       xlab = "Time (seconds)", xaxt = 'n', yaxt = 'n')
  axis( 2, at = c(0:4 * 10), las = 2,
       labels = as.character(c(0:4 * 10)))
  axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
    for(i in 1:length(ConFilt)){
        lines(x = 2:1800, y = csTest[ ConFilt[i], 3:1801], 
              col = alpha(i, 0.5), lty = 3)
  }
  lines(x = 2:1800, y = MeanSpeedCont[3:1801], 
        col = 1, lty = 1)
}

#pdf(file = "SpeedPlots.pdf")
#pdf("TABLES_GRAPHS/SpeedPlots.pdf", height = 9, width = 6)
jpeg("TABLES_GRAPHS/SpeedPlots.jpeg", height = 9, width = 6, res = 300, 
     units = "in")
par(mfrow = c(3,2), oma = c(1,1,2,1))
PlotSpeed(csCVR4, 1)
PlotSpeed(csCVR2, 3)
PlotSpeed(csCVR3, 12)
mtext(paste("Speed of Insects Between Each Observation", sep = " "), side = 3, line = 0, 
            outer = TRUE, cex = 1.2)
dev.off()

####
#Lets check some things. Look at R2T1
wrongdots<- which(CompVidRep2$insect.id == "2-1-8")
View(CompVidRep2[wrongdots,])
imshow(getFrame(vidR2T1C2, 1))
imshow(getFrame(vidR2T1C2, 500))
imshow(getFrame(vidR2T1C2, 1000))


