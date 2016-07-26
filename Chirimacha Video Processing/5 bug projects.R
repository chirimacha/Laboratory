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
frfbvid<-getFrame(fbvid, 20)
getpoint<-function(frame) { # Do not change Quartz size
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
# #Repeat the above code to find points. Manually enter them in the data frames.
tester <- getpoint(frfbvid)
# tester

##########################
###Set Working Directory
setwd("/Users/Justin/Desktop")
###Bring in video'
# File is 108MB. Too large for Github
chiri.vid <- readVid("20160722 tablet2.mp4")
filter.vid <- readVid("RedtoGrey.mp4")
#MP4 and WMV can be found on Google Drive
#https://drive.google.com/open?id=0BymPutRx4sc2Yjh3YXJXVXV6QzA

###Reset the working director
setwd("/Users/Justin/Desktop/Levy_Research/Laboratory/Inesfly_Paint_Bed_Bug_Trial/Pesticide_Detection")

##########################
# Create the background; serves as comparison or "bugless" control
bg <- backgrounder(filter.vid, n=150, method="median", color= TRUE)
# Create a colorless background
# bgcl<-backgrounder(fbvid, n=150, method="median", color= TRUE)
# Create a mask to reduce glare and other issues simple tracker
mat1 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
# Create white hole for each petri dish in complete mask. The matrix works 
# left to right, BUT top to bottom. Graph works bottom to top so we need 
# correction.
mat1[((bg$dim[1])-418):((bg$dim[1])-204), 312:504] <- 1
imask <- d2ddd(r2img(mat1))
# Blend mask and background to get a masked background
mbg <- (blend(bg, imask, "*"))

## Data collection from video
# 1/6
my.list <- vector('list', 1000)
my.df <- data.frame()
bugpos_past <- data.frame()
bugpos_save1 <- data.frame()
bugpos_save2 <- data.frame()
bugpos_save3 <- data.frame()
bugpos_save4 <- data.frame()
bugpos_save5 <- data.frame()
bugpos_save6 <- data.frame()
for (j in 1:383) {
  tic(msg = NULL, quiet = TRUE, func.tic = NULL)
  end.frame <- (j * 29)
  begin.frame <- (end.frame - 28)
  bugpos <- data.frame() 
  if (identical(j, 2)) {
    bugpos_past <- bugpos_save1
  }
  else if (identical(j, 3)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2)
  }
  else if (identical(j, 4)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3)
  }
  else if (identical(j, 5)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4)
  }
  else if (identical(j, 6)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4,
                         bugpos_save5)
  }
  else if (j > 6) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4,
                         bugpos_save5, bugpos_save6)
  }
  for (l in begin.frame:end.frame) {
    res <- getFrame(fbvid, l) # extract individual frames
    mask <- blend(res, imask, "*") # mask other petri dishes
    sub <- blend(mbg, res, "-") # subtract background from the mask 
    bw <- thresholding(sub, thres = 50, "binary") # set a threshold difference 
    bugcords <- blobDetector(bw) # detect the white blobs that are created; 
    bugcords <- mutate(bugcords, frame = l, track = NA)
    stout <- simpleTracker(past = bugpos_past, current = bugcords, maxDist = 20) 
    bugpos <- rbind(bugpos, stout)
  }
  if (identical(j, 1)) {
    bugpos_save1 <- bugpos
  }
  else if (identical(j, 2)) {
    bugpos_save2 <- bugpos
  }
  else if (identical(j, 3)) {
    bugpos_save3 <- bugpos
  }
  else if (identical(j, 4)) {
    bugpos_save4 <- bugpos
  }
  else if (identical(j, 5)) {
    bugpos_save5 <- bugpos
  }
  else if (identical(j, 6)) {
    bugpos_save6 <- bugpos
  }
  else if (j > 6) {
    bugpos_save1 <- bugpos_save2
    bugpos_save2 <- bugpos_save3
    bugpos_save3 <- bugpos_save4
    bugpos_save4 <- bugpos_save5
    bugpos_save5 <- bugpos_save6
    bugpos_save6 <- bugpos
  }
  my.list[[j]] <- bugpos
  toc(log = FALSE, quiet = FALSE, func.toc = toc.outmsg)
}
my.df <- do.call('rbind', my.list)

# 1/6 (SECOND PART)
my.list_secondpart <- vector('list', 1000)
my.df_secondpart <- data.frame()
bugpos_past <- data.frame()
bugpos_save1 <- data.frame()
bugpos_save2 <- data.frame()
bugpos_save3 <- data.frame()
bugpos_save4 <- data.frame()
bugpos_save5 <- data.frame()
bugpos_save6 <- data.frame()
for (j in 1:180) {
  tic(msg = NULL, quiet = TRUE, func.tic = NULL)
  end.frame <- (j * 29)
  begin.frame <- (end.frame - 28)
  bugpos <- data.frame() 
  if (identical(j, 2)) {
    bugpos_past <- bugpos_save1
  }
  else if (identical(j, 3)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2)
  }
  else if (identical(j, 4)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3)
  }
  else if (identical(j, 5)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4)
  }
  else if (identical(j, 6)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4,
                         bugpos_save5)
  }
  else if (j > 6) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4,
                         bugpos_save5, bugpos_save6)
  }
  for (l in begin.frame:end.frame) {
    res <- getFrame(fbvid_secondpart, l) # extract individual frames
    mask <- blend(res, imask, "*") # mask other petri dishes
    sub <- blend(mbg, res, "-") # subtract background from the mask 
    bw <- thresholding(sub, thres = 50, "binary") # set a threshold difference 
    bugcords <- blobDetector(bw) # detect the white blobs that are created; 
    bugcords <- mutate(bugcords, frame = l, track = NA)
    stout <- simpleTracker(past = bugpos_past, current = bugcords, maxDist = 20) 
    bugpos <- rbind(bugpos, stout)
  }
  if (identical(j, 1)) {
    bugpos_save1 <- bugpos
  }
  else if (identical(j, 2)) {
    bugpos_save2 <- bugpos
  }
  else if (identical(j, 3)) {
    bugpos_save3 <- bugpos
  }
  else if (identical(j, 4)) {
    bugpos_save4 <- bugpos
  }
  else if (identical(j, 5)) {
    bugpos_save5 <- bugpos
  }
  else if (identical(j, 6)) {
    bugpos_save6 <- bugpos
  }
  else if (j > 6) {
    bugpos_save1 <- bugpos_save2
    bugpos_save2 <- bugpos_save3
    bugpos_save3 <- bugpos_save4
    bugpos_save4 <- bugpos_save5
    bugpos_save5 <- bugpos_save6
    bugpos_save6 <- bugpos
  }
  my.list_secondpart[[j]] <- bugpos
  toc(log = FALSE, quiet = FALSE, func.toc = toc.outmsg)
}
my.df_secondpart <- do.call('rbind', my.list_secondpart)

# 2/6
my.list2 <- vector('list', 1000)
my.df2 <- data.frame()
bugpos_past <- data.frame()
bugpos_save1 <- data.frame()
bugpos_save2 <- data.frame()
bugpos_save3 <- data.frame()
bugpos_save4 <- data.frame()
bugpos_save5 <- data.frame()
bugpos_save6 <- data.frame()
for (j in 383:686) {
  tic(msg = NULL, quiet = TRUE, func.tic = NULL)
  end.frame <- (j * 29)
  begin.frame <- (end.frame - 28)
  bugpos <- data.frame()
  if (identical(j, 2)) {
    bugpos_past <- bugpos_save1
  }
  else if (identical(j, 3)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2)
  }
  else if (identical(j, 4)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3)
  }
  else if (identical(j, 5)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4)
  }
  else if (identical(j, 6)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4,
                         bugpos_save5)
  }
  else if (j > 6) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4,
                         bugpos_save5, bugpos_save6)
  }
  for (l in begin.frame:end.frame) {
    res <- getFrame(fbvid, l) # extract individual frames
    mask <- blend(res, imask, "*") # mask other petri dishes
    sub <- blend(mbg, res, "-") # subtract background from the mask 
    bw <- thresholding(sub, thres = 50, "binary") # set a threshold difference 
    bugcords <- blobDetector(bw) # detect the white blobs that are created; 
    bugcords <- mutate(bugcords, frame = l, track = NA)
    stout <- simpleTracker(past = bugpos_past, current = bugcords, maxDist = 20) 
    bugpos <- rbind(bugpos, stout)
  }
  if (identical(j, 1)) {
    bugpos_save1 <- bugpos
  }
  else if (identical(j, 2)) {
    bugpos_save2 <- bugpos
  }
  else if (identical(j, 3)) {
    bugpos_save3 <- bugpos
  }
  else if (identical(j, 4)) {
    bugpos_save4 <- bugpos
  }
  else if (identical(j, 5)) {
    bugpos_save5 <- bugpos
  }
  else if (identical(j, 6)) {
    bugpos_save6 <- bugpos
  }
  else if (j > 6) {
    bugpos_save1 <- bugpos_save2
    bugpos_save2 <- bugpos_save3
    bugpos_save3 <- bugpos_save4
    bugpos_save4 <- bugpos_save5
    bugpos_save5 <- bugpos_save6
    bugpos_save6 <- bugpos
  }
  my.list2[[j]] <- bugpos
  toc(log = FALSE, quiet = FALSE, func.toc = toc.outmsg)
}
my.df2 <- do.call('rbind', my.list2)

# 3/6
my.list3 <- vector('list', 1000)
my.df3 <- data.frame()
bugpos_past <- data.frame()
bugpos_save1 <- data.frame()
bugpos_save2 <- data.frame()
bugpos_save3 <- data.frame()
bugpos_save4 <- data.frame()
bugpos_save5 <- data.frame()
bugpos_save6 <- data.frame()
for (j in 690:831) {
  tic(msg = NULL, quiet = TRUE, func.tic = NULL)
  end.frame <- (j * 29)
  begin.frame <- (end.frame - 28)
  bugpos <- data.frame() 
  if (identical(j, 2)) {
    bugpos_past <- bugpos_save1
  }
  else if (identical(j, 3)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2)
  }
  else if (identical(j, 4)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3)
  }
  else if (identical(j, 5)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4)
  }
  else if (identical(j, 6)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4,
                         bugpos_save5)
  }
  else if (j > 6) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4,
                         bugpos_save5, bugpos_save6)
  }
  for (l in begin.frame:end.frame) {
    res <- getFrame(fbvid, l) # extract individual frames
    mask <- blend(res, imask, "*") # mask other petri dishes
    sub <- blend(mbg, res, "-") # subtract background from the mask 
    bw <- thresholding(sub, thres = 50, "binary") # set a threshold difference 
    bugcords <- blobDetector(bw) # detect the white blobs that are created; 
    bugcords <- mutate(bugcords, frame = l, track = NA)
    stout <- simpleTracker(past = bugpos_past, current = bugcords, maxDist = 30) 
    bugpos <- rbind(bugpos, stout)
  }
  if (identical(j, 1)) {
    bugpos_save1 <- bugpos
  }
  else if (identical(j, 2)) {
    bugpos_save2 <- bugpos
  }
  else if (identical(j, 3)) {
    bugpos_save3 <- bugpos
  }
  else if (identical(j, 4)) {
    bugpos_save4 <- bugpos
  }
  else if (identical(j, 5)) {
    bugpos_save5 <- bugpos
  }
  else if (identical(j, 6)) {
    bugpos_save6 <- bugpos
  }
  else if (j > 6) {
    bugpos_save1 <- bugpos_save2
    bugpos_save2 <- bugpos_save3
    bugpos_save3 <- bugpos_save4
    bugpos_save4 <- bugpos_save5
    bugpos_save5 <- bugpos_save6
    bugpos_save6 <- bugpos
  }
  my.list3[[j]] <- bugpos
  toc(log = FALSE, quiet = FALSE, func.toc = toc.outmsg)
}
my.df3 <- do.call('rbind', my.list3)

# 4/6
my.list4 <- vector('list', 1000)
my.df4 <- data.frame()
bugpos_past <- data.frame()
bugpos_save1 <- data.frame()
bugpos_save2 <- data.frame()
bugpos_save3 <- data.frame()
bugpos_save4 <- data.frame()
bugpos_save5 <- data.frame()
bugpos_save6 <- data.frame()
for (j in 835:1027) {
  tic(msg = NULL, quiet = TRUE, func.tic = NULL)
  end.frame <- (j * 29)
  begin.frame <- (end.frame - 28)
  bugpos <- data.frame() 
  if (identical(j, 2)) {
    bugpos_past <- bugpos_save1
  }
  else if (identical(j, 3)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2)
  }
  else if (identical(j, 4)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3)
  }
  else if (identical(j, 5)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4)
  }
  else if (identical(j, 6)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4,
                         bugpos_save5)
  }
  else if (j > 6) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4,
                         bugpos_save5, bugpos_save6)
  }
  for (l in begin.frame:end.frame) {
    res <- getFrame(fbvid, l) # extract individual frames
    mask <- blend(res, imask, "*") # mask other petri dishes
    sub <- blend(mbg, res, "-") # subtract background from the mask 
    bw <- thresholding(sub, thres = 50, "binary") # set a threshold difference 
    bugcords <- blobDetector(bw) # detect the white blobs that are created; 
    bugcords <- mutate(bugcords, frame = l, track = NA)
    stout <- simpleTracker(past = bugpos_past, current = bugcords, maxDist = 10) 
    bugpos <- rbind(bugpos, stout)
  }
  if (identical(j, 1)) {
    bugpos_save1 <- bugpos
  }
  else if (identical(j, 2)) {
    bugpos_save2 <- bugpos
  }
  else if (identical(j, 3)) {
    bugpos_save3 <- bugpos
  }
  else if (identical(j, 4)) {
    bugpos_save4 <- bugpos
  }
  else if (identical(j, 5)) {
    bugpos_save5 <- bugpos
  }
  else if (identical(j, 6)) {
    bugpos_save6 <- bugpos
  }
  else if (j > 6) {
    bugpos_save1 <- bugpos_save2
    bugpos_save2 <- bugpos_save3
    bugpos_save3 <- bugpos_save4
    bugpos_save4 <- bugpos_save5
    bugpos_save5 <- bugpos_save6
    bugpos_save6 <- bugpos
  }
  my.list4[[j]] <- bugpos
  toc(log = FALSE, quiet = FALSE, func.toc = toc.outmsg)
}
my.df4 <- do.call('rbind', my.list4)

# 5/6
my.list5 <- vector('list', 1000)
my.df5 <- data.frame()
bugpos_past <- data.frame()
bugpos_save1 <- data.frame()
bugpos_save2 <- data.frame()
bugpos_save3 <- data.frame()
bugpos_save4 <- data.frame()
bugpos_save5 <- data.frame()
bugpos_save6 <- data.frame()
for (j in 1218:1601) {
  tic(msg = NULL, quiet = TRUE, func.tic = NULL)
  end.frame <- (j * 29)
  begin.frame <- (end.frame - 28)
  bugpos <- data.frame() 
  if (identical(j, 2)) {
    bugpos_past <- bugpos_save1
  }
  else if (identical(j, 3)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2)
  }
  else if (identical(j, 4)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3)
  }
  else if (identical(j, 5)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4)
  }
  else if (identical(j, 6)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4,
                         bugpos_save5)
  }
  else if (j > 6) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4,
                         bugpos_save5, bugpos_save6)
  }
  for (l in begin.frame:end.frame) {
    res <- getFrame(fbvid, l) # extract individual frames
    mask <- blend(res, imask, "*") # mask other petri dishes
    sub <- blend(mbg, res, "-") # subtract background from the mask 
    bw <- thresholding(sub, thres = 50, "binary") # set a threshold difference 
    bugcords <- blobDetector(bw) # detect the white blobs that are created; 
    bugcords <- mutate(bugcords, frame = l, track = NA)
    stout <- simpleTracker(past = bugpos_past, current = bugcords, maxDist = 20) 
    bugpos <- rbind(bugpos, stout)
  }
  if (identical(j, 1)) {
    bugpos_save1 <- bugpos
  }
  else if (identical(j, 2)) {
    bugpos_save2 <- bugpos
  }
  else if (identical(j, 3)) {
    bugpos_save3 <- bugpos
  }
  else if (identical(j, 4)) {
    bugpos_save4 <- bugpos
  }
  else if (identical(j, 5)) {
    bugpos_save5 <- bugpos
  }
  else if (identical(j, 6)) {
    bugpos_save6 <- bugpos
  }
  else if (j > 6) {
    bugpos_save1 <- bugpos_save2
    bugpos_save2 <- bugpos_save3
    bugpos_save3 <- bugpos_save4
    bugpos_save4 <- bugpos_save5
    bugpos_save5 <- bugpos_save6
    bugpos_save6 <- bugpos
  }
  my.list5[[j]] <- bugpos
  toc(log = FALSE, quiet = FALSE, func.toc = toc.outmsg)
}
my.df5 <- do.call('rbind', my.list5)

# 6/6
my.list6 <- vector('list', 1000)
my.df6 <- data.frame()
bugpos_past <- data.frame()
bugpos_save1 <- data.frame()
bugpos_save2 <- data.frame()
bugpos_save3 <- data.frame()
bugpos_save4 <- data.frame()
bugpos_save5 <- data.frame()
bugpos_save6 <- data.frame()
for (j in 1601:1913) {
  tic(msg = NULL, quiet = TRUE, func.tic = NULL)
  end.frame <- (j * 29)
  begin.frame <- (end.frame - 28)
  bugpos <- data.frame() 
  if (identical(j, 2)) {
    bugpos_past <- bugpos_save1
  }
  else if (identical(j, 3)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2)
  }
  else if (identical(j, 4)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3)
  }
  else if (identical(j, 5)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4)
  }
  else if (identical(j, 6)) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4,
                         bugpos_save5)
  }
  else if (j > 6) {
    bugpos_past <- rbind(bugpos_save1, bugpos_save2, bugpos_save3, bugpos_save4,
                         bugpos_save5, bugpos_save6)
  }
  for (l in begin.frame:end.frame) {
    res <- getFrame(fbvid, l) # extract individual frames
    mask <- blend(res, imask, "*") # mask other petri dishes
    sub <- blend(mbg, res, "-") # subtract background from the mask 
    bw <- thresholding(sub, thres = 50, "binary") # set a threshold difference 
    bugcords <- blobDetector(bw) # detect the white blobs that are created; 
    bugcords <- mutate(bugcords, frame = l, track = NA)
    stout <- simpleTracker(past = bugpos_past, current = bugcords, maxDist = 20) 
    bugpos <- rbind(bugpos, stout)
  }
  if (identical(j, 1)) {
    bugpos_save1 <- bugpos
  }
  else if (identical(j, 2)) {
    bugpos_save2 <- bugpos
  }
  else if (identical(j, 3)) {
    bugpos_save3 <- bugpos
  }
  else if (identical(j, 4)) {
    bugpos_save4 <- bugpos
  }
  else if (identical(j, 5)) {
    bugpos_save5 <- bugpos
  }
  else if (identical(j, 6)) {
    bugpos_save6 <- bugpos
  }
  else if (j > 6) {
    bugpos_save1 <- bugpos_save2
    bugpos_save2 <- bugpos_save3
    bugpos_save3 <- bugpos_save4
    bugpos_save4 <- bugpos_save5
    bugpos_save5 <- bugpos_save6
    bugpos_save6 <- bugpos
  }
  my.list6[[j]] <- bugpos
  toc(log = FALSE, quiet = FALSE, func.toc = toc.outmsg)
}
my.df6 <- do.call('rbind', my.list6)

# Show lines
plot.new()
imshow(fr.filter.vid)
for (i in 1:1000){
  insect <- which(filter.tracks$track == i)
  lines(x = c(filter.tracks$x[insect]), y = c(filter.tracks$y[insect]), col = i) 
}

plot.new()
imshow(fr.chiri.vid)
for (i in 1:1000){
  insect <- which(trial2.chiri$track == i)
  lines(x = c(trial2.chiri$x[insect]), y = c(trial2.chiri$y[insect]), col = i) 
}

write.csv(my.df.final, "fullviddata.csv")


