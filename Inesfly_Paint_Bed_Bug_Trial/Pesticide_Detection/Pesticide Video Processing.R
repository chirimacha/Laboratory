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
#Open Libraries
library(videoplayR)
library(dplyr)
library(clue)
library(shiny)
library(splancs)
library(grid)

##Simple Tracker (package not available for new R)
##Skip to line  163--figure out how to source this code to save space
pdiff <- function(a, b) {
  nr <- length(a)
  nc <- length(b)
  
  ma <- matrix(a, nrow = nr, ncol = nc)
  mb <- matrix(b, nrow = nr, ncol = nc, byrow = TRUE)
  
  ma - mb
}

# declare variables in the function name
# what are current and past? defined elsewhere?
simpleTracker <- function(current, past, lookBack = 30, maxDist = 10) { #assign lookBack and maxDist w/ immutable numbers?
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
                       alpha = numeric(n), major = numeric(n), minor = numeric(n),
                       area = numeric(n), frame = numeric(n) - 2 * lookBack, track = numeric(n))
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
    fps <- (i - oldFrame + 1) / as.numeric(difftime(newTime, oldTime, unit = "secs"))
    old <- new
    oldFrame <- i
    oldTime <- newTime
    pb$set(value = old / 100, detail = paste0(old, "% - ", round(fps, digits = 2), "fps"))
  }
}
  }

tracks[1:pos, ]
}

###Set Working Directory
#lab computer
setwd("/Users/mzlevy/Laboratory/Inesfly_Paint_Bed_Bug_Trial/Pesticide_Detection")
#Justin's Computer
#setwd("")

#bring in video(s)
marchpilot <- readVid("MarchPilot.mp4")
#Import Videos for First Repetition
##Repetition 1 (Recorded on "2016-04-21")
#Trial One
R1T1C1<- readVid("Trial1Cam1.mp4")
R1T1C2<- readVid("Trial1Cam2.mp4")
#Trial Two
R1T2C1<- readVid("Trial2Cam1.mp4")
R1T2C2<- readVid("Trial2Cam2.mp4")
#Trial Three
R1T3C1<- readVid("Trial3Cam1.mp4")
R1T3C2<- readVid("Trial3Cam2.mp4")
#Trial Four
R1T4C1<- readVid("Trial4Cam1.mp4")
R1T4C2<- readVid("Trial4Cam2.mp4")
#Trial Five
R1T5C1<- readVid("Trial5Cam1.mp4")
R1T5C2<- readVid("Trial5Cam2.mp4")
#Trial Six
R1T6C1<- readVid("Trial6Cam1.mp4")
R1T6C2<- readVid("Trial6Cam2.mp4")

#Install Data Tables with times, dates, humidity
#and quadrant assignments.
#<- read.csv("")

###############################################################################
###Begin developing code.
#Look at Simon's code from twitter
#############################
# #Simon's code from twitter
# for (i in 1:vid$length){
#   res<-getFrame(vid, i) %>%
#     ddd2d() %>%
#   blend(mask, "*") %>%
#   blend(bg, ., "-") %>%
#     thresholding(30, "binary") %>%
#     blobDetector() %>%
#   dplyr::mutate(frame = i, track = NA) %>%
#   simpleTracker(past = res, lookBack= 60, maxDist= 10) %>%
#   rbind(res, .)
# }


##see what Simon Garnier's loop is doing by taking only 1 frame
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
#create the background
mbg <- backgrounder(marchpilot, n = 100, method = "mean", color = FALSE)
#create a blank data frame for loop output
marbugpos <- data.frame()

###Each quadrant will have to build upon
#Quadrant 2
mmat <- matrix(0, nrow = mbg$dim[1], ncol = mbg$dim[2])
#sadly, for each dish we need to define the area by hand.
mmat[150:268, 370:480] <- 1
#go through matrix and ask if it is in or out of the polygon
pmaskm <- (r2img(mmat))
#now bring the mask and the background together
nbgm<-blend(mbg, pmaskm, "*")
imshow(nbgm)

###############################################################################
#Substring
removeRight <- function(x, y, n){
  substr(x, y, nchar(x)-n)
}

#Determine the coordinates for matrix and lines
rto <- res$dim[1]/res$dim[2]
quartz(width=6, height=rto*6)
imshow(nbgm)
a<-grid.locator(unit = "npc")
gcx<-as.numeric(a$x)
gcy<-as.numeric(a$y)
X <- ceiling(gcx*nbgm$dim[2])
Y <- ceiling(gcy*nbgm$dim[1])
imshow(nbgm)
points(x=c(X), y=c(Y), col="red", pch=19, cex = 0.1 )
X
Y
#Repeat the above code to find the points and manually enter them below

###Create tables
#Video 1
Tray<-c(1,2,3,4,5,6)
MX1 <-c(,,,,,)
MX2 <-c(,,,,,)
MY1 <-c(,,,,,)
MY2 <-c(,,,,,)
BPX <-c(,,,,,)
BPY <-c(,,,,,)
TPX <-c(,,,,,)
TPY <-c(,,,,,)
RPX <-c(,,,,,)
RPY <-c(,,,,,)
LPX <-c(,,,,,)
LPY <-c(,,,,,)

CoordTabMP<-data.frame(Tray, MX1, MX2, BPX, BPY, TPX, TPY, RPX, RPY, LPX, LPY)

#one that is used to create lines


###############################################################################
#Determine the coordinates for matrix and lines

#only want to use 1 hour. 1 hour is 1frame/sec*60 sec*60min=3600
for (i in 1:marchpilot$length){
  #extract individual frames
  res<-getFrame(marchpilot, i) 
  #put frame into grey scale.
  gryscl <- ddd2d(res) 
  #mask other petri dishes
  mask<-blend(gryscl, pmaskm, "*")
  #subtract background from the mask. Only movement will show 
  sub<-blend(nbgm, mask, "-") 
  #set a threshold difference to remove changes due to glare/reflection
  bw<-thresholding(sub, 50, "binary")
  #detect the black blobs that are created. Get coordinates
  bugcords<-blobDetector(bw) 
  # add track # to data frame
  if(nrow(bugcords)>0) {
   bugcords<-mutate(bugcords, frame = i, track = NA) 
    #determines what points are linked. Optimally each insect given 1 track each
    #because there is only one object, we can max out maxDist. 
    stout<-simpleTracker(past = marbugpos, current = bugcords, maxDist=1000) 
    #combine tables previous in the loop.
   marbugpos<- rbind(marbugpos, stout)
  }
  #do we want to add an else value? idk if it will mess up simpleTracker
}

#Now that we have the coordinates
###Now define the border between control and pesticide
imshow(nbgm)
ya<-c(224,320)
xa<-c(423,421)  
lines(xa,ya, col= "red",lwd = 1)

# #In the future I will need to do the same thing in x chord.
 yb<-c(272,272) #96
 xb<-c(375,471)  
 lines(xb,yb, col= "red",lwd = 1)

#generate line equation
line1a<-lm(ya~xa)
line1b<-lm(yb~xb)

newsa<-data.frame(xa = marbugpos$x)
newsb<-data.frame(xb = marbugpos$x)
marbugpos$pred1 <- predict(line1a, newsa, na.rm=TRUE)
marbugpos$pred2 <- predict(line1b, newsb, na.rm=TRUE)

#determine if y of bug is above or below line (differnet from predicted y)
belowa<-which(marbugpos$y<marbugpos$pred1)
abovea<-which(marbugpos$y>=marbugpos$pred1)
marbugpos$yse1[above]<-1
marbugpos$ysem[below]<-0

belowb<-which(marbugpos$y<marbugpos$pred2)
aboveb<-which(marbugpos$y>=marbugpos$pred2)
marbugpos$xsem[above]<-1
marbugpos$xsem[below]<-0

#bind for quadrant specs
marbugpos$quad[intersect(abovea,aboveb)]<-1
marbugpos$quad[intersect(abovea,belowb)]<-2
marbugpos$quad[intersect(belowa,aboveb)]<-3
marbugpos$quad[intersect(belowa,belowb)]<-4


#For this example let say quadrants 1 and 4 have pesticide.
marbugpos$onpest[which(marbugpos$quad==1)]<-1
marbugpos$onpest[which(marbugpos$quad==2)]<-0
marbugpos$onpest[which(marbugpos$quad==3)]<-0
marbugpos$onpest[which(marbugpos$quad==4)]<-1

# #save output as a csv
# write.csv(marbugpos, "marchpilot_controldata.csv")
# 
# #plot the tracks
# pdf("marchpilot_controltrackplot.pdf")
# imshow(mbg)
# for(i in 1:length(marbugpos$track)){
#   lines(x=marbugpos$x[which(marbugpos$track==i)], y=marbugpos$y[which(marbugpos$track==i)], col=i)
# }
# dev.off()

###############################################################################
####====FULL_VIDS==============================================================
###
#Get Frames in order to find coordinates
FR1T1C1 <- getFrame(R1T1C1, 5)
FR1T1C2 <- getFrame(R1T1C2, 5)
FR1T2C1 <- getFrame(R1T2C1, 5)
FR1T2C2 <- getFrame(R1T2C2, 5)
FR1T3C1 <- getFrame(R1T3C1, 5)
FR1T3C2 <- getFrame(R1T3C2, 5)
FR1T4C1 <- getFrame(R1T4C1, 5)
FR1T4C2 <- getFrame(R1T4C2, 5)
FR1T5C1 <- getFrame(R1T5C1, 5)
FR1T5C2 <- getFrame(R1T5C2, 5)
FR1T6C1 <- getFrame(R1T6C1, 5)
FR1T6C2 <- getFrame(R1T6C2, 5)

#Now Identify points and create tables.
getpoint<-function(frame){
  rto <- frame$dim[1]/frame$dim[2]
  quartz(width=6, height=rto*6)
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

tester<-getpoint(FR1T1C2)
#Repeat the above code to find the points and manually enter them below

###Create tables
#Video FR1T1C1
#Note that if X coords for TP and BP are exactly equal, it may create errors.
Tray<-c(1,2,3,4,5,6)
aMXL <-c(,,,,,)
aMXR <-c(,,,,,)
aMYT <-c(,,,,,)
aMYB <-c(,,,,,)
aTPX <-c(,,,,,)
aTPY <-c(,,,,,)
aBPX <-c(,,,,,)
aBPY <-c(,,,,,)
aLPX <-c(,,,,,)
aLPY <-c(,,,,,)
aRPX <-c(,,,,,)
aRPY <-c(,,,,,)
#c(,,,,,)
#create a coordinate table
CoTbR1T1C1<-data.frame(Tray, aMXL, aMXR, aMYT, aMYB, aBPX, aBPY, aTPX, aTPY, 
                       aRPX, aRPY, aLPX, aLPY)
#rename so that colums can be found in function
names(CoTbR1T1C1)<-c(Tray,MXL, MXR, MYT, MYB, BPX, BPY, TPX, TPY, 
                     RPX, RPY, LPX, LPY)

#Video FR1T1C2

Tray<-c(1,2,3,4,5,6)
bMXL <-c(168, 351, 521, 178, 349, 517)
bMXR <-c(351, 521, 703, 349, 517, 703)
bMYT <-c(427, 427, 427, 238, 238, 238)
bMYB <-c(238, 238, 238,  59,  59,  59)
bTPX <-c(258, 434, 608, 267, 439, 596)
bTPY <-c(404, 406, 410, 224, 224, 219)
bBPX <-c(284, 439, 604, 276, 438, 601)
bBPY <-c(254, 255, 252,  87,  82,  77)
bLPX <-c(199, 363, 532, 198, 362, 527)
bLPY <-c(316, 329, 330, 153, 155, 150)
bRPX <-c(338, 511, 685, 337, 515, 678)
bRPY <-c(338, 331, 333, 158, 154, 155)
#c(,,,,,)
#create a coordinate table
CoTbR1T1C2<-data.frame(Tray, aMXL, aMXR, aMYT, aMYB, aBPX, aBPY, aTPX, aTPY, 
                       aRPX, aRPY, aLPX, aLPY)
names(CoTbR1T1C2)<-c("Tray","MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
#Plot out lines to double check

###############################################################################
#run over function
bg <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
VideoDos<-VidAnalysis(video=R1T1C2, coordtab=CoTbR1T1C2, thresholda=50, maxDistb=1000)


ya<-c(0,100)
xa<-c(50,51)  

# #In the future I will need to do the same thing in x chord.
yb<-c(50,50) #96
xb<-c(0,100)  

#generate line equation
line1a<-lm(ya~xa)
line1b<-lm(yb~xb)

newsa<-data.frame(xa = c(40, 40, 60, 60))
newsb<-data.frame(xb = c(40, 40, 60, 60))
pred1 <- predict(line1a, newsa, na.rm=TRUE)
pred2 <- predict(line1b, newsb, na.rm=TRUE)

#determine if y of bug is above or below line (differnet from predicted y)
belowa<-which(bugpos$y<bugpos$pred1)
abovea<-which(bugpos$y>=bugpos$pred1)
belowb<-which(bugpos$y<bugpos$pred2)
aboveb<-which(bugpos$y>=bugpos$pred2)