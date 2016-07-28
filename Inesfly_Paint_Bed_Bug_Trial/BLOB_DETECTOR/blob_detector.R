###Code for video tracking
#To determine if bed bugs can detect pesticides.

###Install Packages and open libraries.
#Install VideoPlayR
 if (!require(devtools)) {
   install.packages("devtools")
 }
# 
devtools::install_github("swarm-lab/videoplayR")
# 
# #install Other packages:
  install.packages("dplyr")
  install.packages("clue")
  install.packages("shiny")
  install.packages("splancs")
  install.packages("Rcpp")
  install.packages("RcppArmadillo")
  install.packages("pbapply")
  install.packages("sp")
#Open Libraries
  library(Rcpp)
  library(RcppArmadillo)
  library(pbapply)
  library(videoplayR)
  library(dplyr)
  library(clue)
  library(shiny)
  library(sp)
  library(splancs)
###Set Working Directory
#WD connected to github.

#setwd("/Users/mzlevy/Laboratory/Inesfly_Paint_Bed_Bug_Trial/Pesticide_Detection")
  setwd("/home/gianfranco/Documentos/github/Laboratory/Inesfly_Paint_Bed_Bug_Trial/BLOB_DETECTOR")

#bring in video(s)
  pilotchiri <- readVid("test3.avi")
  
  pilotchiri$length
  framepic <- getFrame(pilotchiri, 200)
  imshow(framepic)

#Simple Tracker Code(package not available for new R)
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

###############################################################################
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

#the code doesn't propperly call the other parts, it also doesn't define
#the background or the masks

#Try to create the background once, this takes too long, especially if median is used.
  bg <- backgrounder(pilotchiri, n = 200, method = "mean", color = FALSE)
  imshow(bg)
  
#Create "mask" that only allows one petri dish to be analyzed at a time
  mat <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
#sadly, for each dish we need to define the area by hand.
  #mat[50:620, 10:480] <- 1 #Al tanteo
  mat[0:622, 25:480] <- 1 #Al tanteo
  
#go through matrix and ask if it is in or out of the polygon
  pmaska <- (r2img(mat))
  imshow(pmaska)
#now bring the mask and the background together
  nbga<-blend(bg, pmaska, "*")
  imshow(nbga)

#get polygon
#mypoly<-getpoly()
#mypoly2<-getpoly()
#inorout  <- matrix( 0, nrow = bg$dim[1], ncol = bg$dim[2] )
#inorout2 <- inorout

#for(i in 1:dim(mat)[1]){
#  for(j in 1:dim(mat)[2]){
#    point<-as.points(i,j)
#    if (inout(point, mypoly)==TRUE) inorout[i,j] <- 1
#  }
#}

# quartz(height=3, width=6)
# 
# for(i in 1:dim(mat)[1]){
#   for(j in 1:dim(mat)[2]){
#     point<-as.points(c(i), c(j))
#     inorout2[(dim(mat)[1]-i+1),j] <- inout(point, mypoly)
#   }
# }

#make the matrix into an image
#quartz()
# pmask <- r2img(mat)
# #now bring the mask and the background together
# nbg<-blend(bg, pmask, "*")
# imshow(nbg)


##see what Simon Garnier's loop is doing by taking only 1 frame
#  rev<-getFrame(pilotvidr1, 5)
#  grscl<-ddd2d(rev)
#  mask<-blend(grscl, pmaska, "*")
#  neg<-blend(nbg, mask, "-") #the order matters
#  #mult<-blend(neg1, neg1, "*")
#  ths<-thresholding(neg, 60, "binary")
#  imshow(ths)
#  bugloc<-blobDetector(ths)
#  bcoutputx<-mutate(bugloc, frame=5, track=NA)
#  stoutx<-simpleTracker(bcoutputx, past=bugpos, maxDist= 10)
#  bugpos<- rbind(bugpos, stoutx)

#create output data frame
bugpos<- data.frame()

#Loop over each frame in the video.
#i<- 200
for (i in 1:pilotchiri$length-1){
  #extract individual frames
    res<-getFrame(pilotchiri, i) 
  #put frame into grey scale.
    gryscl <- ddd2d(res) 
  #mask other petri dishes
    mask<-blend(gryscl, pmaska, "*")
  #subtract background from the mask. Only movement will show 
    sub<-blend(nbga, mask, "-") 
  #set a threshold difference to remove changes due to glare/reflection
    bw<-thresholding(sub, 20, "binary") # Preguntaaaaa: Por que escogio el valor de 70
  #detect the black blobs that are created. Get coordinates
    bugcords<-blobDetector(bw) 
  # add track # to data frame
  if(nrow(bugcords)>0) {
      bugcords<-mutate(bugcords, frame = i, track = NA) 
    #determines what points are linked. Optimally each insect given 1 track each
    #because there is only one object, we can max out maxDist. 
      stout<-simpleTracker(past = bugpos, current = bugcords,maxDist=1000) 
    #combine tables previous in the loop.
      bugpos<- rbind(bugpos, stout)
  }
  #do we want to add an else value? idk if it will mess up simpleTracker
}

pdf("test2.pdf")
imshow(bg)
for(i in 2:2){
  lines(x=bugpos$x[which(bugpos$track==i)], y=bugpos$y[which(bugpos$track==i)], col=i)
}
dev.off()
imshow(bg)
for (i in 1:17) {
  points(x=aux$x[i], y=aux$y[i], col="green")
}
lines(x=aux$x, y=aux$y, col="yellow")

abx <- bugpos$x[which(bugpos$track==2)]
aby <- bugpos$y[which(bugpos$track==2)]
