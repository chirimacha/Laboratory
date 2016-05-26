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

###############################################################################
#March Bug Pilot Analysis works for 1 quadrant.  
#Uncomment and Run this code to 327 to see it work.

# #create the background
# mbg <- backgrounder(marchpilot, n = 100, method = "mean", color = FALSE)
# #create a blank data frame for loop output
# marbugpos <- data.frame()
# 
# ###Each quadrant will have to build upon
# #Quadrant 2
# mmat <- matrix(0, nrow = mbg$dim[1], ncol = mbg$dim[2])
# #sadly, for each dish we need to define the area by hand.
# mmat[150:268, 370:480] <- 1
# #go through matrix and ask if it is in or out of the polygon
# pmaskm <- (r2img(mmat))
# #now bring the mask and the background together
# nbgm<-blend(mbg, pmaskm, "*")
# imshow(nbgm)
# 
# ###############################################################################
# #Determine the coordinates for matrix and lines
# 
# #only want to use 1 hour. 1 hour is 1frame/sec*60 sec*60min=3600
# for (i in 1:marchpilot$length){
#   #extract individual frames
#   res<-getFrame(marchpilot, i) 
#   #put frame into grey scale.
#   gryscl <- ddd2d(res) 
#   #mask other petri dishes
#   mask<-blend(gryscl, pmaskm, "*")
#   #subtract background from the mask. Only movement will show 
#   sub<-blend(nbgm, mask, "-") 
#   #set a threshold difference to remove changes due to glare/reflection
#   bw<-thresholding(sub, 50, "binary")
#   #detect the black blobs that are created. Get coordinates
#   bugcords<-blobDetector(bw) 
#   # add track # to data frame
#   if(nrow(bugcords)>0) {
#    bugcords<-mutate(bugcords, frame = i, track = NA) 
#     #determines what points are linked. Optimally each insect given 1 track each
#     #because there is only one object, we can max out maxDist. 
#     stout<-simpleTracker(past = marbugpos, current = bugcords, maxDist=1000) 
#     #combine tables previous in the loop.
#    marbugpos<- rbind(marbugpos, stout)
#   }
#   #do we want to add an else value? idk if it will mess up simpleTracker
# }
# 
# #Now that we have the coordinates
# ###Now define the border between control and pesticide
# imshow(nbgm)
# ya<-c(224,320)
# xa<-c(423,421)  
# lines(xa,ya, col= "red",lwd = 1)
# 
# # #In the future I will need to do the same thing in x chord.
#  yb<-c(272,272) #96
#  xb<-c(375,471)  
#  lines(xb,yb, col= "red",lwd = 1)
# 
# #generate line equation
# line1a<-lm(ya~xa)
# line1b<-lm(yb~xb)
# 
# newsa<-data.frame(xa = marbugpos$x)
# newsb<-data.frame(xb = marbugpos$x)
# marbugpos$pred1 <- predict(line1a, newsa, na.rm=TRUE)
# marbugpos$pred2 <- predict(line1b, newsb, na.rm=TRUE)
# 
# #determine if y of bug is above or below line (differnet from predicted y)
# belowa<-which(marbugpos$y<marbugpos$pred1)
# abovea<-which(marbugpos$y>=marbugpos$pred1)
# marbugpos$yse1[above]<-1
# marbugpos$ysem[below]<-0
# 
# belowb<-which(marbugpos$y<marbugpos$pred2)
# aboveb<-which(marbugpos$y>=marbugpos$pred2)
# marbugpos$xsem[above]<-1
# marbugpos$xsem[below]<-0
# 
# #bind for quadrant specs
# marbugpos$quad[intersect(abovea,aboveb)]<-1
# marbugpos$quad[intersect(abovea,belowb)]<-2
# marbugpos$quad[intersect(belowa,aboveb)]<-3
# marbugpos$quad[intersect(belowa,belowb)]<-4
# 
# 
# #For this example let say quadrants 1 and 4 have pesticide.
# marbugpos$onpest[which(marbugpos$quad==1)]<-1
# marbugpos$onpest[which(marbugpos$quad==2)]<-0
# marbugpos$onpest[which(marbugpos$quad==3)]<-0
# marbugpos$onpest[which(marbugpos$quad==4)]<-1
# 
# # #save output as a csv
# # write.csv(marbugpos, "marchpilot_controldata.csv")
# # 
# # #plot the tracks
# # pdf("marchpilot_controltrackplot.pdf")
# # imshow(mbg)
# # for(i in 1:length(marbugpos$track)){
# #   lines(x=marbugpos$x[which(marbugpos$track==i)], y=marbugpos$y[which(marbugpos$track==i)], col=i)
# # }
# # dev.off()
# 



#     #determine if bug is above or below line (differnet from predicted y)
#     belowa<-which((bugpos$y) <  (bugpos$pred1))
#     abovea<-which((bugpos$y) >= (bugpos$pred1))
#     belowb<-which((bugpos$y) <  (bugpos$pred2))
#     aboveb<-which((bugpos$y) >= (bugpos$pred2))
#     which()
# # Determine Quadrants #change depending on slope of verticle line
#     if((coordtab$TPX[tn]) > (coordtab$BPX[tn])) {
#       bugpos$quad[intersect(belowa,aboveb)]<-1
#       bugpos$quad[intersect(abovea,aboveb)]<-4
#       bugpos$quad[intersect(belowa,belowb)]<-2
#       bugpos$quad[intersect(abovea,belowb)]<-3
#     } else {
#       bugpos$quad[intersect(abovea,aboveb)]<-1
#       bugpos$quad[intersect(belowa,aboveb)]<-4
#       bugpos$quad[intersect(abovea,belowb)]<-2
#       bugpos$quad[intersect(belowa,belowb)]<-3
#     }

################################################################################
# messing around with for loops
repetition = 2
trial = 6
camera = 2

for (i in 2:repetition) { 
  for (j in 1:trial) {
    for (k in 1:camera) {
      nam <- paste("R", i, "T", j, "C", k, sep = "")
      video_name <- paste("R", i, "T", j, "C", k, ".mp4", sep = "")
      assign(nam, readVid(video_name))
    }
  }
}

################################################################################
# old background stuff
# Repetition 1
# Trial 1
bgR1T1C1 <- backgrounder(R1T1C1, n = 1600, method = "mean", color = FALSE)
bgR1T1C2 <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# Trial 2
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# Trial 3
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# Trial 4
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# Trial 5
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# Trial 6
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)
# <- backgrounder(R1T1C2, n = 1600, method = "mean", color = FALSE)

# Repetition 2
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
# # Trial 5
# bgiRB <- backgrounder(R2T5C1, n = 1800, method = "mean", color = FALSE)
# bgjRB <- backgrounder(R2T5C2, n = 1800, method = "mean", color = FALSE)
# # Trial 6
# bgkRB <- backgrounder(R2T6C1, n = 1800, method = "mean", color = FALSE)
# bglRB <- backgrounder(R2T6C2, n = 1800, method = "mean", color = FALSE)

################################################################################
# Simple Tracker (package not available for new R)
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

################################################################################
#  See what Simon Garnier's loop is doing by taking only 1 frame
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

################################################################################
# Old Frame stuff
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

################################################################################
# Old matrix stuff
for (i in 1:6) {
  temp_mat <- paste("mat", i, sep = "")
  assign(temp_mat, matrix(0, nrow = bg$dim[1], ncol = bg$dim[2]))
}
mat1 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
mat2 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
mat3 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
mat4 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
mat5 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
mat6 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])

for (i in 1:6) {
  matrix <- get(paste("mat", i))
  matrix[((bg$dim[1])-coordtab$MYT[i]):((bg$dim[1])-coordtab$MYB[i]),
         coordtab$MXL[i]:coordtab$MXR[i]] <- 1
}
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
for (i in 1:6) {
  pmaska <- r2img(get(paste("mat", i)))
}

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

################################################################################
for (i in 1:6) {
  temp_pdt <- paste("pdt", i, sep = "")
  assign(temp_pdt, Coords(video, imask=get(paste("imask", i, sep = "")),
                          maskBG=get(paste("maskBG", i, sep = "")), 
                          coordtaba = coordtab, tn = i, 
                          threshold = thresholda, maxDista = maxDistb))
}
# Old coords stuff
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

# Debugged on May 25, 2016 by going through and seeing which loop did not work
# The loop that did not work was the one reassigning the matrix index values.
# For some reason the old pesticide video processing does not work though.
# Original problem was something with simpleTracker. The original error message
# from OLD pesticide video processing was:
# Error in clue::solve_LSAP(mat) : x must not have more rows than columns. 
6 stop("x must not have more rows than columns.") 
5 clue::solve_LSAP(mat) 
4 as.vector(clue::solve_LSAP(mat)) 
3 simpleTracker(past = bugpos, current = bugcords, maxDist = maxDista) 
2 Coords(video, pmaskc, nbga3, coordtaba = coordtab, tn = 3, threshold = thresholda, 
         maxDista = maxDistb) 
1 VidAnalysis(video = vidR2T1C1, bg = bgR2T1C1, coordtab = CoTbR2T1C1, 
              thresholda = 25, maxDistb = 1000, cam = 1, rep = 2, trial = 1) 

# NEW pesticide video processing error was:
5 stop(sprintf(ngettext(N, "replacement has %d row, data has %d", 
                        "replacement has %d rows, data has %d"), N, nrows), domain = NA) 
4 `$<-.data.frame`(`*tmp*`, "pred1", value = structure(c(284.999999999997, 
                                                         451.000000000001), .Names = c("1", "2"))) 
3 `$<-`(`*tmp*`, "pred1", value = structure(c(284.999999999997, 
                                              451.000000000001), .Names = c("1", "2"))) 
2 Coords(video, imask1, maskBG1, coordtaba = coordtab, tn = 1, 
         threshold = thresholda, maxDista = maxDistb) 
1 VidAnalysis(video = vidR2T1C1, bg = bgR2T1C1, coordtab = CoTbR2T1C1, 
              thresholda = 25, maxDistb = 1000, cam = 1, rep = 2, trial = 1) 
