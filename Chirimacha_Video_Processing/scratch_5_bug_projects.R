## 1) Old data collection of tracks from videos
## Data collection from video
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


## 2) Stitching attempt
# 4
plot.new()
imshow(chiri.bg)
for (i in 4:4){
  insect <- which(chiri.tracks$track == i)
  lines(x = c(chiri.tracks$x[insect]), y = c(chiri.tracks$y[insect]), col = i) 
}

# 2, 5, 8, 11
# plot.new()
# imshow(chiri.bg)
for (i in 2:2){
  insect <- which(chiri.tracks$track == i)
  lines(x = c(chiri.tracks$x[insect]), y = c(chiri.tracks$y[insect]), col = i) 
}
for (i in 5:5){
  insect <- which(chiri.tracks$track == i)
  lines(x = c(chiri.tracks$x[insect]), y = c(chiri.tracks$y[insect]), col = "red") 
}
for (i in 8:8){
  insect <- which(chiri.tracks$track == i)
  lines(x = c(chiri.tracks$x[insect]), y = c(chiri.tracks$y[insect]), col = "red") 
}
for (i in 11:11){
  insect <- which(chiri.tracks$track == i)
  lines(x = c(chiri.tracks$x[insect]), y = c(chiri.tracks$y[insect]), col = "red") 
}

# 3, 6, 10
# plot.new()
# imshow(chiri.bg)
for (i in 3:3){
  insect <- which(chiri.tracks$track == i)
  lines(x = c(chiri.tracks$x[insect]), y = c(chiri.tracks$y[insect]), col = "green") 
}
for (i in 6:6){
  insect <- which(chiri.tracks$track == i)
  lines(x = c(chiri.tracks$x[insect]), y = c(chiri.tracks$y[insect]), col = "green") 
}
for (i in 10:10){
  insect <- which(chiri.tracks$track == i)
  lines(x = c(chiri.tracks$x[insect]), y = c(chiri.tracks$y[insect]), col = "green") 
}
for (i in 11:11){
  insect <- which(chiri.tracks$track == i)
  lines(x = c(chiri.tracks$x[insect]), y = c(chiri.tracks$y[insect]), col = "green") 
}

# 1, 7, 9
# plot.new()
# imshow(chiri.bg)
for (i in 1:1){
  insect <- which(chiri.tracks$track == i)
  lines(x = c(chiri.tracks$x[insect]), y = c(chiri.tracks$y[insect]), col = i) 
}
for (i in 7:7){
  insect <- which(chiri.tracks$track == i)
  lines(x = c(chiri.tracks$x[insect]), y = c(chiri.tracks$y[insect]), col = "black") 
}
for (i in 9:9){
  insect <- which(chiri.tracks$track == i)
  lines(x = c(chiri.tracks$x[insect]), y = c(chiri.tracks$y[insect]), col = "black") 
}