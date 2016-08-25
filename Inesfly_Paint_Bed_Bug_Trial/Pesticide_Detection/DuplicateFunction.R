#currently VidRep4 is giving 12 warnings error
#j is 0

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
  #duplicates <- which(df$delete == 1)
  #df <- df[-duplicates,]
  return(df)
}


TestCVR3<- DupCorrect(CompVidRep4)

DupCorrect <- function(df){
  df <- CompVidRep4
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
  #duplicates <- which(df$delete == 1)
  #df <- df[-duplicates,]
  return(df)
}

