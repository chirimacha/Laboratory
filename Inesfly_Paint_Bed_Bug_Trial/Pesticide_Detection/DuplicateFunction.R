#Scratch for Duplicate Correction

twomany<- which(CompVidRep4$id == 2)
firstfr<- which(CompVidRep4$frame == 1)
intersect(twomany, firstfr)


DupCorrect<- function(df){
  df$delete <- df$x*0
  #m.id <- max(df$id)
  dups <- which(df$id > 1)
  twos <- which(df$id == 2)
  d.fr <- df$frame[dups]
  d.in <- df$frame[dups]
  ud.fr <- unique(d.fr)
  first.frs <- which(ud.fr == 1)
  ud.fr<-ud.fr[-first.frs]
  for(i in 1:length(ud.fr)){
    mal.fr <-which(df$frame == ud.fr[i])
    mtwo.fr<- intersect(mal.fr, twos)
    for(j in 1:length(mtwo.fr)){
      jid<- which(df$insect.id == df$insect.id[mtwo.fr[j]] )
      jf<- which(df$frame == df$frame[mtwo.fr[j]] )
      less.fr <- which(df$frame < df$frame[mtwo.fr[j]])
      less.ifr <- intersect(less.fr, jid)
      rev.p.obv <- which(df$frame == max(df$frame[less.ifr]))
      rev.obvs <- intersect(jid, jf)
      px <- df$x[rev.p.obv]
      py <- df$y[rev.p.obv]
      dif.output <- rev.obvs*0
      for(k in 1:length(rev.obvs)){
        kx <- df$x[rev.obvs[k]]
        ky <- df$y[rev.obvs[k]]
        dif.output[k]<- sqrt((kx-px)^2+(ky-py)^2)
      }
      maxi<- which(dif.output == max(dif.output))
      del.ob<- which(dif.output != max(dif.output))
      df$id[rev.obvs[maxi]] <- 1
      df$delete[rev.obvs[del.ob]] <- 1
    }
  }
  duplicates<- which(df$delete == 1)
  df <- df[-duplicates,]
}

TestCVR3<- DupCorrect(CompVidRep3)

