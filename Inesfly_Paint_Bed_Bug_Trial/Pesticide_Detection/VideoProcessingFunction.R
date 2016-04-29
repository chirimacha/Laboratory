VidAnalysis<-function(video, coordtab, thresholda, maxDistb){
  #create the background
  #bg <- backgrounder(video, n = 1800, method = "mean", color = FALSE)
  
  #create black masks to isolate each petridish
  mat1 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  mat2 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  mat3 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  mat4 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  mat5 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  mat6 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  #create whole for each petridish in each mask
  #sadly, for each dish we need to define the area by hand.
  #mat works left to right, but top to bottom.  
  #Graphing works bottom to top so we need correction
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

  #Make Mask Matrix into an image
  pmaska <- (r2img(mat1))
  pmaskb <- (r2img(mat2))
  pmaskc <- (r2img(mat3))
  pmaskd <- (r2img(mat4))
  pmaske <- (r2img(mat5))
  pmaskf <- (r2img(mat6))
  
  #now bring the mask and the background together
  nbga1<-blend(bg, pmaska, "*")
  nbga2<-blend(bg, pmaskb, "*")
  nbga3<-blend(bg, pmaskc, "*")
  nbga4<-blend(bg, pmaskd, "*")
  nbga5<-blend(bg, pmaske, "*")
  nbga6<-blend(bg, pmaskf, "*")
  
  #Create Function that finds the coordinate of the insect in each quadrant in each frame
  Coords<-function(video, pmask, nbga, coordtaba, tn, threshold, maxDista){
    #determine loop length
    if (video$length<1800) {
      fr <- video$length
      } else {
        fr<-1800
      }
    bugpos <- NULL
    bugpos<-data.frame()
    for (i in 1:fr){
     #extract individual frames
     res<-getFrame(video, i) 
     #put frame into grey scale.
     gryscl <- ddd2d(res) 
     #mask other petri dishes
     mask<-blend(gryscl, pmask, "*")
     #subtract background from the mask. Only movement will show 
     sub<-blend(nbga, mask, "-") 
     #set a threshold difference to remove changes due to glare/reflection
     bw<-thresholding(sub, threshold, "binary")
     #detect the black blobs that are created. Get coordinates
     bugcords<-blobDetector(bw) 
     # add track # to data frame
        if(nrow(bugcords)>0) {
          bugcords<-mutate(bugcords, frame = i, track = NA) 
      #determines what points are linked. Optimally each insect given 1 track each
      #because there is only one object, we can max out maxDist. 
          stout<-simpleTracker(past = bugpos, current = bugcords, maxDist = maxDista) 
      #combine tables previous in the loop.
          bugpos<- rbind(bugpos, stout)
          }
      }
    ya<-c(coordtaba$BPY[tn],coordtaba$TPY[tn])
    xa<-c(coordtaba$BPX[tn],coordtaba$TPX[tn])  
    
    # #In the future I will need to do the same thing in x chord.
    yb<-c(coordtaba$LPY[tn],coordtab$RPY[tn]) #96
    xb<-c(coordtaba$LPX[tn],coordtab$RPX[tn])  
    
    #generate line equation
    line1a<-lm(ya~xa)
    line1b<-lm(yb~xb)
    
    newsa<-data.frame(xa = bugpos$x)
    newsb<-data.frame(xb = bugpos$x)
    bugpos$pred1 <- predict(line1a, newsa, na.rm=TRUE)
    bugpos$pred2 <- predict(line1b, newsb, na.rm=TRUE)
    
    #determine if y of bug is above or below line (differnet from predicted y)
    belowa<-which(bugpos$y<bugpos$pred1)
    abovea<-which(bugpos$y>=bugpos$pred1)
    belowb<-which(bugpos$y<bugpos$pred2)
    aboveb<-which(bugpos$y>=bugpos$pred2)
    
    #Determine Quadrants #change depending on slope of verticle line
    if(coordtab$TPX[tn]>coordtab$BPX[tn]) {
      bugpos$quad[intersect(belowa,aboveb)]<-1
      bugpos$quad[intersect(abovea,aboveb)]<-4
      bugpos$quad[intersect(belowa,belowb)]<-2
      bugpos$quad[intersect(abovea,belowb)]<-3
    } else {
        bugpos$quad[intersect(abovea,aboveb)]<-1
        bugpos$quad[intersect(belowa,aboveb)]<-4
        bugpos$quad[intersect(abovea,belowb)]<-2
        bugpos$quad[intersect(belowa,belowb)]<-3
    }

    bugpos$trayn<-tn
    return(bugpos)   
   }
  
#Now run this subfunction over the 6 dishes  
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

#Bind All the tables
MasterTab<-rbind(pdt1, pdt2, pdt3, pdt4, pdt5, pdt6)
#Output as single data table
return(MasterTab)
}