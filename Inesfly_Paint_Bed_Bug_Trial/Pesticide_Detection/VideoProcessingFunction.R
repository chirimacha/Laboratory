function(video, maskdimtab, linecoordstab, threshold, maxDist){
  #create blank output table
  bugpos1<- data.frame()
  bugpos2<- data.frame()
  bugpos3<- data.frame()
  bugpos4<- data.frame()
  bugpos5<- data.frame()
  bugpos6<- data.frame()
  
  #create the background
  bg <- backgrounder(video, n = 1800, method = "mean", color = FALSE)
  #create black masks to isolate each petridish
  mat1 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  mat2 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  mat3 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  mat4 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  mat5 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  mat6 <- matrix(0, nrow = bg$dim[1], ncol = bg$dim[2])
  #create whole for each petridish in each mask
  #sadly, for each dish we need to define the area by hand.
  mat1[maskdimtab$top[1]:maskdimtab$bottom[1], maskdimtab$right[1]:maskdimtab$left[1]] <- 1
  mat2[maskdimtab$top[2]:maskdimtab$bottom[2], maskdimtab$right[2]:maskdimtab$left[2]] <- 1
  mat3[maskdimtab$top[3]:maskdimtab$bottom[3], maskdimtab$right[3]:maskdimtab$left[3]] <- 1
  mat4[maskdimtab$top[4]:maskdimtab$bottom[4], maskdimtab$right[4]:maskdimtab$left[4]] <- 1
  mat5[maskdimtab$top[5]:maskdimtab$bottom[5], maskdimtab$right[5]:maskdimtab$left[5]] <- 1
  mat6[maskdimtab$top[6]:maskdimtab$bottom[6], maskdimtab$right[6]:maskdimtab$left[6]] <- 1

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
  
  Coords<-functio(video, pmask, nbga, threshold, maxDist){
    for (i in 1:1800){
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
          stout<-simpleTracker(past = bugpos, current = bugcords, maxDist = maxDist) 
      #combine tables previous in the loop.
          bugpos<- rbind(bugpos, stout)
          }
      }
    return(bugpos)   
   }
  
#Now run this subfunction over the 6 dishes  
pdt1 <-Coords(video, pmaska, nbga1, threshold==50, maxDist==1000)
pdt2 <-Coords(video, pmaskb, nbga2, threshold==50, maxDist==1000)
pdt3 <-Coords(video, pmaskc, nbga3, threshold==50, maxDist==1000)
pdt4 <-Coords(video, pmaskd, nbga4, threshold==50, maxDist==1000)
pdt5 <-Coords(video, pmaske, nbga5, threshold==50, maxDist==1000)
pdt6 <-Coords(video, pmaskf, nbga6, threshold==50, maxDist==1000)

#Now find lines and designate quadrants.



}