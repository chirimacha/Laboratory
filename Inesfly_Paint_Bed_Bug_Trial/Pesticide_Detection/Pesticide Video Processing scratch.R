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