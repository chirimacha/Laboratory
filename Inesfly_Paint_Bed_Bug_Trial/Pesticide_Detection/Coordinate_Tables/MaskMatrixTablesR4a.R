library(videoplayR)
library(grid)

setwd("/Users/mzlevy/Desktop"")

#Bring in Videos
for (i in 4) { 
  for (j in 1:6) {
    for (k in 1:2) {
      temp_name1 <- paste("vidR", i, "T", j, "C", k, sep = "")
      video_name <- paste("Videos/R", i, "T", j, "C", k, ".mp4", sep = "")
      assign(temp_name1, readVid(video_name))
    }
  }
}

# Lab computer
setwd("/Users/mzlevy/Laboratory/Inesfly_Paint_Bed_Bug_Trial/Pesticide_Detection")

getpoint<-function(frame){
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



visualize<-function(CD, frame){
  imshow(frame)
  for(i in 1:6){
    lines(x = c(CD$MXR[i], CD$MXL[i]), y = c(CD$MYT[i], CD$MYT[i]), col=i) 
    lines(x = c(CD$MXR[i], CD$MXL[i]), y = c(CD$MYB[i], CD$MYB[i]),col=i) 
    lines(x = c(CD$MXR[i], CD$MXR[i]), y = c(CD$MYT[i], CD$MYB[i]),col=i) 
    lines(x = c(CD$MXL[i], CD$MXL[i]), y = c(CD$MYT[i], CD$MYB[i]),col=i) 
    lines(x = c(CD$TPX[i], CD$BPX[i]), y = c(CD$TPY[i], CD$BPY[i]),col=i)
    lines(x = c(CD$LPX[i], CD$RPX[i]), y = c(CD$LPY[i], CD$RPY[i]),col=i)
  }
}

#FRAMES
#Repetition 2 Frame 5
FR4T1C1 <- getFrame(vidR4T1C1, 5)
FR4T1C2 <- getFrame(vidR4T1C2, 5)
FR4T2C1 <- getFrame(vidR4T2C1, 5)
FR4T2C2 <- getFrame(vidR4T2C2, 5)
FR4T3C1 <- getFrame(vidR4T3C1, 5)
FR4T3C2 <- getFrame(vidR4T3C2, 5)
FR4T4C1 <- getFrame(vidR4T4C1, 5)
FR4T4C2 <- getFrame(vidR4T4C2, 5)
FR4T5C1 <- getFrame(vidR4T5C1, 5)
FR4T5C2 <- getFrame(vidR4T5C2, 5)
FR4T6C1 <- getFrame(vidR4T6C1, 5)
FR4T6C2 <- getFrame(vidR4T6C2, 5)

###############################################################################
#USE TESTER FUNCTION TO GET POINTS
tester<-getpoint(FR4T1C1)
tester

###Trial 1 Cam 1
Tray<-c(1,2,3,4,5,6)
Camera <-c (1,1,1,1,1,1)
Trial <-c(1,1,1,1,1,1)
Rep <-c(4,4,4,4,4,4)
cMXL <-c(160,345,525,160,345,525)
cMXR <-c(345,525,712,345,525,712)
cMYT <-c(420,420,420,220,230,230)
cMYB <-c(220,230,230, 030, 048, 048)
cTPX <-c(250,425,602,261,435,616)
cTPY <-c(389,392,400,201,209,225)
cBPX <-c(269,436,608,269,448,615)
cBPY <-c(230,239,243,047,056,065)
cLPX <-c(,,,,,)
cLPY <-c(,,,,,)
cRPX <-c(,,,,,)
cRPY <-c(,,,,,)
#c(,,,,,)

#create a coordinate table
CoTbR4T1C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)

#rename so that colums can be found in function
names(CoTbR4T1C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
#write.csv(CoTbR4T1C1, "Coordinate_Tables/CoTbR4T1C1.csv")

visualize(frame = FR4T1C1, CD=CoTbR4T1C1)

###############################################################################
###Trial 1 Cam 2
tester<-getpoint(FR4T1C2)
tester

Tray<-c(1,2,3,4,5,6)
cMXL <-c(160,345,525,160,345,525)
cMXR <-c(345,525,700,345,525,70)
cMYT <-c(435,435,435,250,250,250)
cMYB <-c(250,250,250, 060, 060, 060)
cTPX <-c(,,,,,)
cTPY <-c(,,,,,)
cBPX <-c(,,,,,)
cBPY <-c(,,,,,)
cLPX <-c(,,,,,)
cLPY <-c(,,,,,)
cRPX <-c(,,,,,)
cRPY <-c(,,,,,)
#c(,,,,,)

#create a coordinate table
CoTbR4T1C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR4T1C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
#write.csv(CoTbR4T1C2, "Coordinate_Tables/CoTbR4T1C2.csv")

visualize(frame = FR4T1C2, CD=CoTbR4T1C2)
###############################################################################
#R4T2C1
#RECORD THE POINTS GATHERED
Tray<-c(1,2,3,4,5,6)
cMXL <-c(160,345,525,160,345,525)
cMXR <-c(345,525,712,345,525,712)
cMYT <-c(420,420,420,220,230,230)
cMYB <-c(220,230,230, 030, 048, 048)
cTPX <-c(,,,,,)
cTPY <-c(,,,,,)
cBPX <-c(,,,,,)
cBPY <-c(,,,,,)
cLPX <-c(,,,,,)
cLPY <-c(,,,,,)
cRPX <-c(,,,,,)
cRPY <-c(,,,,,)
#c(,,,,,)

#create a coordinate table
CoTbR4T2C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR4T2C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
#write.csv(CoTbR4T2C1, "Coordinate_Tables/CoTbR4T2C1.csv")
visualize(frame = FR4T2C1, CD=CoTbR4T2C1)


###############################################################################
tester<-getpoint(FR4T2C2)
tester
###Trial 2 Cam 2
Tray<-c(1,2,3,4,5,6)
cMXL <-c(149,348,540,149,359,536)
cMXR <-c(348,540,734,359,536,735)
cMYT <-c(456,456,456,258,258,258)
cMYB <-c(258,258,258,069,069,069)
cTPX <-c(,,,,,)
cTPY <-c(,,,,,)
cBPX <-c(,,,,,)
cBPY <-c(,,,,,)
cLPX <-c(,,,,,)
cLPY <-c(,,,,,)
cRPX <-c(,,,,,)
cRPY <-c(,,,,,)
#c(,,,,,)

#create a coordinate table
CoTbR4T2C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR4T2C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR4T2C2, "Coordinate_Tables/CoTbR4T2C2.csv")
visualize(frame = FR4T2C2, CD=CoTbR4T2C2)

###############################################################################
###Trial 3 Cam 1
tester<-getpoint(FR4T3C1)
tester

Tray<-c(1,2,3,4,5,6)
cMXL <-c(160,345,525,160,345,525)
cMXR <-c(345,525,712,345,525,712)
cMYT <-c(420,420,420,220,230,230)
cMYB <-c(220,230,230, 030, 048, 048)
cTPX <-c(,,,,,)
cTPY <-c(,,,,,)
cBPX <-c(,,,,,)
cBPY <-c(,,,,,)
cLPX <-c(,,,,,)
cLPY <-c(,,,,,)
cRPX <-c(,,,,,)
cRPY <-c(,,,,,)
#c(,,,,,)

#create a coordinate table
CoTbR4T3C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR4T3C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR4T3C1, "Coordinate_Tables/CoTbR4T3C1.csv")
visualize(frame = FR4T3C1, CD=CoTbR4T3C1)


###############################################################################
###Trial 3 Cam 2
tester<-getpoint(FR4T3C2)
tester

Tray<-c(1,2,3,4,5,6)
cMXL <-c(149,348,540,149,359,536)
cMXR <-c(348,540,734,359,536,720)
cMYT <-c(456,456,456,258,258,258)
cMYB <-c(258,258,258,069,069,069)
cTPX <-c(,,,,,)
cTPY <-c(,,,,,)
cBPX <-c(,,,,,)
cBPY <-c(,,,,,)
cLPX <-c(,,,,,)
cLPY <-c(,,,,,)
cRPX <-c(,,,,,)
cRPY <-c(,,,,,)
#c(,,,,,)

#create a coordinate table
CoTbR4T3C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR4T3C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR4T3C2, "Coordinate_Tables/CoTbR4T3C2.csv")
visualize(frame = FR4T3C2, CD=CoTbR4T3C2)

###############################################################################
###Trial 4 Cam 1
tester<-getpoint(FR4T4C1)
tester

Tray<-c(1,2,3,4,5,6)
cMXL <-c(160,345,525,160,345,525)
cMXR <-c(345,525,712,345,525,712)
cMYT <-c(420,420,420,220,230,230)
cMYB <-c(220,230,230, 030, 048, 048)
cTPX <-c(,,,,,)
cTPY <-c(,,,,,)
cBPX <-c(,,,,,)
cBPY <-c(,,,,,)
cLPX <-c(,,,,,)
cLPY <-c(,,,,,)
cRPX <-c(,,,,,)
cRPY <-c(,,,,,)
#c(,,,,,)

#create a coordinate table
CoTbR4T4C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR4T4C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR4T4C1, "Coordinate_Tables/CoTbR4T4C1.csv")
visualize(frame = FR4T4C1, CD=CoTbR4T4C1)


###############################################################################
###Trial 4 Cam 2
tester<-getpoint(FR4T4C2)
tester

Tray <- c(1,2,3,4,5,6)
Trial <- c(2,2,2,2,2,2)
cMXL <-c(162,354,548,165,359,548)
cMXR <-c(357,548,748,362,548,730)
cMYT <-c(465,465,465,258,258,258)
cMYB <-c(258,258,258,069,069,069)
cTPX <-c(,,,,,)
cTPY <-c(,,,,,)
cBPX <-c(,,,,,)
cBPY <-c(,,,,,)
cLPX <-c(,,,,,)
cLPY <-c(,,,,,)
cRPX <-c(,,,,,)
cRPY <-c(,,,,,)
#c(,,,,,)

#create a coordinate table
CoTbR4T4C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR4T4C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR4T4C2, "Coordinate_Tables/CoTbR4T4C2.csv")
visualize(frame = FR4T4C2, CD=CoTbR4T4C2)


###############################################################################
###Trial 5 Cam 1
tester<-getpoint(FR4T5C1)
tester
Tray<-c(1,2,3,4,5,6)
cMXL <-c(160,345,525,160,345,525)
cMXR <-c(345,525,712,345,525,712)
cMYT <-c(420,420,420,220,230,230)
cMYB <-c(220,230,230, 030, 048, 048)
cTPX <-c(,,,,,)
cTPY <-c(,,,,,)
cBPX <-c(,,,,,)
cBPY <-c(,,,,,)
cLPX <-c(,,,,,)
cLPY <-c(,,,,,)
cRPX <-c(,,,,,)
cRPY <-c(,,,,,)
#c(,,,,,)

#create a coordinate table
CoTbR4T5C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR4T5C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR4T5C1, "Coordinate_Tables/CoTbR4T5C1.csv")
visualize(frame = FR4T5C1, CD=CoTbR4T5C1)


###############################################################################
###Trial 5 Cam 2
tester<-getpoint(FR4T5C2)
tester
Tray<-c(1,2,3,4,5,6)
cMXL <-c(168,360,547,168,363,545)
cMXR <-c(360,547,745,363,545,735)
cMYT <-c(465,465,466,258,258,258)
cMYB <-c(258,258,258,069,069,069)
cTPX <-c(,,,,,)
cTPY <-c(,,,,,)
cBPX <-c(,,,,,)
cBPY <-c(,,,,,)
cLPX <-c(,,,,,)
cLPY <-c(,,,,,)
cRPX <-c(,,,,,)
cRPY <-c(,,,,,)
#c(,,,,,)

#create a coordinate table
CoTbR4T5C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR4T5C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR4T5C2, "Coordinate_Tables/CoTbR4T5C2.csv")
visualize(frame = FR4T5C2, CD=CoTbR4T5C2)

###############################################################################
###Trial 6 Cam 1
tester<-getpoint(FR4T6C1)
tester
Tray<-c(1,2,3,4,5,6)
cMXL <-c(160,345,525,160,345,525)
cMXR <-c(345,525,712,345,525,712)
cMYT <-c(420,420,420,220,230,230)
cMYB <-c(220,230,230, 030, 048, 048)
cTPX <-c(,,,,,)
cTPY <-c(,,,,,)
cBPX <-c(,,,,,)
cBPY <-c(,,,,,)
cLPX <-c(,,,,,)
cLPY <-c(,,,,,)
cRPX <-c(,,,,,)
cRPY <-c(,,,,,)
#c(,,,,,)

#create a coordinate table
CoTbR4T6C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR4T6C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR4T6C1, "Coordinate_Tables/CoTbR4T6C1.csv")
visualize(frame = FR4T6C1, CD=CoTbR4T6C1)

###############################################################################
###Trial 6 Cam 2
tester<-getpoint(FR4T6C2)
tester
Tray<-c(1,2,3,4,5,6)
cMXL <-c(152,348,540,160,359,539)
cMXR <-c(348,540,734,359,539,720)
cMYT <-c(465,465,469,265,265,258)
cMYB <-c(265,265,258,069,069,069)
cTPX <-c(,,,,,)
cTPY <-c(,,,,,)
cBPX <-c(,,,,,)
cBPY <-c(,,,,,)
cLPX <-c(,,,,,)
cLPY <-c(,,,,,)
cRPX <-c(,,,,,)
cRPY <-c(,,,,,)
#c(,,,,,)

#create a coordinate table
CoTbR4T6C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR4T6C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR4T6C2, "Coordinate_Tables/CoTbR4T6C2.csv")
visualize(frame = FR4T6C2, CD=CoTbR4T6C2)

###############################################################################

