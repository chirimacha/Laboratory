#For Tester Function, load "Pesticide Video Processing.R"
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

#FRAMES
#Repetition 2 Frame 5
FR2T1C1 <- getFrame(R2T1C1, 5)
FR2T1C2 <- getFrame(R2T1C2, 5)
FR2T2C1 <- getFrame(R2T2C1, 5)
FR2T2C2 <- getFrame(R2T2C2, 5)
FR2T3C1 <- getFrame(R2T3C1, 5)
FR2T3C2 <- getFrame(R2T3C2, 5)
FR2T4C1 <- getFrame(R2T4C1, 5)
FR2T4C2 <- getFrame(R2T4C2, 5)
FR2T5C1 <- getFrame(R2T5C1, 5)
FR2T5C2 <- getFrame(R2T5C2, 5)
FR2T6C1 <- getFrame(R2T6C1, 5)
FR2T6C2 <- getFrame(R2T6C2, 5)



###############################################################################
#USE TESTER FUNCTION TO GET POINTS
tester<-getpoint(FR2T1C1)
tester

###Trial 1 Cam 1
Tray<-c(1,2,3,4,5,6)
Camerac(1,1,1,1,1,1)
Trial <-c(1,1,1,1,1,1)
Rep <-c(2,2,2,2,2,2)
cMXL <-c(173,374,568,173,374,568)
cMXR <-c(374,568,780,374,568,762)
cMYT <-c(468,468,468,271,271,271)
cMYB <-c(271,271,271, 90, 90, 90)
cTPX <-c(280,471,682,296,467,669)
cTPY <-c(451,440,438,250,251,247)
cBPX <-c(286,468,669,295,471,662)
cBPY <-c(285,277,278,104,104, 99)
cLPX <-c(206,390,597,218,396,592)
cLPY <-c(363,355,359,179,174,179)
cRPX <-c(362,552,755,368,549,746)
cRPY <-c(364,354,355,177,180,176)
#c(,,,,,)

#create a coordinate table
CoTbR2T1C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR2T1C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
#write.csv(CoTbR2T1C1, "CoTbR2T1C1.csv")

visualize(frame = FR2T1C1, CD=CoTbR2T1C1)

###############################################################################
###Trial 1 Cam 2
tester<-getpoint(FR2T1C2)
tester

Tray<-c(1,2,3,4,5,6)
cMXL <-c(149,348,540,149,359,538)
cMXR <-c(348,540,734,359,538,735)
cMYT <-c(456,456,456,258,258,258)
cMYB <-c(258,258,258,069,069,069)
cTPX <-c(259,446,639,269,449,620)
cTPY <-c(445,445,443,244,240,243)
cBPX <-c(259,447,630,272,447,617)
cBPY <-c(278,277,272,091,091,090)
cLPX <-c(176,366,559,183,371,544)
cLPY <-c(363,357,357,169,168,166)
cRPX <-c(334,527,719,344,530,698)
cRPY <-c(358,358,356,170,166,170)
#c(,,,,,)

#create a coordinate table
CoTbR2T1C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR2T1C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
#write.csv(CoTbR2T1C2, "CoTbR2T1C2.csv")

visualize(frame = FR2T1C2, CD=CoTbR2T1C2)
###############################################################################
#R2T2C1
#RECORD THE POINTS GATHERED
Tray<-c(1,2,3,4,5,6)
cMXL <-c(173,374,568,173,374,568)
cMXR <-c(374,568,762,374,568,762)
cMYT <-c(468,468,468,271,271,271)
cMYB <-c(271,271,271,101,101,101)
cTPX <-c(283,471,683,284,467,672)
cTPY <-c(457,455,444,257,260,254)
cBPX <-c(290,468,667,286,463,658)
cBPY <-c(294,287,283,111,109,108)
cLPX <-c(206,391,596,210,391,159)
cLPY <-c(369,367,365,187,190,187)
cRPX <-c(360,549,757,354,542,745)
cRPY <-c(373,368,359,184,185,178)
#c(,,,,,)

#create a coordinate table
CoTbR2T2C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR2T2C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
#write.csv(CoTbR2T2C1, "CoTbR2T2C1.csv")
visualize(frame = FR2T2C1, CD=CoTbR2T2C1)


###############################################################################
tester<-getpoint(FR2T2C2)
tester
###Trial 2 Cam 2
Tray<-c(1,2,3,4,5,6)
cMXL <-c(149,348,540,149,359,536)
cMXR <-c(348,540,734,359,536,735)
cMYT <-c(456,456,456,258,258,258)
cMYB <-c(258,258,258,069,069,069)
cTPX <-c(257,441,631,260,446,619)
cTPY <-c(443,443,445,249,247,248)
cBPX <-c(258,443,625,268,447,613)
cBPY <-c(282,280,281,098,095,097)
cLPX <-c(174,362,552,186,367,542)
cLPY <-c(367,357,362,173,172,176)
cRPX <-c(333,523,711,340,526,694)
cRPY <-c(360,362,365,175,172,172)
#c(,,,,,)

#create a coordinate table
CoTbR2T2C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR2T2C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR2T2C2, "CoTbR2T2C2.csv")
visualize(frame = FR2T2C2, CD=CoTbR2T2C2)

###############################################################################
###Trial 3 Cam 1
tester<-getpoint(FR2T3C1)
tester

Tray<-c(1,2,3,4,5,6)
cMXL <-c(173,374,568,173,374,568)
cMXR <-c(374,568,780,374,568,762)
cMYT <-c(468,468,468,271,271,271)
cMYB <-c(271,271,271,090,090,090)
cTPX <-c(283,474,681,297,463,665)
cTPY <-c(451,456,450,255,258,252)
cBPX <-c(288,465,676,292,470,659)
cBPY <-c(292,293,289,109,109,103)
cLPX <-c(207,392,602,215,388,589)
cLPY <-c(372,377,364,187,183,180)
cRPX <-c(360,552,736,368,545,737)
cRPY <-c(375,369,371,179,187,180)
#c(,,,,,)

#create a coordinate table
CoTbR2T3C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR2T3C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR2T3C1, "CoTbR2T3C1.csv")
visualize(frame = FR2T3C1, CD=CoTbR2T3C1)


###############################################################################
###Trial 3 Cam 2
tester<-getpoint(FR2T3C2)
tester

Tray<-c(1,2,3,4,5,6)
cMXL <-c(149,348,540,149,359,536)
cMXR <-c(348,540,734,359,536,720)
cMYT <-c(456,456,456,258,258,258)
cMYB <-c(258,258,258,069,069,069)
cTPX <-c(247,439,636,258,441,619)
cTPY <-c(444,448,442,245,245,244)
cBPX <-c(264,448,634,274,449,617)
cBPY <-c(286,282,281,098,095,093)
cLPX <-c(174,363,557,189,372,541)
cLPY <-c(361,357,357,169,167,170)
cRPX <-c(329,526,709,341,527,701)
cRPY <-c(371,369,365,178,175,172)
#c(,,,,,)

#create a coordinate table
CoTbR2T3C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR2T3C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR2T3C2, "CoTbR2T3C2.csv")
visualize(frame = FR2T3C2, CD=CoTbR2T3C2)

###############################################################################
###Trial 4 Cam 1
tester<-getpoint(FR2T4C1)
tester

Tray<-c(1,2,3,4,5,6)
cMXL <-c(173,374,568,173,374,568)
cMXR <-c(374,568,780,374,568,762)
cMYT <-c(468,468,468,271,271,271)
cMYB <-c(271,271,271,090,090,090)
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
CoTbR2T4C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR2T4C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR2T4C1, "CoTbR2T4C1.csv")
visualize(frame = FR2T4C1, CD=CoTbR2T4C1)


###############################################################################
###Trial 4 Cam 2
tester<-getpoint(FR2T4C2)
tester

Tray <- c(1,2,3,4,5,6)
Trial <- c(2,2,2,2,2,2)
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
CoTbR2T4C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR2T4C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR2T4C2, "CoTbR2T4C2.csv")
visualize(frame = FR2T4C2, CD=CoTbR2T4C2)


###############################################################################
###Trial 5 Cam 1
tester<-getpoint(FR2T5C1)
tester
Tray<-c(1,2,3,4,5,6)
cMXL <-c(173,374,568,173,374,568)
cMXR <-c(374,568,780,374,568,762)
cMYT <-c(468,468,468,271,271,271)
cMYB <-c(271,271,271,090,090,090)
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
CoTbR2T5C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR2T5C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR2T5C1, "CoTbR2T5C1.csv")
visualize(frame = FR2T5C1, CD=CoTbR2T5C1)


###############################################################################
###Trial 5 Cam 2
tester<-getpoint(FR2T5C2)
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
CoTbR2T5C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR2T5C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR2T5C2, "CoTbR2T5C2.csv")
visualize(frame = FR2T5C2, CD=CoTbR2T5C2)

###############################################################################
###Trial 6 Cam 1
tester<-getpoint(FR2T6C1)
tester
Tray<-c(1,2,3,4,5,6)
cMXL <-c(173,374,568,173,374,568)
cMXR <-c(374,568,780,374,568,762)
cMYT <-c(468,468,468,271,271,271)
cMYB <-c(271,271,271,090,090,090)
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
CoTbR2T6C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR2T6C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR2T6C1, "CoTbR2T6C1.csv")
visualize(frame = FR2T6C1, CD=CoTbR2T6C1)

###############################################################################
###Trial 6 Cam 2
tester<-getpoint(FR2T6C2)
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
CoTbR2T6C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR2T6C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR2T6C2, "CoTbR2T6C2.csv")
visualize(frame = FR2T6C2, CD=CoTbR2T6C2)

###############################################################################