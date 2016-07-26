library(videoplayR)

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

#Bring in Videos
for (i in 3) { 
  for (j in 1:6) {
    for (k in 1:2) {
      temp_name1 <- paste("vidR", i, "T", j, "C", k, sep = "")
      video_name <- paste("Videos/R", i, "T", j, "C", k, ".mp4", sep = "")
      assign(temp_name1, readVid(video_name))
    }
  }
}

#FRAMES
#Repetition 2 Frame 5
FR3T1C1 <- getFrame(vidR3T1C1, 5)
FR3T1C2 <- getFrame(vidR3T1C2, 5)
FR3T2C1 <- getFrame(vidR3T2C1, 5)
FR3T2C2 <- getFrame(vidR3T2C2, 5)
FR3T3C1 <- getFrame(vidR3T3C1, 5)
FR3T3C2 <- getFrame(vidR3T3C2, 5)
FR3T4C1 <- getFrame(vidR3T4C1, 5)
FR3T4C2 <- getFrame(vidR3T4C2, 5)
FR3T5C1 <- getFrame(vidR3T5C1, 5)
FR3T5C2 <- getFrame(vidR3T5C2, 5)
FR3T6C1 <- getFrame(vidR3T6C1, 5)
FR3T6C2 <- getFrame(vidR3T6C2, 5)

###############################################################################
#USE TESTER FUNCTION TO GET POINTS
tester<-getpoint(FR3T1C1)
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
CoTbR3T1C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)

#rename so that colums can be found in function
names(CoTbR3T1C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
#write.csv(CoTbR3T1C1, "Coordinate_Tables/CoTbR3T1C1.csv")

visualize(frame = FR3T1C1, CD=CoTbR3T1C1)

###############################################################################
###Trial 1 Cam 2
tester<-getpoint(FR3T1C2)
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
CoTbR3T1C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR3T1C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
#write.csv(CoTbR3T1C2, "Coordinate_Tables/CoTbR3T1C2.csv")

visualize(frame = FR3T1C2, CD=CoTbR3T1C2)
###############################################################################
#R3T2C1
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
CoTbR3T2C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR3T2C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
#write.csv(CoTbR3T2C1, "Coordinate_Tables/CoTbR3T2C1.csv")
visualize(frame = FR3T2C1, CD=CoTbR3T2C1)


###############################################################################
tester<-getpoint(FR3T2C2)
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
CoTbR3T2C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR3T2C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR3T2C2, "Coordinate_Tables/CoTbR3T2C2.csv")
visualize(frame = FR3T2C2, CD=CoTbR3T2C2)

###############################################################################
###Trial 3 Cam 1
tester<-getpoint(FR3T3C1)
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
CoTbR3T3C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR3T3C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR3T3C1, "Coordinate_Tables/CoTbR3T3C1.csv")
visualize(frame = FR3T3C1, CD=CoTbR3T3C1)


###############################################################################
###Trial 3 Cam 2
tester<-getpoint(FR3T3C2)
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
CoTbR3T3C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR3T3C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR3T3C2, "Coordinate_Tables/CoTbR3T3C2.csv")
visualize(frame = FR3T3C2, CD=CoTbR3T3C2)

###############################################################################
###Trial 4 Cam 1
tester<-getpoint(FR3T4C1)
tester

Tray<-c(1,2,3,4,5,6)
cMXL <-c(182,378,582,182,379,568)
cMXR <-c(378,582,793,379,568,781)
cMYT <-c(468,468,468,271,271,271)
cMYB <-c(271,271,271,090,090,090)
cTPX <-c(291,483,695,292,469,676)
cTPY <-c(462,447,446,257,253,255)
cBPX <-c(299,479,679,303,477,667)
cBPY <-c(289,280,283,111,104,104)
cLPX <-c(214,402,605,221,395,592)
cLPY <-c(368,363,364,182,176,180)
cRPX <-c(367,561,766,371,545,749)
cRPY <-c(370,362,363,187,182,180)
#c(,,,,,)

#create a coordinate table
CoTbR3T4C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR3T4C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR3T4C1, "Coordinate_Tables/CoTbR3T4C1.csv")
visualize(frame = FR3T4C1, CD=CoTbR3T4C1)


###############################################################################
###Trial 4 Cam 2
tester<-getpoint(FR3T4C2)
tester

Tray <- c(1,2,3,4,5,6)
Trial <- c(2,2,2,2,2,2)
cMXL <-c(162,354,548,165,359,548)
cMXR <-c(357,548,748,362,548,730)
cMYT <-c(465,465,465,258,258,258)
cMYB <-c(258,258,258,069,069,069)
cTPX <-c(258,452,640,273,456,631)
cTPY <-c(453,450,451,255,252,250)
cBPX <-c(268,452,642,274,455,623)
cBPY <-c(289,283,289,100,98,98)
cLPX <-c(184,373,564,196,376,553)
cLPY <-c(371,366,364,176,177,174)
cRPX <-c(342,531,722,348,533,704)
cRPY <-c(372,364,373,177,175,169)
#c(,,,,,)

#create a coordinate table
CoTbR3T4C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR3T4C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR3T4C2, "Coordinate_Tables/CoTbR3T4C2.csv")
visualize(frame = FR3T4C2, CD=CoTbR3T4C2)


###############################################################################
###Trial 5 Cam 1
tester<-getpoint(FR3T5C1)
tester
Tray<-c(1,2,3,4,5,6)
cMXL <-c(184,385,578,184,385,572)
cMXR <-c(385,578,793,385,572,773)
cMYT <-c(472,468,468,271,271,271)
cMYB <-c(271,271,271,090,090,090)
cTPX <-c(289,482,692,298,476,676)
cTPY <-c(456,447,448,257,263,260)
cBPX <-c(299,478,680,306,480,669)
cBPY <-c(294,286,286,108,115,108)
cLPX <-c(218,404,605,222,403,595)
cLPY <-c(374,368,367,180,187,185)
cRPX <-c(367,557,767,371,552,749)
cRPY <-c(376,368,364,183,191,183)
#c(,,,,,)

#create a coordinate table
CoTbR3T5C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR3T5C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR3T5C1, "Coordinate_Tables/CoTbR3T5C1.csv")
visualize(frame = FR3T5C1, CD=CoTbR3T5C1)


###############################################################################
###Trial 5 Cam 2
tester<-getpoint(FR3T5C2)
tester
Tray<-c(1,2,3,4,5,6)
cMXL <-c(168,360,547,168,363,545)
cMXR <-c(360,547,745,363,545,735)
cMYT <-c(465,465,466,258,258,258)
cMYB <-c(258,258,258,069,069,069)
cTPX <-c(268,447,642,266,455,627)
cTPY <-c(447,445,450,252,251,251)
cBPX <-c(275,454,646,283,459,628)
cBPY <-c(285,286,287,97,99,98)
cLPX <-c(192,373,565,196,377,555)
cLPY <-c(365,362,366,168,173,172)
cRPX <-c(348,529,719,350,533,708)
cRPY <-c(368,368,374,179,175,175)
#c(,,,,,)

#create a coordinate table
CoTbR3T5C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR3T5C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR3T5C2, "Coordinate_Tables/CoTbR3T5C2.csv")
visualize(frame = FR3T5C2, CD=CoTbR3T5C2)

###############################################################################
###Trial 6 Cam 1
tester<-getpoint(FR3T6C1)
tester
Tray<-c(1,2,3,4,5,6)
cMXL <-c(173,374,568,173,374,568)
cMXR <-c(374,568,780,374,568,768)
cMYT <-c(468,468,468,271,271,271)
cMYB <-c(271,271,271,090,090,090)
cTPX <-c(278,472,678,288,473,671)
cTPY <-c(452,454,450,260,260,258)
cBPX <-c(292,471,661,297,466,662)
cBPY <-c(293,293,289,112,112,109)
cLPX <-c(207,392,595,217,395,591)
cLPY <-c(366,372,371,181,188,182)
cRPX <-c(359,551,748,362,541,742)
cRPY <-c(373,370,364,187,180,181)
#c(,,,,,)

#create a coordinate table
CoTbR3T6C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR3T6C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR3T6C1, "Coordinate_Tables/CoTbR3T6C1.csv")
visualize(frame = FR3T6C1, CD=CoTbR3T6C1)

###############################################################################
###Trial 6 Cam 2
tester<-getpoint(FR3T6C2)
tester
Tray<-c(1,2,3,4,5,6)
cMXL <-c(152,348,540,160,359,539)
cMXR <-c(348,540,734,359,539,720)
cMYT <-c(465,465,469,265,265,258)
cMYB <-c(265,265,258,069,069,069)
cTPX <-c(245,439,637,264,447,623)
cTPY <-c(444,448,452,252,254,252)
cBPX <-c(258,446,634,270,451,619)
cBPY <-c(284,286,287,101,101,103)
cLPX <-c(171,364,555,187,369,546)
cLPY <-c(359,361,367,174,175,177)
cRPX <-c(326,520,712,344,526,700)
cRPY <-c(366,371,374,177,180,177)
#c(,,,,,)

#create a coordinate table
CoTbR3T6C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR3T6C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR3T6C2, "Coordinate_Tables/CoTbR3T6C2.csv")
visualize(frame = FR3T6C2, CD=CoTbR3T6C2)

###############################################################################
