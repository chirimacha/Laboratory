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

#set working directory
setwd("/Users/mzlevy/Desktop")
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

#reset wd()
setwd("/Users/mzlevy/Laboratory/Inesfly_Paint_Bed_Bug_Trial/Pesticide_Detection")

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
Tray <- c(1,2,3,4,5,6)
Camera <- c(1,1,1,1,1,1)
Trial <- c(1,1,1,1,1,1)
Rep <- c(3,3,3,3,3,3)
cMXL <- c(150,316,482,150,330,494)
cMXR <- c(316,482,662,330,494,662)
cMYT <- c(393,393,393,228,228,228)
cMYB <- c(228,228,228,055,055,055)
cTPX <- c(236,401,557,250,413,568)
cTPY <- c(378,385,379,212,218,219)
cBPX <- c(242,403,555,251,412,570)
cBPY <- c(234,237,238,062,080,075)
cLPX <- c(158,322,486,176,337,502)
cLPY <- c(308,310,306,141,150,150)
cRPX <- c(306,476,623,317,486,641)
cRPY <- c(308,311,309,142,151,150)
#c(,,,,,)
#c(0,0,0,0,0,0)


#create a coordinate table
CoTbR3T1C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)

#rename so that colums can be found in function
names(CoTbR3T1C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR3T1C1, "Coordinate_Tables/CoTbR3T1C1.csv")

visualize(frame = FR3T1C1, CD=CoTbR3T1C1)

###############################################################################
###Trial 1 Cam 2
tester<-getpoint(FR3T1C2)
tester

Tray<-c(1,2,3,4,5,6)
cMXL <-c(168,342,513,168,342,513)
cMXR <-c(342,513,678,342,513,678)
cMYT <-c(418,418,418,239,239,239)
cMYB <-c(239,239,239,076,076,076)
cTPX <-c(259,423,589,263,426,590)
cTPY <-c(404,405,406,233,229,231)
cBPX <-c(263,422,593,265,419,588)
cBPY <-c(263,265,264,093,087,091)
cLPX <-c(186,351,522,193,353,521)
cLPY <-c(333,337,331,162,164,162)
cRPX <-c(330,496,658,329,496,662)
cRPY <-c(333,338,336,165,159,163)
#c(,,,,,)

#create a coordinate table
CoTbR3T1C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR3T1C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR3T1C2, "Coordinate_Tables/CoTbR3T1C2.csv")

visualize(frame = FR3T1C2, CD=CoTbR3T1C2)
###############################################################################
#R3T2C1
tester<-getpoint(FR3T2C1)
tester

#RECORD THE POINTS GATHERED
Tray<-c(1,2,3,4,5,6)
cMXL <- c(150,316,482,150,330,488)
cMXR <- c(316,482,662,330,488,662)
cMYT <- c(393,393,393,228,228,228)
cMYB <- c(228,228,228,055,065,065)
cTPX <-c(246,408,563,251,408,561)
cTPY <-c(377,382,384,214,220,223)
cBPX <-c(238,403,567,253,413,561)
cBPY <-c(239,242,243,067,080,080)
cLPX <-c(168,335,500,173,339,492)
cLPY <-c(311,315,310,143,149,156)
cRPX <-c(307,474,635,322,485,635)
cRPY <-c(304,310,315,143,153,155)
#c(,,,,,)

#create a coordinate table
CoTbR3T2C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR3T2C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR3T2C1, "Coordinate_Tables/CoTbR3T2C1.csv")
visualize(frame = FR3T2C1, CD=CoTbR3T2C1)


###############################################################################
tester<-getpoint(FR3T2C2)
tester
###Trial 2 Cam 2
Tray<-c(1,2,3,4,5,6)
cMXL <-c(168,342,513,168,342,513)
cMXR <-c(342,513,678,342,513,678)
cMYT <-c(418,418,418,239,239,239)
cMYB <-c(239,239,239,076,076,076)
cTPX <-c(259,427,589,263,428,590)
cTPY <-c(400,405,406,229,231,227)
cBPX <-c(265,426,593,266,426,591)
cBPY <-c(259,262,262,087,090,093)
cLPX <-c(186,352,521,190,357,523)
cLPY <-c(330,336,332,158,165,163)
cRPX <-c(329,498,661,331,498,661)
cRPY <-c(331,333,336,161,160,163)
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
cMXL <- c(140,316,482,140,330,494)
cMXR <- c(316,482,662,330,494,662)
cMYT <- c(410,410,410,228,228,228)
cMYB <- c(228,228,228,065,065,065)
cTPX <- c(234,404,560,243,412,569)
cTPY <- c(383,386,384,216,222,219)
cBPX <- c(239,401,567,247,415,570)
cBPY <- c(238,245,244,071,080,078)
cLPX <- c(159,328,494,171,338,501)
cLPY <- c(310,316,311,142,149,150)
cRPX <- c(310,479,635,316,488,644)
cRPY <- c(312,313,316,148,153,153)
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
cMXL <-c(168,342,513,168,342,513)
cMXR <-c(342,513,678,342,513,678)
cMYT <-c(418,418,418,239,239,239)
cMYB <-c(239,239,239,076,076,076)
cTPX <-c(265,421,596,263,423,586)
cTPY <-c(402,408,411,232,235,232)
cBPX <-c(267,422,592,266,421,589)
cBPY <-c(257,258,265,093,095,092)
cLPX <-c(186,347,523,189,347,518)
cLPY <-c(332,332,339,164,164,160)
cRPX <-c(335,491,662,334,494,659)
cRPY <-c(331,335,337,167,164,164)
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
cMXL <- c(150,330,500,150,330,500)
cMXR <- c(330,500,662,330,500,662)
cMYT <- c(405,405,405,228,228,228)
cMYB <- c(228,228,228,060,060,060)
cTPX <-c(238,417,568,251,417,574)
cTPY <-c(384,389,392,212,218,222)
cBPX <-c(248,416,581,260,416,578)
cBPY <-c(242,243,250,068,073,077)
cLPX <-c(167,344,508,173,339,507)
cLPY <-c(312,315,314,137,147,149)
cRPX <-c(318,490,645,323,493,654)
cRPY <-c(313,314,326,146,148,152)
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
cMXL <-c(168,342,513,168,342,513)
cMXR <-c(342,513,678,342,513,678)
cMYT <-c(418,418,418,239,239,239)
cMYB <-c(239,239,239,076,076,076)
cTPX <-c(265,423,610,276,430,602)
cTPY <-c(400,407,404,229,230,233)
cBPX <-c(269,432,611,267,434,597)
cBPY <-c(256,261,254,091,093,093)
cLPX <-c(189,355,543,192,363,529)
cLPY <-c(329,331,329,163,161,167)
cRPX <-c(337,500,686,339,501,661)
cRPY <-c(331,334,335,157,164,161)
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
Tray <-c(1,2,3,4,5,6)
cMXL <- c(140,316,482,150,330,494)
cMXR <- c(316,482,662,330,494,662)
cMYT <- c(393,393,393,220,228,228)
cMYB <- c(220,228,228,055,065,065)
cTPX <-c(231,403,561,249,413,570)
cTPY <-c(374,383,382,212,217,222)
cBPX <-c(234,407,562,252,411,575)
cBPY <-c(226,241,242,066,072,083)
cLPX <-c(158,327,493,175,338,504)
cLPY <-c(302,311,312,136,146,150)
cRPX <-c(304,475,634,321,481,646)
cRPY <-c(302,315,312,141,148,153)
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
Tray <- c(1,2,3,4,5,6)
cMXL <-c(168,342,513,168,342,513)
cMXR <-c(342,513,678,342,513,678)
cMYT <-c(418,418,418,239,239,239)
cMYB <-c(239,239,239,076,076,076)
cTPX <-c(262,423,592,264,428,596)
cTPY <-c(403,409,407,230,235,230)
cBPX <-c(264,421,594,265,427,593)
cBPY <-c(260,265,269,091,095,091)
cLPX <-c(190,349,526,189,353,527)
cLPY <-c(334,339,336,161,165,162)
cRPX <-c(331,496,661,336,499,666)
cRPY <-c(329,335,338,163,162,161)
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

Tray <- c(1,2,3,4,5,6)
cMXL <- c(150,320,494,150,330,494)
cMXR <- c(320,494,662,330,494,670)
cMYT <- c(393,393,393,220,228,228)
cMYB <- c(220,228,228,050,065,065)
cTPX <- c(245,412,567,250,414,582)
cTPY <- c(372,382,386,213,222,224)
cBPX <- c(247,409,566,250,417,584)
cBPY <- c(232,238,244,061,077,077)
cLPX <- c(166,340,500,170,345,512)
cLPY <- c(301,311,312,137,147,151)
cRPX <- c(317,485,637,320,489,656)
cRPY <- c(303,309,314,138,153,153)
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
cMXL <-c(168,342,513,168,342,513)
cMXR <-c(342,513,678,342,513,678)
cMYT <-c(418,418,418,239,239,239)
cMYB <-c(239,239,239,076,076,076)
cTPX <-c(267,430,594,270,430,590)
cTPY <-c(406,411,411,226,234,229)
cBPX <-c(266,425,590,268,430,596)
cBPY <-c(261,267,264,089,091,094)
cLPX <-c(189,349,521,192,354,523)
cLPY <-c(334,344,341,161,163,162)
cRPX <-c(333,499,666,335,499,663)
cRPY <-c(328,334,336,162,162,166)
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
