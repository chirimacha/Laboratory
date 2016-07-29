library(videoplayR)
library(grid)

setwd("/Users/mzlevy/Desktop")

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
cLPX <-c(174,350,532,175,360,542)
cLPY <-c(302,308,318,118,128,144)
cRPX <-c(337,512,686,345,519,693)
cRPY <-c(316,317,328,133,146,143)
#c(,,,,,)

#create a coordinate table
CoTbR4T1C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)

#rename so that colums can be found in function
names(CoTbR4T1C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR4T1C1, "Coordinate_Tables/CoTbR4T1C1.csv")

visualize(frame = FR4T1C1, CD=CoTbR4T1C1)

###############################################################################
###Trial 1 Cam 2
tester<-getpoint(FR4T1C2)
tester

Tray<-c(1,2,3,4,5,6)
cMXL <-c(160,345,525,160,345,525)
cMXR <-c(345,525,700,345,525,700)
cMYT <-c(435,435,435,250,250,250)
cMYB <-c(250,250,250, 060, 060, 060)
cTPX <-c(260,433,603,261,431,615)
cTPY <-c(416,422,421,246,237,234)
cBPX <-c(268,431,606,266,432,596)
cBPY <-c(257,264,268,090,088,078)
cLPX <-c(187,353,530,177,357,535)
cLPY <-c(333,346,339,171,161,166)
cRPX <-c(339,512,678,338,510,681)
cRPY <-c(340,340,348,169,163,151)
#c(,,,,,)

#create a coordinate table
CoTbR4T1C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR4T1C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR4T1C2, "Coordinate_Tables/CoTbR4T1C2.csv")

visualize(frame = FR4T1C2, CD=CoTbR4T1C2)
###############################################################################
#R4T2C1
tester<-getpoint(FR4T2C1)
tester

#RECORD THE POINTS GATHERED
Tray<-c(1,2,3,4,5,6)
cMXL <-c(160,349,525,160,349,525)
cMXR <-c(349,525,712,349,525,712)
cMYT <-c(420,420,420,220,230,230)
cMYB <-c(220,230,230, 030, 048, 048)
cTPX <-c(263,431,615,268,440,616)
cTPY <-c(391,397,397,210,216,226)
cBPX <-c(269,437,612,270,449,617)
cBPY <-c(237,245,247,055,064,072)
cLPX <-c(178,359,537,189,365,540)
cLPY <-c(313,315,323,132,131,147)
cRPX <-c(343,517,692,344,526,693)
cRPY <-c(317,323,319,134,139,147)
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
cMXL <-c(168,350,523,168,350,523)
cMXR <-c(350,523,700,350,523,700)
cMYT <-c(430,430,430,240,240,240)
cMYB <-c(240,240,240, 060, 060, 060)
cTPX <-c(269,437,605,263,440,599)
cTPY <-c(417,414,404,245,234,232)
cBPX <-c(261,438,603,260,435,607)
cBPY <-c(255,258,246,093,082,082)
cLPX <-c(181,359,529,178,361,529)
cLPY <-c(339,335,325,172,160,153)
cRPX <-c(338,515,683,336,514,680)
cRPY <-c(330,337,325,166,159,157)
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
cMXL <-c(160,350,528,160,350,530)
cMXR <-c(350,528,712,350,530,712)
cMYT <-c(420,420,420,220,230,230)
cMYB <-c(220,230,230, 030, 048, 048)
cTPX <-c(261,439,612,265,446,608)
cTPY <-c(396,399,398,210,215,221)
cBPX <-c(270,437,610,269,444,620)
cBPY <-c(237,240,244,051,054,071)
cLPX <-c(183,358,532,188,369,544)
cLPY <-c(310,321,325,130,134,140)
cRPX <-c(342,516,690,344,526,696)
cRPY <-c(318,320,322,133,137,153)
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
cTPX <-c(268,436,609,257,428,604)
cTPY <-c(413,407,408,243,234,233)
cBPX <-c(270,434,603,258,432,595)
cBPY <-c(258,253,253,094,085,085)
cLPX <-c(183,356,533,178,348,525)
cLPY <-c(336,333,336,169,156,162)
cRPX <-c(340,513,680,333,509,673)
cRPY <-c(333,331,334,168,162,156)
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
cMXL <-c(160,350,530,160,350,530)
cMXR <-c(350,530,712,350,530,712)
cMYT <-c(420,420,420,220,220,230)
cMYB <-c(220,220,230,035,035,048)
cTPX <-c(261,433,616,275,447,617)
cTPY <-c(389,391,239,205,210,221)
cBPX <-c(269,438,614,273,444,622)
cBPY <-c(233,237,241,053,054,065)
cLPX <-c(176,355,536,183,366,546)
cLPY <-c(306,306,317,131,134,141)
cRPX <-c(340,518,693,347,525,699)
cRPY <-c(314,316,322,133,135,151)
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
cMXL <-c(170,348,519,170,340,519)
cMXR <-c(348,519,702,340,519,702)
cMYT <-c(435,435,435,250,250,250)
cMYB <-c(250,250,250,069,069,069)
cTPX <-c(268,435,605,261,429,601)
cTPY <-c(413,418,414,246,236,236)
cBPX <-c(265,436,607,263,431,595)
cBPY <-c(257,258,258,090,085,083)
cLPX <-c(182,355,533,177,351,525)
cLPY <-c(339,339,333,167,161,162)
cRPX <-c(341,506,681,337,515,674)
cRPY <-c(334,340,336,170,162,160)
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
Tray <-c(1,2,3,4,5,6)
cMXL <-c(160,348,530,160,348,530)
cMXR <-c(348,530,712,348,530,712)
cMYT <-c(410,410,410,220,230,230)
cMYB <-c(220,230,230,030,048,048)
cTPX <-c(260,428,610,265,442,614)
cTPY <-c(389,392,395,208,214,218)
cBPX <-c(277,439,610,270,444,620)
cBPY <-c(233,234,244,046,060,063)
cLPX <-c(186,358,536,185,365,546)
cLPY <-c(303,314,319,127,135,140)
cRPX <-c(342,514,686,342,524,699)
cRPY <-c(313,321,321,133,137,144)
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
cMXL <-c(170,348,519,170,340,519)
cMXR <-c(348,519,702,340,519,702)
cMYT <-c(435,435,435,250,250,250)
cMYB <-c(250,250,250,069,069,069)
cTPX <-c(266,431,604,262,432,600)
cTPY <-c(416,409,415,246,242,238)
cBPX <-c(265,438,602,262,434,596)
cBPY <-c(256,254,257,094,090,087)
cLPX <-c(185,358,529,186,354,524)
cLPY <-c(337,327,336,172,165,164)
cRPX <-c(340,517,680,333,510,669)
cRPY <-c(336,336,337,167,167,163)
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
cMXL <-c(160,350,525,160,355,530)
cMXR <-c(350,525,712,355,530,712)
cMYT <-c(415,415,415,220,230,230)
cMYB <-c(220,230,230,030,048,048)
cTPX <-c(263,433,616,267,438,620)
cTPY <-c(389,394,396,207,216,224)
cBPX <-c(269,437,622,274,449,626)
cBPY <-c(230,235,242,048,059,71)
cLPX <-c(177,356,543,187,364,533)
cLPY <-c(311,312,319,126,132,145)
cRPX <-c(340,512,699,348,523,700)
cRPY <-c(313,319,324,133,142,153)
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
cMXL <-c(177,353,523,177,353,523)
cMXR <-c(353,523,700,353,523,700)
cMYT <-c(435,435,435,250,250,250)
cMYB <-c(250,250,250,069,069,069)
cTPX <-c(269,437,607,271,438,604)
cTPY <-c(412,415,414,247,236,236)
cBPX <-c(277,441,605,275,439,600)
cBPY <-c(254,261,256,091,084,083)
cLPX <-c(189,361,533,190,361,529)
cLPY <-c(335,333,337,167,163,165)
cRPX <-c(342,517,684,342,514,671)
cRPY <-c(337,337,336,173,163,164)
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

