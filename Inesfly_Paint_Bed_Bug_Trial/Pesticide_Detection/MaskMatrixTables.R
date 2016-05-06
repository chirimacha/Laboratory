
#Repeat the above code to find the points and 
#manually enter them in the dataframes below.
tester<-getpoint(FR1T1C1)
tester

Video FR1T1C1
Note that if X coords for TP and BP are exactly equal, it may create errors.
Tray<-c(1,2,3,4,5,6)
aMXL <-c(144,331,527,144,333,527)
aMXR <-c(331,527,728,333,527,728)
aMYT <-c(422,422,422,227,227,227)
aMYB <-c(227,227,227, 25, 25, 25)
aTPX <-c(248,428,364,247,424,626)
aTPY <-c(404,395,390,210,207,194)
aBPX <-c(246,423,620,244,420,617)
aBPY <-c(245,231,232, 67, 65, 50)
aLPX <-c(167,341,547,162,347,550)
aLPY <-c(327,314,316,143,139,126)
aRPX <-c(322,509,709,319,503,697)
aRPY <-c(318,310,311,139,137,122)
#c(,,,,,)
#create a coordinate table
CoTbR1T1C1<-data.frame(Tray, aMXL, aMXR, aMYT, aMYB, aBPX, aBPY, aTPX, aTPY, 
                       aRPX, aRPY, aLPX, aLPY)
#rename so that colums can be found in function
names(CoTbR1T1C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR1T1C1, "CoTbR1T1C1.csv")

###============================
Tray<-c(1,2,3,4,5,6)
#Left Matrix limit for mask
bMXL <-c(168, 351, 521, 178, 349, 517)
#right Matrix limit for mask
bMXR <-c(351, 521, 703, 349, 517, 703)
#Top matrix limit
bMYT <-c(427, 427, 427, 238, 238, 238)
#bottom matrix limit
bMYB <-c(238, 238, 238,  59,  59,  59)

#Top point of vericle line (X-Coord)
bTPX <-c(258, 434, 608, 267, 439, 596)
#Top point of verticle line (Y-Coord)
bTPY <-c(404, 406, 410, 224, 224, 219)
#Bottom Point
bBPX <-c(284, 439, 604, 276, 438, 601)
bBPY <-c(254, 255, 252,  87,  82,  77)
#Left Point of horizontal line
bLPX <-c(199, 363, 532, 198, 362, 527)
bLPY <-c(316, 329, 330, 153, 155, 150)
#Right Point of Horizontal line
bRPX <-c(338, 511, 685, 337, 515, 678)
bRPY <-c(338, 331, 333, 158, 154, 155)
#c(,,,,,)
#create a coordinate table
CoTbR1T1C2<-data.frame(Tray, bMXL, bMXR, bMYT, bMYB, bBPX, bBPY, bTPX, bTPY, 
                       bRPX, bRPY, bLPX, bLPY)
#rename so values are easily called in loop or function
names(CoTbR1T1C2)<-c("Tray","MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
#write.csv(CoTbR1T1C2,"CoTbR1T1C2.csv")
