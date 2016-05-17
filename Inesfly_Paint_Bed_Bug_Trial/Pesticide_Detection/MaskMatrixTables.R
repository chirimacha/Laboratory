
#Repeat the above code to find the points and 
#manually enter them in the dataframes below.
tester<-getpoint(FR1T2C2)
tester

#Video FR1T1C1
#Note that if X coords for TP and BP are exactly equal, it may create errors.
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
#write.csv(CoTbR1T1C1, "CoTbR1T1C1.csv")

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
#==============================================================================

#Video FR1T2C1
#Note that if X coords for TP and BP are exactly equal, it may create errors.
Tray<-c(1,2,3,4,5,6)
cMXL <-c(134,333,526,134,329,526)
cMXR <-c(333,526,727,333,526,727)
cMYT <-c(424,424,424,230,230,208)
cMYB <-c(230,230,208, 30, 30, 30)
cTPX <-c(240,436,634,246,428,623)
cTPY <-c(410,404,388,214,209,199)
cBPX <-c(240,430,620,247,423,615)
cBPY <-c(242,236,224,066, 63, 50)
cLPX <-c(157,351,551,172,349,546)
cLPY <-c(328,321,309,145,139,127)
cRPX <-c(314,513,713,319,502,698)
cRPY <-c(324,316,304,143,137,125)
#c(,,,,,)
#create a coordinate table
CoTbR1T2C1<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR1T2C1)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR1T2C1, "CoTbR1T2C1.csv")
#==============================================================================
#Video FR1T2C1
Tray<-c(1,2,3,4,5,6)
cMXL <-c(162,346,520,162,346,520)
cMXR <-c(346,520,718,346,520,718)
cMYT <-c(423,423,423,239,239,239)
cMYB <-c(239,239,239,043,043,043)
cTPX <-c(252,431,615,272,436,608)
cTPY <-c(403,409,412,221,219,205)
cBPX <-c(,,,,,)
cBPY <-c(,,,,,)
cLPX <-c(,,,,,)
cLPY <-c(,,,,,)
cRPX <-c(,,,,,)
cRPY <-c(,,,,,)
#c(,,,,,)
#create a coordinate table
CoTbR1T2C2<-data.frame(Tray, cMXL, cMXR, cMYT, cMYB, cBPX, cBPY, cTPX, cTPY, 
                       cRPX, cRPY, cLPX, cLPY)
#rename so that colums can be found in function
names(CoTbR1T2C2)<-c("Tray", "MXL", "MXR", "MYT", "MYB", "BPX", "BPY", "TPX", "TPY", 
                     "RPX", "RPY", "LPX", "LPY")
write.csv(CoTbR1T2C2, "CoTbR1T2C2.csv")

