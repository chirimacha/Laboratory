###Code to createFig2RunningAvg
##create two working directory paths
#One containing the data
#inwd <- paste("/Users/dtracy198/Documents/GitHub/Laboratory/",
 #             "Inesfly_Paint_Bed_Bug_Trial/TABLES_GRAPHS","/Fig2RunningAvg",
  #            sep = "")
#A second for the location the file is saved. (image may be too large for git)
outwd <- ("/Users/jeniferpeterson/Desktop")

inwd <- paste("/Users/jeniferpeterson/Desktop/Laboratory/",
             "Inesfly_Paint_Bed_Bug_Trial/TABLES_GRAPHS","/Fig2RunningAvg",
            sep = "")
#A second for the location 

###FOR OTHERS DEPENDING ON YOUR HOME DIRECTORY SETTINGS
#homeDir <- path.expand('~')
#inwd<-paste(homeDir,"/Laboratory/Inesfly_Paint_Bed_Bug_Trial/TABLES_GRAPHS/Fig2RunningAvg", sep="")
#outwd<-paste(homeDir,"/Laboratory/Inesfly_Paint_Bed_Bug_Trial/TABLES_GRAPHS/Fig2RunningAvg", sep="")

#set the working director to bring in data
setwd(inwd)

##Bring in Data
#running averages of proportion of time quadrant 1 or 3 for all insects
ima.CVR2 <- read.csv("ima_CVR2.csv")
ima.CVR3 <- read.csv("ima_CVR3.csv")
ima.CVR4 <- read.csv("ima_CVR4.csv")

#running group average
ma.CompVidRep2 <- read.csv("ma_CompVidRep2.csv")
ma.control.CompVidRep2 <- read.csv("ma_control_CompVidRep2.csv")
ma.CompVidRep3 <- read.csv("ma_CompVidRep3.csv")
ma.control.CompVidRep3 <- read.csv("ma_control_CompVidRep3.csv")
#NOTE REP 4 is DAY 1 ; REP 1 VIDEOS WERE 
ma.CompVidRep4 <- read.csv("ma_CompVidRep4.csv")
ma.control.CompVidRep4 <- read.csv("ma_control_CompVidRep4.csv")

#######################Plot Individual Running Averages########################
setwd(outwd)

##select file type of output
#pdf("IndRrunning_avg.pdf", height = 9, width = 6)
jpeg("IndRrunning_avg.jpeg", height = 9, width = 6, res = 300,
     units = "in")
#tiff("IndRrunning_avg.tiff", height = 9, width = 6, res = 1081, 
#     units = "in")

#set graphing parameters
par(mfrow = c(3, 2), oma = c(1,1,2,1))

### Plot CompVidRep4 data (1 day)
#create blank plot for exposed insects
plot(x = c(1, 1800), y = c(0, 1), type = "n", xlab = "Time (seconds)",
     ylab = "Proportion of time in quadrants 1 and 3", 
     main = "1 Day - Insecticide group", xaxt = 'n', yaxt = 'n')
#create axis
axis( 2, at = c(0:5 / 5), las = 2,
      labels = as.character(c(0:5 / 5)))
axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
#create filter to select all exposed insects
filter <- which(ima.CVR4[,3] == 1)
#create loop to plot surival lines
for(i in 1:length(filter)){
  lines(x = 1:1800, y = ima.CVR4[filter[i], 3:1802], 
        col = i, lty = 3)
}
#plot the average of the exposed insects
lines(x = 1:1800, y = ma.CompVidRep4[,2], lty = 1, lwd = 1.5, col = 1)

##plot controls
#plot new blank plot
plot(x = c(1, 1800), y = c(0, 1), type = "n", xlab = "Time (seconds)",
     ylab = "Proportion of time in quadrants 1 and 3",
     main = "1 Day - Control group", xaxt = 'n', yaxt = 'n')
axis( 2, at = c(0:5 / 5), las = 2,
      labels = as.character(c(0:5 / 5)))
axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
filter <- which(ima.CVR4[,3] == 0)
for(i in 1:length(filter)){
  lines(x = 1:1800, y = ima.CVR4[filter[i], 3:1802], col = i, lty = 3)
}
lines(x = 1:1800, y = ma.control.CompVidRep4[,2], lty = 1, lwd = 1.5, col = 1)

### CompVidRep2 (3 Weeks)
#plot for exposed insects
plot(x = c(1, 1800), y = c(0, 1), type = "n", xlab = "Time (seconds)",
     ylab = "Proportion of time in quadrants 1 and 3",
     main = "3 Weeks - Insecticide group", xaxt = 'n', yaxt = 'n')
axis( 2, at = c(0:5 / 5), las = 2,
      labels = as.character(c(0:5 / 5)))
axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
filter <- which(ima.CVR2[,3] == 1)
for(i in 1:length(filter)){
  lines(x = 1:1800, y = ima.CVR2[filter[i], 3:1802], 
        col = i, lty = 3)
}#(ima.CompVidRep2[filter[i],2]+1)
lines(x = 1:1800, y = ma.CompVidRep2[,2], lty = 1, lwd = 1.5, col = 1)

#plot for control insects
plot(x = c(1, 1800), y = c(0, 1), type = "n", xlab = "Time (seconds)",
     ylab = "Proportion of time in quadrants 1 and 3",
     main = "3 Weeks - Control group", xaxt = 'n', yaxt = 'n')
axis( 2, at = c(0:5 / 5), las = 2,
      labels = as.character(c(0:5 / 5)))
axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
filter <- which(ima.CVR2[,3] == 0)
for(i in 1:length(filter)){
  lines(x = 1:1800, y = ima.CVR2[filter[i], 3:1802], col = i, lty = 3)
}
lines(x = 1:1800, y = ma.control.CompVidRep2[,2], lty = 1, lwd = 1.5, col = 1)

### CompVidRep3 (12 Weeks)
##plot exposed
plot(x = c(1, 1800), y = c(0, 1), type = "n", xlab = "Time (seconds)",
     ylab = "Proportion of time in quadrants 1 and 3",
     main = "12 Weeks - Insecticide group", xaxt = 'n', yaxt = 'n')
axis( 2, at = c(0:5 / 5), las = 2,
      labels = as.character(c(0:5 / 5)))
axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
filter <- which(ima.CVR3[,3] == 1)
for(i in 1:length(filter)){
  lines(x = 1:1800, y = ima.CVR3[filter[i], 3:1802], 
        col = i, lty = 3)
}#(ima.CompVidRep2[filter[i],2]+1)
lines(x = 1:1800, y = ma.CompVidRep3[,2], lty = 1, lwd = 1.5, col = 1)

##plot control
plot(x = c(1, 1800), y = c(0, 1), type = "n", xlab = "Time (seconds)",
     ylab = "Proportion of time in quadrants 1 and 3",
     main = "12 Weeks - Control group", xaxt = 'n', yaxt = 'n')
axis( 2, at = c(0:5 / 5), las = 2,
      labels = as.character(c(0:5 / 5)))
axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
filter <- which(ima.CVR3[,3] == 0)
for(i in 1:length(filter)){
  lines(x = 1:1800, y = ima.CVR3[filter[i], 3:1802], col = i, lty = 3)
}
lines(x = 1:1800, y = ma.control.CompVidRep3[,2], lty = 1, lwd = 1.5, col = 1)

##Add title
#mtext("Running Average of Proportion of Time that Individual ", side = 3, 
#      line = 0.3, outer = TRUE, cex = 1.2)
#mtext("Insects Spent on Quadrants 1 and 3", side = 3, 
 #     line = -1.2, outer = TRUE, cex = 1.2)
#save file type
dev.off()
