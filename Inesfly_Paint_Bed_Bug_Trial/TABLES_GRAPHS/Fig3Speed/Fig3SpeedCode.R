###Code to create Fig3Speed

##make two working directory paths
#One containing the data
inwd <- paste("/Users/dtracy198/Documents/GitHub/Laboratory/",
              "Inesfly_Paint_Bed_Bug_Trial/TABLES_GRAPHS",
              "/Fig3Speed", sep = "")
#A second for the location the file is saved. (image may be too large for git)
outwd <- ("/Users/dtracy198/Documents")

#bring in data for individual insects
csCVR2 <- read.csv("csCVR2.csv")
csCVR3 <- read.csv("csCVR3.csv")
csCVR4 <- read.csv("csCVR4.csv")

#bring in data for average for the exposed
ExAS2 <- read.csv("ExAS2.csv")
ExAS3 <- read.csv("ExAS3.csv")
ExAS4 <- read.csv("ExAS4.csv")
#average data for the controls
CoAS2 <- read.csv("CoAS2.csv")
CoAS3 <- read.csv("CoAS3.csv")
CoAS4 <- read.csv("CoAS4.csv")

#create function that plots the speed of indivudal insects
PlotSpeed <- function(csTest, num){
  filter <- which(csTest[,2] == 1)
  ConFilt <- which(csTest[,2] == 0)
  MeanSpeedPest <- csTest[1,]
  MeanSpeedCont <- csTest[1,]
  MeanSpeedPest[1] <- "MeanSpeedPest"
  MeanSpeedCont[1] <- "MeanSpeedCont"
  MeanSpeedPest[2] <- 1
  MeanSpeedCont[2] <- 0
  for(i in 3:1801){
    MeanSpeedPest[i] <- mean(csTest[filter, i], na.rm = T)
    MeanSpeedCont[i] <- mean(csTest[ConFilt, i], na.rm = T)
  }
  if(num == 1){ 
    d <- "Day -"
  } else {d <- "Weeks -"}
  mn.tile.t <- paste(num, d, "Exposed")
  mn.tile.c <- paste(num, d, "Control")
  #pdf("InstSpeed_3weeks")
  plot(x = c(1, 1800), y = c(0, 40), type = "n", 
       main = mn.tile.t, ylab = "Speed (pixels/sec)",
       xlab = "Time (seconds)", xaxt = 'n', yaxt = 'n')
  axis( 2, at = c(0:4 * 10), las = 2,
        labels = as.character(c(0:4 * 10)))
  axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
  for(i in 1:length(filter)){
    lines(x = 2:1800, y = csTest[ filter[i], 3:1801], 
          col = alpha(i, 0.5), lty = 3)
  }
  lines(x = 2:1800, y = MeanSpeedPest[3:1801], 
        col = 1, lty = 1)
  plot(x = c(1, 1800), y = c(0, 40), type = "n", 
       main = mn.tile.c, ylab = "Speed (pixels/sec)",
       xlab = "Time (seconds)", xaxt = 'n', yaxt = 'n')
  axis( 2, at = c(0:4 * 10), las = 2,
        labels = as.character(c(0:4 * 10)))
  axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
  for(i in 1:length(ConFilt)){
    lines(x = 2:1800, y = csTest[ ConFilt[i], 3:1801], 
          col = alpha(i, 0.5), lty = 3)
  }
  lines(x = 2:1800, y = MeanSpeedCont[3:1801], 
        col = 1, lty = 1)
}
 
##select file type
#pdf(file = "SpeedPlots.pdf")
#pdf("Figures/SpeedPlots.pdf", height = 9, width = 6)
tiff("Figures/SpeedPlots.tiff", width = 9, height = 6, units = "in", res = 1081)
#pdf("Figures/SpeedPlots.pdf", height = 9, width = 6)
#jpeg("Figures/SpeedPlots.jpeg", height = 9, width = 6, res = 300, 
#    units = "in")

#set parameters
par(mfrow = c(3,2), oma = c(1,1,2,1))

#plot graphs
PlotSpeed(csCVR4, 1)
PlotSpeed(csCVR2, 3)
PlotSpeed(csCVR3, 12)

#add titl
mtext(paste("Speed of Insects Each Observation", sep = " "), side = 3, line = 0, 
      outer = TRUE, cex = 1.2)

#turn of plotting device
dev.off()