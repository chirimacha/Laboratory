#Fig3Speed
read.csv("csCVR2.csv")
read.csv("csCVR3.csv")
read.csv("csCVR4.csv")

read.csv("ExAS2.csv")
read.csv("ExAS3.csv")
read.csv("ExAS4.csv")
read.csv("CoAS2.csv")
read.csv("CoAS3.csv")
read.csv("CoAS4.csv")

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

#pdf(file = "SpeedPlots.pdf")
#pdf("Figures/SpeedPlots.pdf", height = 9, width = 6)
tiff("Figures/SpeedPlots.tiff", width = 9, height = 6, units = "in", res = 1081)
#pdf("Figures/SpeedPlots.pdf", height = 9, width = 6)
#jpeg("Figures/SpeedPlots.jpeg", height = 9, width = 6, res = 300, 
#    units = "in")
par(mfrow = c(3,2), oma = c(1,1,2,1))
PlotSpeed(csCVR4, 1)
PlotSpeed(csCVR2, 3)
PlotSpeed(csCVR3, 12)
mtext(paste("Speed of Insects Between Each Observation", sep = " "), side = 3, line = 0, 
      outer = TRUE, cex = 1.2)
dev.off()