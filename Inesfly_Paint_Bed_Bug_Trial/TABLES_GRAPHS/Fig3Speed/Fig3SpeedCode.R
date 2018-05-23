###Code to create Fig3Speed

##make two working directory paths
#One containing the data
inwd <- paste("/Users/dtracy198/Documents/GitHub/Laboratory/",
              "Inesfly_Paint_Bed_Bug_Trial/TABLES_GRAPHS",
              "/Fig3Speed", sep = "")
#A second for the location the file is saved. (image may be too large for git)
outwd <- ("/Users/dtracy198/Documents")

#FOR OTHERS USING FILE DEPEDING ON HOME DIRECTORY SETTINGS
#SEE LINK BELOW To CHANGE settings for your "~" (ESP. FOR WINDOWS USERS)
#https://stackoverflow.com/questions/11004531/change-path-expand-location-win-7
homeDir <- path.expand('~')
inwd<-paste(homeDir,
      "/Laboratory/Inesfly_Paint_Bed_Bug_Trial/TABLES_GRAPHS/Fig2RunningAvg",
      sep="")
outwd<-paste(homeDir,"/Documents", sep="")



#set working directory
setwd(inwd)



#packages
#install.packages("scales") #allows for 
library(scales)

#bring in data for individual insect ;
csCVR2 <- read.csv("csCVR2.csv")
csCVR3 <- read.csv("csCVR3.csv")
csCVR4 <- read.csv("csCVR4.csv")
#NOTE: First colom is insect id, then date, then which plate
#X2=1 means they were on the partially treated plate. 
#X3-X1801 are the speed observations

#set to the out director to save the file appropriately
setwd(outwd)

#create function that plots the speed of indivudal insects
PlotSpeed <- function(csTest, num){
  #identif which insects are those on the partially treated plates
  filter <- which(csTest[,3] == 1)
  #identify insects that are on the control plates.
  ConFilt <- which(csTest[,3] == 0)
  #create data frame to be filled by loop
  MeanSpeedPest <- csTest[1,]
  MeanSpeedCont <- csTest[1,]
  #Give Names in first row
  MeanSpeedPest[1] <- "MeanSpeedPest"
  MeanSpeedCont[1] <- "MeanSpeedCont"
  #put treatment indicator in the second (1=treat,0=control plate)
  MeanSpeedPest[2] <- 1
  MeanSpeedCont[2] <- 0
  #loop that finds the mean value of all insects in treatment at
  #time/frame i., This will be the black lines 
  for(i in 3:1801){
    MeanSpeedPest[i] <- mean(csTest[filter, i], na.rm = T)
    MeanSpeedCont[i] <- mean(csTest[ConFilt, i], na.rm = T)
  }
  #create graph labels based on number indicator in start of function.
  #takes into consideration days vs weeks
  if(num == 1){ 
    d <- "Day -"
  } else {d <- "Weeks -"}
  #adds the treatment group to the label
  mn.tile.t <- paste(num, d, "Partially Treated Plates")
  mn.tile.c <- paste(num, d, "Control Plates")
  
  #create blank plot to be filled in (note type = n)
  plot(x = c(1, 1800), y = c(0, 40), type = "n", 
       main = mn.tile.t, ylab = "Speed (pixels/sec)",
       xlab = "Time (seconds)", xaxt = 'n', yaxt = 'n')
  #creates plot axis ticks ;
  #for 0, 10, 20, 30 ,and 40 pixels/ second on the y axis;
  axis( 2, at = c(0:4 * 10), las = 2,
        labels = as.character(c(0:4 * 10)))
  #and over 0, 600, 1200, and 1800 second on the bottom
  axis( 1, at = c(0:3 * 600), labels = as.character(c(0:3 * 600)))
  
  #add lines to the plot for each insect on the pesticide plates
  for(i in 1:length(filter)){
    lines(x = 2:1800, y = csTest[ filter[i], 3:1801], 
          #allows for different transparent colors
          col = alpha(i, 0.5), lty = 3)
  }
  #add the black line for the average
  lines(x = 2:1800, y = MeanSpeedPest[3:1801], 
        col = 1, lty = 1)
  
  #repeat of the above code for bugs on the control plates
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
#tiff("Figures/SpeedPlots.tiff", width = 9, height = 6, units = "in", res = 1081)
#pdf("Figures/SpeedPlots.pdf", height = 9, width = 6)
jpeg("SpeedPlots.jpeg", height = 9, width = 6, res = 300, 
    units = "in")

#set parameters
par(mfrow = c(3,2), oma = c(1,1,2,1))

#plot graphs
PlotSpeed(csCVR4, 1)
PlotSpeed(csCVR2, 3)
PlotSpeed(csCVR3, 12)

#add titl
mtext(paste("Speed of Insects at Each Observation", sep = " "), side = 3, line = 0, 
      outer = TRUE, cex = 1.2)

#turn of plotting device
dev.off()

