###Code to create Fig1SurvivalArrayWQuads

##make two working directory paths
#One containing the data

#this code makes it simple for anyone to use the code by placing it in their home directory
homeDir <- path.expand('~')
inwd<-paste(homeDir,"/Laboratory/Inesfly_Paint_Bed_Bug_Trial/TABLES_GRAPHS/Fig1SurvivalArrayWQuads", sep="")


inwd <- paste("/Users/dtracy198/Documents/GitHub/Laboratory/",
              "Inesfly_Paint_Bed_Bug_Trial/TABLES_GRAPHS",
              "/Fig1SurvivalArrayWQuads", sep = "")



#A second for the location the file is saved. (image may be too large for git)

outwd<-paste(homeDir,"/Documents", sep="")
outwd <- ("/Users/dtracy198/Documents")

setwd(inwd)

###Bring in the data
quadsum <- read.csv("quadsum.csv")
twenties<- which(quadsum$pch == 20)
quadsum$pch[twenties] <- 16

setwd(outwd)
##Select file type and dimensions in inches
#jpeg("Bioassay_Graphs_Array_Quads.jpg", width = 6, height = 9, units = "in",
    #res = 300 )
#tiff("Bioassay_Graphs_Array_Quads.tiff", width = 6, 
#    height = 9, units = "in", res = 300)
#dev.new(height = 9, width = 6,noRStudioGD = TRUE)

#create indicator for loop through each "days after paiting" treatment
#(1, 180, 90)
dap <- unique(quadsum$days.after.paint)
#put them in order (1, 90, 180)
dap <- dap[order(dap)]
#create indicator for exposure time (1,3, 6, 24 hours) for loop
ext <- unique(quadsum$exp.time)
#set graphing parameters to have a grid that is 3 across 4 down)
par(mfrow = c(4,3), oma = c(1,1,2,1)) 

#Loop through exposure times
for(k in 1:length(ext)){
  #select all values data with same time
  tsdap <- which(quadsum$exp.time == ext[k])
  #then loop through all days after painting
  for(j in 1:length(dap)){
    #select the data all the data with that value of days after painting
    tsext <- which(quadsum$days.after.paint == dap[j])
    #find the data that share the same days after painting and exposure time
    tsde <- intersect(tsdap, tsext)  
    #generate the individual plot for the corresponding time.
    plot(y = quadsum$prop.alive, x = quadsum$day, 
         pch = quadsum$pch, col = quadsum$paint,
         type = "n", #main = as.character(paste("J =", ext[k], "hrs:", "K =", dap[j], 
         #            d, sep = " ")), 
         ylab = "Proportion Alive", xlab = "Days Since Exposure", 
         xaxt = 'n', yaxt = 'n')
    udays <- c( 0, 7, 14, 21, 28)
    axis( 2, at = c(0:5 / 5), las = 2,
          labels = as.character(c(0:5 / 5)))
    axis( 1, at = udays, labels = udays)
    quads.tmp <- unique(quadsum$TreatQuad[tsde])
    for(i in 1:length(quads.tmp)){
      tr <- which(quadsum$TreatQuad == quads.tmp[i])
      temp <- quadsum[tr,]
      points(y = temp$prop.liv, x = temp$day, pch = temp$pch[1],
             col = gray(as.numeric(temp$paint[1])/4-0.25))
      lines(y = temp$prop.liv, x = temp$day, pch = temp$pch[1], 
            col = gray(as.numeric(temp$paint[1])/4-0.25), lty = as.numeric(temp$paint[1]))      
    }
  }
}

#title
#mtext("Proportion of Alive Bugs", side = 3, line = 0, outer = TRUE, cex = 1.2)
#Top Headings
mtext("1 Day After Painting", side = 3, line = -2 , outer = TRUE, cex=0.8, at = 0.18) 
mtext("90 days after painting", side = 3, line = -2 , outer = TRUE, cex=0.8, at = 0.50) 
mtext("180 days after painting", side = 3, line =-2 , outer = TRUE, cex=0.8, at = 0.84) 
#side headings
mtext("Exposed 1 Hour", side = 2, line = -0.2, outer = T, at = 0.938, adj = 1, cex = 0.8) 
mtext("Exposed 3 Hours", side = 2, line = -0.2, outer =T, at = 0.69, adj = 1, cex = 0.8) 
mtext("Exposed 6 Hours", side = 2, line = -0.2, outer = T, at = 0.4392, adj = 1, cex = 0.8) 
mtext("Exposed 24 Hours", side = 2, line = -0.2, outer = T, at = 0.198, adj = 1, cex = 0.8) 
#turn off pdf or jpeg
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend(x = "bottom", legend = c("Control","5A-IGR", "Chlorfenapyr", "5A v CO", "5A v CF", "CF v CO"),
       col = c(gray(0.5), gray(0), gray(0.25)), pch = c(16, 15, 17, 93, 125, 41), lty = c(2,1,3,0,0,0), 
       cex = 1, horiz= TRUE)

# mtext("***", side = 3, line = -8.7, outer = T, at= .327, cex = .8)
# mtext("]", side = 3, line = -9.1, outer = T, at= 0.305, cex = 2.9, 
#       col = grey(0))
# mtext("***", side = 3, line = -22.5, outer = T, at= .325, cex = .9)
# mtext("]", side = 3, line = -22.5, outer = T, at= 0.31, cex = 1.9, 
#       col = grey(0))
# mtext("***", side = 3, line = -37.25, outer = T, at= .325, cex = .9)
# mtext("]", side = 3, line = -37.25, outer = T, at= 0.31, cex = 2.75, 
#       col = grey(0))
# mtext("***", side = 3, line = -37.25, outer = T, at= .325, cex = .9)
# mtext("]", side = 3, line = -52.1, outer = T, at= 0.31, cex = 3.1, 
#       col = grey(0))
# mtext("***", side = 3, line = -52.1, outer = T, at= .326, cex = .9)
# mtext("}", side = 3, line = -52, outer = T, at= 0.31, cex = 2.5, 
#       col = grey(0.4))
# mtext("***", side = 3, line = -37.25, outer = T, at= .325, cex = .9, 
#       col = grey(0.4))
# mtext(")", side = 3, line = -52, outer = T, at= 0.31, cex = 2.5, 
#       col = grey(0.6))
# mtext("***", side = 3, line = -37.25, outer = T, at= .325, cex = .9, 
#       col = grey(0.6))
# 
# mtext("***", side = 3, line = -8.5, outer = T, at= .64, cex = .9)
# mtext("]", side = 3, line = -8.5, outer = T, at= 0.635, cex = 1.9, col = grey(0))
# mtext("***", side = 3, line = -22.5, outer = T, at= .64, cex = .9)
# mtext("]", side = 3, line = -22.5, outer = T, at= 0.635, cex = 1.9, col = grey(0))
# mtext("***", side = 3, line = -37.25, outer = T, at= .64, cex = .9)
# mtext("]", side = 3, line = -37.25, outer = T, at= 0.635, cex = 2.75, col = grey(0))
# mtext("***", side = 3, line = -52, outer = T, at= .64, cex = .9)
# mtext("]", side = 3, line = -52, outer = T, at= 0.635, cex = 2.5, col = grey(0))
# mtext("}", side = 3, line = -52, outer = T, at= 0.635, cex = 2.5, col = grey(0.4))
# mtext(")", side = 3, line = -52, outer = T, at= 0.635, cex = 2.5, col = grey(0.6))
# 
# mtext("***", side = 3, line = -8.5, outer = T, at= .96, cex = .9)
# mtext("]", side = 3, line = -8.5, outer = T, at= 0.955, cex = 1.9, col = grey(0))
# mtext("***", side = 3, line = -22.5, outer = T, at= .96, cex = .9)
# mtext("]", side = 3, line = -22.5, outer = T, at= 0.955, cex = 1.9, col = grey(0))
# mtext("***", side = 3, line = -37.25, outer = T, at= .96, cex = .9)
# mtext("]", side = 3, line = -37.25, outer = T, at= 0.955, cex = 2.75, col = grey(0))
# mtext("***", side = 3, line = -52, outer = T, at= .96, cex = .9)
# mtext("]", side = 3, line = -52, outer = T, at= 0.955, cex = 2.5, col = grey(0))
# mtext("}", side = 3, line = -52, outer = T, at= 0.955, cex = 2.5, col = grey(0.4))
# mtext(")", side = 3, line = -52, outer = T, at= 0.955, cex = 2.5, col = grey(0.5))

#mtext("***", side = 3, line = -50, outer = T, at= 0.5)
#mtext("***", side = 3, line = -50, outer = T, at= 0.5)

#dev.off()