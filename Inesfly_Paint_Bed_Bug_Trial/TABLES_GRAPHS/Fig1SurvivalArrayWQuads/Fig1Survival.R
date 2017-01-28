###Code to create Fig1SurvivalArrayWQuads
install.packages("extrafont")

##make two working directory paths
#One containing the data
inwd <- paste("/Users/dtracy198/Documents/GitHub/Laboratory/",
              "Inesfly_Paint_Bed_Bug_Trial/TABLES_GRAPHS",
              "/Fig1SurvivalArrayWQuads", sep = "")
#A second for the location the file is saved. (image may be too large for git)
outwd <- ("/Users/dtracy198/Documents")

setwd(inwd)

###Bring in the data
quadsum <- read.csv("quadsum.csv")
twenties<- which(quadsum$pch == 20)
quadsum$pch[twenties] <- 16

#jpeg("TABLES_GRAPHS/Bioassay_Array/Bioassay_Graphs_Array_Quads.jpeg", width = 6, 
#     height = 9, units = "in", res = 300)
#tiff("TABLES_GRAPHS/Bioassay_Array/Bioassay_Graphs_Array_Quads.jpeg", width = 6, 
#     height = 9, units = "in", res = 300)
dap <- unique(quadsum$days.after.paint)
dap <- dap[order(dap)]
ext <- unique(quadsum$exp.time)
par(mfrow = c(4,3), oma = c(1,1,2,1)) #4 across 3 
for(k in 1:length(ext)){
  tsdap <- which(quadsum$exp.time == ext[k])
  for(j in 1:length(dap)){
    tsext <- which(quadsum$days.after.paint == dap[j])
    tsde <- intersect(tsdap, tsext)  
    #d <- "days"
    #if(dap[j] == 1){d <- "day"}
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
mtext("Proportion of Alive Bugs", side = 3, line = 0, outer = TRUE, cex = 1.2)
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

mtext("***", side = 3, line = -8.5, outer = T, at= .325, cex = .9)
mtext("]", side = 3, line = -8.5, outer = T, at= 0.31, cex = 1.9, col = grey(0.2))
mtext("***", side = 3, line = -22.5, outer = T, at= .325, cex = .9)
mtext("]", side = 3, line = -22.5, outer = T, at= 0.31, cex = 1.9, col = grey(0.2))
mtext("***", side = 3, line = -37.25, outer = T, at= .325, cex = .9)
mtext("]", side = 3, line = -37.25, outer = T, at= 0.31, cex = 2.75, col = grey(0.2))
mtext("***", side = 3, line = -37.25, outer = T, at= .325, cex = .9)
mtext("]", side = 3, line = -37.25, outer = T, at= 0.31, cex = 2.5, col = grey(0.2))
mtext("}", side = 3, line = -37.25, outer = T, at= 0.31, cex = 2.5, col = grey(0.2))
mtext(")", side = 3, line = -30.25, outer = T, at= 0.31, cex = 2.5, col = grey(0.2))

mtext("***", side = 3, line = -8.5, outer = T, at= .64, cex = .9)
mtext("]", side = 3, line = -8.5, outer = T, at= 0.635, cex = 1.9, col = grey(0.2))
mtext("***", side = 3, line = -22.5, outer = T, at= .64, cex = .9)
mtext("]", side = 3, line = -22.5, outer = T, at= 0.635, cex = 1.9, col = grey(0.2))
mtext("***", side = 3, line = -37.25, outer = T, at= .64, cex = .9)
mtext("]", side = 3, line = -37.25, outer = T, at= 0.635, cex = 2.75, col = grey(0.2))
mtext("***", side = 3, line = -52, outer = T, at= .64, cex = .9)
mtext("]", side = 3, line = -52, outer = T, at= 0.635, cex = 2.5, col = grey(0.2))
mtext("}", side = 3, line = -52, outer = T, at= 0.635, cex = 2.5, col = grey(0.2))
mtext(")", side = 3, line = -52, outer = T, at= 0.635, cex = 2.5, col = grey(0.2))

mtext("***", side = 3, line = -8.5, outer = T, at= .96, cex = .9)
mtext("]", side = 3, line = -8.5, outer = T, at= 0.955, cex = 1.9, col = grey(0.2))
mtext("***", side = 3, line = -22.5, outer = T, at= .96, cex = .9)
mtext("]", side = 3, line = -22.5, outer = T, at= 0.955, cex = 1.9, col = grey(0.2))
mtext("***", side = 3, line = -37.25, outer = T, at= .96, cex = .9)
mtext("]", side = 3, line = -37.25, outer = T, at= 0.955, cex = 2.75, col = grey(0.2))
mtext("***", side = 3, line = -52, outer = T, at= .96, cex = .9)
mtext("]", side = 3, line = -52, outer = T, at= 0.955, cex = 2.5, col = grey(0.2))
mtext("}", side = 3, line = -52, outer = T, at= 0.31, cex = 2.5, col = grey(0.2))
mtext(")", side = 3, line = -52, outer = T, at= 0.31, cex = 2.5, col = grey(0.2), font = 2)

#mtext("***", side = 3, line = -50, outer = T, at= 0.5)
#mtext("***", side = 3, line = -50, outer = T, at= 0.5)

##dev.off() 
