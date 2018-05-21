#===================================================================================
# Code to make the polygons for each of the cameras
# mayo 2018 justin
#===================================================================================
#===================================================================================
# Part I: Dependencies, libraries
#===================================================================================
library("devtools")
library("sp")
library("videoplayR")
library("splancs")

#===================================================================================
# Part II: Function to make the polygons
#===================================================================================
makePolyExp <- function(dia, cam) {
  # Create the polygon from the 20th image of the first hour video of the camera
  setwd(paste0("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day", dia, "/cam", cam))
  vid  <- readVid(paste0("DAY", dia, "_CAM", cam, "_1HR.mp4"))
  imshow(getFrame(vid, 20))
  poly <- getpoly(quiet=FALSE)
  
  # Make poly into a readable format
  polyREORG <- c(poly[1,1], poly[1,2], 
                 poly[2,1], poly[2,2],
                 poly[3,1], poly[3,2],
                 poly[4,1], poly[4,2])
  
  # Assign the polygon to the polygon variable name
  varName <- paste0("POLYDAY", dia, "CAM", cam)
  assign(varName, polyREORG)
}

#===================================================================================
# Part III: Make the polygons for each of the 32 cameras
#===================================================================================
#===================================================================================
# Dia 1
#===================================================================================
makePolyExp(dia=1, cam=1)
makePolyExp(dia=1, cam=2)
makePolyExp(dia=1, cam=3)
makePolyExp(dia=1, cam=4)

#===================================================================================
# Dia 2
#===================================================================================
makePolyExp(dia=2, cam=1)
makePolyExp(dia=2, cam=2)
makePolyExp(dia=2, cam=3)
makePolyExp(dia=2, cam=4)

#===================================================================================
# Dia 3
#===================================================================================
makePolyExp(dia=3, cam=1)
makePolyExp(dia=3, cam=2)
makePolyExp(dia=3, cam=3)
makePolyExp(dia=3, cam=4)

#===================================================================================
# Dia 4
#===================================================================================
makePolyExp(dia=4, cam=1)
makePolyExp(dia=4, cam=2)
makePolyExp(dia=4, cam=3)
makePolyExp(dia=4, cam=4)

#===================================================================================
# Dia 5
#===================================================================================
makePolyExp(dia=5, cam=1)
makePolyExp(dia=5, cam=2)
makePolyExp(dia=5, cam=3)
makePolyExp(dia=5, cam=4)

#===================================================================================
# Dia 6
#===================================================================================
makePolyExp(dia=6, cam=1)
makePolyExp(dia=6, cam=2)
makePolyExp(dia=6, cam=3)
makePolyExp(dia=6, cam=4)

#===================================================================================
# Dia 7
#===================================================================================
makePolyExp(dia=7, cam=1)
makePolyExp(dia=7, cam=2)
makePolyExp(dia=7, cam=3)
makePolyExp(dia=7, cam=4)

#===================================================================================
# Dia 8
#===================================================================================
makePolyExp(dia=8, cam=1)
makePolyExp(dia=8, cam=2)
makePolyExp(dia=8, cam=3)
makePolyExp(dia=8, cam=4)

#===================================================================================
# Part IV: Combine everything so it can be saved and used in the variables file
#===================================================================================
c("POLYDAY1CAM1", POLYDAY1CAM1)
