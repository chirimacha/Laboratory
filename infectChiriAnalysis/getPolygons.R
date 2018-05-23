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
makePolyExp <- function(dia, cam, csvFile) {
  # Load video
  if (is.na(csvFile)) {
    # Create the polygon from the 1000th image of the first hour video of the camera
    setwd(paste0("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/FIXED CSVs/day", dia, "/cam", cam))
    vid  <- readVid(paste0("DAY", dia, "_CAM", cam, "_1HR.mp4"))
  } else {
    setwd(paste0("/Volumes/TOSHIBA_EXT/BACKUP_UPCH_AQP_21oct2015/JUSTIN_ASSAY/VIDEOS_FIRST_ASSAY/day_", dia, "/cam", cam, "(preprocessed)"))
    vid  <- readVid(paste0(csvFile, ".mp4"))
  }
  
  # Manually make polygon
  imshow(getFrame(vid, 1000))
  poly <- getpoly(quiet=FALSE)
  
  return(poly)
}

#===================================================================================
# Part III: Make the polygons for each of the 32 cameras
# IMPORTANT: Go from TL to TR to BR to BL when making the polygon
#===================================================================================
#===================================================================================
# Dia 1
#===================================================================================
POLYDAY1CAM1 <- makePolyExp(dia=1, cam=1, "2016_09_19 17_54_59.dur.60 min")
POLYDAY1CAM2 <- makePolyExp(dia=1, cam=2, "2016_09_19 18_00_04.dur.60.min")
POLYDAY1CAM3 <- makePolyExp(dia=1, cam=3, "2016_09_19 18_00_26.dur.60.min")
POLYDAY1CAM4 <- makePolyExp(dia=1, cam=4, "2016_09_19 17_59_39.dur.60.min")

#===================================================================================
# Dia 2
#===================================================================================
POLYDAY2CAM1 <- makePolyExp(dia=2, cam=1, "2016_09_20 18_59_49 dur 60 min")
POLYDAY2CAM2 <- makePolyExp(dia=2, cam=2, "2016_09_20 18_01_34 dur 60 min")
POLYDAY2CAM3 <- makePolyExp(dia=2, cam=3, "2016_09_20 18_01_53 dur 60 min")
POLYDAY2CAM4 <- makePolyExp(dia=2, cam=4, "2016_09_20 18_01_06 dur 60 min")

#===================================================================================
# Dia 3
#===================================================================================
POLYDAY3CAM1 <- makePolyExp(dia=3, cam=1, "2016_09_22 17_45_50 dur 60 min")
POLYDAY3CAM2 <- makePolyExp(dia=3, cam=2, "2016_09_22 17_46_48 dur 60 min")
POLYDAY3CAM3 <- makePolyExp(dia=3, cam=3, "2016_09_22 17_45_55 dur 60 min")
POLYDAY3CAM4 <- makePolyExp(dia=3, cam=4, "2016_09_22 17_46_05 dur 60 min")

#===================================================================================
# Dia 4
#===================================================================================
POLYDAY4CAM1 <- makePolyExp(dia=4, cam=1, "2016_09_23 17_50_07 dur 60 min")
POLYDAY4CAM2 <- makePolyExp(dia=4, cam=2, "2016_09_23 17_51_27 dur 60 min")
POLYDAY4CAM3 <- makePolyExp(dia=4, cam=3, "2016_09_23 17_51_48 dur 60 min")
POLYDAY4CAM4 <- makePolyExp(dia=4, cam=4, "2016_09_23 17_50_58 dur 60 min")

#===================================================================================
# Dia 5
#===================================================================================
POLYDAY5CAM1 <- makePolyExp(dia=5, cam=1, NA)
POLYDAY5CAM2 <- makePolyExp(dia=5, cam=2, NA)
POLYDAY5CAM3 <- makePolyExp(dia=5, cam=3, NA)
POLYDAY5CAM4 <- makePolyExp(dia=5, cam=4, NA)

#===================================================================================
# Dia 6
#===================================================================================
POLYDAY6CAM1 <- makePolyExp(dia=6, cam=1, NA)
POLYDAY6CAM2 <- makePolyExp(dia=6, cam=2, NA)
POLYDAY6CAM3 <- makePolyExp(dia=6, cam=3, NA)
POLYDAY6CAM4 <- makePolyExp(dia=6, cam=4, NA)

#===================================================================================
# Dia 7
#===================================================================================
POLYDAY7CAM1 <- makePolyExp(dia=7, cam=1, NA)
POLYDAY7CAM2 <- makePolyExp(dia=7, cam=2, NA)
POLYDAY7CAM3 <- makePolyExp(dia=7, cam=3, NA)
POLYDAY7CAM4 <- makePolyExp(dia=7, cam=4, NA)

#===================================================================================
# Dia 8
#===================================================================================
POLYDAY8CAM1 <- makePolyExp(dia=8, cam=1, NA)
POLYDAY8CAM2 <- makePolyExp(dia=8, cam=2, NA)
POLYDAY8CAM3 <- makePolyExp(dia=8, cam=3, NA)
POLYDAY8CAM4 <- makePolyExp(dia=8, cam=4, NA)

#===================================================================================
# Part IV: Combine everything so it can be saved and used in the variables file
#===================================================================================
left <- c(rep("POLYDAY1CAM1",4), rep("POLYDAY1CAM2",4), rep("POLYDAY1CAM3",4), rep("POLYDAY1CAM4",4),
         rep("POLYDAY2CAM1",4), rep("POLYDAY2CAM2",4), rep("POLYDAY2CAM3",4), rep("POLYDAY2CAM4",4),
         rep("POLYDAY3CAM1",4), rep("POLYDAY3CAM2",4), rep("POLYDAY3CAM3",4), rep("POLYDAY3CAM4",4),
         rep("POLYDAY4CAM1",4), rep("POLYDAY4CAM2",4), rep("POLYDAY4CAM3",4), rep("POLYDAY4CAM4",4),
         rep("POLYDAY5CAM1",4), rep("POLYDAY5CAM2",4), rep("POLYDAY5CAM3",4), rep("POLYDAY5CAM4",4),
         rep("POLYDAY6CAM1",4), rep("POLYDAY6CAM2",4), rep("POLYDAY6CAM3",4), rep("POLYDAY6CAM4",4),
         rep("POLYDAY7CAM1",4), rep("POLYDAY7CAM2",4), rep("POLYDAY7CAM3",4), rep("POLYDAY7CAM4",4),
         rep("POLYDAY8CAM1",4), rep("POLYDAY8CAM2",4), rep("POLYDAY8CAM3",4), rep("POLYDAY8CAM4",4))
right <- rbind(POLYDAY1CAM1, POLYDAY1CAM2, POLYDAY1CAM3, POLYDAY1CAM4,
               POLYDAY2CAM1, POLYDAY2CAM2, POLYDAY2CAM3, POLYDAY2CAM4,
               POLYDAY3CAM1, POLYDAY3CAM2, POLYDAY3CAM3, POLYDAY3CAM4,
               POLYDAY4CAM1, POLYDAY4CAM2, POLYDAY4CAM3, POLYDAY4CAM4,
               POLYDAY5CAM1, POLYDAY5CAM2, POLYDAY5CAM3, POLYDAY5CAM4,
               POLYDAY6CAM1, POLYDAY6CAM2, POLYDAY6CAM3, POLYDAY6CAM4,
               POLYDAY7CAM1, POLYDAY7CAM2, POLYDAY7CAM3, POLYDAY7CAM4,
               POLYDAY8CAM1, POLYDAY8CAM2, POLYDAY8CAM3, POLYDAY8CAM4)
output <- cbind(left, right)

filename <- file.path("~/Desktop/Laboratory/infectChiriAnalysis", paste0("polygons.csv"))
write.table(output, filename, row.names = F)
