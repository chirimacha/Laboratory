# Analysis of results

library(vcd)
library(fitdistrplus)

setwd("/Users/Justin/Desktop/Laboratory/infectChiriAnalysisJustin/data")
rawData <- read.csv("rawData10022018.csv")

infected <- rawData[which(rawData$treatment == "infected"),]
control <- rawData[which(rawData$treatment == "control"),]

# total distance
hist(infected$total_distance)
f1 = fitdist(infected$total_distance[infected$total_distance!=0],  densfun="gamma", list(shape = 1, rate = 0.1), lower = 0.4)
gf<-goodfit(infected$total_distance[infected$total_distance!=0], type="gamma",method= "MinChisq")
summary(gf)
hist(control$total_distance)

# time spent in open (away from wall)
hist(infected$total_opentime)
hist(control$total_opentime)

gf <- goodfit(infected$total_distance, "poisson")
plot(gf, type = "standing", scale = "raw")