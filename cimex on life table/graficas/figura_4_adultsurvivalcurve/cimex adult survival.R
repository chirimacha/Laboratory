###########################################################
#Cimex on life tables (T cruzi-infected and control)
# Code to cimex adult survival by sex graph         
# Written by Renzo Salazar                          
#updated: dic 2016
###########################################################

# R packages being used in case needed:
# Use the commented command if the package is not installed
#install.packages("reshape")
#install.packages("ggplot2")
#install.packages("RColorBrewer")

#loading needed libraries
library("ggplot2")
library("reshape")
library("RColorBrewer")

#setting working directory
setwd("D:/LAB/ENSAYOS/CICLO VIDA CIMEX-CRUZI/R code")
getwd()

#read csv
adsurv<-read.csv(file="cimex adult survival.csv")
head(adsurv)

#using just pull columns
colnames(adsurv)
pullad<-adsurv[,c(14:19)]
head(pullad)

#to get proportions
#create a empty db
dim(pullad)
pulladprop<-data.frame(matrix(NA,nrow=length(adsurv$weeks),ncol=6))
dim(pulladprop)

#fill rows with proportions, dividing by the first row, 
#other way is using sweep function, create a vector with 1st row values.
pulladprop<-pullad/c(pullad[1,])
colnames(pulladprop)<-c("co male","co female","inf male","inf female","co tot adults","inf tot adults")
colnames(pulladprop)

#adding time column (weeks)
pulladprop$time<-c(1:48)
head(pulladprop)

# uncomment if is required the dataframe in CSV file 
#write.csv(pulladprop, file = "pulladprop.csv")

#reshape database to plot.
adprop<-melt(pulladprop, id="time")
colnames(adprop)<-c("time","cohort","proportion")
head(adprop)

# create a basic plot with survival proportion ~ cohort 
ggplot(adprop, aes(x = time, y = proportion, color = cohort, fill = cohort)) +
  geom_line()

#customizing survival curves plot
ggplot(adprop, aes(x = time, y = proportion, color = cohort, fill = cohort)) +
  geom_line(aes(linetype=cohort, size= cohort)) + 
  scale_color_manual(values = c("#1F78B4","#1F78B4","#E31A1C","#E31A1C","#1F78B4","#E31A1C")) +
  scale_linetype_manual(values=c("dotted","dashed","dotted","dashed","solid","solid"))+
  scale_size_manual(values=c(1.3,0.9,1.3,0.9,1.3,1.3))+
  ggtitle("Adulthood Survival by sex") + xlab("Time (Weeks)") + ylab("Proportion") +
  theme(text = element_text(size=24),plot.title = element_text(hjust = 0.5),axis.text.x=element_text(size=18),axis.text.y = element_text(size=18))

#########################################################################################
