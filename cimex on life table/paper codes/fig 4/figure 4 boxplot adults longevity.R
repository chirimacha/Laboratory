###########################################################
#Cimex on life tables (T cruzi-infected and control)
# Code to graph 4: boxplot adult longevity  by replicate      
# Written by Renzo Salazar                          
# updated: September 2017
##########################################################

### R packages being used:
# Use the commented command if the package is not installed
# install.packages("ggplot2")
#install.packages("reshape")
# install.packages("RColorBrewer")
# install.packages("gridExtra")
# install.packages("grid")
# install.packages("gtable")
#install.packages("sjPlot")
#install.packages("SparseM")
#install.packages("car")
#install.packages("plyr")

#loading needed libraries
library("ggplot2")
library("reshape")
library("RColorBrewer")
library("gridExtra")
library("grid")
library("gtable")
library("sjPlot")
library("SparseM")
library("car")
library("plyr")

#setting working directory
#setwd()
getwd()
###############
## PDF file  ##
###############
#Uncomment the line below to create a pdf file with all graphics in current directory
#pdf(file = "figure 4 boxplot adults longevity.pdf",width=8,height=6,paper='special')
###############################################################################
#read csvs
df<-read.csv(file="adults in reproduction.csv")
dim(df)
colnames(df)
summary(df)
#replace NA's by 0
df[is.na(df)] <- 0

#############################
#setting data frame
############################
n.adult<-apply(df[,6:53], 1, function(x) list(ADULT.LONGEVITY=sum(x,na.rm=TRUE)))
n.adult.sum <- ldply (n.adult, data.frame)

#built data frames to plot
adultdf<-data.frame(df[,c(1:3,5)],n.adult.sum)
max(adultdf$ADULT.LONGEVITY)

adultdfm<-adultdf
adultdfm$replicate<-adultdfm$cohort
#replace values in replicate
adultdfm$replicate<-revalue(adultdfm$replicate,c("rep1A"="Replicate 1","rep1B"="Replicate 1",
                                                 "rep2A"="Replicate 2","rep2B"="Replicate 2",
                                                 "rep3A"="Replicate 3","rep3B"="Replicate 3"))

#reorder levels
levels(adultdfm$code)
adultdfm$code <- factor(adultdfm$code, levels = c("co-F", "inf-F", "co-M", "inf-M"))
# change values to code variable
adultdfm$code<-revalue(adultdfm$code,c("co-F"="Control", "inf-F"="Infected",
                                       "co-M"="Control ", "inf-M"="Infected "))

#adding all in replicates
todo<-adultdfm
#replace values in todo$replicate
todo$replicate<-revalue(todo$replicate,c("Replicate 1"="All Replicates",
                                         "Replicate 2"="All Replicates",
                                         "Replicate 3"="All Replicates"))
#paste data frames
ndf<-data.frame(rbind(adultdfm,todo))



###############################################
# Plotting data
####################################
#  fill by sex (white, grey)
#####################################

p2<-ggplot(ndf,aes(x=code, y=ADULT.LONGEVITY, fill=sex))+
  geom_boxplot(width=.7, alpha=0.8)+
  stat_summary(fun.y=mean, geom="point", shape=8, size=3)+
  geom_point(aes(colour=code),position = position_jitter(width = 0.15),size=1.5)+
  scale_fill_manual(labels=c("Female","Male"),
                    values=c("white","gray70"))+ 
  scale_colour_manual(values=c("blue","red","blue","red"))+ 
  scale_y_continuous(breaks = c(seq(from=0,to=50,by=5)),limits = c(0,50))+
  facet_grid(~replicate)+
  labs(fill = "Sex") +
  guides(colour=FALSE)+
  theme_bw(base_size = 15)+
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Weeks")

p2

# dev.off()

###########
# END
###########