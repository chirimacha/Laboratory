#######################################################
#Cimex on life tables (T cruzi-infected and control)
# Code to graph n: boxplot lx by stage / gx by stage     
# Written by Renzo Salazar                          
# updated: November 2017
######################################################

### R packages being used:
# Use the commented command if the package is not installed
# install.packages("ggplot2")
#install.packages("reshape")
# install.packages("RColorBrewer")
# install.packages("gridExtra")
# install.packages("grid")
# install.packages("gtable")
# install.packages("sjPlot")
#install.packages("sparseM")
#install.packages("car")

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


#setting working directory
#setwd()
getwd()
###############
## PDF file  ##
###############
#Uncomment the line below to create a pdf file with all graphics in current directory
#pdf(file = "figure 2 lx gx.pdf",width=7,height=4,paper='special')
###############################################################################
#read csvs
lt<-read.csv(file="lifetable cohorts.csv")
head(lt)
dim(lt)
colnames(lt)

#####################################
#A. setting data to lx 
#####################################
# extract usefull columns
nn<-c(25:60)
ltc<-lt[nn,c(1,2,4,10)]
#melt dataframe
ltcm<-melt(ltc)
colnames(ltcm)
#replace values in variable
levels(ltcm$variable)
ltcm$variable<-recode(ltcm$variable,'"lx"="control";"X.lx."="infected"')
#rename columns
colnames(ltcm)<-c("cohort","stage","Treatment","lx")
#reorder columns
ltcm<-ltcm[,c(3,1,2,4)]

# ploting data to lx
##########################################
plx<-ggplot(ltcm,aes(x=Treatment, y=lx, fill=Treatment))+
  geom_boxplot(width=.5, alpha=0.2)+
  stat_summary(fun.y=mean, geom="point", shape=8, size=3)+
  geom_point(aes(colour=Treatment),position = position_jitter(width = 0.15),size=2)+
  scale_fill_manual(values=c("#1F78B4","#E94849"))+ 
  scale_colour_manual(values=c("blue","red"))+ 
  scale_y_continuous(breaks = c(seq(from=0.2,to=1,by=0.1)),limits = c(0.2,1))+
  facet_grid(~stage)+
  labs(y = expression("Survival probability, l "[x]))+
  theme_bw(base_size = 15)+
  theme( axis.title.x=element_blank(),axis.text.x=element_blank(), 
         axis.ticks.x=element_blank())
plx

#####################################################
# B.Setting data to gx (n1-n5)
####################################################
# extract usefull columns
nnn<-c(25:60)
nn1<-seq(from=6, to=36, by=6)
nnn<-nnn[-nn1]
ltcg<-lt[nnn,c(1,2,5,11)]
#melt dataframe
ltcmg<-melt(ltcg)
colnames(ltcmg)
#replace values in variable
levels(ltcmg$variable)
ltcmg$variable<-recode(ltcmg$variable,'"gx"="control";"X.gx."="infected"')
#rename columns
colnames(ltcmg)<-c("cohort","stage","Treatment","gx")
#reorder columns
ltcmg<-ltcmg[,c(3,1,2,4)]

# ploting data to gx
##########################################
pgx<-ggplot(ltcmg,aes(x=Treatment, y=gx, fill=Treatment))+
  geom_boxplot(width=.5, alpha=0.2)+
  stat_summary(fun.y=mean, geom="point", shape=8, size=3)+
  geom_point(aes(colour=Treatment),position = position_jitter(width = 0.15),size=2)+
  scale_fill_manual(values=c("#1F78B4","#E94849"))+ 
  scale_colour_manual(values=c("blue","red"))+ 
  scale_y_continuous(breaks = c(seq(from=0.2,to=1,by=0.1)),limits = c(0.2,1))+
  facet_grid(~stage)+
  labs(y = expression("Stage-specific survival probability, g "[x]))+
  theme_bw(base_size = 15)+
  theme( axis.title.x=element_blank(),axis.text.x=element_blank(), 
         axis.ticks.x=element_blank())

pgx

#dev.off()

#############
# END
############