##############################################
#Cimex on life tables (T cruzi-infected and control)
# Code to nymphal development   
# Written by Renzo Salazar                          
# updated: September 2017
##############################################

### R packages being used:
# Use the commented command if the package is not installed
# install.packages("ggplot2")
#install.packages("reshape")
#install.packages("xtable")
# install.packages("gridExtra")
# install.packages("grid")
# install.packages("gtable")
# install.packages("plyr")

#loading needed libraries
library("ggplot2")
library("reshape")
library("xtable")
library("gridExtra")
library("grid")
library("gtable")
library("plyr")


#setting working directory
#setwd()
getwd()
###############
## PDF file  ##
###############
#Uncomment the line below to create a pdf file with all graphics in current directory
#pdf(file = "figure 3 nymphal development.pdf",width=7,height=4,paper='special')
###############################################################################
#read csvs
lt<-read.csv(file="nymphal development time.csv")
head(lt)
dim(lt)
colnames(lt)
class(lt$stage)
#reorder levels
levels(lt$stage)
lt$stage<-factor(lt$stage, levels = c("n5","n4","n3","n2","n1"))

#add columns to max sd min sd
lt$maxsd<-lt$mean.sum+lt$sd
lt$minsd<-round(lt$mean.sum-lt$sd,digits = 2)
#add code column
lt$code<-paste(lt$treatment,lt$stage,sep = " ")
#add column code
levels(lt$code)
lt$code<-factor(lt$code, levels = c("control n5","control n4","control n3","control n2",
                                    "control n1","infected n5","infected n4","infected n3",
                                    "infected n2","infected n1"))

#replace values in replicate
lt$replicate<-revalue(lt$replicate,c("replicate 1"="Replicate 1","replicate 2"="Replicate 2",
                                     "replicate 3"="Replicate 3","all replicates"="All Replicates"))

#reorder levels in replicates
levels(lt$replicate)
lt$replicate <- factor(lt$replicate, levels = c("Replicate 1", "Replicate 2", "Replicate 3", "All Replicates"))

#ploting data
############################
# a) Fill: white
############################
etkc<-lt[1:20,c(1:4,6)]
etkc["media"]<-round(etkc[,4]/2,digits = 2)
etkc["center"]<-etkc[,5]-etkc[,6]

etki<-lt[21:40,c(1:4,6)]
etki["media"]<-round(etki[,4]/2,digits = 2)
etki["center"]<-etki[,5]-etki[,6]

P<-ggplot(lt,aes(x=treatment, y=mean, fill= treatment, color=treatment))+
  geom_bar(width=.8, alpha=0.9,size=1,position="stack",stat="identity")+
  scale_fill_manual(values=c("white","white"))+ 
  geom_errorbar(aes(ymin=minsd, ymax=maxsd, color=stage), width=.4, size=0.8, position = position_dodge(width = -0.3))+
  scale_colour_manual(values=c("black","black","black","black","black","black",
                               "black","blue","red"),guide=FALSE)+
  facet_grid(~replicate)+
  scale_y_continuous(breaks = c(seq(from=0,to=16,by=2)),limits = c(0,16))+
  labs(color = "Treatment ") +
  guides(fill=FALSE)+
  ylab("Weeks") + xlab("Treatment")+
  theme_bw(base_size = 15)+
  theme( axis.title.x=element_blank(),#axis.text.x=element_blank(), 
         axis.ticks.x=element_blank())+
  geom_text(data=etkc, aes(label=paste(stage),color=c("pink")), x=0.7, y=etkc$center)+
  geom_text(data=etki, aes(label=paste(stage),color=c("yellow")), x=1.7, y=etki$center)

P

######################################
#b) Fill: color scale and legend
######################################
P1<-ggplot(lt,aes(x=treatment, y=mean, fill=code, color=treatment))+
  geom_bar(width=.5, alpha=0.3,position="stack",stat="identity")+
  scale_fill_manual(values=c("#0548bc","#045df7","#3d81f7","#6a9df7","#8bb3f9",
                             "#c10303","#f90202","#f72e2e","#f75656","#f98989"))+ 
  geom_errorbar(aes(ymin=minsd, ymax=maxsd, color=code), width=.7, position = "dodge")+
  scale_colour_manual(values=c("blue","black","black","black","black","black",
                               "red","black","black","black","black","black"))+
  facet_grid(~replicate)+
  scale_y_continuous(breaks = c(seq(from=0,to=16,by=2)),limits = c(0,16))+
  labs(color = " ", fill="Treatment & Stage") +
  guides(colour=FALSE)+
  ylab("Weeks") + xlab("Treatment")+
  theme_bw(base_size = 15)+
  theme( axis.title.x=element_blank(),#axis.text.x=element_blank(), 
         axis.ticks.x=element_blank())

P1

#dev.off()

###########
# END
###########