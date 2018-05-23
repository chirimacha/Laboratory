#####################################################
#Cimex on life tables (T cruzi-infected and control)
#code to plot parasitemia in mice
#Written by Renzo Salazar
#updated:dic 2016
#####################################################

### R packages being used:
# Use the commented command if the package is not installed
# install.packages("ggplot2")
#install.packages("reshape2")
# install.packages("RColorBrewer")

#load required packages
library("ggplot2")
library("reshape")
library("RColorBrewer")


#setting working directory
setwd("D:/LAB/ENSAYOS/CICLO VIDA CIMEX-CRUZI/R code")
getwd()

###############
## PDF file  ##
###############
#Uncomment the line below to create a pdf file with all graphics in current directory
#pdf(file = "mice parasitemia cimex on lifetables assay t.pdf")

######################################################################################
# Loading the data
db <- read.csv("parasitemia.csv")
head (db)
#adding column to show parasite number^6
db$par<-db$nro.Parasitos.ml/1000000

#removing not usefull columns to reshape and plot

dbp<-db[,c(1,4,13)]
colnames(dbp)<-c("rep","time","paras")
head(dbp)

#ploting data changing leyend name and labels, 
#be carefull to modify each scale parameter used (colour, fill...)

ggplot(dbp, aes(x = time, y = paras, colour = rep)) +
  geom_line(aes(size=rep))+
  scale_size_manual(values=c(1.3,1.3,1.3), name="Legend",
                    breaks=c("rep1","rep2","rep3"),
                    labels=c("mouse rep1","mouse rep2","mouse rep3"))+
  scale_colour_hue(name="Legend", breaks=c("rep1","rep2","rep3"),
                   labels=c("mouse rep1","mouse rep2","mouse rep3"))+
  ggtitle("Figure 1. Parasitemia per week") + xlab("Weeks post inoculation") + ylab(" Parasites/ml x 10^6") +
  theme(text = element_text(size=16),plot.title = element_text(hjust = 0.5),axis.text.x=element_text(size=12),axis.text.y=element_text(size=12),legend.position="bottom")+
  geom_vline(aes(xintercept=2),linetype="dashed")+
  geom_vline(aes(xintercept=5),linetype="dashed")+
  annotate("text",x=3.8,y=0.9, label="weeks feeding bedbugs")+
  annotate("text",x=3.8,y=0.4, label="for infected cohorts")+
  annotate("rect",xmin=2, xmax=5,ymin=0,ymax=5.6, alpha=0.2)
 
  
#another customized features 
#be carefull to modify each scale parameter used to change labels in legend (colour, fill...)
ggplot(dbp, aes(x = time, y = paras, color = rep, fill = rep)) +
  geom_line(aes(linetype=rep, size= rep)) + 
  scale_color_manual(values = c("#E31A1C","#E31A1C","#E31A1C"),name="Legend",
                     breaks=c("rep1","rep2","rep3"),
                     labels=c("mouse rep1","mouse rep2","mouse rep3")) +
  scale_linetype_manual(values=c("solid","dotted","dotdash"),name="Legend",
                        breaks=c("rep1","rep2","rep3"),
                        labels=c("mouse rep1","mouse rep2","mouse rep3"))+
  scale_size_manual(values=c(1.3,1.3,1.3),
                    breaks=c("rep1","rep2","rep3"),name="Legend",
                    labels=c("mouse rep1","mouse rep2","mouse rep3"))+
  ggtitle("Figure 1. Parasitemia per week") + xlab("Weeks post inoculation") + ylab(" Parasites/ml x 10^6") +
  theme(text = element_text(size=18),plot.title = element_text(hjust = 0.5),axis.text.x=element_text(size=14),axis.text.y=element_text(size=14),legend.position="bottom")+
  geom_vline(aes(xintercept=2),linetype="dashed")+
  geom_vline(aes(xintercept=5),linetype="dashed")+
  annotate("text",x=3.8,y=0.9, label="weeks feeding bedbugs")+
  annotate("text",x=3.8,y=0.4, label="for infected cohorts")+
  annotate("rect",xmin=2, xmax=5,ymin=0,ymax=5.6, alpha=0.2)

#uncomment next line to finish PDF file (just in case lne # 28 was uncommented)
#dev.off()
##################################################################