#####################################################
#Cimex on life tables (T cruzi-infected and control)
#code for total life span graphics
#Written by Renzo Salazar
#updated:dic 2016
#######################################################

### R packages being used in case needed:
# Use the commented command if the package is not installed
#install.packages("reshape")
#install.packages("ggplots")
#install.packages("RColorBrewer")

#loading needed libraries
library("reshape")
library("ggplot2")
library("RColorBrewer")  


####################################################################

#getting working directory uncomment if needed
setwd("D:/LAB/ENSAYOS/CICLO VIDA CIMEX-CRUZI/R code")
#getwd()

# Load csv file
stcimex <- read.csv(file = 'survival total cimex.csv')
dim(stcimex)
colnames(stcimex)
head(stcimex)
tail(stcimex)

#dead by week indicate how much weeks has alive each insect
pull<- stcimex[stcimex$repeticion == "pull",]
dim(pull)
cocx<-pull[pull$grupo=="control",c(4,22)]
infcx<-pull[pull$grupo=="infectado",c(4,22)]
cocx

#transform into a long vectors
coxw<-rep(cocx$semana,cocx$Muetxweek)
infxw<-rep(infcx$semana,infcx$Muetxweek)
length(infxw)

# as co and inf have a different length (CO=240, INF=280), 
#is needed to add NAs to get the same length to built a dataframe
coxw1 = c(coxw, rep(NA, length(infxw) - length(coxw)))

#built a new data frame
newdb<-as.data.frame(cbind(coxw1,infxw))
head(newdb)

#reshape dataframe
colnames(newdb)<-c("control", "infected")
longdb<-na.omit(melt(newdb))
colnames(longdb)<-c("cohort","time")
head(longdb)

############################################################################
#Violin plot with boxplot
############################################################################
ggplot(longdb, aes(x=cohort, y=time, fill=cohort))+
  geom_violin()+
  scale_fill_manual(values=c("#1F78B4","#E94849"))+
  ggtitle("Total life span by infection status") + xlab("Cohort") + ylab(" Time (weeks)") +
  theme(text = element_text(size=16),plot.title = element_text(hjust = 0.5),axis.text.x=element_text(size=12),axis.text.y=element_text(size=12))+
  geom_boxplot(width=0.05,fill="white")

############################################################################
#Histogram for total life span
############################################################################

#using the same dataframe to violin plot
ggplot(longdb, aes(x=time, fill=cohort))+
  geom_histogram(binwidth=5, alpha=.6, position="identity",col=I("black"))+
  scale_fill_manual(values=c("#1F78B4","#E94849"))+
  ggtitle("Histogram Total life span") + xlab("Time(weeks)") + ylab("Insects") +
  theme(text = element_text(size=16),plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=12),axis.text.y=element_text(size=12))


##################################################################################
#multiple violin plots by reps
##################################################################################
#prepare data frame
#separate data frame by infection status and select columns to work
#dead by week indicate how much weeks has alive each insect
coreps<-stcimex[stcimex$grupo=="control",c(1,4,22)]
infreps<-stcimex[stcimex$grupo=="infectado",c(1,4,22)]

#repeat values to long data
colnames(coreps)
corepsexpan <- coreps[rep(row.names(coreps), coreps$Muetxweek), c(1,2)]
summary(corepsexpan)
infrepsexpan <- infreps[rep(row.names(infreps), infreps$Muetxweek), c(1,2)]
summary(infrepsexpan)
dim(corepsexpan)

#join data frames
reps<-data.frame(rbind(corepsexpan,infrepsexpan))
dim(reps)
#add column to identify cohorts
reps$cohort<-c(rep("control",length(corepsexpan$repeticion)),rep("infected",length(infrepsexpan$repeticion)))
colnames(reps)<-c("group","time","cohort")


#built the plot
#########################################
#set same position to violin and boxplot
dodge <- position_dodge(width = 0.6)
#plot
ggplot(reps, aes(x=group, y=time, fill=cohort))+
  geom_violin(alpha=.6,position = dodge)+
  scale_fill_manual(values=c("#1F78B4","#E94849"))+
  ggtitle("Total life span by repetition") + xlab("Repetition") + ylab(" Time (weeks)") +
  theme(text = element_text(size=16),plot.title = element_text(hjust = 0.5),
        axis.text.x=element_text(size=12),axis.text.y=element_text(size=12),
        legend.position="bottom")+
  geom_boxplot(width=0.1,position=dodge)
  


