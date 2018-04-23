############################################################
# Code for insecticide drywall analysis 2nd, 3rd replicate
#written by Renzo Salazar
# updated january 2018
############################################################

### R packages being used:
# Use the commented command if the package is not installed

# install.packages("ggplot2")
#install.packages("reshape2")
# install.packages("RColorBrewer")
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/survminer")
#install.packages("gridExtra")
#install.packages("cowplot") #or
#devtools::install_github("wilkelab/cowplot")

#load required packages
library("ggplot2")
library("reshape")
library("reshape2")
library("RColorBrewer")
library("survminer")
require("survival")
library("gridExtra")
library("cowplot")
library("plyr")


#setting working directory
setwd("D:/LABORATORIO/ENSAYOS/BIOENSAYO INSECTICIDA DRYWALL")
getwd()

######################################################################################
######################################################################################
#cimex data deltametrine in drywall 1st assay
######################################################################################
######################################################################################
# Reading the data
cmx <- read.csv("cimex drywall ldb.csv")
head (cmx)

#Adding new columns and values for proportions
cmx$prop_v<-cmx$vivo/cmx$nro_insectos
cmx$prop_c<-cmx$caido/cmx$nro_insectos
cmx$prop_m<-cmx$muerto/cmx$nro_insectos

#separate blank (blank) from delta treatments.
blank <- cmx[cmx$tratamiento == "blanco",]
delta <- cmx[cmx$tratamiento == "deltametrina  SC 5%",]
row.names(blank)<-NULL
row.names(delta)<-NULL

#arrange data base with useful rows( delete pre tratamiento)
dim(blank)
dim(delta)
blank<- blank[43:406,]
delta<- delta[43:406,]

#removing not usefull columns to reshape and data analysis survival insects
colnames(blank)
blankdb<-blank[,c(1,2,10,11,12)]
deltadb<-delta[,c(1,2,10,11,12)]
#removing not usefull rows (first elavuation period 21 days post exp to insecticide effect)
blankdb<-blankdb[blankdb$dia_post_exp<22,]
deltadb<-deltadb[deltadb$dia_post_exp<22,]
colnames(blankdb)<-c("time","colony","surv_prop","kd_prop","dead_prop")
colnames(deltadb)<-c("time","colony","surv_prop","kd_prop","dead_prop")
blankdb0<-blankdb
deltadb0<-deltadb
#remove not resistant colonies to compare with rep 2 and 3
row.names(blankdb)<-NULL
row.names(deltadb)<-NULL
a<-seq(from=1, to= 308, by=14)
b<-seq(from=2, to= 308, by=14)
c<-seq(from=6, to= 308, by=14)
d<-seq(from=7, to= 308, by=14)
e<-seq(from=8, to= 308, by=14)
f<-seq(from=9, to= 308, by=14)
g<-seq(from=10, to= 308, by=14)
h<-seq(from=13, to= 308, by=14)

blankdb1<-blankdb[c(a,b,c,d,e,f,g,h),]
deltadb1<-deltadb[c(a,b,c,d,e,f,g,h),]

##############################
#ploting data
##############################
#survival plots
g1<-ggplot(data=blankdb1, aes(x=time, y=surv_prop, group=colony, colour=colony, fill=colony))+
  geom_line()+
  geom_point( size=2, shape=21)+
  ggtitle("cimex survival blank") + xlab("Time (days)") + ylab("Proportion")+
  #annotate("text",x=-1.6,y=0.4, label="exp period")+
  #annotate("text",x=10,y=0.4, label="post exp period")+
  annotate("rect",xmin=0, xmax=22,ymin=0,ymax=0.5, alpha=0.15)+
  ylim(0,1)

h1<-ggplot(data=deltadb1, aes(x=time, y=surv_prop, group=colony, colour=colony, fill=colony))+
  geom_line()+
  geom_point( size=2, shape=21)+
  ggtitle("cimex survival insecticide exposed group") + xlab("Time (days)") + ylab("Proportion")+
  annotate("rect",xmin=0, xmax=22,ymin=0,ymax=0.5, alpha=0.15)+
  ylim(0,1)

######################################################################################
######################################################################################
#cimex drywall 2nd assay
######################################################################################
######################################################################################
# Reading the data
cmx <- read.csv("BIOENSAYO CIMEX DRYWALL 2DA REPETICION.csv")
head (cmx)

#Adding new columns and values for proportions
cmx$prop_v<-cmx$vivo/cmx$nro_insectos
cmx$prop_c<-cmx$caido/cmx$nro_insectos
cmx$prop_m<-cmx$muerto/cmx$nro_insectos

#separate blank (blank) from delta treatments.
blank <- cmx[cmx$tratamiento == "BLANCO",]
delta <- cmx[cmx$tratamiento == "INSECTICIDA",]

#removing not usefull columns to reshape and data analysis survival insects
colnames(blank)
blankdb<-blank[,c(1,2,10,11,12)]
deltadb<-delta[,c(1,2,10,11,12)]
#removing not usefull rows (first elavuation period 21 days post exp to insecticide effect)
blankdb2<-blankdb[blankdb$dia_post_exp<22,]
deltadb2<-deltadb[deltadb$dia_post_exp<22,]
colnames(blankdb2)<-c("time","colony","surv_prop","kd_prop","dead_prop")
colnames(deltadb2)<-c("time","colony","surv_prop","kd_prop","dead_prop")
tail(blankdb2)
head(deltadb2)

##############################
#ploting data
##############################
#survival plots
g<-ggplot(data=blankdb2, aes(x=time, y=surv_prop, group=colony, colour=colony, fill=colony))+
  geom_line()+
  geom_point( size=2, shape=21)+
  ggtitle("cimex survival blank") + xlab("Time (days)") + ylab("Proportion")+
  annotate("rect",xmin=0, xmax=22,ymin=0,ymax=0.5, alpha=0.15)+
  ylim(0,1)
g

h<-ggplot(data=deltadb2, aes(x=time, y=surv_prop, group=colony, colour=colony, fill=colony))+
  geom_line()+
  geom_point( size=2, shape=21)+
  ggtitle("cimex survival insecticide exposed group") + xlab("Time (days)") + ylab("Proportion")+
  annotate("rect",xmin=0, xmax=22,ymin=0,ymax=0.5, alpha=0.15)+
  ylim(0,1)
h
#############################################################################
#############################################################################
#############################################################################
#cimex drywall 3nd assay
######################################################################################
######################################################################################
# Reading the data
cmx <- read.csv("BIOENSAYO CIMEX DRYWALL 3RA REPETICION.csv")
head (cmx)

#Adding new columns and values for proportions
cmx$prop_v<-cmx$vivo/cmx$nro_insectos
cmx$prop_c<-cmx$caido/cmx$nro_insectos
cmx$prop_m<-cmx$muerto/cmx$nro_insectos

#separate blank (blank) from delta treatments.
blank <- cmx[cmx$tratamiento == "BLANCO",]
delta <- cmx[cmx$tratamiento == "INSECTICIDA",]

#removing not usefull columns to reshape and data analysis survival insects
colnames(blank)
blankdb<-blank[,c(1,2,10,11,12)]
deltadb<-delta[,c(1,2,10,11,12)]
#removing not usefull rows (first elavuation period 21 days post exp to insecticide effect)
blankdb3<-blankdb[blankdb$dia_post_exp<22,]
deltadb3<-deltadb[deltadb$dia_post_exp<22,]
colnames(blankdb3)<-c("time","colony","surv_prop","kd_prop","dead_prop")
colnames(deltadb3)<-c("time","colony","surv_prop","kd_prop","dead_prop")
tail(blankdb)
head(deltadb)

##############################
#ploting data
##############################
#survival plots
gg<-ggplot(data=blankdb3, aes(x=time, y=surv_prop, group=colony, colour=colony, fill=colony))+
  geom_line()+
  geom_point( size=2, shape=21)+
  ggtitle("cimex survival blank") + xlab("Time (days)") + ylab("Proportion")+
  annotate("rect",xmin=0, xmax=22,ymin=0,ymax=0.5, alpha=0.15)+
  ylim(0,1)
gg

hh<-ggplot(data=deltadb3, aes(x=time, y=surv_prop, group=colony, colour=colony, fill=colony))+
  geom_line()+
  geom_point( size=2, shape=21)+
  ggtitle("cimex survival insecticide exposed group") + xlab("Time (days)") + ylab("Proportion")+
  annotate("rect",xmin=0, xmax=22,ymin=0,ymax=0.5, alpha=0.15)+
  ylim(0,1)
hh

##############################################################################
#############################################################################
# MEANS FROM REP1,2,3
##################################################
dim(deltadb3)
#reorder db rep 2, 3
a<-seq(from=1, to= 176, by=8)
b<-seq(from=2, to= 176, by=8)
c<-seq(from=3, to= 176, by=8)
d<-seq(from=4, to= 176, by=8)
e<-seq(from=5, to= 176, by=8)
f<-seq(from=6, to= 176, by=8)
gs<-seq(from=7, to= 176, by=8)
hs<-seq(from=8, to= 176, by=8)

blankdb2<-blankdb2[c(a,b,c,d,e,f,gs,hs),]
deltadb2<-deltadb2[c(a,b,c,d,e,f,gs,hs),]

blankdb3<-blankdb3[c(a,b,c,d,e,f,gs,hs),]
deltadb3<-deltadb3[c(a,b,c,d,e,f,gs,hs),]

#nueva base de datos pra medias
newbdf<-data.frame(cbind(blankdb1,blankdb2[,3:5],blankdb3[,3:5]))
newddf<-data.frame(cbind(deltadb1,deltadb2[,3:5],deltadb3[,3:5]))
colnames(newbdf)

#blank
newbdf["surv_mean"]<-NA
for(i in 1:176) {
  newbdf[i,12]<-rowMeans(newbdf[i,c(3,6,9)])}

newbdf["caida_mean"]<-NA
for(i in 1:176) {
  newbdf[i,13]<-rowMeans(newbdf[i,c(4,7,10)])}

newbdf["death_mean"]<-NA
for(i in 1:176) {
  newbdf[i,14]<-rowMeans(newbdf[i,c(5,8,11)])}
#delta

newddf["surv_mean"]<-NA
for(i in 1:176) {
  newddf[i,12]<-rowMeans(newddf[i,c(3,6,9)])}

newddf["caida_mean"]<-NA
for(i in 1:176) {
  newddf[i,13]<-rowMeans(newddf[i,c(4,7,10)])}

newddf["death_mean"]<-NA
for(i in 1:176) {
  newddf[i,14]<-rowMeans(newddf[i,c(5,8,11)])}

########################
# performing plots
########################
#survival plots
gs<-ggplot(data=newbdf, aes(x=time, y=surv_mean, group=colony, colour=colony, fill=colony))+
  geom_line()+
  geom_point( size=2, shape=21)+
  ggtitle("Mean cimex survival -Control") + xlab("Time (days)") + ylab("Proportion")+
  #annotate("text",x=-1.6,y=0.4, label="exp period")+
  #annotate("text",x=10,y=0.4, label="post exp period")+
  annotate("rect",xmin=0, xmax=22,ymin=0,ymax=0.5, alpha=0.15)+
  ylim(0,1)
gs

hs<-ggplot(data=newddf, aes(x=time, y=surv_mean, group=colony, colour=colony, fill=colony))+
  geom_line()+
  geom_point( size=2, shape=21)+
  ggtitle("Mean cimex survival - Insecticide exposed group") + xlab("Time (days)") + ylab("Proportion")+
  annotate("rect",xmin=0, xmax=22,ymin=0,ymax=0.5, alpha=0.15)+
  ylim(0,1)
hs

#############################################################################
#############################################################################
#############################################################################
#cimex drywall 4TH assay (NEW COLONIES)
######################################################################################
######################################################################################
# Reading the data
cmx <- read.csv("BIOENSAYO CIMEX DRYWALL 4TA REPETICION.csv")
head (cmx)

#Adding new columns and values for proportions
cmx$prop_v<-cmx$vivo/cmx$nro_insectos
cmx$prop_c<-cmx$caido/cmx$nro_insectos
cmx$prop_m<-cmx$muerto/cmx$nro_insectos

#separate blank (blank) from delta treatments.
blank <- cmx[cmx$tratamiento == "BLANCO",]
delta <- cmx[cmx$tratamiento == "INSECTICIDA",]

#removing not usefull columns to reshape and data analysis survival insects
colnames(blank)
blankdb<-blank[,c(1,2,10,11,12)]
deltadb<-delta[,c(1,2,10,11,12)]
#removing not usefull rows (first elavuation period 21 days post exp to insecticide effect)
blankdb3<-blankdb[blankdb$dia_post_exp<22,]
deltadb3<-deltadb[deltadb$dia_post_exp<22,]
colnames(blankdb3)<-c("time","colony","surv_prop","kd_prop","dead_prop")
colnames(deltadb3)<-c("time","colony","surv_prop","kd_prop","dead_prop")
tail(blankdb)
head(deltadb)

##############################
#ploting data
##############################
#survival plots
gg4<-ggplot(data=blankdb3, aes(x=time, y=surv_prop, group=colony, colour=colony, fill=colony))+
  geom_line()+
  geom_point( size=2, shape=21)+
  ggtitle("cimex survival(new colonies)- control") + xlab("Time (days)") + ylab("Proportion")+
  annotate("rect",xmin=0, xmax=22,ymin=0,ymax=0.5, alpha=0.15)+
  facet_wrap(~colony, ncol = 3)+
  ylim(0,1)+
  geom_hline(yintercept = .5)+
  geom_hline(yintercept = .25, linetype="dashed", color="gray50")+
  geom_hline(yintercept = .75, linetype="dashed", color="gray50")

gg4

hh4<-ggplot(data=deltadb3, aes(x=time, y=surv_prop, group=colony, colour=colony, fill=colony))+
  geom_line()+
  geom_point( size=2, shape=21)+
  ggtitle("cimex survival(new Colonies) - insecticide exposed group") + xlab("Time (days)") + ylab("Proportion")+
  annotate("rect",xmin=0, xmax=22,ymin=0,ymax=0.5, alpha=0.15)+
  facet_wrap(~colony, ncol = 3)+
  ylim(0,1)+
  geom_hline(yintercept = .5)+
  geom_hline(yintercept = .25, linetype="dashed", color="gray50")+
  geom_hline(yintercept = .75, linetype="dashed", color="gray50")

hh4

########################################################################
#        GETTING MEAN, SD SE, IC   (merge dataframes all reps)         #
########################################################################
colnames(blankdb3)
colnames(deltadb1)
tblank<-rbind(blankdb0[,1:3],blankdb1[,1:3],blankdb2[,1:3],blankdb3[,1:3])
tdelta<-rbind(deltadb0[,1:3],deltadb1[,1:3],deltadb2[,1:3],deltadb3[,1:3])
#add columns to treatment and repetition
tblank["repetition"]<-c(rep("Repetition 1",nrow(blankdb0)),
                        rep("Repetition 1",nrow(blankdb1)),
                        rep("Repetition 2",nrow(blankdb2)),
                        rep("Repetition 3",nrow(blankdb3)))
tblank["treatment"]<-rep("Control",nrow(tblank))

tdelta["repetition"]<-c(rep("Repetition 1",nrow(deltadb0)),
                        rep("Repetition 1",nrow(deltadb1)),
                        rep("Repetition 2",nrow(deltadb2)),
                        rep("Repetition 3",nrow(deltadb3)))
tdelta["treatment"]<-rep("deltamethrin",nrow(tblank))


#join dfs
tdf<-data.frame(rbind(tblank,tdelta))

tdfsum <- ddply(tdf, c( "treatment","colony","time"), summarise,
                  N    = sum(!is.na(surv_prop)),
                  mean = round(mean(surv_prop, na.rm=TRUE),digits = 3),
                  sd   = round(sd(surv_prop, na.rm=TRUE),digits=3),
                  se   = round(sd / sqrt(N),digits = 3),
                  LCI  = round(mean-(1.96*se),digits = 3),
                  UCI  = round(mean+(1.96*se),digits = 3))

#add column to survival score
tdfsum["surv_score"]<-NA

for(i in 1:nrow(tdfsum)){
    if(tdfsum[i,2]=="CX1"|tdfsum[i,2]=="CX10"|tdfsum[i,2]=="CX2"|tdfsum[i,2]=="CX6"|
     tdfsum[i,2]=="CX8"|tdfsum[i,2]=="CX9"|tdfsum[i,2]=="CX18"|tdfsum[i,2]=="CX19"|
     tdfsum[i,2]=="CX21"|tdfsum[i,2]=="CX23"){tdfsum[i,10]<-"over 50"}
  }

for(i in 1:nrow(tdfsum)){
    if(tdfsum[i,2]=="CX5"|tdfsum[i,2]=="CX7"|tdfsum[i,2]=="CX11"|tdfsum[i,2]=="CX15"){tdfsum[i,10]<-"under 25"}
  }

for(i in 1:nrow(tdfsum)){
  if(tdfsum[i,2]=="CX20"|tdfsum[i,2]=="CX13"|tdfsum[i,2]=="CX4"|tdfsum[i,2]=="CX3"){tdfsum[i,10]<-"50 to 75"}
}

for(i in 1:nrow(tdfsum)){
  if(tdfsum[i,2]=="CX16"|tdfsum[i,2]=="CX12"|tdfsum[i,2]=="CX14"){tdfsum[i,10]<-"25 to 50"}
}

tdfsum$surv_score[is.na(tdfsum$surv_score)]<-"A"

for(i in 1:nrow(tdfsum)){
    if(tdfsum[i,10]=="A"){tdfsum[i,10]<-"middle"}
  }

#prepare data frame to plot
levels(tdfsum$colony)
#reorder levels
tdfsum$colony <-factor(tdfsum$colony,levels(tdfsum$colony)[c(1,7:14,2:6,15:24)])
levels(tdfsum$colony)
class(tdfsum$surv_score)
tdfsum$surv_score<-as.factor(tdfsum$surv_score)
levels(tdfsum$surv_score)
tdfsum$surv_score <-factor(tdfsum$surv_score,levels(tdfsum$surv_score)[c(4,2,1,5,3)])
levels(tdfsum$surv_score)

########################################################
#plot
########################################################
colnames(tdfsum)
gt1<-ggplot(tdfsum, aes(x =time, y = mean, color = colony))+
  #geom_errorbar(aes(ymin=LCI, ymax=UCI), alpha=1, width = 0.15, position= position_jitter(width = 0.15)) +
  geom_line(size=.8,alpha=.6)+ 
 # stat_smooth()+
  geom_point(size=1)+
  #geom_point(position = position_jitter(width = 0.1),size=1)+
    #scale_colour_manual(values=c("darkgreen","gray30"))+
  xlab("Time (days)") +
  ylab("survival proportion (mean)") +
  scale_y_continuous(breaks = c(seq(from=0,to=1,by=.25)),limits = c(0,1))+
  scale_x_continuous(breaks = c(seq(from=0,to=20,by=5)),limits = c(0,21))+
  facet_grid(surv_score~treatment)+
  theme_bw()+
  theme(text = element_text(size=12),axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=10))+
  geom_hline(yintercept = .5)+
  geom_hline(yintercept = .25, linetype="dashed", color="gray50")+
  geom_hline(yintercept = .75, linetype="dashed", color="gray50")
gt1

#split data frame to better view
levels(tdfsum$surv_score)
df1<-tdfsum[tdfsum$surv_score=="over 50",]
df2<-tdfsum[tdfsum$surv_score=="50 to 75",]
df3<-tdfsum[tdfsum$surv_score=="25 to 50",]
df4<-tdfsum[tdfsum$surv_score=="under 25",]
df5<-tdfsum[tdfsum$surv_score=="middle",]

gt2<-ggplot(df1, aes(x =time, y = mean, color = colony))+
  geom_line(size=.8,alpha=.6)+ 
  geom_point(size=1)+
  xlab("Time (days)") +
  ylab("survival proportion (mean)") +
  scale_y_continuous(breaks = c(seq(from=0,to=1,by=.25)),limits = c(0,1))+
  scale_x_continuous(breaks = c(seq(from=0,to=20,by=5)),limits = c(0,21))+
  facet_grid(surv_score~treatment)+
  theme_bw()+
  theme(text = element_text(size=12),axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=10))+
  geom_hline(yintercept = .5)+
  geom_hline(yintercept = .25, linetype="dashed", color="gray50")+
  geom_hline(yintercept = .75, linetype="dashed", color="gray50")
gt2

gt3<-ggplot(df2, aes(x =time, y = mean, color = colony))+
  geom_line(size=.8,alpha=.6)+ 
  geom_point(size=1)+
  xlab("Time (days)") +
  ylab("survival proportion (mean)") +
  scale_y_continuous(breaks = c(seq(from=0,to=1,by=.25)),limits = c(0,1))+
  scale_x_continuous(breaks = c(seq(from=0,to=20,by=5)),limits = c(0,21))+
  facet_grid(surv_score~treatment)+
  theme_bw()+
  theme(text = element_text(size=12),axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=10))+
  geom_hline(yintercept = .5)+
  geom_hline(yintercept = .25, linetype="dashed", color="gray50")+
  geom_hline(yintercept = .75, linetype="dashed", color="gray50")
gt3

gt4<-ggplot(df3, aes(x =time, y = mean, color = colony))+
  geom_line(size=.8,alpha=.6)+ 
  geom_point(size=1)+
  xlab("Time (days)") +
  ylab("survival proportion (mean)") +
  scale_y_continuous(breaks = c(seq(from=0,to=1,by=.25)),limits = c(0,1))+
  scale_x_continuous(breaks = c(seq(from=0,to=20,by=5)),limits = c(0,21))+
  facet_grid(surv_score~treatment)+
  theme_bw()+
  theme(text = element_text(size=12),axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=10))+
  geom_hline(yintercept = .5)+
  geom_hline(yintercept = .25, linetype="dashed", color="gray50")+
  geom_hline(yintercept = .75, linetype="dashed", color="gray50")
gt4

gt5<-ggplot(df4, aes(x =time, y = mean, color = colony))+
  #geom_errorbar(aes(ymin=LCI, ymax=UCI), alpha=1, width = 0.15, position= position_jitter(width = 0.15)) +
  geom_line(size=.8,alpha=.6)+ 
  geom_point(size=1)+
  xlab("Time (days)") +
  ylab("survival proportion (mean)") +
  scale_y_continuous(breaks = c(seq(from=0,to=1,by=.25)),limits = c(0,1))+
  scale_x_continuous(breaks = c(seq(from=0,to=20,by=5)),limits = c(0,21))+
  facet_grid(surv_score~treatment)+
  theme_bw()+
  theme(text = element_text(size=12),axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=10))+
  geom_hline(yintercept = .5)+
  geom_hline(yintercept = .25, linetype="dashed", color="gray50")+
  geom_hline(yintercept = .75, linetype="dashed", color="gray50")
gt5

gt6<-ggplot(df5, aes(x =time, y = mean, color = colony))+
  #geom_errorbar(aes(ymin=LCI, ymax=UCI), alpha=1, width = 0.15, position= position_jitter(width = 0.15)) +
  geom_line(size=.8,alpha=.6)+ 
  geom_point(size=1)+
  xlab("Time (days)") +
  ylab("survival proportion (mean)") +
  scale_y_continuous(breaks = c(seq(from=0,to=1,by=.25)),limits = c(0,1))+
  scale_x_continuous(breaks = c(seq(from=0,to=20,by=5)),limits = c(0,21))+
  facet_grid(surv_score~treatment)+
  theme_bw()+
  theme(text = element_text(size=12),axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=10))+
  geom_hline(yintercept = .5)+
  geom_hline(yintercept = .25, linetype="dashed", color="gray50")+
  geom_hline(yintercept = .75, linetype="dashed", color="gray50")
gt6

##############################################################
#get the pdf
##############################################################
pdf(file = "survival of bedbugs exposed to deltamethrin.pdf", width=6,height=4,paper='special')#, paper="A4r",)

gt1
gt2
gt3
gt4
gt5
gt6
dev.off()
