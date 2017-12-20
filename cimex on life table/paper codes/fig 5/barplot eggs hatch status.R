###########################################################
# Cimex on life tables (T cruzi-infected and control)
# Code to Figure 5: barplot to eggs hatched and not hatched       
# Written by Renzo Salazar                          
# updated: November 2017
###########################################################

# R packages being used in case needed:
# Use the commented command if the package is not installed
#install.packages("ggplot2")
#install.packages("reshape")
#install.packages("xtable")
#install.packages("gridExtra")
#install.packages("grid")
#install.packages("gtable")
#install.packages("plyr")
#install.packages("sjPlot")

#loading needed libraries
library("ggplot2")
library("reshape")
library("xtable")
library("gridExtra")
library("grid")
library("gtable")
library("plyr")
library("sjPlot")

#setting working directory
setwd("D:/LAB/ENSAYOS/CICLO VIDA CIMEX-CRUZI/R code/paper ok")
getwd()
###############
## PDF file  ##
###############
#Uncomment the line below to create a pdf file with all graphics in current directory
#pdf(file = "figure 5 hatching.pdf")#),width=5,height=5,paper='special')
###############################################################################
#read csvs
reprod<-read.csv(file="cimex reproduction by cohort summary.csv")
head(reprod)
dim(reprod)

########################################################
#perform data frame and get totals by replicate
##########################################################
#reproduction parameters
colnames(reprod)
levels(reprod$cohort)

#getting usefull columns
df<-reprod[,c(1,2,8,9)]
colnames (df)
#add colun to replicate
df$replicate<-df$cohort
#replace values in replicate
df$replicate<-revalue(df$replicate,c("rep1A"="Replicate 1","rep1B"="Replicate 1",
                                     "rep2A"="Replicate 2","rep2B"="Replicate 2",
                                     "rep3A"="Replicate 3","rep3B"="Replicate 3"))

#add column to eggs not hatched
df$not.hatched<-df[,3]-df[,4]

#getting summary
df1<-aggregate(.~treatment+replicate, data = df, FUN=function(x) sum(x))

#reshape data frame
df2<-df1[,c(1,2,5,6)]
colnames(df2)<-c("treatment","Replicate","yes","no")
df2m<-melt(df2)
colnames(df2m)<-c("treatment","Replicate","Hatched","eggs.laid")

#Getting percentage %
df3<-df1[,1:2]
df3["yes"]<-round((df1[,5]/df1[,4])*100, digits = 1)
df3["no"]<-round((df1[,6]/df1[,4])*100, digits = 1)

#reshepe df with melt 
df3m<-melt(df3)
colnames(df3m)<-c(colnames(df3m[1:2]),"Hatched","Percentage")

#reorder levels
levels(df3m$Hatched)
df3m$Hatched <- factor(df3m$Hatched, levels = c("no","yes"))

###############################
#ploting data
################################
px<-ggplot(df3m,aes(x=treatment, y=Percentage, fill=Hatched, color=treatment))+
  geom_bar(width=.5, alpha=0.8,stat="identity",size=1)+
  scale_fill_manual(values=c("white","grey47"))+ 
  scale_colour_manual(values=c("grey20","grey20"))+
  facet_grid(~replicate)+
  scale_y_continuous(breaks = c(seq(from=0,to=100,by=10)),limits = c(0,100))+
  labs(fill = "Hatched ") +
   guides(colour=FALSE)+
  ylab("Percentage (%)") + 
  theme_bw(base_size = 15)+
  theme( axis.title.x=element_blank(),#axis.text.x=element_blank(), 
         legend.background = element_rect(fill="gray85", size=.5, linetype="dotted"))+
  geom_text(aes(label=paste0(Percentage, "%")), size=4, hjust=0.5, vjust=6, position = "stack")
px

#dev.off()

############
# END
############
