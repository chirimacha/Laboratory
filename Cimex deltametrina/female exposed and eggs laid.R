##############################################################
# Code for analysis to oviposition from cimex female exposed  
#written by Renzo Salazar
#updated march 2018
##############################################################

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
#install.packages("data.table")
#install.packages("agricolae")
#install.packages("multcompView")
#install.packages("ggpmisc")

#load required packages
library("ggplot2")
library("reshape")
library("reshape2")
library("RColorBrewer")
library("plyr")
library("data.table")
library("agricolae")
library("knitr")
library("xtable")
library("gridExtra")
library("grid")
library("gtable")
library("plotrix")
library("multcompView")
library("ggpmisc")

################################
#setting working directory
setwd("D:/LABORATORIO/ENSAYOS/BIOENSAYO INSECTICIDA DRYWALL")
getwd()
####################
# Reading the data
####################
huevos <- read.csv("BASE HUEVO PILOTO II.csv")
colnames (huevos)
levels(huevos$colonia)
a<-huevos[!is.na(huevos$huevos_w1),]
#add column code
a["code"]<-paste(a$tratamiento,a$colonia, sep = "-")

#separar puestos de eclosionados 
laid<-a[,c(1:3,6,8,10,12,14,16,18,20,22)]
hatch<-a[,c(1:3,6,9,11,13,15,17,19,21,23)]

#melt data frame laid
#laidcx1<-laid[laid$colonia=="CX1",]
mlaid<-melt(laid, id.vars = c("tratamiento","colonia","nro.pareja","X2da_exposicion"))

#replace values in variable (weeks)
mlaid$variable<-revalue(mlaid$variable,c("huevos_w1"="1 ","huevos_w2"="2 ","huevos_w3"="3 ",
                                     "huevos_w4"="4 ","huevos_w5"="5 ","huevos_w6"="6 ","huevos_w7"="7 ","huevos_w8"="8 "))
levels(mlaid$variable)
#change values of 2nd exposition
colnames(mlaid)
levels(mlaid$X2da_exposicion)
mlaid$X2da_exposicion<-revalue(mlaid$X2da_exposicion,c("NO"="with out 2nd expsition","SI"="with 2nd expsition"))
levels(mlaid$X2da_exposicion)

#melt data frame HATCH
#hatch<-laid[laid$colonia=="CX1",]
colnames(hatch)
sapply(hatch,class)
mhatch<-melt(hatch, id.vars = c("tratamiento","colonia","nro.pareja","X2da_exposicion"))

#replace values in variable (weeks)
mhatch$variable<-revalue(mhatch$variable,c("heclo_w1"="1 ","heclo_w2"="2 ","heclo_w3"="3 ",
                                         "heclo_w4"="4 ","heclo_w5"="5 ","heclo_w6"="6 ","heclo_w7"="7 ","heclo_w8"="8 "))
levels(mhatch$variable)

#change values of 2nd exposition
colnames(mhatch)
levels(mhatch$X2da_exposicion)
mhatch$X2da_exposicion<-revalue(mhatch$X2da_exposicion,c("NO"="with out 2nd expsition","SI"="with 2nd expsition"))
levels(mhatch$X2da_exposicion)

b<-aggregate(.~colonia+tratamiento, data=huevos[,c(1,2,5)],FUN=function(x) round(mean(x),digits = 2))
c<-aggregate(.~colonia+tratamiento, data=huevos[,c(1,2,5)],FUN=function(x) round(median(x),digits = 2))

#################################
# BOXPLOTS PARA HUEVOS PUESTOS
#####################################
#A) PLOT BY TRATAMIENTO Y SEGUNDA EXPOSICION
p2<-ggplot(mlaid,aes(x=variable, y=value, fill=X2da_exposicion))+
  geom_boxplot(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=8, size=3)+
  scale_fill_manual(labels=c("Sin II exposicion","Con II exposicion"),
                    values=c("#1F78B4","#E94849"))+
  scale_y_continuous(breaks = c(seq(from=0,to=25,by=5)),limits = c(0,25))+
  facet_grid(~tratamiento)+
  labs(fill = " ") +
  guides(colour=FALSE)+
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        #axis.title.x=element_blank(),# axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("n eggs laid by female")+
  xlab("weeks")+
  geom_vline(xintercept = 3.5, color = "red", size=1)+
  annotate("text", label = "weeks post 2nd exposition", x = 6, y = 24, size = 3, colour = "red")
p2

#PLOT JUST BY TREATMENT
p2B<-ggplot(mlaid,aes(x=variable, y=value))+
  geom_boxplot(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=8, size=3)+
  geom_point(aes(colour=tratamiento),position = position_jitter(width = 0.05),size=.5)+
  scale_y_continuous(breaks = c(seq(from=0,to=25,by=5)),limits = c(0,25))+
  facet_grid(X2da_exposicion~tratamiento)+
  guides(colour=FALSE)+
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        #axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  xlab("weeks") +
  ylab("n laid eggs by female")+
  geom_vline(xintercept = 3.5, color = "red", size=1)+
  annotate("text", label = c("","","weeks post 2nd exposition","weeks post 2nd exposition"), x = 6, y = 20, size = 3, colour = "red")

p2B

#######################################################################
#######################################################################
#######################################################################
## huevos eclosionados
p2h<-ggplot(mhatch,aes(x=variable, y=value, fill=X2da_exposicion))+
  geom_boxplot(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=8, size=3)+
  scale_fill_manual(labels=c("Sin II exposicion","Con II exposicion"),
                    values=c("#1F78B4","#E94849"))+
  scale_y_continuous(breaks = c(seq(from=0,to=25,by=5)),limits = c(0,25))+
  facet_grid(~tratamiento)+
  labs(fill = " ") +
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        #axis.title.x=element_blank(), #axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("n hatch eggs")+
  xlab("weeks")+
  geom_vline(xintercept = 3.5, color = "red", size=1)+
  annotate("text", label = "weeks post 2nd exposition", x = 6, y = 24, size = 3, colour = "red")

p2h

#PLOT JUST BY TREATMENT
p2Bh<-ggplot(mhatch,aes(x=variable, y=value))+
  geom_boxplot(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=8, size=3)+
  geom_point(aes(colour=tratamiento),position = position_jitter(width = 0.05),size=.5)+
  scale_y_continuous(breaks = c(seq(from=0,to=25,by=5)),limits = c(0,25))+
  facet_grid(X2da_exposicion~tratamiento)+
  guides(colour=FALSE)+
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        #axis.title.x=element_blank(),#axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  xlab("weeks") +
  ylab("n hatch eggs")+
  geom_vline(xintercept = 3.5, color = "red", size=1)+
  annotate("text", label = "weeks post 2nd exposition", x = 6, y = 24, size = 3, colour = "red")

p2Bh

#########################################################################
#plot means laid and hatching
colnames(mlaid)
sapply(mlaid,class)
colnames(mhatch)
sapply(mhatch,class)

############################################
#summarize data to get mean, se,sd, N
mlaid1<-mlaid
mlaid1$value[mlaid1$value==0]<-NA
mlaid1<-na.omit(mlaid1)
laidsum <- ddply(mlaid1, c( "tratamiento","X2da_exposicion","variable"), summarise,
               N    = sum(!is.na(value)),
               mean = round(mean(value, na.rm=TRUE),digits = 1),
               sd   = round(sd(value, na.rm=TRUE),digits=1),
               se   = round(sd / sqrt(N),digits = 1),
               LCI  = round(mean-(1.96*se),digits = 1),
               UCI  = round(mean+(1.96*se),digits = 1))

mhatch1<-na.omit(mhatch)

hatchsum <- ddply(mhatch1, c( "tratamiento","X2da_exposicion","variable"), summarise,
                 N    = sum(!is.na(value)),
                 mean = round(mean(value, na.rm=TRUE),digits = 1),
                 sd   = round(sd(value, na.rm=TRUE),digits=1),
                 se   = round(sd / sqrt(N),digits = 1),
                 LCI  = round(mean-(1.96*se),digits = 1),
                 UCI  = round(mean+(1.96*se),digits = 1))

#merge tables
eggssum<-data.frame(rbind(laidsum,hatchsum))
eggssum["eggs"]<-c(rep("laid",nrow(laidsum)),rep("hatch",nrow(hatchsum)))
colnames(eggssum)[3]<-"week"
sapply(eggssum, class)
eggssum$week<-as.integer(eggssum$week)

#change values of 2nd exposition
levels(eggssum$X2da_exposicion)
eggssum$X2da_exposicion<-revalue(eggssum$X2da_exposicion,c("NO"="with out 2nd expsition",
                                                       "SI"="with 2nd expsition"))
class(eggssum$week)
eggssum$week<-as.numeric(eggssum$week)

#ploting
pp<-ggplot(eggssum, aes(x =week, y = mean, color = eggs))+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), alpha=1, width = 0.15, position= position_jitter(width = 0.15)) +
  geom_line(size=.8,alpha=.6)+ 
  geom_point(position = position_jitter(width = 0.1),size=1.5)+
  scale_colour_manual(values=c("darkgreen","gray30"))+
  xlab("Time (Weeks)") +
  ylab("eggs (mean, 95% CI) ") +
  scale_y_continuous(breaks = c(seq(from=0,to=20,by=4)),limits = c(0,20))+
  scale_x_continuous(breaks = c(seq(from=0,to=8,by=1)),limits = c(0,8.5))+
  facet_grid(X2da_exposicion~tratamiento)+
  theme_bw()+
  theme(legend.position="bottom",text = element_text(size=8),axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=8))+
  geom_vline(xintercept = 3.5, color = "red", size=1)+
  annotate("text", label = c("","","weeks post 2nd exposition","weeks post 2nd exposition"), x = 6, y = 20, size = 3, colour = "red")

pp

#############################################################################
############################################################################
###### SURVIVAL PROPORTIONS
############################################################################
## Reading the data
db <- read.csv("SURVIVAL PILOTO II.csv")
colnames (db)
db<-db[,c(1,4,6,8:15)]
#remove empty rows
df<-db[!is.na(db$w1),]

#get survival mean (first get the numer of weeks alive by each bug)
colnames(df)
df["tot.weeks"]<-NA
for(i in 1:nrow(df)){
df[i,12]<-sum(df[i,4:11],na.rm = T)
}

promedio<-aggregate(.~sexo+X2da_exposicion+tratamiento, data=df[,c(1,2,3,12)],FUN=function(x) round(mean(x),digits = 1))

# get proportions by week to plot survival
dfw<-aggregate(cbind(w1,w2,w3,w4,w5,w6,w7,w8)~sexo+X2da_exposicion+tratamiento, data=df[,c(1,2,3,4:11)],FUN=function(x) sum(x,na.rm = T))
class(df$w8)
dfwp<-dfw
for(j in 4:ncol(dfw)){
  for(k in 1:nrow(dfw)){
    dfwp[k,j]<-round(dfw[k,j]/dfw[k,4],digits = 2)
  }
}

#reshape df
ndf<-melt(dfwp, id.vars = c("tratamiento","sexo","X2da_exposicion"))
#revalue weeks
ndf$variable<-revalue(ndf$variable,c("w1"=1,"w2"=2,"w3"=3,"w4"=4,"w5"=5,"w6"=6,"w7"=7,"w8"=8))
#change values of 2nd exposition
colnames(ndf)
levels(ndf$X2da_exposicion)
ndf$X2da_exposicion<-revalue(ndf$X2da_exposicion,c("NO"="with out 2nd expsition","SI"="with 2nd expsition"))
                                                     
#and change class
class(ndf$variable)
ndf$variable<-as.numeric(ndf$variable)
ndf1<-ndf[ndf$sexo=="hembra",]

#############################################
#plot survival curves
g<-ggplot(ndf, aes(x =variable, y = value, color = tratamiento))+#, fill = Cohorts)) +
  geom_line(aes(linetype=sexo),size=1)+#, size= Cohorts)) + 
  xlab("Time (Weeks)") +
  ylab("Proportion Alive") +
  scale_y_continuous(breaks = c(seq(from=0,to=1,by=.2)),limits = c(0,1))+
  scale_x_continuous(breaks = c(seq(from=0,to=8,by=1)),limits = c(0,8))+
  facet_grid(X2da_exposicion~tratamiento)+
  theme(legend.position="bottom",text = element_text(size=12),axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12))+
geom_vline(xintercept = 3.5, color = "black", size=1)+
annotate("text", label = c("","","weeks post 2nd exposition","weeks post 2nd exposition"), x = 6, y = .3, size = 3, colour = "red")

g
##########################################################################
#PLOT N EGGS LAID + SURVIVAL
pn<-p2+ geom_line(data = ndf1,aes(x=variable,y=value*25, color=X2da_exposicion),size=1, alpha=0.8)+
    scale_y_continuous(sec.axis = sec_axis(~./25, name = "female survival proportion"))+
    scale_colour_manual(values=c("#1F78B4","#E94849"))+
    theme(legend.position="bottom")
pn

p2bn<-p2B+geom_line(data = ndf1,aes(x=variable,y=value*25, color=X2da_exposicion),size=1, alpha=0.6)+
  scale_y_continuous(sec.axis = sec_axis(~./25, name = "Female survival proportion"))+
  scale_colour_manual(values=c("#1F78B4","#E94849","blue","red"))+
  theme(legend.position="bottom")
p2bn

#change values of 2nd exposition
dfpp<-ndf1
colnames(dfpp)[4:5]<-c("week","eggs")
class(dfpp$week)
levels(dfpp$sexo)
dfpp$sexo<-revalue(dfpp$sexo,c("hembra"="survival (F)"))
levels(dfpp$sexo)

ppp<-pp+geom_line(data = dfpp,aes(x=week,y=eggs*20,color=sexo),size=1, alpha=0.6)+
  scale_y_continuous(sec.axis = sec_axis(~./20, name = "Female survival proportion"))+
  scale_colour_manual(values=c("darkgreen","gray10","purple"))+
  #scale_fill_gradient(low="red", high="blue")+
  theme(legend.position="bottom")+
  labs(color = "Legend:")

ppp
#
##############################################################
# anova to total eggs laid by female
##############################################################
laid["toteggs"]<-NA
colnames(laid)
for(i in 1:nrow(laid)){
  laid[i,13]<-sum(laid[i,5:12],na.rm = T)
}

laid["group"]<-paste(laid$tratamiento,laid$X2da_exposicion, sep = "-")
class(laid$group)
laid$group<-as.factor(laid$group)
levels(laid$group)
#revalue groups
laid$group<-revalue(laid$group,c("control-NO"="ctrl_ctrl","control-SI"="ctrl_deltam",
                                 "deltametrina-NO"="deltam_ctrl","deltametrina-SI"="deltam_deltam"))

laid$group<-as.factor(laid$group)
levels(laid$group)
class(laid$toteggs)
negg.laid.anova<-aov(toteggs ~ group, data = laid)
yy<-round(anova(negg.laid.anova),digits = 3)
yy

ANOVA<-c("group", "Residuals")
rownames(yy)
dim(yy)
yy<-data.frame(cbind(ANOVA,yy))
ttt<-xtable(yy)

# Save table ANOVA as image with Title
t1 <- tableGrob(ttt, theme = ttheme_minimal(),rows =  NULL)
title <- textGrob("ANOVA to total eggs laid",gp=gpar(fontsize=15))
padding <- unit(5,"mm")
tabla <- gtable_add_rows(t1, heights = grobHeight(title) + padding,pos = 0)
tabla <- gtable_add_grob(tabla, title, 1, 1, 1, ncol(tabla))
grid.newpage()
grid.draw(tabla)

######################
# TUKEY TEST
#####################
tukey.test<-TukeyHSD(negg.laid.anova)
tukey.test
tukey.test2 <- HSD.test(negg.laid.anova, trt = 'group')
sigg<-tukey.test2$groups
dim(sigg)
groups<-rownames(sigg)
sigg<-data.frame(cbind(groups,sigg))

# Save TUKEY table  as image with Title
t1 <- tableGrob(sigg, theme = ttheme_minimal(),rows =  NULL)
title <- textGrob("Tukey test outcome",gp=gpar(fontsize=15))
padding <- unit(5,"mm")
tabla <- gtable_add_rows(t1, heights = grobHeight(title) + padding,pos = 0)
tabla <- gtable_add_grob(tabla, title, 1, 1, 1, ncol(tabla))
grid.newpage()
grid.draw(tabla)

#testing normality
plot(negg.laid.anova,2)
# Extract the residuals
aov_residuals <- residuals(object = negg.laid.anova )
# Run Shapiro-Wilk test
ll<-as.numeric(shapiro.test(x = aov_residuals )[2])
ll
plot(negg.laid.anova,1)
plot(negg.laid.anova,2)

#plot
tegg<-ggplot(laid,aes(x=group, y=toteggs, fill=group))+
  geom_boxplot(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=8, size=3)+
  geom_point(aes(colour=group),position = position_jitter(width = 0.05),size=.5)+
  scale_fill_manual(labels = c("Control-Control", "control-delta","delta-control","delta-delta"),
                    values = c("blue", "red","purple","black"))+
  scale_color_manual(labels = c("Control-Control", "control-delta","delta-control","delta-delta"),
                     values = c("blue", "red","purple","black"))+
  ylab("eggs laid by female")+
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
tegg

# plot tukey
########################
# Tuckey test representation :
plot(tukey.test , las=1 , col="blue" )

#boxplot with values and letters
splot<-sigg
class(splot$groups)
colnames(laid)
#get max by group
mx<-aggregate(.~group, data=laid[,c(14,13)],FUN=max)
#reorder levels
levels(mx$group)
mx<-mx[c(1,3,4,2),]

splot<-data.frame(cbind(splot[,c(2,3)],mx))
colnames(splot)[4]<-"max"

ttegg<- tegg+geom_text(data = splot, aes( label= groups.1, x = groups, y = max+5 ))+
        scale_color_manual(labels = c("Control-Control", "control-delta",
                                      "delta-control","delta-delta",
                                      "Control-Control", "control-delta",
                                      "delta-control","delta-delta"),
                           values = c("blue", "red","purple","black","blue", 
                                      "red","purple","black"))
ttegg





















































##############################################################
# anova to total eggs hatch by female
##############################################################
hatch["toteggs"]<-NA
colnames(hatch)
for(i in 1:nrow(hatch)){
  hatch[i,13]<-sum(hatch[i,5:12],na.rm = T)
}

hatch["group"]<-paste(hatch$tratamiento,hatch$X2da_exposicion, sep = "-")
class(hatch$group)
hatch$group<-as.factor(hatch$group)
levels(hatch$group)
#revalue groups
hatch$group<-revalue(hatch$group,c("control-NO"="ctrl_ctrl","control-SI"="ctrl_deltam",
                                 "deltametrina-NO"="deltam_ctrl","deltametrina-SI"="deltam_deltam"))

hatch$group<-as.factor(hatch$group)
levels(hatch$group)
class(hatch$toteggs)
negg.hatch.anova<-aov(toteggs ~ group, data = hatch)
yyh<-round(anova(negg.hatch.anova),digits = 3)
yyh
ANOVAh<-c("group", "Residuals")
rownames(yyh)
dim(yyh)
yyh<-data.frame(cbind(ANOVAh,yyh))
ttth<-xtable(yyh)
# Save table ANOVA as image with Title
t1h <- tableGrob(ttth, theme = ttheme_minimal(),rows =  NULL)
title <- textGrob("ANOVA to total hatch",gp=gpar(fontsize=15))
padding <- unit(5,"mm")
tablah <- gtable_add_rows(t1h, heights = grobHeight(title) + padding,pos = 0)
tablah <- gtable_add_grob(tablah, title, 1, 1, 1, ncol(tablah))
grid.newpage()
grid.draw(tablah)

######################
# TUKEY TEST
#####################
tukey.testh<-TukeyHSD(negg.hatch.anova)
tukey.testh
tukey.test2h <- HSD.test(negg.hatch.anova, trt = 'group')
siggh<-tukey.test2h$groups
dim(siggh)
groups<-rownames(siggh)
siggh<-data.frame(cbind(groups,siggh))

# Save TUKEY table  as image with Title
t1h <- tableGrob(siggh, theme = ttheme_minimal(),rows =  NULL)
title <- textGrob("Tukey test outcome",gp=gpar(fontsize=15))
padding <- unit(5,"mm")
tabla <- gtable_add_rows(t1h, heights = grobHeight(title) + padding,pos = 0)
tabla <- gtable_add_grob(tabla, title, 1, 1, 1, ncol(tabla))
grid.newpage()
grid.draw(tabla)

#testing normality
plot(negg.hatch.anova,2)
# Extract the residuals
aov_residualsh <- residuals(object = negg.hatch.anova )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residualsh )
plot(negg.hatch.anova,1)
plot(negg.hatch.anova,2)
#plot
teggh<-ggplot(hatch,aes(x=group, y=toteggs, fill=group))+
  geom_boxplot(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=8, size=3)+
  geom_point(aes(colour=group),position = position_jitter(width = 0.05),size=.5)+
  scale_fill_manual(labels = c("Control-Control", "control-delta","delta-control","delta-delta"), values = c("blue", "red","purple","black"))+
  scale_color_manual(labels = c("Control-Control", "control-delta","delta-control","delta-delta"), values = c("blue", "red","purple","black"))+
  ylab("eggs hatch by female")+
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
teggh

# plot tukey
##############################
# Tuckey test representation :
plot(tukey.testh , las=1 , col="blue" )
#boxplot with values and letters
sploth<-siggh
class(sploth$groups)
colnames(hatch)
#get max by group
mxh<-aggregate(.~group, data=hatch[,c(14,13)],FUN=max)
#reorder levels
levels(mxh$group)
mxh<-mxh[c(1,3,4,2),]
sploth<-data.frame(cbind(sploth[,c(2,3)],mxh))
colnames(sploth)[4]<-"max"

tteggh<- teggh+geom_text(data = sploth, 
                         aes( label= groups.1, x = groups, y = max+5 ))+
  scale_color_manual(labels = c("Control-Control", "control-delta",
                                "delta-control","delta-delta",
                                "Control-Control", "control-delta",
                                "delta-control","delta-delta"), 
                     values = c("blue", "red","purple","black","blue",
                                "red","purple","black"))
tteggh

##################################
# plot together laid and hatch
#################################
colnames(laid)
colnames(hatch)
ptog<-rbind(laid[,c(1:4,13,14)],hatch[,c(1:4,13,14)])
ptog["eggtype"]<-c(rep("laid",nrow(laid)),rep("hatch",nrow(hatch)))
class(ptog$eggtype)
ptog$eggtype<-as.factor(ptog$eggtype)
levels(ptog$eggtype)
#reorder levels
ptog$eggtype <-factor(ptog$eggtype,levels(ptog$eggtype)[c(2,1)])

#plot
pt<-ggplot(ptog,aes(x=group, y=toteggs, fill=eggtype))+
  geom_boxplot(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=c(8), size=c(3),color=c("black"),
               position=position_dodge(0.8))+
  scale_fill_manual( values = c("gray20","white"))+
  scale_color_manual(labels = c("Control-Control", "control-delta",
                                "delta-control","delta-delta"),
                     values = c("blue", "red","purple","black"))+
  ylab("n eggs by female")+
  xlab("treatment")+
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        axis.ticks.x=element_blank())

pt

###################################
#  Hatching difference analysis
###################################
colnames(laid)
egdif<-cbind(laid[,c(1:4,14,13)],hatch[,13])
colnames(egdif)[6:7]<-c("toteggslaid","toteggshatch")
egdif["eggs_dif"]<-egdif$toteggslaid-egdif$toteggshatch
class(egdif$eggs_dif)

################################################
# anova to eggs difference
levels(egdif$group)
#log from differenca to normal distribution
egdif["logdif"]<-log((egdif$eggs_dif+1))
negg.dif.anova<-aov(logdif ~ group, data = egdif)
yyhd<-round(anova(negg.dif.anova),digits = 3)
yyhd
ANOVA<-c("group", "Residuals")
rownames(yyhd)
dim(yyhd)
yyhd<-data.frame(cbind(ANOVA,yyhd))
ttthd<-xtable(yyhd)

# Save ANOVA table  as image with Title
t1hd <- tableGrob(ttthd, theme = ttheme_minimal(),rows =  NULL)
title <- textGrob("ANOVA to hatching eggs diffeerence (laid-hatch)",gp=gpar(fontsize=15))
padding <- unit(5,"mm")
tablah <- gtable_add_rows(t1hd, heights = grobHeight(title) + padding,pos = 0)
tablah <- gtable_add_grob(tablah, title, 1, 1, 1, ncol(tablah))
grid.newpage()
grid.draw(tablah)

######################
# TUKEY TEST
#####################
tukey.testhd<-TukeyHSD(negg.dif.anova)
tukey.testhd
tukey.test2hd <- HSD.test(negg.dif.anova, trt = 'group')
sigghd<-tukey.test2hd$groups
dim(sigghd)
groups<-rownames(sigghd)
sigghd<-data.frame(cbind(groups,sigghd))

# Save TUKEY table  as image with Title
t1hd <- tableGrob(sigghd, theme = ttheme_minimal(),rows =  NULL)
title <- textGrob("Tukey test outcome to hatching eggs diffeerence (laid-hatch)",gp=gpar(fontsize=15))
padding <- unit(5,"mm")
tabla <- gtable_add_rows(t1hd, heights = grobHeight(title) + padding,pos = 0)
tabla <- gtable_add_grob(tabla, title, 1, 1, 1, ncol(tabla))
grid.newpage()
grid.draw(tabla)

#testing normality
plot(negg.dif.anova,2)
# Extract the residuals
aov_residualshd <- residuals(object = negg.dif.anova )
# Run Shapiro-Wilk test
l<-as.numeric(shapiro.test(x = aov_residualshd )[2])
plot(negg.hatch.anova,1)
plot(negg.hatch.anova,2)
text(-1, 1.8,  paste("Shapiro-Wilk normality test\n p value =",round(l,digits = 5)),
     cex=0.8, pos=3,col="red") 
#plot
tegghd<-ggplot(egdif,aes(x=group, y=logdif, fill=group))+
  geom_boxplot(width=.7, alpha=0.4)+
  stat_summary(fun.y=mean, geom="point", shape=8, size=3)+
  geom_point(aes(colour=group),position = position_jitter(width = 0.05),size=.5)+
  scale_fill_manual(labels = c("Control-Control", "control-delta",
                               "delta-control","delta-delta"), 
                    values = c("blue", "red","purple","black"))+
  scale_color_manual(labels = c("Control-Control", "control-delta",
                                "delta-control","delta-delta"),
                     values = c("blue", "red","purple","black"))+
  ylab("log eggs difference (laid-hatch)")+
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

tegghd

# plot tukey
##############################
# Tuckey test representation :
plot(tukey.testhd , las=1 , col="blue" )
#boxplot with values and letters
splothd<-sigghd
class(splothd$groups)
colnames(egdif)
#get max by group
mxhd<-aggregate(.~group, data=egdif[,c(5,9)],FUN=max)
splothd<-data.frame(cbind(splothd[,c(1,2,3)],mxhd))
colnames(splothd)[5]<-"max"

ttegghd<- tegghd+geom_text(data = splothd, aes( label= groups.1, x = groups, y = max+.5 ))+
  scale_color_manual( values = c("blue", "red","purple","black",
                                 "blue", "red","purple","black"))+
  scale_y_continuous(breaks = c(seq(from=0,to=5,by=1)),limits = c(0,5))
  
ttegghd

##################################################
# EGGS REGRESSION (X=NFEMALE, Y NEGGS LAID)
##################################################
#summarize data
colnames(laid)
reglaid <- ddply(mlaid, c( "tratamiento","X2da_exposicion","colonia","variable"), summarise,
                 N    = sum(!is.na(value)),
                 suma = sum(value, na.rm = TRUE),
                 mean = round(mean(value, na.rm=TRUE),digits = 1),
                 sd   = round(sd(value, na.rm=TRUE),digits=1),
                 se   = round(sd / sqrt(N),digits = 1),
                 LCI  = round(mean-(1.96*se),digits = 1),
                 UCI  = round(mean+(1.96*se),digits = 1))

reglaid["group"]<-paste(reglaid$tratamiento,reglaid$X2da_exposicion, sep = "-")
#revalue groups
class(reglaid$group)
reglaid$group<-as.factor(reglaid$group)
levels(reglaid$group)
reglaid$group<-revalue(reglaid$group,c("control-with out 2nd expsition"="ctrl_ctrl","control-with 2nd expsition"="ctrl_deltam",
                                   "deltametrina-with out 2nd expsition"="deltam_ctrl","deltametrina-with 2nd expsition"="deltam_deltam"))

class(reglaid$variable)
reglaid$variable<-as.integer(reglaid$variable)
fit <- lm(mean ~ variable+group, data = reglaid)
#fit model
fit <- lm(suma ~ N+group, data = reglaid)
summary(fit1)
# plot
ggplot(reglaid, aes(x = N, y = mean, colour=group)) + 
  geom_point(aes(color=group),position = position_jitter(width = 0.2)) +
  stat_smooth(method = "lm",se=F)+
  scale_x_reverse(breaks = c(seq(from=0,to=10,by=2)),limits = c(10,0))

ggplot(reglaid, aes(x = N, y = suma, colour=group)) + 
  geom_point(aes(color=group),position = position_jitter(width = 0.2)) +
  # geom_abline(aes(color=group))
  stat_smooth(method = "lm",se=F)+
  scale_x_reverse(breaks = c(seq(from=0,to=10,by=2)),limits = c(10,0))

##################################################
# EGGS REGRESSION (X=WEEKS, Y NEGGS LAID)
##################################################
#summarize data
colnames(laid)
reglaid1 <- ddply(mlaid, c( "tratamiento","X2da_exposicion","colonia","variable"), summarise,
                 N    = sum(!is.na(value)),
                 suma = sum(value, na.rm = TRUE),
                 mean = round(mean(value, na.rm=TRUE),digits = 1),
                 sd   = round(sd(value, na.rm=TRUE),digits=1),
                 se   = round(sd / sqrt(N),digits = 1),
                 LCI  = round(mean-(1.96*se),digits = 1),
                 UCI  = round(mean+(1.96*se),digits = 1))


reglaid1["group"]<-paste(reglaid1$tratamiento,reglaid1$X2da_exposicion, sep = "-")
#revalue groups
class(reglaid1$group)
reglaid1$group<-as.factor(reglaid1$group)
levels(reglaid1$group)
reglaid1$group<-revalue(reglaid1$group,c("control-with out 2nd expsition"="ctrl_ctrl","control-with 2nd expsition"="ctrl_deltam",
                                       "deltametrina-with out 2nd expsition"="deltam_ctrl","deltametrina-with 2nd expsition"="deltam_deltam"))

#fit model
class(reglaid1$variable)
reglaid1$variable<-as.integer(reglaid1$variable)
fit1<- lm(mean ~ variable+group, data = reglaid1)
summary(fit11)

# plot
ggplot(reglaid1, aes(x = variable, y = mean, colour=group)) + 
  geom_point(aes(color=group),position = position_jitter(width = 0.2)) +
  stat_smooth(method = "lm",se=F)+
  scale_x_continuous(breaks = c(seq(from=0,to=9,by=1)),limits = c(1,8))

ggplot(reglaid1, aes(x = N, y = mean, colour=group)) + 
  geom_point(aes(color=group),position = position_jitter(width = 0.2)) +
  # geom_abline(aes(color=group))
  stat_smooth(method = "lm",se=F)+
  scale_x_continuous(breaks = c(seq(from=0,to=9,by=1)),limits = c(1,8))

pr<-ggplot(reglaid1, aes(x = variable, y = mean, colour=group)) + 
    geom_point(aes(color=group),position = position_jitter(width = 0.2)) +
    # geom_abline(aes(color=group))
    stat_smooth(method = "lm",se=T)+
    scale_x_continuous(breaks = c(seq(from=0,to=9,by=1)),limits = c(1,8))+
    facet_grid(.~group)+
    ylab("mean eggs laid")+
    xlab("time (weeks)")+
    theme(legend.position="bottom",plot.title = element_text(hjust = 0.5))
pr

#adding linnear regression formula
reglaid2<-reglaid
colnames(reglaid2)
levels(reglaid2$group)
reglaid2cc<-reglaid2[reglaid2$group=="ctrl_ctrl",]
reglaid2cd<-reglaid2[reglaid2$group=="ctrl_deltam",]
reglaid2dc<-reglaid2[reglaid2$group=="deltam_ctrl",]
reglaid2dd<-reglaid2[reglaid2$group=="deltam_deltam",]

# Create function.
regression=function(reglaid2){
  #setting the regression function. 
  reg_fun<-lm(formula=reglaid2$mean~reglaid2$variable)
  #getting the slope, intercept, R square and adjusted R squared of 
  #the regression function (with 3 decimals).
  slope<-round(coef(reg_fun)[2],2)  
  intercept<-round(coef(reg_fun)[1],2) 
  R2<-round(as.numeric(summary(reg_fun)[8]),2)
  R2.Adj<-round(as.numeric(summary(reg_fun)[9]),3)
  c(slope,intercept,R2,R2.Adj)
}


regressions_data<-ddply(reglaid2,"group",regression)
colnames(regressions_data)<-c ("group","slope","intercept","R2","R2.Adj")

prr<-ggplot(reglaid1, aes(x = variable, y = mean, colour=group)) +
  geom_smooth(method="lm")+
  geom_point(aes(color=group),position = position_jitter(width = 0.2)) +
  facet_grid(.~group)+ggtitle("Regressions to eggs laid")+
  geom_label(data=regressions_data, inherit.aes=FALSE,
             aes(x = 4.5, y = 20,label=paste("y =",intercept,"+",slope,"x"," , R^2=",R2)),size=2.2)+
  scale_x_continuous(breaks = c(seq(from=1,to=8,by=1)),limits = c(0,9))+
  ylab("mean eggs laid")+
  xlab("time (weeks)")+
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5))
        
prr

##############################################################
#get the pdf
##############################################################
pdf(file = "eggs laid from cimex female exposed.pdf", width=6,height=4,paper='special')#, paper="A4r",)
g
p2bn
pn
ppp
plot(negg.laid.anova,2,main="eggs laid")
text(-1, 1.8,  paste("Shapiro-Wilk normality test\n p value =",
                     round(ll,digits = 5)),
     cex=0.8, pos=3,col="red")

# Save table as image with Title
t1 <- tableGrob(ttt, theme = ttheme_minimal(),rows =  NULL)
title <- textGrob("ANOVA to total eggs laid by group",gp=gpar(fontsize=15))
padding <- unit(5,"mm")
tabla <- gtable_add_rows(t1, heights = grobHeight(title) + padding,pos = 0)
tabla <- gtable_add_grob(tabla, title, 1, 1, 1, ncol(tabla))
grid.newpage()
grid.draw(tabla)

# TUKEY table  as image with Title
t1 <- tableGrob(sigg, theme = ttheme_minimal(),rows =  NULL)
title <- textGrob("Tukey test outcome to eggs laid",gp=gpar(fontsize=15))
padding <- unit(5,"mm")
tabla <- gtable_add_rows(t1, heights = grobHeight(title) + padding,pos = 0)
tabla <- gtable_add_grob(tabla, title, 1, 1, 1, ncol(tabla))
grid.newpage()
grid.draw(tabla)
plot(tukey.test , las=1 , col="blue" )
ttegg
pt

# normality hatching eggs
plot(negg.hatch.anova,2,main="eggs hatching difference (laid - hatch)")
text(-1, 1.8,  paste("Shapiro-Wilk normality test\n p value =",round(l,digits = 5)),
     cex=0.8, pos=3,col="red") 

# ANOVA table to TOTAL HATCHED EGGS
t1hd <- tableGrob(ttthd, theme = ttheme_minimal(),rows =  NULL)
title <- textGrob("ANOVA to total hatching eggs",gp=gpar(fontsize=15))
padding <- unit(5,"mm")
tablah <- gtable_add_rows(t1hd, heights = grobHeight(title) + padding,pos = 0)
tablah <- gtable_add_grob(tablah, title, 1, 1, 1, ncol(tablah))
grid.newpage()
grid.draw(tablah)

# TUKEY table  to TOTAL HATCHED EGGS
t1hd <- tableGrob(sigghd, theme = ttheme_minimal(),rows =  NULL)
title <- textGrob("Tukey test outcome to hatching\n difference (laid - hatch)",gp=gpar(fontsize=15))
padding <- unit(5,"mm")
tabla <- gtable_add_rows(t1hd, heights = grobHeight(title) + padding,pos = 0)
tabla <- gtable_add_grob(tabla, title, 1, 1, 1, ncol(tabla))
grid.newpage()
grid.draw(tabla)

ttegghd
prr

dev.off()

############
# end
#############