###########################################################
#Cimex on life tables (T cruzi-infected and control)
# Code to survival models         
# Written by Renzo Salazar                          
#updated: dic 2016
###########################################################

### R packages being used:
# Use the commented command if the package is not installed

# install.packages("ggplot2")
# install.packages("reshape")
# install.packages("RColorBrewer")
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/survminer")

#Loading libraries
library("ggplot2")
library("reshape")
library("RColorBrewer")
library("survminer")
require("survival")

######
# Uncomment the line below to create a pdf with all of the graphics in the current directory of this file
# Make sure to uncomment the last line of this script, "dev.off()", to finish writing the file
######
#pdf(file = "Survival models cimex on life tables.pdf")


#setting working directory
setwd("D:/LAB/ENSAYOS/CICLO VIDA CIMEX-CRUZI/R code")
getwd()
# Reading the data
dat <- read.csv("survival total cimex.csv")
head(dat)
pull <- dat[dat$repeticion == "pull",]
head(pull)

#repeat values to long data
pull.expanded <- pull[rep(row.names(pull), pull$Muetxweek), c(2,4)]
dim(pull.expanded)
pull.expanded$event <- 1
head(pull.expanded)
dim(pull.expanded)

############################################################################
#Kaplan Meier
############################################################################
a<-(Surv(pull.expanded$semana,pull.expanded$event))

plot(survfit(a~pull.expanded$grupo),col=c("blue","red"),main="Kaplan Meier-survival cimex plot",xlab="time (weeks)",ylab="survival rate")
legend("topright",legend= c("Control","Infected"),col=c("blue","red"), pch=0)

###############################
#using survminer, basic plot
###############################
fit <- survfit(Surv(semana, event) ~ grupo, data = pull.expanded)
ggsurvplot(fit, leyend= c(2,2), main="survival")

#customizing survival curves
ggsurvplot(fit,  size = 0.9,  # change line size
           main="Kaplan-Meier Survival Curves",# add title
           xlab= "Time (Weeks)", #change xlab name
           palette = c("#1F78B4","#E31A1C"), # custom color palettes
           conf.int = TRUE, # Add confidence interval
           pval = TRUE, # Add p-value
           break.time.by = 10, #change x axe scale
           risk.table = TRUE, # Add risk table
           risk.table.y.text.col = TRUE,
           risk.table.col = "grupo", # Risk table color by groups
           legend= c(0.8,0.8), #change legend position
           font.legend = 14, #change font size
           legend.title= "Cohort", #change legend title
           legend.labs = c("Control", "Infected"), # Change legend labels
           xlim=c(0,60), #change x limit
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme()#_bw() # Change ggplot2 theme
)

#cumulative hazard and cumulative event
ggsurvplot(fit,  size = 0.9,  # change line size
           #main="Cumulative hazard plot",# add title to cumhaz
           main="Cumulative event plot", # add title to cum event
           xlab= "Time (Weeks)", #change xlab name
           palette = c("#1F78B4","#E31A1C"), # custom color palettes
           conf.int = TRUE, # Add confidence interval
           #pval = TRUE, # Add p-value
           break.time.by = 10, #change x axe scale
           risk.table = TRUE, # Add risk table
           risk.table.y.text.col = TRUE,
           risk.table.col = "grupo", # Risk table color by groups
           legend= c(0.2,0.8), #change legend position cumhaz
           #legend= c(0.9,0.4), #change legend position cumhaz
           font.legend = 14, #change font size
           legend.title= "Cohort", #change legend title
           legend.labs = c("Control", "Infected"), # Change legend labels
           xlim=c(0,60), #change x limit
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme(),#_bw() # Change ggplot2 theme
           fun="cumhaz" #uncoment to cumulative hazard
           #fun="event" # uncomment to cumulative event
)


############################################################################
# cox proportional hazards regression model
############################################################################

coxsurv<-coxph(a~pull.expanded$grupo,data=pull.expanded)

coxsurv

plot(survfit(coxsurv),main=expression("Cox PH-estimate with CI"),
    xlab="time (Weeks)", ylab="Survival Proportion", lwd=1)

#COMPROBANDO ASUMCION
cox.zph(coxsurv)

plot(survfit(coxsurv),fun="cloglog",col=c("black","blue","red"),main="cloglog plot for grupo (co, inf)")

############################################################################
# Weibull Model 
############################################################################

weib<-survreg(a~pull.expanded$grupo,data=pull.expanded, dist="weibull")
summary(weib)

plot(survfit(coxsurv),main=expression("Weibull survival curve"),
     xlab="time (Weeks)", ylab="Survival Proportion", lwd=1)

#dev.off()

