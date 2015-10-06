##############################################
## Code for survival models
##############################################

### R packages being used:
# Use the commented command if the package is not installed

# install.packages("ggplot2")
library("ggplot2")

# install.packages("reshape")
library("reshape")

# install.packages("RColorBrewer")
library("RColorBrewer")


######
# Uncomment the line below to create a pdf with all of the graphics in the current directory of this file
# Make sure to uncomment the last line of this script, "dev.off()", to finish writing to the file
######
#pdf(file = "Survival models cimex on life tables.pdf")


# Reading the data
dat <- read.csv("survival total cimex.csv")
head(dat)
pull <- dat[dat$repeticion == "pull",]
head(pull)
muerto <- c()
for(i in 1: length(pull$repeticion)-1){
  muerto[i+1] <- pull$Muett[i+1] - pull$Muett[i]
}
muerto[1] <- 0
muerto[57] <- 0

pull$muerto <- muerto
pull.expanded <- pull[rep(row.names(pull), pull$muerto), c(2,4)]
pull.expanded$event <- 1
head(pull.expanded)
dim(pull.expanded)
library(survival)
#CODIGO para sobrevivencia

a<-(Surv(pull.expanded$semana,pull.expanded$event))

#############
#Kaplan Meier
#############
plot(survfit(a~pull.expanded$grupo),col=c("blue","red"),main="Kaplan Meier-survival cimex plot",xlab="time (weeks)",ylab="survival rate")
legend("topright",legend= c("Control","Infected"),col=c("blue","red"), pch=0)

plot(survfit(a~pull.expanded$grupo),col=c("blue","red"),fun="cloglog",main="cloglog survival cimex plot",xlab="time (weeks)",ylab="survival rate")
legend("bottomright",legend= c("Control","Infected"),col=c("blue","red"), pch=0)

###########################################
#cox proportional hazards regression model
###########################################

coxsurv<-coxph(a~pull.expanded$grupo,data=pull.expanded)

coxsurv

plot(survfit(coxsurv),main=expression("Cox PH-estimate with CI"),
    xlab="time (Weeks)", ylab="Survival Proportion", lwd=1)

#COMPROBANDO ASUMCION
cox.zph(coxsurv)

#plot(survfit(coxsurv),fun="cloglog",col=c("black","blue","red"),main="cloglog plot for grupo (co, inf)")

#################
# Weibull Model 
#################

weib<-survreg(a~pull.expanded$grupo,data=pull.expanded, dist="weibull")
summary(weib)

plot(survfit(coxsurv),main=expression("Weibull survival curve"),
     xlab="time (Weeks)", ylab="Survival Proportion", lwd=1)

#dev.off()

