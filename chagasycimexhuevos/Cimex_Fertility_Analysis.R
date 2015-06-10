#set directory and bring in files to be analyzed.
setwd("c:\\Users\\tradylan\\Documents\\GitHub\\Laboratory\\chagasycimexhuevos")
cimfertpilot <- read.csv("Cimex_FertP.csv")
cimfert1 <- read.csv("Cimex_FertR1.csv")
cimfert2 <- read.csv("Cimex_FertR2.csv")

####The Goal of this analysis is to see how bed bugs fertility may change based on infection with chagas disease.
####To do so we must explore some confounds such as age of insect.

#Create eggs laid charg
postura<-data.frame(cimfertpilot$Nro_.pareja, cimfertpilot$s1_v, cimfertpilot$s2_v, cimfertpilot$s3_v, cimfertpilot$s4_v),
 cimfertpilot$s4_v,cimfertpilot$s5_v,cimfertpilot$s6_v, cimfertpilot$s7_v, cimfertpilot$s8_v, cimfertpilot$s9_v, cimfertpilot$s10_v,cimfertpilot$s11_v,
cimfertpilot$s12_v,cimfertpilot$s13_v,cimfertpilot$s14_v, cimfertpilot$s15_v, cimfertpilot$s16_v, cimfertpilot$s17_v, cimfertpilot$s18_v,cimfertpilot$s19_v,
#Create chart for eggs







#-----------------------------------------------------
###Figure out let us start by finding the base stats
###Longevity
#identify groups
controls <- which(cimfertpilot$Procedencia=="CO")
infectA <- which(cimfertpilot$Procedencia=="I-R1")
infectB <- which(cimfertpilot$Procedencia=="I-R2")
infect <-c:(infectA, infectB)

c


#find number in each cohort
nmcontrols <- length(controls)
nminfectA <- length(infectA)
nminfectB <- length(infectB)
nminfect <- length(infect)

#find the average 
longcont <- sum(cimfertpilot$Longevidad[controls])/nmcontrols #16.78571
longinfA <- sum(cimfertpilot$Longevidad[infectA])/nminfectA  #18.5
longinfB <- sum(cimfertpilot$Longevidad[infectB])/nminfectB  #15.90909
longinf <-sum(cimfertpilot$Longevidad[infect])/nminfect  #17.51724
------------------------------------------------------------------------------------
##For each time line
glm(cases~rhs(data$year,2003)+lhs(data$year,2003)+ offset(log(population)), data=data, subset=28:36, family=poisson())

