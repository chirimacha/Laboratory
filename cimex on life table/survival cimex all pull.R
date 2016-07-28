##############################################
## Code for intepreting life tables 
## Written by: Casey Bartow-McKenney
## Levy Rotation, Winter/Spring 2015
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
#pdf(file = "R_lifetables_graphics_SEP2015.pdf")



############################
## Cimex all base Data
############################



# Load in csv download of life tables
raw_cb_csv <- read.csv(file = 'survival total cimex.csv')



############
## Set Week number below
## How many weeks (including week zero, e.g. weeks (0,1, and 2) would be 3 weeks)
cb_week_num = 56
# Current up to "week 46", so 47 weeks

############ 
## Set How many bugs were used for each group (e.g. 40 in Infected A)
#PARA PULL
cb_bug_num_copull = 240
cb_bug_num_infpull = 280
#PARA REP1 y 2
cb_bug_num_coreps = 100
cb_bug_num_infreps = 100
#para piloto
cb_bug_num_copil = 40
cb_bug_num_infpil = 80



# Remove Recorded date column (will use week column)
raw_cb_csv[5] <- NULL
raw_cb_csv[1] <- NULL
raw_cb_csv[1] <- NULL
raw_cb_csv[1] <- NULL
# ONLY RUN THESE ONCE

# Replace blank cells with 0
raw_cb_csv[is.na(raw_cb_csv)] <- 0

#### CHANGE THESE TO DYNAMIC VAR
# Setting up groups, copull = control, infpull = infected, ETC...
# pull
cb_copull <- raw_cb_csv[1:56,]
cb_infpull <- raw_cb_csv[57:112,]
# REP1
cb_corep1 <- raw_cb_csv[113:168,]
cb_infrep1 <- raw_cb_csv[169:224,]
#rep2
cb_corep2 <- raw_cb_csv[225:280,]
cb_infrep2 <- raw_cb_csv[281:336,]
#piloto
cb_copil <- raw_cb_csv[337:392,]
cb_infpil <- raw_cb_csv[393:448,]


### FOR CONTROL pull NEED LINES 1-56
### FOR INFECTED pull NEED LINES 57-112
### FOR control rep1 NEED LINES 113-168
### for infected rep1 need lines 169-224
### for control rep2 need lines 225-280
### for infected rep 2 need lines 281-336
### for control pilot need lines 337-392
### for infected pilot need lines 393-448

### Total life and death counts per week
#para pull
cb_alive_copull <- cb_copull[16]/cb_bug_num_copull
cb_alive_infpull <- cb_infpull[16]/cb_bug_num_infpull
#para rep1
cb_alive_corep1 <- cb_corep1[16]/cb_bug_num_coreps
cb_alive_infrep1 <- cb_infrep1[16]/cb_bug_num_infreps
#para rep2
cb_alive_corep2 <- cb_corep2[16]/cb_bug_num_coreps
cb_alive_infrep2 <- cb_infrep2[16]/cb_bug_num_infreps
#para piloto
cb_alive_copil <- cb_copil[16]/cb_bug_num_copil
cb_alive_infpil <- cb_infpil[16]/cb_bug_num_infpil


# Create data frame with vectors of life totals for the 4 groups, A and B of control and infected
cb_All_total_survival <- cbind(cb_alive_copull,cb_alive_infpull,cb_alive_copil,cb_alive_infpil,cb_alive_corep1,cb_alive_infrep1,cb_alive_corep2,cb_alive_infrep2)
cb_All_total_survival <- as.data.frame(t(cb_All_total_survival))

# Create column names for the weeks of recording, and row names for group
colnames(cb_All_total_survival) <- c(1:cb_week_num)
cb_All_total_survival$groupName <-  c("Co Pull","Inf Pull","Co Rep 1","Inf Rep 1","Co Rep 2","Inf Rep 2","Co Rep 3","Inf Rep 3")

# Reshape data frame into "re_all" so that all values are recorded as a 1-dimensional list
# Reshaping allows for easy manipulation of data into graphs
cb_re_all <- reshape(cb_All_total_survival, varying=c(1:cb_week_num), v.names='Proportion',
                     direction='long', idvar='groupName')

# Plot graph of percentage alive
ggplot(cb_re_all, aes(x = time, y = Proportion, color = groupName, fill = groupName)) +
  #geom_line()+
  geom_line(aes(linetype=groupName, size= groupName)) + 
  scale_color_manual(values = c("#1F78B4","#1F78B4","#1F78B4","#1F78B4","#E31A1C","#E31A1C","#E31A1C","#E31A1C")) +
  #scale_color_manual(values = c("#1F78B4", "#00E600","#0000CC","#0C3048","#E31A1C","#FFFF00","#E94849","#990000")) +
  scale_linetype_manual(values=c("solid","dotted","dotdash","longdash","solid","dotted","dotdash","longdash"))+
  #scale_linetype_manual(values=c("dashed","solid"))+
 scale_size_manual(values=c(1.3,0.9,0.7,0.7,1.3,0.9,0.7,0.7,0.7))+
  #scale_size_manual(values= c(1.5,15))+
   ggtitle("Cimex All Proportion Alive per week") + xlab("Time (Weeks)") + ylab("Proportion Alive") +
  theme(text = element_text(size=16),axis.text.x=element_text(size=14),axis.text.x=element_text(size=14))+
  geom_vline(aes(xintercept=8),linetype="dashed")+
  annotate("text",x=2.5,y=0.5, label="nymphal")+
  annotate("text",x=2.5,y=0.4, label="period")+
  annotate("text",x=30,y=0.7, label="adulthood")+
annotate("rect",xmin=0, xmax=8,ymin=0,ymax=1, alpha=0.1)
  xlim(0,60)

#head(raw_cb_csv)
colnum<-c(2,4,6,8,10)
semanas<-c(1:30)
ninfal<-raw_cb_csv[1:15,colnum]
bdninfal<-data.frame(ninfal)
head(bdninfal)
#plot(semanas,ninfal,type="l")
matplot(bdninfal, type="l", col=1:5)
legend("topright", legend= c("N1","N2","N3","N4","N5"),col=1:5, pch=1)

#dev.off()
