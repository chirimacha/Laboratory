#####################################################
#Cimex on life tables (T cruzi-infected and control)
#code to plot cimex survival curves
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
#pdf(file = "Total survival Cimex graphics.pdf")

######################################################################################
# Load in csv download of life tables
raw_cb_csv <- read.csv(file = 'survival total cimex.csv')
head(raw_cb_csv)

############
## Set Week number below
## How many weeks (including week zero, e.g. weeks (0,1, and 2) would be 3 weeks)
cb_week_num = 56

## Set How many bugs were used for each group
#to PULL
cb_bug_num_copull = 240
cb_bug_num_infpull = 280
#to REP1 y 2
cb_bug_num_coreps = 100
cb_bug_num_infreps = 100
#to piloto
cb_bug_num_copil = 40
cb_bug_num_infpil = 80

# Remove Recorded date column (will use week column)
raw_cb_csv[5] <- NULL
raw_cb_csv[1] <- NULL
raw_cb_csv[1] <- NULL
raw_cb_csv[1] <- NULL
# ONLY RUN THESE LINES ONCE TIME

# Replace blank cells with 0
raw_cb_csv[is.na(raw_cb_csv)] <- 0

#### CHANGE THESE TO DYNAMIC VAR
# Setting up groups, copull = control, infpull = infected, ETC...
### FOR CONTROL pull NEED LINES 1-56     ### FOR INFECTED pull NEED LINES 57-112
### FOR control rep1 NEED LINES 113-168  ### for infected rep1 need lines 169-224
### for control rep2 need lines 225-280  ### for infected rep 2 need lines 281-336
### for control pilot need lines 337-392 ### for infected pilot need lines 393-448

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

### Total life and death counts per week
#to pull
cb_alive_copull <- cb_copull[16]/cb_bug_num_copull
cb_alive_infpull <- cb_infpull[16]/cb_bug_num_infpull
#to rep1
cb_alive_corep1 <- cb_corep1[16]/cb_bug_num_coreps
cb_alive_infrep1 <- cb_infrep1[16]/cb_bug_num_infreps
#to rep2
cb_alive_corep2 <- cb_corep2[16]/cb_bug_num_coreps
cb_alive_infrep2 <- cb_infrep2[16]/cb_bug_num_infreps
#to piloto
cb_alive_copil <- cb_copil[16]/cb_bug_num_copil
cb_alive_infpil <- cb_infpil[16]/cb_bug_num_infpil

# Create dataframe with vectors of total life
cb_All_total_survival <- cbind(cb_alive_copull,cb_alive_infpull,cb_alive_copil,
                               cb_alive_infpil,cb_alive_corep1,cb_alive_infrep1,
                               cb_alive_corep2,cb_alive_infrep2)
cb_All_total_survival <- as.data.frame(t(cb_All_total_survival))

# Create column names for the weeks of recording, and row names for group
colnames(cb_All_total_survival) <- c(1:cb_week_num)
cb_All_total_survival$Cohorts <-  c("Co Pull","Inf Pull","Co Rep 1","Inf Rep 1",
                                    "Co Rep 2","Inf Rep 2","Co Rep 3","Inf Rep 3")

# Reshape data frame into "re_all" so that all values are recorded as a 1-dimensional list
# Reshape allows for easy manipulation of data into graphs
cb_re_all <- reshape(cb_All_total_survival, varying=c(1:cb_week_num),
                     v.names='Proportion', direction='long', idvar='Cohorts')

# Plot graph of percentage alive
ggplot(cb_re_all, aes(x = time, y = Proportion, color = Cohorts, fill = Cohorts)) +
  geom_line(aes(linetype=Cohorts, size= Cohorts)) + 
  scale_color_manual(values = c("#1F78B4","#1F78B4","#1F78B4","#1F78B4","#E31A1C",
                                "#E31A1C","#E31A1C","#E31A1C")) +
  scale_linetype_manual(values=c("solid","dotted","dotdash","longdash","solid",
                                 "dotted","dotdash","longdash"))+
  scale_size_manual(values=c(1.3,0.9,0.7,0.7,1.3,0.9,0.7,0.7,0.7))+
  ggtitle("Cimex All Proportion Alive per week") + xlab("Time (Weeks)") +
          ylab("Proportion Alive") +
  theme(text = element_text(size=24),axis.text.x=element_text(size=24),
        axis.text.y = element_text(size=24))+
  geom_vline(aes(xintercept=8),linetype="dashed")+
  annotate("text",x=2.5,y=0.5, label="Nymphal")+
  annotate("text",x=2.5,y=0.4, label="Period")+
  annotate("text",x=20,y=0.7, label="Adulthood")+
  annotate("rect",xmin=0, xmax=8,ymin=0,ymax=1, alpha=0.1)
  xlim(0,60)
  
#uncomment next line to finish PDF file (just in case lne # 27 was uncommented)  
#dev.off()
  
###################################################################################
