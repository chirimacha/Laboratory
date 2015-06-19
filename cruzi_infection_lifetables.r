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
 pdf(file = "R_lifetables_graphics_jun2015.pdf")


############################
## Cimex Rep 1 data
############################

# Load in csv download of life tables for cimex Rep 1
raw_c1_csv <- read.csv(file = "ciclo vida cimex Rep 1 - cimex.csv",skip=2)

############
## Set Week number below
## How many weeks (including week zero, e.g. weeks (0,1, and 2) would be 3 weeks)
c1_week_num = 42
# Current up to week 25, so 26 weeks

############ 
## Set How many bugs were used for each group (e.g. 50 in Infected A)
c1_bug_num = 50

# Remove Recorded date column (will use week column)
raw_c1_csv[2] <- NULL

# Replace blank cells with 0
raw_c1_csv[is.na(raw_c1_csv)] <- 0

# Setting up groups, group1 = control, group2 = infected
c1_group1 <- raw_c1_csv
c1_group2 <- raw_c1_csv

# Remove group 2 observations from group1
for(i in seq (19:34)){
  c1_group1[19] <- NULL
}

# Remove group 1 observations from group2
for(i in seq (3:18)){
  c1_group2[3] <- NULL
}

# Create sequences of odd and even indices
c1_even_indices <- seq (2,c1_week_num*2,2)
c1_odd_indices <- seq (1,c1_week_num*2,2)

# Uninfected Controls, Total life and death counts per week
c1_alive_control_totals <- c1_group1[17]/c1_bug_num


# Totals of A and B groups
c1_A_control_totals <- c1_alive_control_totals[c1_odd_indices,1]
c1_B_control_totals <- c1_alive_control_totals[c1_even_indices,1]

# Infected Cimex, Total life and death counts per week
c1_alive_inf_totals <- c1_group2[17]/c1_bug_num

# Totals of A and B infected groups
c1_A_inf_totals <- c1_alive_inf_totals[c1_odd_indices,1]
c1_B_inf_totals <- c1_alive_inf_totals[c1_even_indices,1]

# Create data frame with vectors of life totals for the 4 groups, A and B of control and infected
c1_All_total_survival <- cbind(c(c1_A_control_totals),c(c1_B_control_totals),c(c1_A_inf_totals),c(c1_B_inf_totals))
c1_All_total_survival <- as.data.frame(t(c1_All_total_survival))

# Create column names for the weeks of recording, and row names for group
colnames(c1_All_total_survival) <- c(0:(c1_week_num - 1))
c1_All_total_survival$groupName <-  c("Control_A","Control_B","Infected_A","Infected_B")

# Reshape data frame into "re_all" so that all values are recorded as a 1-dimensional list
# Reshaping allows for easy manipulation of data into graphs
c1_re_all <- reshape(c1_All_total_survival, varying=c(1:c1_week_num), v.names='Proportion',
                  direction='long', idvar='groupName')

# Plot graph of percentage alive
ggplot(c1_re_all, aes(x = time, y = Proportion, color = groupName, fill = groupName)) +
  geom_line() + scale_color_manual(values = c("#A6CEE3", "#1F78B4", "#FB9A99", "#E31A1C")) +
  ggtitle("Cimex Rep. 1 Proportion Alive") + xlab("Week") + ylab("Proportion Alive") +
  theme(text = element_text(size=16),axis.text.x=element_text(size=14),axis.text.x=element_text(size=14))


# Combine A & B into one group
c1_controls <- (c1_A_control_totals+c1_B_control_totals)/2
c1_infected <- (c1_A_inf_totals+c1_B_inf_totals)/2

# Create data frame with vectors of life totals for the 4 groups, A and B of control and infected
c1_comb_total_survival <- cbind(c(c1_controls),c(c1_infected))
c1_comb_total_survival <- as.data.frame(t(c1_comb_total_survival))

# Create column names for the weeks of recording, and row names for group
colnames(c1_comb_total_survival) <- c(0:(c1_week_num-1))
c1_comb_total_survival$groupName <-  c("Controls","Infected")

# Reshape data frame into "re_all" so that all values are recorded as a 1-dimensional list
# Reshaping allows for easy manipulation of data into graphs
c1_re_comb <- reshape(c1_comb_total_survival, varying=c(1:c1_week_num), v.names='Proportion',
                     direction='long', idvar='groupName')

# Plot graph of percentage alive
ggplot(c1_re_comb, aes(x = time, y = Proportion, color = groupName, fill = groupName)) +
  geom_line() + scale_color_manual(values = c("#1F78B4", "#E31A1C")) +
  ggtitle("Cimex Rep. 1 Proportion Alive (Combined)") + xlab("Week") + ylab("Proportion Alive") +
  theme(text = element_text(size=16),axis.text.x=element_text(size=14),axis.text.x=element_text(size=14))


############################
## Triatoma Rep 1 data
############################


# Load in csv download of life tables
raw_t1_csv <- read.csv(file = 'ciclo vida cimex Rep 1 - triatoma.csv',skip=3)

############
## Set Week number below
## How many weeks (including week zero, e.g. weeks (0,1, and 2) would be 3 weeks)
t1_week_num = 42
# Current up to week 25, so 26 weeks

############ 
## Set How many bugs were used for each group (e.g. 50 in Infected A)
t1_bug_num = 50



# Remove Recorded date column (will use week column)
raw_t1_csv[2] <- NULL

# Replace blank cells with 0
raw_t1_csv[is.na(raw_t1_csv)] <- 0

# Setting up groups, group1 = control, group2 = infected
t1_group1 <- raw_t1_csv
t1_group2 <- raw_t1_csv

# Splitting up groups, and removing blank lines below
t1_group1 <- head(t1_group1, t1_week_num * 2)
t1_group2 <- head(t1_group2, t1_week_num * 2)


t1_even_indices <- seq (2,t1_week_num*2,2)
t1_odd_indices <- seq (1,t1_week_num*2,2)


# Remove group 2 observations from group1
for(i in seq (19:35)){
  t1_group1[19] <- NULL
}

# Remove group 1 observations from group2
for(i in seq (3:19)){
  t1_group2[3] <- NULL
}

# Uninfected Controls, Total life and death counts per week
t1_alive_control_totals <- t1_group1[17]/t1_bug_num


# Totals of A and B groups
t1_A_control_totals <- t1_alive_control_totals[t1_odd_indices,1]
t1_B_control_totals <- t1_alive_control_totals[t1_even_indices,1]

# Infected Triatoma, Total life and death counts per week
t1_alive_inf_totals <- t1_group2[17]/t1_bug_num


# Totals of A and B infected groups
t1_A_inf_totals <- t1_alive_inf_totals[t1_odd_indices,1]
t1_B_inf_totals <- t1_alive_inf_totals[t1_even_indices,1]

# Create data frame with vectors of life totals for the 4 groups, A and B of control and infected
t1_All_total_survival <- cbind(c(t1_A_control_totals),c(t1_B_control_totals),c(t1_A_inf_totals),c(t1_B_inf_totals))
t1_All_total_survival <- as.data.frame(t(t1_All_total_survival))

# Create column names for the weeks of recording, and row names for group
colnames(t1_All_total_survival) <- c(0: (t1_week_num - 1))
t1_All_total_survival$groupName <-  c("Control_A","Control_B","Infected_A","Infected_B")

# Reshape data frame into "re_all" so that all values are recorded as a 1-dimensional list
# Reshaping allows for easy manipulation of data into graphs
t1_re_all <- reshape(t1_All_total_survival, varying=c(1:t1_week_num), v.names='Proportion',
                  direction='long', idvar='groupName')

# Plot graph of percentage alive

ggplot(t1_re_all, aes(x = time, y = Proportion, color = groupName, fill = groupName)) +
  geom_line() + scale_color_manual(values = c("#A6CEE3", "#1F78B4", "#FB9A99", "#E31A1C")) +
  ggtitle("Triatoma Rep. 1 Proportion Alive") + xlab("Week") + ylab("Proportion Alive") +
  theme(text = element_text(size=16),axis.text.x=element_text(size=14),axis.text.x=element_text(size=14))


# Combine A & B into one group
t1_controls <- (t1_A_control_totals+t1_B_control_totals)/2
t1_infected <- (t1_A_inf_totals+t1_B_inf_totals)/2

# Create data frame with vectors of life totals for the 4 groups, A and B of control and infected
t1_comb_total_survival <- cbind(c(t1_controls),c(t1_infected))
t1_comb_total_survival <- as.data.frame(t(t1_comb_total_survival))

# Create column names for the weeks of recording, and row names for group
colnames(t1_comb_total_survival) <- c(0:(t1_week_num-1))
t1_comb_total_survival$groupName <-  c("Controls","Infected")

# Reshape data frame into "re_all" so that all values are recorded as a 1-dimensional list
# Reshaping allows for easy manipulation of data into graphs
t1_re_comb <- reshape(t1_comb_total_survival, varying=c(1:t1_week_num), v.names='Proportion',
                      direction='long', idvar='groupName')

# Plot graph of percentage alive
ggplot(t1_re_comb, aes(x = time, y = Proportion, color = groupName, fill = groupName)) +
  geom_line() + scale_color_manual(values = c("#1F78B4", "#E31A1C")) +
  ggtitle("Triatoma Rep. 1 Proportion Alive (Combined)") + xlab("Week") + ylab("Proportion Alive") +
  theme(text = element_text(size=16),axis.text.x=element_text(size=14),axis.text.x=element_text(size=14))

############################
## Civex Rep 2 data
############################

# Load in csv download of life tables
raw_c2_csv <- read.csv(file = 'ciclo vida cimex Rep 2 - cimex.csv',skip=2)


############
## Set Week number below
## How many weeks (including week zero, e.g. weeks (0,1, and 2) would be 3 weeks)
c2_week_num = 36
# Current up to "week 11", so 12 weeks

############ 
## Set How many bugs were used for each group (e.g. 50 in Infected A)
c2_bug_num = 50


# Remove Recorded date column (will use week column)
raw_c2_csv[2] <- NULL

# Replace blank cells with 0
raw_c2_csv[is.na(raw_c2_csv)] <- 0

# Setting up groups, group1 = control, group2 = infected
c2_group1 <- raw_c2_csv
c2_group2 <- raw_c2_csv

# Remove group 2 observations from group1
for(i in seq (19:35)){
  c2_group1[19] <- NULL
}

# Remove group 1 observations from group2
for(i in seq (3:19)){
  c2_group2[3] <- NULL
}

# Uninfected Controls, Total life and death counts per week
c2_alive_control_totals <- c2_group1[17]/c2_bug_num


c2_even_indices <- seq (2,c2_week_num*2,2)
c2_odd_indices <- seq (1,c2_week_num*2,2)



# Totals of A and B groups
c2_A_control_totals <- c2_alive_control_totals[c2_odd_indices,1]
c2_B_control_totals <- c2_alive_control_totals[c2_even_indices,1]

# Infected Cimex, Total life and death counts per week
c2_alive_inf_totals <- c2_group2[17]/c2_bug_num

# Totals of A and B infected groups
c2_A_inf_totals <- c2_alive_inf_totals[c2_odd_indices,1]
c2_B_inf_totals <- c2_alive_inf_totals[c2_even_indices,1]

# Create data frame with vectors of life totals for the 4 groups, A and B of control and infected
c2_All_total_survival <- cbind(c(c2_A_control_totals),c(c2_B_control_totals),c(c2_A_inf_totals),c(c2_B_inf_totals))
c2_All_total_survival <- as.data.frame(t(c2_All_total_survival))

# Create column names for the weeks of recording, and row names for group
colnames(c2_All_total_survival) <- c(0:( c2_week_num - 1))
c2_All_total_survival$groupName <-  c("Control_A","Control_B","Infected_A","Infected_B")

# Reshape data frame into "re_all" so that all values are recorded as a 1-dimensional list
# Reshaping allows for easy manipulation of data into graphs
c2_re_all <- reshape(c2_All_total_survival, varying=c(1:c2_week_num), v.names='Proportion',
                     direction='long', idvar='groupName')

# Plot graph of percentage alive
ggplot(c2_re_all, aes(x = time, y = Proportion, color = groupName, fill = groupName)) +
  geom_line() + scale_color_manual(values = c("#A6CEE3", "#1F78B4", "#FB9A99", "#E31A1C")) +
  ggtitle("Cimex Rep. 2 Proportion Alive") + xlab("Week") + ylab("Proportion Alive") +
  theme(text = element_text(size=16),axis.text.x=element_text(size=14),axis.text.x=element_text(size=14))


# Combine A & B into one group
c2_controls <- (c2_A_control_totals+c2_B_control_totals)/2
c2_infected <- (c2_A_inf_totals+c2_B_inf_totals)/2

# Create data frame with vectors of life totals for the 4 groups, A and B of control and infected
c2_comb_total_survival <- cbind(c(c2_controls),c(c2_infected))
c2_comb_total_survival <- as.data.frame(t(c2_comb_total_survival))

# Create column names for the weeks of recording, and row names for group
colnames(c2_comb_total_survival) <- c(0:(c2_week_num-1))
c2_comb_total_survival$groupName <-  c("Controls","Infected")

# Reshape data frame into "re_all" so that all values are recorded as a 1-dimensional list
# Reshaping allows for easy manipulation of data into graphs
c2_re_comb <- reshape(c2_comb_total_survival, varying=c(1:c2_week_num), v.names='Proportion',
                      direction='long', idvar='groupName')

# Plot graph of percentage alive
ggplot(c2_re_comb, aes(x = time, y = Proportion, color = groupName, fill = groupName)) +
  geom_line() + scale_color_manual(values = c("#1F78B4", "#E31A1C")) +
  ggtitle("Cimex Rep. 2 Proportion Alive (Combined)") + xlab("Week") + ylab("Proportion Alive") +
  theme(text = element_text(size=16),axis.text.x=element_text(size=14),axis.text.x=element_text(size=14))


############################
## Triatoma Rep 2 data
############################

# Load in csv download of life tables
raw_t2_csv <- read.csv(file = 'ciclo vida cimex Rep 2 - triatoma.csv',skip=3)


############
## Set Week number below
## How many weeks (including week zero, e.g. weeks (0,1, and 2) would be 3 weeks)
t2_week_num = 36
# Current up to "week 11", so 12 weeks

############ 
## Set How many bugs were used for each group (e.g. 50 in Infected A)
t2_bug_num = 50



# Remove Recorded date column (will use week column)
raw_t2_csv[2] <- NULL

# Replace blank cells with 0
raw_t2_csv[is.na(raw_t2_csv)] <- 0

# Setting up groups, group1 = control, group2 = infected
t2_group1 <- raw_t2_csv
t2_group2 <- raw_t2_csv

# Clear any empty lines below data
t2_group1 <- head(t2_group1, t2_week_num*2)
t2_group2 <- head(t2_group2, t2_week_num*2)

# Remove group 2 observations from group1
for(i in seq (19:35)){
  t2_group1[19] <- NULL
}

# Remove group 1 observations from group2
for(i in seq (3:19)){
  t2_group2[3] <- NULL
}



t2_even_indices <- seq (2,t2_week_num*2,2)
t2_odd_indices <- seq (1,t2_week_num*2,2)



# Uninfected Controls, Total life and death counts per week

t2_alive_control_totals <- t2_group1[17]/t2_bug_num

# Totals of A and B groups
t2_A_control_totals <- t2_alive_control_totals[t2_odd_indices,1]
t2_B_control_totals <- t2_alive_control_totals[t2_even_indices,1]

# Infected Triatoma, Total life and death counts per week
t2_alive_inf_totals <- t2_group2[17]/t2_bug_num


# Totals of A and B infected groups
t2_A_inf_totals <- t2_alive_inf_totals[t2_odd_indices,1]
t2_B_inf_totals <- t2_alive_inf_totals[t2_even_indices,1]

# Create data frame with vectors of life totals for the 4 groups, A and B of control and infected
t2_All_total_survival <- cbind(c(t2_A_control_totals),c(t2_B_control_totals),c(t2_A_inf_totals),c(t2_B_inf_totals))
t2_All_total_survival <- as.data.frame(t(t2_All_total_survival))

# Create column names for the weeks of recording, and row names for group
colnames(t2_All_total_survival) <- c(0: ( t2_week_num - 1))
t2_All_total_survival$groupName <-  c("Control_A","Control_B","Infected_A","Infected_B")

# Reshape data frame into "re_all" so that all values are recorded as a 1-dimensional list
# Reshaping allows for easy manipulation of data into graphs
t2_re_all <- reshape(t2_All_total_survival, varying=c(1:t2_week_num), v.names='Proportion',
                     direction='long', idvar='groupName')

# Plot graph of percentage alive
ggplot(t2_re_all, aes(x = time, y = Proportion, color = groupName, fill = groupName)) +
  geom_line() + scale_color_manual(values = c("#A6CEE3", "#1F78B4", "#FB9A99", "#E31A1C")) +
  ggtitle("Triatoma Rep. 2 Proportion Alive") + xlab("Week") + ylab("Proportion Alive") +
  theme(text = element_text(size=16),axis.text.x=element_text(size=14),axis.text.x=element_text(size=14))



# Combine A & B into one group
t2_controls <- (t2_A_control_totals+t2_B_control_totals)/2
t2_infected <- (t2_A_inf_totals+t2_B_inf_totals)/2

# Create data frame with vectors of life totals for the 4 groups, A and B of control and infected
t2_comb_total_survival <- cbind(c(t2_controls),c(t2_infected))
t2_comb_total_survival <- as.data.frame(t(t2_comb_total_survival))

# Create column names for the weeks of recording, and row names for group
colnames(t2_comb_total_survival) <- c(0:(t2_week_num-1))
t2_comb_total_survival$groupName <-  c("Controls","Infected")

# Reshape data frame into "re_all" so that all values are recorded as a 1-dimensional list
# Reshaping allows for easy manipulation of data into graphs
t2_re_comb <- reshape(t2_comb_total_survival, varying=c(1:t2_week_num), v.names='Proportion',
                      direction='long', idvar='groupName')

# Plot graph of percentage alive
ggplot(t2_re_comb, aes(x = time, y = Proportion, color = groupName, fill = groupName)) +
  geom_line() + scale_color_manual(values = c("#1F78B4", "#E31A1C")) +
  ggtitle("Triatoma Rep. 2 Proportion Alive (Combined)") + xlab("Week") + ylab("Proportion Alive") +
  theme(text = element_text(size=16),axis.text.x=element_text(size=14),axis.text.x=element_text(size=14))




############################

#dev.off()




############################
## Cimex Base Pilot Data
############################



# Load in csv download of life tables
raw_cb_csv <- read.csv(file = 'base datos piloto - cimex.csv')



############
## Set Week number below
## How many weeks (including week zero, e.g. weeks (0,1, and 2) would be 3 weeks)
cb_week_num = 56
# Current up to "week 46", so 47 weeks

############ 
## Set How many bugs were used for each group (e.g. 40 in Infected A)
cb_bug_num = 40



# Remove Recorded date column (will use week column)
raw_cb_csv[5] <- NULL
raw_cb_csv[1] <- NULL
raw_cb_csv[1] <- NULL
raw_cb_csv[1] <- NULL
# ONLY RUN THESE ONCE

# Replace blank cells with 0
raw_cb_csv[is.na(raw_cb_csv)] <- 0

#### CHANGE THESE TO DYNAMIC VAR
# Setting up groups, group1 = control, group2 = infected
# Control
cb_group1 <- raw_cb_csv[1:56,]
# Rat 1
cb_group2 <- raw_cb_csv[57:112,]
# Rat 2
cb_group3 <- raw_cb_csv[113:168,]


### FOR CONTROL NEED LINES 1-56
### FOR INFECTED ONE NEED LINES 57-112
### FOR INFECTED TWO NEED LINES 113-168


# Uninfected Controls, Total life and death counts per week
cb_alive_control_totals <- cb_group1[16]/cb_bug_num

# Infected Cimex, Total life and death counts per week
cb_alive_inf1_totals <- cb_group2[16]/cb_bug_num

cb_alive_inf2_totals <- cb_group3[16]/cb_bug_num

# Create data frame with vectors of life totals for the 4 groups, A and B of control and infected
cb_All_total_survival <- cbind(cb_alive_control_totals,cb_alive_inf1_totals,cb_alive_inf2_totals)
cb_All_total_survival <- as.data.frame(t(cb_All_total_survival))

# Create column names for the weeks of recording, and row names for group
colnames(cb_All_total_survival) <- c(1:cb_week_num)
cb_All_total_survival$groupName <-  c("Control","Infected_1","Infected_2")

# Reshape data frame into "re_all" so that all values are recorded as a 1-dimensional list
# Reshaping allows for easy manipulation of data into graphs
cb_re_all <- reshape(cb_All_total_survival, varying=c(1:cb_week_num), v.names='Proportion',
                     direction='long', idvar='groupName')

# Plot graph of percentage alive
ggplot(cb_re_all, aes(x = time, y = Proportion, color = groupName, fill = groupName)) +
  geom_line() + scale_color_manual(values = c("#1F78B4", "#FB9A99", "#E31A1C")) +
  ggtitle("Cimex Pilot Proportion Alive") + xlab("Week") + ylab("Proportion Alive") +
  theme(text = element_text(size=16),axis.text.x=element_text(size=14),axis.text.x=element_text(size=14))


############################
## Triatoma Pilot data
############################
#this part was done from a unclean data CSV file. 
#I updated the CSV and changed the spreadsheet format as the spreadsheet for "base datos piloto - cimex. csv" 
#to run this part I used the same code developed above for "Cimex pilot data"
#Aditional I disable the code to run the uncean data. Renzo


# Load in csv download of life tables
#raw_tb_csv <- read.csv(file = 'base datos piloto - triatoma.csv',skip=3)
# Remove Recorded date column (will use week column)


############
## Week number determined later (different spreadsheet format)
############ 


############ 
## Set How many bugs were used for each group (e.g. 40 in Infected A)
#tb_bug_num = 20


#raw_tb_csv[35] <- NULL
#raw_tb_csv[18] <- NULL


# Replace blank cells with 0
#raw_tb_csv[is.na(raw_tb_csv)] <- 0

# Setting up groups, group1 = control, group2 = infected
#tb_group1 <- raw_tb_csv
#tb_group2 <- raw_tb_csv
#tb_group3 <- raw_tb_csv

######## THIS MAY CHANGE IF THE SPREADSHEET IS ALTERED,
# Assumes pilot study won't change
#tb_group1 <- head(tb_group1, 221)
#tb_group2 <- head(tb_group2, 221)
#tb_group3 <- head(tb_group2, 221)

# Remove group 2 observations from group1
#for(i in seq (2:33)){
#  tb_group1[2] <- NULL
#}

# Remove group 1 and...  observations from group2
#for(i in seq (18:49)){
#  tb_group2[18] <- NULL
#}

# Remove
#for(i in seq (2:17)){
#  tb_group3[2] <- NULL
#}
#for(i in seq (18:33)){
#  tb_group3[18] <- NULL
#}


# Uninfected Controls, Total life and death counts per week
#tb_alive_control_totals <- tb_group1[16]/tb_bug_num

# Infected Cimex, Total life and death counts per week
#tb_alive_inf1_totals <- tb_group2[16]/tb_bug_num

#tb_alive_inf2_totals <- tb_group3[16]/tb_bug_num

# Create data frame with vectors of life totals for the 4 groups, A and B of control and infected
#tb_raw_total_survival <- cbind(tb_alive_control_totals,tb_alive_inf1_totals,tb_alive_inf2_totals)
# Remove Rows with only zeros
#temp_tb_ALL <- tb_raw_total_survival[rowSums(tb_raw_total_survival)!=0, ]
#numWeeks <- nrow(temp_tb_ALL)

#b_even_indices <- seq (2,numWeeks,2)
#b_odd_indices <- seq (1,numWeeks,2)

#tb_ALL <- cbind(temp_tb_ALL[b_odd_indices,1],temp_tb_ALL[b_even_indices,1],temp_tb_ALL[b_odd_indices,2],
#                temp_tb_ALL[b_even_indices,2],temp_tb_ALL[b_odd_indices,3],temp_tb_ALL[b_even_indices,3])

#tb_ALL_comb <- cbind((temp_tb_ALL[b_odd_indices,1]+temp_tb_ALL[b_even_indices,1])*.5,(temp_tb_ALL[b_odd_indices,2]+
#                temp_tb_ALL[b_even_indices,2])*.5,(temp_tb_ALL[b_odd_indices,3]+temp_tb_ALL[b_even_indices,3])*.5)

#tb_All_total_survival <- as.data.frame(t(tb_ALL))
#tb_All_total_surv_comb <- as.data.frame(t(tb_ALL_comb))


# Create column names for the weeks of recording, and row names for group
#colnames(tb_All_total_survival) <- c(1:ncol(tb_All_total_survival))
#colnames(tb_All_total_surv_comb) <- c(1:ncol(tb_All_total_surv_comb))
#tb_All_total_survival$groupName <-  c("Control A","Control B","Infected_1 A",
#                                      "Infected_1 B","Infected_2 A","Infected_2 B")
#tb_All_total_surv_comb$groupName <-  c("Control","Infected_1","Infected_2")


# Reshape data frame into "re_all" so that all values are recorded as a 1-dimensional list
# Reshaping allows for easy manipulation of data into graphs
#tb_re_all <- reshape(tb_All_total_survival, varying=c(1:(ncol(tb_All_total_survival)-1)), v.names='Proportion',
#                     direction='long', idvar='groupName')

#tb_comb_re_all <- reshape(tb_All_total_surv_comb, varying=c(1:(ncol(tb_All_total_surv_comb)-1)), v.names='Proportion',
#                     direction='long', idvar='groupName')

# Plot graph of percentage alive

#ggplot(tb_re_all, aes(x = time, y = Proportion, color = groupName, fill = groupName)) +
#  geom_line() + scale_color_manual(values = c("#1F78B4","#1F78B4","#FB9A99","#FB9A99","#E31A1C","#E31A1C")) +
#  ggtitle("Triatoma Pilot Proportion Alive") + xlab("Week") + ylab("Proportion Alive") +
#  theme(text = element_text(size=16),axis.text.x=element_text(size=14),axis.text.x=element_text(size=14))


# Graph plotting percentage alive (Combined)
#ggplot(tb_comb_re_all, aes(x = time, y = Proportion, color = groupName, fill = groupName)) +
#  geom_line() + scale_color_manual(values = c("#1F78B4","#FB9A99","#E31A1C")) +
#  ggtitle("Triatoma Pilot Proportion Alive (Combined)") + xlab("Week") + ylab("Proportion Alive") +
#  theme(text = element_text(size=16),axis.text.x=element_text(size=14),axis.text.x=element_text(size=14))


#############################################
## triatoma Base Pilot Data fixed by Renzo ##
#############################################



# Load in csv download of life tables
raw_cb_csv <- read.csv(file = 'base datos piloto - triatoma.csv')



############
## Set Week number below
## How many weeks (including week zero, e.g. weeks (0,1, and 2) would be 3 weeks)
cb_week_num = 67
# Current up to "week 46", so 47 weeks

############ 
## Set How many bugs were used for each group (e.g. 40 in Infected A)
cb_bug_num = 40



# Remove Recorded date column (will use week column)
raw_cb_csv[5] <- NULL
raw_cb_csv[1] <- NULL
raw_cb_csv[1] <- NULL
raw_cb_csv[1] <- NULL
# ONLY RUN THESE ONCE

# Replace blank cells with 0
raw_cb_csv[is.na(raw_cb_csv)] <- 0

#### CHANGE THESE TO DYNAMIC VAR
# Setting up groups, group1 = control, group2 = infected
# Control
cb_group1 <- raw_cb_csv[1:67,]
# Rat 1
cb_group2 <- raw_cb_csv[68:134,]
# Rat 2
cb_group3 <- raw_cb_csv[135:201,]


### FOR CONTROL NEED LINES 1-67
### FOR INFECTED ONE NEED LINES 68-134
### FOR INFECTED TWO NEED LINES 135-201


# Uninfected Controls, Total life and death counts per week
cb_alive_control_totals <- cb_group1[16]/cb_bug_num

# Infected Cimex, Total life and death counts per week
cb_alive_inf1_totals <- cb_group2[16]/cb_bug_num

cb_alive_inf2_totals <- cb_group3[16]/cb_bug_num

# Create data frame with vectors of life totals for the 4 groups, A and B of control and infected
cb_All_total_survival <- cbind(cb_alive_control_totals,cb_alive_inf1_totals,cb_alive_inf2_totals)
cb_All_total_survival <- as.data.frame(t(cb_All_total_survival))

# Create column names for the weeks of recording, and row names for group
colnames(cb_All_total_survival) <- c(1:cb_week_num)
cb_All_total_survival$groupName <-  c("Control","Infected_1","Infected_2")

# Reshape data frame into "re_all" so that all values are recorded as a 1-dimensional list
# Reshaping allows for easy manipulation of data into graphs
cb_re_all <- reshape(cb_All_total_survival, varying=c(1:cb_week_num), v.names='Proportion',
                     direction='long', idvar='groupName')

# Plot graph of percentage alive
ggplot(cb_re_all, aes(x = time, y = Proportion, color = groupName, fill = groupName)) +
  geom_line() + scale_color_manual(values = c("#1F78B4", "#FB9A99", "#E31A1C")) +
  ggtitle("triatoma Pilot Proportion Alive") + xlab("Week") + ylab("Proportion Alive") +
  theme(text = element_text(size=16),axis.text.x=element_text(size=15),axis.text.x=element_text(size=14))




###
# Uncomment the line below if you are writing these figures to a PDF
###
dev.off()








