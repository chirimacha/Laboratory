file = file.choose() #FILE NAME: camera data control 071818
data<-read.csv(file)

#****THIS SCRIPT IS TO TEST IF WE CAN POOL **CONTROL** DATA ACROSS DAYS AND ARENAS/CAMERAS (THE SAME ARENA WAS USED WITH THE SAME CAMERA)***

#Variables tested in this script:
#-total distance travelled
#-total time spent away from the wall (in 'the open')
#-total distance travelled in the open

#For stats help, consult: http://www.sthda.com/english/wiki/one-way-anova-test-in-r#assumptions-of-anova-test


#1.DAY by DISTANCE TRAVELLED
#++++++++++++++++++
#Check that days are grouped correctly
levels(data$day)

# Visualize data
# Box plots: distance travelled by day
library("ggpubr")
ggboxplot(data, x = "day", y = "total_distance", 
          color = "day", 
          ylab = "Total distance travelled", xlab = "Day")

# Mean plots: distance travelled by day
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(data, x = "day", y = "total_distance", 
       add = c("mean_se", "jitter"), 
       ylab = "total", xlab = "day")

#Generate summary statistics by day
library(dplyr)
group_by(data, day) %>%
  summarise(
    count = n(),
    mean = mean(total_distance, na.rm = TRUE),
    sd = sd(total_distance, na.rm = TRUE)
  )

#check that data fit the anova assumptions (can also do this after the test- see below)

#Normal distribution
shapiro.test(data$total_distance) # if p> 0.05, can assume a normal distribution

#visually inspect distribution
library("ggpubr")
ggdensity(data$total_distance)

#test homoscedasticity (homogeneity of variance)
#If the p-value is not less than the significance level of 0.05, this means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

bartlett.test(data$total_distance,data$day)
#Barlett's test is more sensitive to departures from normality, so if data are not normal, that may explain big differences between the two tests

library(car)
leveneTest(total_distance ~ day, data = data)


#If test assumptions are met, compute one way anova. If test assumptions are not met, alternative tests are below 
res.aov1 <- aov(total_distance ~ day, data = data)
summary(res.aov1)

#If result is significant, use Tukey HSD test to find the difference- p value is adjusted for multiple comparisons
TukeyHSD(res.aov1)

#The function pairwise.t.test() can be also used to calculate pairwise comparisons between group levels with corrections for multiple testing.

pairwise.t.test(data$total_distance, data$day,
                p.adjust.method = "BH")

#can also check that the data fit anova assumptions using anova results
# 1. Homogeneity of variances
plot(res.aov1, 1)

#2. normal distribution using Q-Q plots with Shapiro-Wilks
#if all the points fall approximately along the reference line, we can assume normality.
plot(res.aov1, 2)
# Extract the residuals
aov_residuals1 <- residuals(object = res.aov1 )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals1 )

##ALTERNATIVE TESTS

## when equal variances assumption is not met, can use:
# A Welch one-way test does not require the assumption of equal variances 
oneway.test(total_distance ~ day, data = data)

#Or use a pairwise t-test
pairwise.t.test(data$total_distance, data$day,
                p.adjust.method = "BH", pool.sd = FALSE)

## when data aren't normal, can use a Kruskal Wallace test as a non-parametric alternative
kruskal.test(total_distance ~ day, data = data)

#post-hoc analysis to determine which levels of the independent variable differ from each other 
library(pgirmess)
kruskalmc(total_distance ~ day, data = data)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#2.DAY by TIME SPENT IN THE OPEN (I.E., AWAY FROM THE WALL) 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Check that days are grouped correctly
levels(data$day)

# Visualize data
# Box plots: time spent in open by day
library("ggpubr")
ggboxplot(data, x = "day", y = "total_opentime", 
          color = "day", 
          ylab = "Total time spent away from the wall", xlab = "Day")

# Mean plots: time spent in open by day
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(data, x = "day", y = "total_opentime", 
       add = c("mean_se", "jitter"), 
       ylab = "Total time spent away from the wall", xlab = "day")

#Generate summary statistics by day
library(dplyr)
group_by(data, day) %>%
  summarise(
    count = n(),
    mean = mean(total_opentime, na.rm = TRUE),
    sd = sd(total_opentime, na.rm = TRUE)
  )

#check that data fit the anova assumptions (can also do this after the test- see below)

#Normal distribution
shapiro.test(data$total_opentime) # if p> 0.05, can assume a normal distribution

#visually inspect distribution
library("ggpubr")
ggdensity(data$total_opentime)

#test homoscedasticity (homogeneity of variance)
#If the p-value is not less than the significance level of 0.05, this means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

bartlett.test(data$total_opentime,data$day)

library(car)
leveneTest(total_opentime ~ day, data = data)


#If test assumptions are met, compute one way anova. If test assumptions are not met, alternative tests are below 
res.aov2 <- aov(total_opentime ~ day, data = data)
summary(res.aov2)

#If result is significant, use Tukey HSD test to find the difference- p value is adjusted for multiple comparisons
TukeyHSD(res.aov2)

#The function pairwise.t.test() can be also used to calculate pairwise comparisons between group levels with corrections for multiple testing.

pairwise.t.test(data$total_opentime, data$day,
                p.adjust.method = "BH")

#can also check that the data fit anova assumptions using anova results
# 1. Homogeneity of variances
plot(res.aov2, 1)

#2. normal distribution using Q-Q plots with Shapiro-Wilks
#if all the points fall approximately along the reference line, we can assume normality.
plot(res.aov2, 2)
# Extract the residuals
aov_residuals2 <- residuals(object = res.aov2 )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals2 )

##ALTERNATIVE TESTS

## when equal variances assumption is not met, can use:
# A Welch one-way test does not require the assumption of equal variances 
oneway.test(total_opentime ~ day, data = data)

#Or use a pairwise t-test
pairwise.t.test(data$total_opentime, data$day,
                p.adjust.method = "BH", pool.sd = FALSE)

## when data aren't normal, can use a Kruskal Wallace test as a non-parametric alternative
kruskal.test(total_opentime ~ day, data = data)

#post-hoc analysis to determine which levels of the independent variable differ from each other 
library(pgirmess)
kruskalmc(total_opentime ~ day, data = data)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#3.DAY by DISTANCE TRAVELLED IN THE OPEN (I.E., AWAY FROM THE WALL) 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Check that days are grouped correctly
levels(data$day)

# Visualize data
# Box plots: distance travelled away from wall by day
library("ggpubr")
ggboxplot(data, x = "day", y = "total_open_distance", 
          color = "day", 
          ylab = "Total distance travelled away from the wall", xlab = "Day")

# Mean plots: distance travelled by day
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(data, x = "day", y = "total_open_distance", 
       add = c("mean_se", "jitter"), 
       ylab = "Total distance travelled away from the wall", xlab = "day")

#Generate summary statistics by day
library(dplyr)
group_by(data, day) %>%
  summarise(
    count = n(),
    mean = mean(total_open_distance, na.rm = TRUE),
    sd = sd(total_open_distance, na.rm = TRUE)
  )

#check that data fit the anova assumptions (can also do this after the test- see below)

#Normal distribution
shapiro.test(data$total_open_distance) # if p> 0.05, can assume a normal distribution

#visually inspect distribution
library("ggpubr")
ggdensity(data$total_open_distance)

#test homoscedasticity (homogeneity of variance)
#If the p-value is not less than the significance level of 0.05, this means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

bartlett.test(data$total_open_distance,data$day)

library(car)
leveneTest(total_open_distance ~ day, data = data)


#If test assumptions are met, compute one way anova. If test assumptions are not met, alternative tests are below 
res.aov3 <- aov(total_open_distance ~ day, data = data)
summary(res.aov3)

#If result is significant, use Tukey HSD test to find the difference- p value is adjusted for multiple comparisons
TukeyHSD(res.aov3)

#The function pairwise.t.test() can be also used to calculate pairwise comparisons between group levels with corrections for multiple testing.

pairwise.t.test(data$total_open_distance, data$day,
                p.adjust.method = "BH")

#can also check that the data fit anova assumptions using anova results
# 1. Homogeneity of variances
plot(res.aov3, 1)

#2. normal distribution using Q-Q plots with Shapiro-Wilks
#if all the points fall approximately along the reference line, we can assume normality.
plot(res.aov3, 2)
# Extract the residuals
aov_residuals3 <- residuals(object = res.aov3 )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals3 )

##ALTERNATIVE TESTS

## when equal variances assumption is not met, can use:
# A Welch one-way test does not require the assumption of equal variances 
oneway.test(total_open_distance ~ day, data = data)

#Or use a pairwise t-test
pairwise.t.test(data$total_open_distance, data$day,
                p.adjust.method = "BH", pool.sd = FALSE)

## when data aren't normal, can use a Kruskal Wallace test as a non-parametric alternative
kruskal.test(total_open_distance ~ day, data = data)

#post-hoc analysis to determine which levels of the independent variable differ from each other 
library(pgirmess)
kruskalmc(total_open_distance ~ day, data = data)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#4.CAMERA by DISTANCE TRAVELLED
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Check that camera data are grouped correctly
levels(data$camera)

# Visualize data
# Box plots: distance travelled by camera
library("ggpubr")
ggboxplot(data, x = "camera", y = "total_distance", 
          color = "camera", 
          ylab = "Total distance travelled", xlab = "camera")

# Mean plots: distance travelled by camera
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(data, x = "camera", y = "total_distance", 
       add = c("mean_se", "jitter"), 
       ylab = "Total distance travelled", xlab = "camera")

#Generate summary statistics by arena
library(dplyr)
group_by(data, camera) %>%
  summarise(
    count = n(),
    mean = mean(total_distance, na.rm = TRUE),
    sd = sd(total_distance, na.rm = TRUE)
  )

#check that data fit the anova assumptions (can also do this after the test- see below)

#Normal distribution
shapiro.test(data$total_distance) # if p> 0.05, can assume a normal distribution

#visually inspect distribution
library("ggpubr")
ggdensity(data$total_distance)

#test homoscedasticity (homogeneity of variance)
#If the p-value is not less than the significance level of 0.05, this means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

bartlett.test(data$total_distance,data$camera)

library(car)
leveneTest(total_distance ~ camera, data = data)


#If test assumptions are met, compute one way anova. If test assumptions are not met, alternative tests are below 
res.aov4 <- aov(total_distance ~ camera, data = data)
summary(res.aov4)

#If result is significant, use Tukey HSD test to find the difference- p value is adjusted for multiple comparisons
TukeyHSD(res.aov4)

#The function pairwise.t.test() can be also used to calculate pairwise comparisons between group levels with corrections for multiple testing.

pairwise.t.test(data$total_distance, data$camera,
                p.adjust.method = "BH")

#can also check that the data fit anova assumptions using anova results
# 1. Homogeneity of variances
plot(res.aov4, 1)

#2. normal distribution using Q-Q plots with Shapiro-Wilks
#if all the points fall approximately along the reference line, we can assume normality.
plot(res.aov4, 2)
# Extract the residuals
aov_residuals4 <- residuals(object = res.aov4 )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals4 )

##ALTERNATIVE TESTS

## when equal variances assumption is not met, can use:
# A Welch one-way test does not require the assumption of equal variances 
oneway.test(total_distance ~ camera, data = data)

#Or use a pairwise t-test
pairwise.t.test(data$total_distance, data$camera,
                p.adjust.method = "BH", pool.sd = FALSE)

## when data aren't normal, can use a Kruskal Wallace test as a non-parametric alternative
kruskal.test(total_distance ~ camera, data = data)

#post-hoc analysis to determine which levels of the independent variable differ from each other 
library(pgirmess)
kruskalmc(total_distance ~ camera, data = data)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#5.CAMERA/ARENA by TIME SPENT IN THE OPEN (I.E., AWAY FROM THE WALL) 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Check that days are grouped correctly
levels(data$camera)

# Visualize data
# Box plots: time spent in open by camera/arena
library("ggpubr")
ggboxplot(data, x = "camera", y = "total_opentime", 
          color = "camera", 
          ylab = "Total time spent away from the wall", xlab = "Camera/Arena")

# Mean plots: time spent in open by camera/arena
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(data, x = "camera", y = "total_opentime", 
       add = c("mean_se", "jitter"), 
       ylab = "Total time spent away from the wall", xlab = "Camera/Arena")

#Generate summary statistics by day
library(dplyr)
group_by(data, camera) %>%
  summarise(
    count = n(),
    mean = mean(total_opentime, na.rm = TRUE),
    sd = sd(total_opentime, na.rm = TRUE)
  )

#check that data fit the anova assumptions (can also do this after the test- see below)

#Normal distribution
shapiro.test(data$total_opentime) # if p> 0.05, can assume a normal distribution

#visually inspect distribution
library("ggpubr")
ggdensity(data$total_opentime)

#test homoscedasticity (homogeneity of variance)
#If the p-value is not less than the significance level of 0.05, this means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

bartlett.test(data$total_opentime,data$camera)

library(car)
leveneTest(total_opentime ~ camera, data = data)


#If test assumptions are met, compute one way anova. If test assumptions are not met, alternative tests are below 
res.aov5 <- aov(total_opentime ~camera, data = data)
summary(res.aov5)

#If result is significant, use Tukey HSD test to find the difference- p value is adjusted for multiple comparisons
TukeyHSD(res.aov5)

#The function pairwise.t.test() can be also used to calculate pairwise comparisons between group levels with corrections for multiple testing.

pairwise.t.test(data$total_opentime, data$camera,
                p.adjust.method = "BH")

#can also check that the data fit anova assumptions using anova results
# 1. Homogeneity of variances
plot(res.aov5, 1)

#2. normal distribution using Q-Q plots with Shapiro-Wilks
#if all the points fall approximately along the reference line, we can assume normality.
plot(res.aov5, 2)
# Extract the residuals
aov_residuals5 <- residuals(object = res.aov5 )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals5 )

##ALTERNATIVE TESTS

## when equal variances assumption is not met, can use:
# A Welch one-way test does not require the assumption of equal variances 
oneway.test(total_opentime ~ camera, data = data)

#Or use a pairwise t-test
pairwise.t.test(data$total_opentime, data$camera,
                p.adjust.method = "BH", pool.sd = FALSE)

## when data aren't normal, can use a Kruskal Wallace test as a non-parametric alternative
kruskal.test(total_opentime ~ camera, data = data)

#post-hoc analysis to determine which levels of the independent variable differ from each other 
library(pgirmess)
kruskalmc(total_opentime ~ camera, data = data)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#6.CAMERA/ARENA by DISTANCE TRAVELLED IN THE OPEN (I.E., AWAY FROM THE WALL) 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Check that days are grouped correctly
levels(data$camera)

# Visualize data
# Box plots: distance travelled away from wall by day
library("ggpubr")
ggboxplot(data, x = "camera", y = "total_open_distance", 
          color = "camera", 
          ylab = "Total distance travelled away from the wall", xlab = "Camera/Arena")

# Mean plots: distance travelled by day
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(data, x = "camera", y = "total_open_distance", 
       add = c("mean_se", "jitter"), 
       ylab = "Total distance travelled away from the wall", xlab = "camera")

#Generate summary statistics by day
library(dplyr)
group_by(data, camera) %>%
  summarise(
    count = n(),
    mean = mean(total_open_distance, na.rm = TRUE),
    sd = sd(total_open_distance, na.rm = TRUE)
  )

#check that data fit the anova assumptions (can also do this after the test- see below)

#Normal distribution
shapiro.test(data$total_open_distance) # if p> 0.05, can assume a normal distribution

#visually inspect distribution
library("ggpubr")
ggdensity(data$total_open_distance)

#test homoscedasticity (homogeneity of variance)
#If the p-value is not less than the significance level of 0.05, this means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

bartlett.test(data$total_open_distance,data$camera)

library(car)
leveneTest(total_open_distance ~ camera, data = data)


#If test assumptions are met, compute one way anova. If test assumptions are not met, alternative tests are below 
res.aov6 <- aov(total_open_distance ~ camera, data = data)
summary(res.aov6)

#If result is significant, use Tukey HSD test to find the difference- p value is adjusted for multiple comparisons
TukeyHSD(res.aov6)

#The function pairwise.t.test() can be also used to calculate pairwise comparisons between group levels with corrections for multiple testing.

pairwise.t.test(data$total_open_distance, data$day,
                p.adjust.method = "BH")

#can also check that the data fit anova assumptions using anova results
# 1. Homogeneity of variances
plot(res.aov6, 1)

#2. normal distribution using Q-Q plots with Shapiro-Wilks
#if all the points fall approximately along the reference line, we can assume normality.
plot(res.aov6, 2)
# Extract the residuals
aov_residuals6 <- residuals(object = res.aov6)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals6)

##ALTERNATIVE TESTS

## when equal variances assumption is not met, can use:
# A Welch one-way test does not require the assumption of equal variances 
oneway.test(total_open_distance ~ camera, data = data)

#Or use a pairwise t-test
pairwise.t.test(data$total_open_distance, data$camera,
                p.adjust.method = "BH", pool.sd = FALSE)

## when data aren't normal, can use a Kruskal Wallace test as a non-parametric alternative
kruskal.test(total_open_distance ~ camera, data = data)

#post-hoc analysis to determine which levels of the independent variable differ from each other 
library(pgirmess)
kruskalmc(total_open_distance ~ camera, data = data)




