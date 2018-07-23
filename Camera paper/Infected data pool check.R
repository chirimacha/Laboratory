file1 = file.choose() #FILE NAME: camera data infected 071818
data1<-read.csv(file1)

#****THIS SCRIPT IS TO TEST IF WE CAN POOL **INFECTED** DATA ACROSS DAYS AND ARENAS/CAMERAS (THE SAME ARENA WAS USED WITH THE SAME CAMERA)***

#Variables tested in this script:
#-total distance travelled
#-total time spent away from the wall (in 'the open')
#-total distance travelled in the open

#For stats help, consult: http://www.sthda.com/english/wiki/one-way-anova-test-in-r#assumptions-of-anova-test


#1.DAY by DISTANCE TRAVELLED
#++++++++++++++++++
#Check that days are grouped correctly
levels(data1$day)

# Visualize data
# Box plots: distance travelled by day
library("ggpubr")
ggboxplot(data1, x = "day", y = "total_distance", 
          color = "day", 
          ylab = "Total distance travelled", xlab = "Day")

# Mean plots: distance travelled by day
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(data1, x = "day", y = "total_distance", 
       add = c("mean_se", "jitter"), 
       ylab = "total", xlab = "day")

#Generate summary statistics by day
library(dplyr)
group_by(data1, day) %>%
  summarise(
    count = n(),
    mean = mean(total_distance, na.rm = TRUE),
    sd = sd(total_distance, na.rm = TRUE)
  )

#check that data fit the anova assumptions (can also do this after the test- see below)

#Normal distribution
shapiro.test(data1$total_distance) # if p> 0.05, can assume a normal distribution

#visually inspect distribution
library("ggpubr")
ggdensity(data1$total_distance)

#test homoscedasticity (homogeneity of variance)
#If the p-value is not less than the significance level of 0.05, this means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

bartlett.test(data1$total_distance,data1$day)
#Barlett's test is more sensitive to departures from normality, so if data are not normal, that may explain big differences between the two tests

library(car)
leveneTest(total_distance ~ day, data = data1)


#If test assumptions are met, compute one way anova. If test assumptions are not met, alternative tests are below 
res.aov1A <- aov(total_distance ~ day, data = data1)
summary(res.aov1A)

#If result is significant, use Tukey HSD test to find the difference- p value is adjusted for multiple comparisons
TukeyHSD(res.aov1A)

#The function pairwise.t.test() can be also used to calculate pairwise comparisons between group levels with corrections for multiple testing.

pairwise.t.test(data1$total_distance, data1$day,
                p.adjust.method = "BH")

#can also check that the data fit anova assumptions using anova results
# 1. Homogeneity of variances
plot(res.aov1A, 1)

#2. normal distribution using Q-Q plots with Shapiro-Wilks
#if all the points fall approximately along the reference line, we can assume normality.
plot(res.aov1A, 2)
# Extract the residuals
aov_residuals1A <- residuals(object = res.aov1A )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals1A )

##ALTERNATIVE TESTS

## when equal variances assumption is not met, can use:
# A Welch one-way test does not require the assumption of equal variances 
oneway.test(total_distance ~ day, data = data1)

#Or use a pairwise t-test
pairwise.t.test(data1$total_distance, data1$day,
                p.adjust.method = "BH", pool.sd = FALSE)

## when data aren't normal, can use a Kruskal Wallace test as a non-parametric alternative
kruskal.test(total_distance ~ day, data = data1)

#post-hoc analysis to determine which levels of the independent variable differ from each other 
library(pgirmess)
kruskalmc(total_distance ~ day, data = data1)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#2.DAY by TIME SPENT IN THE OPEN (I.E., AWAY FROM THE WALL) 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Check that days are grouped correctly
levels(data1$day)

# Visualize data
# Box plots: time spent in open by day
library("ggpubr")
ggboxplot(data1, x = "day", y = "total_opentime", 
          color = "day", 
          ylab = "Total time spent away from the wall", xlab = "Day")

# Mean plots: time spent in open by day
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(data1, x = "day", y = "total_opentime", 
       add = c("mean_se", "jitter"), 
       ylab = "Total time spent away from the wall", xlab = "day")

#Generate summary statistics by day
library(dplyr)
group_by(data1, day) %>%
  summarise(
    count = n(),
    mean = mean(total_opentime, na.rm = TRUE),
    sd = sd(total_opentime, na.rm = TRUE)
  )

#check that data fit the anova assumptions (can also do this after the test- see below)

#Normal distribution
shapiro.test(data1$total_opentime) # if p>0.05, can assume a normal distribution

#visually inspect distribution
library("ggpubr")
ggdensity(data1$total_opentime)

#test homoscedasticity (homogeneity of variance)
#If the p-value is not less than the significance level of 0.05, this means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

bartlett.test(data1$total_opentime,data1$day)

library(car)
leveneTest(total_opentime ~ day, data = data1)


#If test assumptions are met, compute one way anova. If test assumptions are not met, alternative tests are below 
res.aov2A <- aov(total_opentime ~ day, data = data1)
summary(res.aov2A)

#If result is significant, use Tukey HSD test to find the difference- p value is adjusted for multiple comparisons
TukeyHSD(res.aov2A)

#The function pairwise.t.test() can be also used to calculate pairwise comparisons between group levels with corrections for multiple testing.

pairwise.t.test(data1$total_opentime, data1$day,
                p.adjust.method = "BH")

#can also check that the data fit anova assumptions using anova results
# 1. Homogeneity of variances
plot(res.aov2A, 1)

#2. normal distribution using Q-Q plots with Shapiro-Wilks
#if all the points fall approximately along the reference line, we can assume normality.
plot(res.aov2A, 2)
# Extract the residuals
aov_residuals2A <- residuals(object = res.aov2A)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals2A )

##ALTERNATIVE TESTS

## when equal variances assumption is not met, can use:
# A Welch one-way test does not require the assumption of equal variances 
oneway.test(total_opentime ~ day, data = data1)

#Or use a pairwise t-test
pairwise.t.test(data1$total_opentime, data1$day,
                p.adjust.method = "BH", pool.sd = FALSE)

## when data aren't normal, can use a Kruskal Wallace test as a non-parametric alternative
kruskal.test(total_opentime ~ day, data = data1)

#post-hoc analysis to determine which levels of the independent variable differ from each other 
library(pgirmess)
kruskalmc(total_opentime ~ day, data = data1)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#3.DAY by DISTANCE TRAVELLED IN THE OPEN (I.E., AWAY FROM THE WALL) 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Check that days are grouped correctly
levels(data1$day)

# Visualize data
# Box plots: distance travelled away from wall by day
library("ggpubr")
ggboxplot(data1, x = "day", y = "total_open_distance", 
          color = "day", 
          ylab = "Total distance travelled away from the wall", xlab = "Day")

# Mean plots: distance travelled by day
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(data1, x = "day", y = "total_open_distance", 
       add = c("mean_se", "jitter"), 
       ylab = "Total distance travelled away from the wall", xlab = "day")

#Generate summary statistics by day
library(dplyr)
group_by(data1, day) %>%
  summarise(
    count = n(),
    mean = mean(total_open_distance, na.rm = TRUE),
    sd = sd(total_open_distance, na.rm = TRUE)
  )

#check that data fit the anova assumptions (can also do this after the test- see below)

#Normal distribution
shapiro.test(data1$total_open_distance) # if p> 0.05, can assume a normal distribution

#visually inspect distribution
library("ggpubr")
ggdensity(data1$total_open_distance)

#test homoscedasticity (homogeneity of variance)
#If the p-value is not less than the significance level of 0.05, this means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

bartlett.test(data1$total_open_distance,data1$day)

library(car)
leveneTest(total_open_distance ~ day, data = data1)


#If test assumptions are met, compute one way anova. If test assumptions are not met, alternative tests are below 
res.aov3A <- aov(total_open_distance ~ day, data = data1)
summary(res.aov3A)

#If result is significant, use Tukey HSD test to find the difference- p value is adjusted for multiple comparisons
TukeyHSD(res.aov3A)

#The function pairwise.t.test() can be also used to calculate pairwise comparisons between group levels with corrections for multiple testing.

pairwise.t.test(data1$total_open_distance, data1$day,
                p.adjust.method = "BH")

#can also check that the data fit anova assumptions using anova results
# 1. Homogeneity of variances
plot(res.aov3A, 1)

#2. normal distribution using Q-Q plots with Shapiro-Wilks
#if all the points fall approximately along the reference line, we can assume normality.
plot(res.aov3A, 2)
# Extract the residuals
aov_residuals3A <- residuals(object = res.aov3A )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals3A )

##ALTERNATIVE TESTS

## when equal variances assumption is not met, can use:
# A Welch one-way test does not require the assumption of equal variances 
oneway.test(total_open_distance ~ day, data = data1)

#Or use a pairwise t-test
pairwise.t.test(data1$total_open_distance, data1$day,
                p.adjust.method = "BH", pool.sd = FALSE)

## when data aren't normal, can use a Kruskal Wallace test as a non-parametric alternative
kruskal.test(total_open_distance ~ day, data = data1)

#post-hoc analysis to determine which levels of the independent variable differ from each other 
library(pgirmess)
kruskalmc(total_open_distance ~ day, data = data1)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#4.CAMERA by DISTANCE TRAVELLED
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Check that camera data are grouped correctly
levels(data1$camera)

# Visualize data
# Box plots: distance travelled by camera
library("ggpubr")
ggboxplot(data1, x = "camera", y = "total_distance", 
          color = "camera", 
          ylab = "Total distance travelled", xlab = "camera")

# Mean plots: distance travelled by camera
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(data1, x = "camera", y = "total_distance", 
       add = c("mean_se", "jitter"), 
       ylab = "Total distance travelled", xlab = "camera")

#Generate summary statistics by arena
library(dplyr)
group_by(data1, camera) %>%
  summarise(
    count = n(),
    mean = mean(total_distance, na.rm = TRUE),
    sd = sd(total_distance, na.rm = TRUE)
  )

#check that data fit the anova assumptions (can also do this after the test- see below)

#Normal distribution
shapiro.test(data1$total_distance) # if p> 0.05, can assume a normal distribution

#visually inspect distribution
library("ggpubr")
ggdensity(data1$total_distance)

#test homoscedasticity (homogeneity of variance)
#If the p-value is not less than the significance level of 0.05, this means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

bartlett.test(data1$total_distance,data1$camera)

library(car)
leveneTest(total_distance ~ camera, data = data1)


#If test assumptions are met, compute one way anova. If test assumptions are not met, alternative tests are below 
res.aov4A <- aov(total_distance ~ camera, data = data1)
summary(res.aov4A)

#If result is significant, use Tukey HSD test to find the difference- p value is adjusted for multiple comparisons
TukeyHSD(res.aov4A)

#The function pairwise.t.test() can be also used to calculate pairwise comparisons between group levels with corrections for multiple testing.

pairwise.t.test(data1$total_distance, data1$camera,
                p.adjust.method = "BH")

#can also check that the data fit anova assumptions using anova results
# 1. Homogeneity of variances
plot(res.aov4A, 1)

#2. normal distribution using Q-Q plots with Shapiro-Wilks
#if all the points fall approximately along the reference line, we can assume normality.
plot(res.aov4A, 2)
# Extract the residuals
aov_residuals4A <- residuals(object = res.aov4A )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals4A )

##ALTERNATIVE TESTS

## when equal variances assumption is not met, can use:
# A Welch one-way test does not require the assumption of equal variances 
oneway.test(total_distance ~ camera, data = data1)

#Or use a pairwise t-test
pairwise.t.test(data1$total_distance, data1$camera,
                p.adjust.method = "BH", pool.sd = FALSE)

## when data aren't normal, can use a Kruskal Wallace test as a non-parametric alternative
kruskal.test(total_distance ~ camera, data = data1)

#post-hoc analysis to determine which levels of the independent variable differ from each other 
library(pgirmess)
kruskalmc(total_distance ~ camera, data = data1)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#5.CAMERA/ARENA by TIME SPENT IN THE OPEN (I.E., AWAY FROM THE WALL) 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Check that days are grouped correctly
levels(data1$camera)

# Visualize data
# Box plots: time spent in open by camera/arena
library("ggpubr")
ggboxplot(data1, x = "camera", y = "total_opentime", 
          color = "camera", 
          ylab = "Total time spent away from the wall", xlab = "Camera/Arena")

# Mean plots: time spent in open by camera/arena
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(data1, x = "camera", y = "total_opentime", 
       add = c("mean_se", "jitter"), 
       ylab = "Total time spent away from the wall", xlab = "Camera/Arena")

#Generate summary statistics by day
library(dplyr)
group_by(data1, camera) %>%
  summarise(
    count = n(),
    mean = mean(total_opentime, na.rm = TRUE),
    sd = sd(total_opentime, na.rm = TRUE)
  )

#check that data fit the anova assumptions (can also do this after the test- see below)

#Normal distribution
shapiro.test(data1$total_opentime) # if p> 0.05, can assume a normal distribution

#visually inspect distribution
library("ggpubr")
ggdensity(data1$total_opentime)

#test homoscedasticity (homogeneity of variance)
#If the p-value is not less than the significance level of 0.05, this means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

bartlett.test(data1$total_opentime,data1$camera)

library(car)
leveneTest(total_opentime ~ camera, data = data1)


#If test assumptions are met, compute one way anova. If test assumptions are not met, alternative tests are below 
res.aov5A <- aov(total_opentime ~camera, data = data1)
summary(res.aov5A)

#If result is significant, use Tukey HSD test to find the difference- p value is adjusted for multiple comparisons
TukeyHSD(res.aov5A)

#The function pairwise.t.test() can be also used to calculate pairwise comparisons between group levels with corrections for multiple testing.

pairwise.t.test(data1$total_opentime, data1$camera,
                p.adjust.method = "BH")

#can also check that the data fit anova assumptions using anova results
# 1. Homogeneity of variances
plot(res.aov5A, 1)

#2. normal distribution using Q-Q plots with Shapiro-Wilks
#if all the points fall approximately along the reference line, we can assume normality.
plot(res.aov5A, 2)
# Extract the residuals
aov_residuals5A <- residuals(object = res.aov5A )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals5A )

##ALTERNATIVE TESTS

## when equal variances assumption is not met, can use:
# A Welch one-way test does not require the assumption of equal variances 
oneway.test(total_opentime ~ camera, data = data1)

#Or use a pairwise t-test
pairwise.t.test(data1$total_opentime, data1$camera,
                p.adjust.method = "BH", pool.sd = FALSE)

## when data aren't normal, can use a Kruskal Wallace test as a non-parametric alternative
kruskal.test(total_opentime ~ camera, data = data1)

#post-hoc analysis to determine which levels of the independent variable differ from each other 
library(pgirmess)
kruskalmc(total_opentime ~ camera, data = data1)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#6.CAMERA/ARENA by DISTANCE TRAVELLED IN THE OPEN (I.E., AWAY FROM THE WALL) 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Check that days are grouped correctly
levels(data1$camera)

# Visualize data
# Box plots: distance travelled away from wall by day
library("ggpubr")
ggboxplot(data1, x = "camera", y = "total_open_distance", 
          color = "camera", 
          ylab = "Total distance travelled away from the wall", xlab = "Camera/Arena")

# Mean plots: distance travelled by day
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(data1, x = "camera", y = "total_open_distance", 
       add = c("mean_se", "jitter"), 
       ylab = "Total distance travelled away from the wall", xlab = "camera")

#Generate summary statistics by day
library(dplyr)
group_by(data1, camera) %>%
  summarise(
    count = n(),
    mean = mean(total_open_distance, na.rm = TRUE),
    sd = sd(total_open_distance, na.rm = TRUE)
  )

#check that data fit the anova assumptions (can also do this after the test- see below)

#Normal distribution
shapiro.test(data1$total_open_distance) # if p> 0.05, can assume a normal distribution

#visually inspect distribution
library("ggpubr")
ggdensity(data1$total_open_distance)

#test homoscedasticity (homogeneity of variance)
#If the p-value is not less than the significance level of 0.05, this means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

bartlett.test(data1$total_open_distance,data1$camera)

library(car)
leveneTest(total_open_distance ~ camera, data = data1)


#If test assumptions are met, compute one way anova. If test assumptions are not met, alternative tests are below 
res.aov6A <- aov(total_open_distance ~ camera, data = data1)
summary(res.aov6A)

#If result is significant, use Tukey HSD test to find the difference- p value is adjusted for multiple comparisons
TukeyHSD(res.aov6A)

#The function pairwise.t.test() can be also used to calculate pairwise comparisons between group levels with corrections for multiple testing.

pairwise.t.test(data1$total_open_distance, data1$camera,
                p.adjust.method = "BH")

#can also check that the data fit anova assumptions using anova results
# 1. Homogeneity of variances
plot(res.aov6A, 1)

#2. normal distribution using Q-Q plots with Shapiro-Wilks
#if all the points fall approximately along the reference line, we can assume normality.
plot(res.aov6A, 2)
# Extract the residuals
aov_residuals6A <- residuals(object = res.aov6A)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals6A)

##ALTERNATIVE TESTS

## when equal variances assumption is not met, can use:
# A Welch one-way test does not require the assumption of equal variances 
oneway.test(total_open_distance ~ camera, data = data1)

#Or use a pairwise t-test
pairwise.t.test(data1$total_open_distance, data1$camera,
                p.adjust.method = "BH", pool.sd = FALSE)

## when data aren't normal, can use a Kruskal Wallace test as a non-parametric alternative
kruskal.test(total_open_distance ~ camera, data = data1)

#post-hoc analysis to determine which levels of the independent variable differ from each other 
library(pgirmess)
kruskalmc(total_open_distance ~ camera, data = data1)

#TEST 5  W/OUTLIER REMOVED
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#7.CAMERA/ARENA by TIME SPENT IN THE OPEN (I.E., AWAY FROM THE WALL)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
file2 = file.choose() #FILE NAME: camera data infected 071818 no outlier
data2<-read.csv(file2)

#Check that days are grouped correctly
levels(data2$camera)

library("ggpubr")
ggline(data2, x = "camera", y = "total_opentime", 
       add = c("mean_se", "jitter"), 
       ylab = "Total time spent away from the wall", xlab = "Camera/Arena")

#check that data fit the anova assumptions (can also do this after the test- see below)

#Normal distribution
shapiro.test(data2$total_opentime) # if p> 0.05, can assume a normal distribution

#visually inspect distribution
library("ggpubr")
ggdensity(data2$total_opentime)

#test homoscedasticity (homogeneity of variance)
#If the p-value is not less than the significance level of 0.05, this means that there is no evidence to suggest that the variance across groups is statistically significantly different. Therefore, we can assume the homogeneity of variances in the different treatment groups.

bartlett.test(data2$total_opentime,data1$camera)

library(car)
leveneTest(total_opentime ~ camera, data = data2)


#If test assumptions are met, compute one way anova. If test assumptions are not met, alternative tests are below 
res.aov7A <- aov(total_opentime ~camera, data = data2)
summary(res.aov7A)

#If result is significant, use Tukey HSD test to find the difference- p value is adjusted for multiple comparisons
TukeyHSD(res.aov7A)

#The function pairwise.t.test() can be also used to calculate pairwise comparisons between group levels with corrections for multiple testing.

pairwise.t.test(data2$total_opentime, data2$camera,
                p.adjust.method = "BH")

#can also check that the data fit anova assumptions using anova results
# 1. Homogeneity of variances
plot(res.aov7A, 1)

#2. normal distribution using Q-Q plots with Shapiro-Wilks
#if all the points fall approximately along the reference line, we can assume normality.
plot(res.aov7A, 2)
# Extract the residuals
aov_residuals7A <- residuals(object = res.aov7A )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals7A )

##ALTERNATIVE TESTS

## when equal variances assumption is not met, can use:
# A Welch one-way test does not require the assumption of equal variances 
oneway.test(total_opentime ~ camera, data = data2)

#Or use a pairwise t-test
pairwise.t.test(data2$total_opentime, data2$camera,
                p.adjust.method = "BH", pool.sd = FALSE)

## when data aren't normal, can use a Kruskal Wallace test as a non-parametric alternative
kruskal.test(total_opentime ~ camera, data = data2)

#post-hoc analysis to determine which levels of the independent variable differ from each other 
library(pgirmess)
kruskalmc(total_opentime ~ camera, data = data2)


