##--QUESTION 1------------------------------------------------------------------
##--PART A----------------------------------------------------------------------
library(faraway)

#read the data
data(pima)

##--METHOD 1:-------------------------------------------------------------------
#to focus on data from pima
attach(pima)

mean(pregnant) 
# [1] 3.845052
median(pregnant) 
# [1] 3
sd(pregnant) 
# [1] 3.369578
var(pregnant)
# [1] 11.35406
mad(pregnant) #median absolute deviation
# [1] 2.9652
IQR(pregnant) #interquartile range
# [1] 2.9652


median(diastolic)
# [1] 72
sd(diastolic)
# [1] 19.35581
var(diastolic)
# [1] 374.6473
mad(diastolic) #median absolute deviation
# [1] 11.8608
IQR(diastolic) #interquartile range
# [1] 18


##--METHOD 2:-------------------------------------------------------------------
#extracting "pregnant" data
pima$pregnant

mean(pima$pregnant)
# [1] 3.845052
median(pima$pregnant)
# [1] 3
sd(pima$pregnant)
# [1] 3.369578
var(pima$pregnant)
# [1] 11.35406
mad(pima$pregnant)
# [1] 2.9652
IQR(pima$pregnant)
# [1] 5

#extracting "diastolic" data
pima$diastolic

mean(pima$diastolic)
# [1] 69.10547
median(pima$diastolic)
# [1] 72
sd(pima$diastolic)
# [1] 19.35581
var(pima$diastolic)
# [1] 374.6473
IQR(pima$diastolic)
# [1] 18

##--PART B----------------------------------------------------------------------
par(mfrow=c(2,2))
boxplot(pregnant,main="Boxplot of Pregnant")
hist(pregnant,main="Histogram of Pregnant",xlab="Number of times pregnant")
qqnorm(pregnant,main="QQ-Plot of Pregnant")
qqline(pregnant)
plot(density(pregnant),main="Kernel Density Estimate of Pregnant")

boxplot(diastolic,main="Boxplot of Diastolic")
hist(diastolic,main="Histogram of Diastolic",xlab="Diastolic blood pressure (mm Hg)")
qqnorm(diastolic,main="QQ-Plot of Diastolic")
qqline(diastolic)
plot(density(diastolic),main="Kernel Density of Diastolic")

##--QUESTION 2------------------------------------------------------------------
##--PART A---------------------------------------------------------------------- 
#input data in the form of vectors
precipitation <- c(0.77,1.20,3.00,1.62,2.81,2.48,1.74,0.47,3.09,1.31,1.87,0.96,0.81,1.43,1.51,0.32,1.18,1.89,1.20,3.37,2.10,0.59,1.35,0.90,1.95,2.20,0.52,0.81,4.75,2.05)
 
#to check the data
precipitation

qqnorm(precipitation,main="QQ-Plot of Precipitation")
qqline(precipitation)
#The points on the QQ-plot exhibit a U-shape which indicates departure from normality

##--PART B----------------------------------------------------------------------
#assigning the data to an object
x <- precipitation

#find a transformation that is the closest to normality
log(x)
qqnorm(log(x))
qqline(log(x))

sqrt(x)
qqnorm(sqrt(x))
qqline(sqrt(x))

#Shapiro-Wilk normality test (i.e. a test for normal distribution)
shapiro.test(precipitation)

# Shapiro-Wilk normality test
# 
# data:  precipitation
# W = 0.923, p-value = 0.03211

# #Important note: If the p-value is below 0.05, we reject the null hypothesis (Null hypothesis = Normal distribution)

shapiro.test (log(x))

# Shapiro-Wilk normality test
#
# data:  log(precipitation)
# W = 0.98444, p-value = 0.9272
# #Important note: If the p-value is above 0.05, we accept the null hypothesis (Null hypothesis = Normal distribution)

##--QUESTION 3------------------------------------------------------------------
##--PART A----------------------------------------------------------------------
#generate 600 random sample with mean 10 and sd 5
y <- rnorm(600,10,4)
help(rnorm) #to inspect the function

##--PART B----------------------------------------------------------------------
hist(y) 
#bell-curved shape i.e. normal distribution

##--QUESTION 4------------------------------------------------------------------
##--PART A----------------------------------------------------------------------
#generate 5000 random samples with rate = 1
z <- rexp(5000,rate=1)
help(rexp)

##--PART B----------------------------------------------------------------------
hist(z) #right skewed



