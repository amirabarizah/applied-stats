##--QUESTION 1------------------------------------------------------------------
##--PART A----------------------------------------------------------------------
library(tidyr)

#Using ANOVA to compare the differences between the mean - If we cannot use t-test
reflectance <- data.frame(
    OperatorA = c(59.88, 60.12, 60.88, 60.98, 59.90),
    OperatorB = c(59.87, 60.32, 60.42, 59.99, 60.12),
    OperatorC = c(60.83, 60.87, 60.56, 61.00, 60.50),
    OperatorD = c(61.01, 60.87, 60.69, 60.53, 60.63)
)
View(reflectance)

reflectance_brightness <- data.frame(
  Operator = rep(c("A", "B", "C", "D"), each = 5),
  Brightness = c(
    59.88, 60.12, 60.88, 60.98, 59.90,
    59.87, 60.32, 60.42, 59.99, 60.12,
    60.83, 60.87, 60.56, 61.00, 60.50,
    61.01, 60.87, 60.69, 60.53, 60.63
  )
)

Brightness <- c(59.88, 60.12, 60.88, 60.98, 59.90,
                59.87, 60.32, 60.42, 59.99, 60.12,
                60.83, 60.87, 60.56, 61.00, 60.50,
                61.01, 60.87, 60.69, 60.53, 60.63)
Operator <- c("A","A","A","A","A","B","B","B","B","B","C","C","C","C","C","D","D","D","D","D")
Operator <- as.factor(Operator)
is.factor(Operator)
model <- lm(Brightness ~ Operator)
summary(model)

# Call:
#   lm(formula = Brightness ~ Operator)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -0.472 -0.220 -0.040  0.194  0.628 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  60.3520     0.1454 415.136   <2e-16 ***
# OperatorB    -0.2080     0.2056  -1.012   0.3267    
# OperatorC     0.4000     0.2056   1.946   0.0695 .  
# OperatorD     0.3940     0.2056   1.916   0.0734 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3251 on 16 degrees of freedom
# Multiple R-squared:  0.4464,	Adjusted R-squared:  0.3426 
# F-statistic:   4.3 on 3 and 16 DF,  p-value: 0.02097


result <- aov(Brightness ~ Operator, data = reflectance_brightness)
summary(result)

#             Df Sum Sq Mean Sq F value Pr(>F)  
# Operator     3  1.363  0.4544     4.3  0.021 *
# Residuals   16  1.691  0.1057                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# The p-value, which is 0.021, is less than 0.05. Therefore, there are 
# statistically significant differences among operators in terms of reading brightness


##--PART B----------------------------------------------------------------------
library(stats)
TukeyHSD(result)

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = Brightness ~ Operator, data = ref_brightness)
# 
# $Operator
# diff         lwr       upr     p adj
# OperatorB-OperatorA -0.208 -0.79621621 0.3802162 0.7450627
# OperatorC-OperatorA  0.400 -0.18821621 0.9882162 0.2489941
# OperatorD-OperatorA  0.394 -0.19421621 0.9822162 0.2603189
# OperatorC-OperatorB  0.608  0.01978379 1.1962162 0.0415316
# OperatorD-OperatorB  0.602  0.01378379 1.1902162 0.0439458
# OperatorD-OperatorC -0.006 -0.59421621 0.5822162 0.9999907

# Only OperatorC-OperatorB and OperatorD-OperatorB differences are not
# significant as the corresponding intervals contain zero

##--QUESTION 2------------------------------------------------------------------
library(tibble)

##--PART A----------------------------------------------------------------------
warp_breaks <- data.frame(
  Wool = rep(c("A", "A", "A", "B", "B", "B"), each = 9),
  Tension = rep(c("L", "M", "H"), each = 9),
  Breaks = c(
    26, 30, 54, 25, 70, 52, 51, 26, 67,
    18, 21, 29, 17, 12, 18, 35, 30, 36,
    36, 21, 24, 18, 10, 43, 28, 15, 26,
    27, 14, 29, 19, 29, 31, 41, 20, 44,
    42, 26, 19, 16, 39, 28, 21, 39, 29,
    20, 21, 24, 17, 13, 15, 15, 16, 28
  )
)

View(warp_breaks)

interaction.plot(
  x.factor = warp_breaks$Wool,
  trace.factor = warp_breaks$Tension, 
  response = warp_breaks$Breaks,
  type = "b"
)

##--PART B----------------------------------------------------------------------
anovatwo <- aov(Breaks ~ Wool*Tension, data=warp_breaks)
summary(anovatwo)

#               Df Sum Sq Mean Sq F value   Pr(>F)    
# Wool          1    451   450.7   3.765 0.058213 .  
# Tension       2   2034  1017.1   8.498 0.000693 ***
# Wool:Tension  2   1003   501.4   4.189 0.021044 *  
# Residuals    48   5745   119.7                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 
##--PART C----------------------------------------------------------------------

# "Tension" factor has a p-value of 0.000693, which is less than 0.05.
# Therefore, the factor is statistically significant

##--PART D----------------------------------------------------------------------

plot(anovatwo)

# log transformation
warp_breaks$log_Breaks <- log(warp_breaks$Breaks)

log_model <- aov(log_Breaks ~ Wool*Tension, data = warp_breaks)
summary(log_model)

#               Df Sum Sq Mean Sq F value  Pr(>F)   
# Wool          1  0.313  0.3125   2.234 0.14151   
# Tension       2  2.176  1.0881   7.779 0.00118 **
# Wool:Tension  2  0.913  0.4566   3.264 0.04686 * 
# Residuals    48  6.714  0.1399                   
# ---
# Signif. codes:  
# 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# exponential transformation
warp_breaks$exp_Breaks <- exp(warp_breaks$Breaks)

exp_model <- aov(exp_Breaks ~ Wool * Tension, data = warp_breaks)
summary(exp_model)

# Df    Sum Sq   Mean Sq F value Pr(>F)
# Wool          1 1.291e+59 1.291e+59   1.113  0.297
# Tension       2 2.583e+59 1.291e+59   1.113  0.337
# Wool:Tension  2 2.583e+59 1.291e+59   1.113  0.337
# Residuals    48 5.568e+60 1.160e+59               

# sqrt transformation
warp_breaks$sqrt_Breaks <- sqrt(warp_breaks$Breaks)

sqrt_model <- aov(sqrt_Breaks ~ Wool * Tension, data = warp_breaks)
summary(sqrt_model)

# Df Sum Sq Mean Sq F value   Pr(>F)    
# Wool          1   2.90   2.902   3.022 0.088542 .  
# Tension       2  15.89   7.946   8.275 0.000817 ***
# Wool:Tension  2   7.20   3.601   3.750 0.030674 *  
# Residuals    48  46.09   0.960                     
# ---
# Signif. codes:  
# 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##--PART E----------------------------------------------------------------------

warp_breaks$sl_factor <- with(warp_breaks, interaction(Wool, Tension, sep = "_"))
aov_res <- aov(Breaks ~ sl_factor, data = warp_breaks)
summary(aov_res)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# sl_factor    5   3488   697.5   5.828 0.000277 ***
# Residuals   48   5745   119.7                     
# ---
# Signif. codes:  
# 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tukey_res <- TukeyHSD(aov_res)
tukey_res

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = Breaks ~ sl_factor, data = warp_breaks)
# 
# $sl_factor
# diff        lwr       upr     p adj
# B_H-A_H  -5.7777778 -21.084100  9.528544 0.8705572
# A_L-A_H  20.0000000   4.693678 35.306322 0.0040955
# B_L-A_H   3.6666667 -11.639655 18.972988 0.9797123
# A_M-A_H  -0.5555556 -15.861877 14.750766 0.9999978
# B_M-A_H   4.2222222 -11.084100 19.528544 0.9626541
# A_L-B_H  25.7777778  10.471456 41.084100 0.0001136
# B_L-B_H   9.4444444  -5.861877 24.750766 0.4560950
# A_M-B_H   5.2222222 -10.084100 20.528544 0.9114780
# B_M-B_H  10.0000000  -5.306322 25.306322 0.3918767
# B_L-A_L -16.3333333 -31.639655 -1.027012 0.0302143
# A_M-A_L -20.5555556 -35.861877 -5.249234 0.0029580
# B_M-A_L -15.7777778 -31.084100 -0.471456 0.0398172
# A_M-B_L  -4.2222222 -19.528544 11.084100 0.9626541
# B_M-B_L   0.5555556 -14.750766 15.861877 0.9999978
# B_M-A_M   4.7777778 -10.528544 20.084100 0.9377205


#A_L-A_H, A_L-B_H, A_L-B_L and A_M-A_H are significantly different

##--QUESTION 3------------------------------------------------------------------

odontoblasts <- data.frame(
  Supplement = rep(c("VC","OJ"), each = 10),
  Dose = rep(c("0.5", "1.0", "2.0"), each = 10),
  Length = c(
  4.2, 11.5, 7.3, 5.8, 6.4, 10.0, 11.2, 11.2, 5.2, 7.0, 
  15.2, 21.5, 17.6, 9.7, 14.5, 10.0, 8.2, 9.4, 16.5, 9.7, 
  16.5, 16.5, 15.2, 17.3, 22.5, 17.3, 13.6, 14.5, 18.8, 15.5, 
  19.7, 23.3, 23.6, 26.4, 20.0, 25.2, 25.8, 21.2, 14.5, 27.3, 
  23.6, 18.5, 33.9, 25.5, 26.4, 32.5, 26.7, 21.5, 23.3, 29.5, 
  25.5, 26.4, 22.4, 24.5, 24.8, 30.9, 26.4, 27.3, 29.4, 23.0
)
)



##--PART A----------------------------------------------------------------------
library(ggplot2)
plot(odontoblasts)

##--PART B----------------------------------------------------------------------

odo_model <- lm(Length ~ Supplement + Dose, data = odontoblasts)
summary(odo_model)

# Call:
#   lm(formula = Length ~ Supplement + Dose, data = odontoblasts)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -13.335  -5.043  -0.515   5.540  16.065 
# 
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    17.190      1.844   9.324 5.42e-13 ***
# SupplementVC   -3.700      1.844  -2.007  0.04959 *  
# Dose1.0         4.345      2.258   1.924  0.05940 .  
# Dose2.0         6.075      2.258   2.691  0.00939 ** 
#   ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.14 on 56 degrees of freedom
# Multiple R-squared:  0.173,	Adjusted R-squared:  0.1287 
# F-statistic: 3.905 on 3 and 56 DF,  p-value: 0.01328
