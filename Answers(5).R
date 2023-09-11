##--QUESTION 1------------------------------------------------------------------
##--PART A----------------------------------------------------------------------
Brightness <- c(59.88, 60.12, 60.88, 60.98, 59.90,
                59.87, 60.32, 60.42, 59.99, 60.12,
                60.83, 60.87, 60.56, 61.00, 60.50,
                61.01, 60.87, 60.69, 60.53, 60.63)
Operator <- c("A","A","A","A","A",
              "B","B","B","B","B",
              "C","C","C","C","C",
              "D","D","D","D","D")

#To identify the variable as factor and not numeric
Operator <- as.factor(Operator)
is.factor(Operator)

#Step 1: Fit a linear model 
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
#              Estimate Std. Error t value Pr(>|t|)    
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

#Step 2: Analyze the data as an ANOVA
anova(model)

# Analysis of Variance Table
# 
# Response: Brightness
#            Df Sum Sq Mean Sq F value  Pr(>F)  
# Operator   3 1.3633 0.45442  4.3001 0.02097 *
# Residuals 16 1.6908 0.10568                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Step 3: State the H0 and H1, interpret the results
# H0 : μA = μB = μC = μD
# H1 : μA ≠ μB = μC = μD or μA ≠ μB ≠ μC ≠ μD

#  Conclusion : Since the p-value is 0.02097 (<0.05), we reject the H0. 
#  Therefore, there is statistically significant differences among operators in making sheets and reading brightness

##--PART B----------------------------------------------------------------------
#Note: To observe whether the combinations are significantly different, use TukeyHSD
TukeyHSD(aov(Brightness ~ Operator))

# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = Brightness ~ Operator)
# 
# $Operator
#       diff      lwr       upr      p adj
# B-A -0.208 -0.79621621 0.3802162 0.7450627
# C-A  0.400 -0.18821621 0.9882162 0.2489941
# D-A  0.394 -0.19421621 0.9822162 0.2603189
# C-B  0.608  0.01978379 1.1962162 0.0415316 (*)
# D-B  0.602  0.01378379 1.1902162 0.0439458 (*)
# D-C -0.006 -0.59421621 0.5822162 0.9999907

# H0 : The μi's are not significantly different 
# H1 : The μi's are significantly different

# Conclusion : 
# There is no evidence that B-A, C-A, D-A and D-C are significantly different as
# as the corresponding intervals contain zeror

##--QUESTION 2------------------------------------------------------------------
##--PART A----------------------------------------------------------------------
Wool <- c("A", "A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B","B", "B", "B",
          "A", "A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B","B", "B", "B",
          "A", "A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B","B", "B", "B"
)

Tension <- c("L", "L", "L", "L", "L", "L", "L", "L", "L","L", "L", "L", "L", "L", "L", "L", "L", "L",
             "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M","M", "M", "M",
             "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N"
)

Breaks <- c(26, 30, 54, 25, 70, 52, 51, 26, 67, 27, 14, 29, 19, 29, 31, 41, 20, 44,
            18, 21, 29, 17, 12, 18, 35, 30, 36, 42, 26, 19, 16, 39, 28, 21, 39, 29,
            36, 21, 24, 18, 10, 43, 28, 15, 26, 20, 21, 24, 17, 13, 15, 15, 16, 28)

Wool <- as.factor(Wool)
is.factor(Wool)
Tension <- as.factor(Tension)
is.factor(Tension)

question2_data <- data.frame(Breaks, Wool, Tension)
View(question2_data)

interaction.plot(Wool,Tension,Breaks)
# There is interaction in the lower mean of Breaks and around the middle mean of Breaks

interaction.plot(Tension,Wool,Breaks)
# There are interactions in the lower mean of Breaks 

##--PART B----------------------------------------------------------------------
question2_model <- lm(Breaks ~ Wool + Tension + Wool*Tension)
anova(question2_model)

# Analysis of Variance Table
# 
# Response: Breaks
#              Df Sum Sq Mean Sq F value    Pr(>F)    
# Wool          1  450.7  450.67  3.7653 0.0582130 .  
# Tension       2 2034.3 1017.13  8.4980 0.0006926 ***
# Wool:Tension  2 1002.8  501.39  4.1891 0.0210442 *  
# Residuals    48 5745.1  119.69                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Note: If the higher-order interaction (i.e. Wool:Tension) are significant, include the parent variable


##--PART C----------------------------------------------------------------------

# Since the p-value for the Tension variable is 0.0006926 (<0.05), there is significance
# The p-value for the interaction effect is 0.0210442, therefore it is significant

##--PART D----------------------------------------------------------------------

#Note: To simplify is to find a bigger value for p-value

#1. Logarithm transformation
question2_data$log_Breaks <- log(question2_data$Breaks)

log_model <- lm(log_Breaks ~ Wool*Tension, data = question2_data)
anova(log_model)

# Analysis of Variance Table
# 
# Response: log_Breaks
# Df Sum Sq Mean Sq F value   Pr(>F)   
# Wool          1 0.3125 0.31253  2.2344 0.141511   
# Tension       2 2.1762 1.08808  7.7792 0.001185 **
# Wool:Tension  2 0.9131 0.45657  3.2642 0.046863 * 
# Residuals    48 6.7138 0.13987                    
# ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.374 on 48 degrees of freedom
# Multiple R-squared:  0.3363,	Adjusted R-squared:  0.2672 
# F-statistic: 4.864 on 5 and 48 DF,  p-value: 0.001116

#Logarithm is not a good transformation (As the p-values are not larger than 0.05)

#2. Exponential transformation
exp_Breaks <- exp(question2_data$Breaks)

exp_model <- lm(exp_Breaks ~ Wool + Tension + Wool*Tension)
anova(exp_model)

#              Df    Sum Sq   Mean Sq F value Pr(>F)
# Wool          1 1.291e+59 1.291e+59   1.113  0.297
# Tension       2 2.583e+59 1.291e+59   1.113  0.337
# Wool:Tension  2 2.583e+59 1.291e+59   1.113  0.337
# Residuals    48 5.568e+60 1.160e+59  

#Exponential is a good transformation (As the p-values are larger than 0.05)


##--PART E----------------------------------------------------------------------

TukeyHSD(aov(Breaks ~ Wool + Tension + Wool*Tension))
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = Breaks ~ Wool + Tension + Wool * Tension)
# 
# $Wool
#       diff       lwr       upr      p adj
# B-A -5.777778 -11.76458 0.2090243 0.058213
# 
# $Tension
#         diff      lwr       upr       p adj
# M-L -10.000000 -18.81965 -1.180353 0.0228554 
# N-L -14.722222 -23.54187 -5.902575 0.0005595
# N-M  -4.722222 -13.54187  4.097425 0.4049442
# 
# $`Wool:Tension`
#             diff       lwr        upr      p adj
# B:L-A:L -16.3333333 -31.63966  -1.027012 0.0302143
# A:M-A:L -20.5555556 -35.86188  -5.249234 0.0029580
# B:M-A:L -15.7777778 -31.08410  -0.471456 0.0398172
# A:N-A:L -20.0000000 -35.30632  -4.693678 0.0040955
# B:N-A:L -25.7777778 -41.08410 -10.471456 0.0001136
# A:M-B:L  -4.2222222 -19.52854  11.084100 0.9626541
# B:M-B:L   0.5555556 -14.75077  15.861877 0.9999978
# A:N-B:L  -3.6666667 -18.97299  11.639655 0.9797123
# B:N-B:L  -9.4444444 -24.75077   5.861877 0.4560950
# B:M-A:M   4.7777778 -10.52854  20.084100 0.9377205
# A:N-A:M   0.5555556 -14.75077  15.861877 0.9999978
# B:N-A:M  -5.2222222 -20.52854  10.084100 0.9114780
# A:N-B:M  -4.2222222 -19.52854  11.084100 0.9626541
# B:N-B:M -10.0000000 -25.30632   5.306322 0.3918767
# B:N-A:N  -5.7777778 -21.08410   9.528544 0.8705572

#Dont reject h0 greater than 5% 
# H0 is equal

