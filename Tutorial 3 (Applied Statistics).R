library(faraway)
data(teengamb)

help("teengamb")
print(names(teengamb))
#[1] "sex"    "status" "income" "verbal" "gamble"

##--QUESTION 1------------------------------------------------------------------
par(mfrow=c(2,2))
attach(teengamb)
hist(sex,main="Gender of the teenagers",xlab="Sex")
hist(status,main="Socioeconomic status of the teenagers",xlab="Status")
hist(income,main="Income of the teenagers",xlab="Income")
hist(verbal,main="Verbal score of the teenagers",xlab="Verbal")
hist(gamble,main="Expenditure on gambling of the teenagers",xlab="Gamble")


##--QUESTION 2------------------------------------------------------------------
model <- lm(gamble ~ sex + status + income + verbal)
summary(model)

# Call:
# lm(formula = gamble ~ sex + status + income + verbal)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -51.082 -11.320  -1.451   9.452  94.252 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  22.55565   17.19680   1.312   0.1968    
# sex         -22.11833    8.21111  -2.694   0.0101 *  
# status        0.05223    0.28111   0.186   0.8535    
# income        4.96198    1.02539   4.839 1.79e-05 ***
# verbal       -2.95949    2.17215  -1.362   0.1803    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 22.69 on 42 degrees of freedom
# Multiple R-squared:  0.5267,	Adjusted R-squared:  0.4816 
# F-statistic: 11.69 on 4 and 42 DF,  p-value: 1.815e-06


##--QUESTION 3------------------------------------------------------------------

# The co-efficient for the "Income" variable, which is 1.79e-05, has a very low p-value (< 0.001) 
# "Income" variable is highly statistically significant

# The co-efficient for "Sex" variable has a p-value of 0.0101, which is less than 0.05
# "Sex" variable is statistically significant

##--QUESTION 4------------------------------------------------------------------

# The R-squared value which is 0.5267 (52.67%) represents the proportion of the 
# total variation in the response variable that is explained by the dependent variables

##--QUESTION 5------------------------------------------------------------------

View(model$residuals)

largest_residual <- max(model$residuals)
largest_residual

# [1] 94.25222

case_number <- which.max(model$residuals)
case_number

# 24

##--QUESTION 6------------------------------------------------------------------

mean_residual <- mean(model$residuals)
mean_residual

# [1] 1.266127e-15

median_residual <- median(model$residuals)
median_residual

# [1] -1.451392

##--QUESTION 7------------------------------------------------------------------

correlation_residual_fitted <- cor(model$residuals, model$fitted.values)
correlation_residual_fitted

# [1] -6.821332e-17 (No correlation)

##--QUESTION 8------------------------------------------------------------------

correlation_residual_income <- cor(model$residuals, teengamb$income)
correlation_residual_income

#[1] -5.717345e-17 (No correlation)

##--QUESTION 9------------------------------------------------------------------

# y = 22.55565 - (22.11833 * X1) + (0.05223 * X2) + (4.96198 * X3) - (2.95 * X4)  
# Expenditure = 22.556 - (22.118 * sex) + (0.052 * status) + (4.961 * income) - (2.95 * verbal)

##--QUESTION 10------------------------------------------------------------------

# The predicted expenditure on gambling for a male, while holding the other predictors constant is 
# expected to be approximately 22.12 units lower than for a female

# i.e. Female spends 22.12 units less than male on gambling


##--QUESTION 11------------------------------------------------------------------

confidence_interval <- confint(model, level = 0.990)
confidence_interval

#                  0.5 %      99.5 %
# (Intercept) -23.8424633 68.95376452
# sex         -44.2724606  0.03580037
# status       -0.7062237  0.81069139
# income        2.1954029  7.72855554
# verbal       -8.8200989  2.90111186

##--QUESTION 12-----------------------------------------------------------------

# Linear regression: Checking the residuals 
# 1. Linearity 
# Relationship between the predictors and the response variable is assumed to be linear

# 2. Independence 
# The observations are independent of each other

# 3. Normality
# Assumed to be normally distributed

# 4. Equal variance - Homosedascity 
# The variance are assumed to be equal

##--QUESTION 13-----------------------------------------------------------------

par(mfrow=c(1,1))
plot(model$fitted.values, model$residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted Values")

# The scatterplot has a cone-shaped, therefore it exhibits non constant variance

##--QUESTION 14-----------------------------------------------------------------

qqnorm(model$residuals, main="QQ-Norm of Residuals")
qqline(model$residuals)

shapiro.test(model$residuals)

# Shapiro-Wilk normality test
# 
# data:  model$residuals
# W = 0.86839, p-value = 8.16e-05
# Reject the null hypothesis (H0 : Normally distributed, H1: Not normally distributed)
# Therefore, the residuals is not normally distributed (The model is not a good model)

##--QUESTION 15-----------------------------------------------------------------

plot(model$residuals, ylab = "Residuals", xlab = "Case number", main = "Residuals")


##---HOW TO IMPROVE THE MODEL?--------------------------------------------------
# 1. Observe the signifance level of the variables (Backward Elimination)
# 2. Check with AIC - The smaller the model, the better




