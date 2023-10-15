##--QUESTION 1------------------------------------------------------------------
##--PART A----------------------------------------------------------------------

library(faraway)
help(pima)
data(pima) #To fetch the dataset of pima

summary(pima)
   #pregnant         glucose        diastolic         triceps         insulin     
# Min.   : 0.000   Min.   :  0.0   Min.   :  0.00   Min.   : 0.00   Min.   :  0.0  
# 1st Qu.: 1.000   1st Qu.: 99.0   1st Qu.: 62.00   1st Qu.: 0.00   1st Qu.:  0.0  
# Median : 3.000   Median :117.0   Median : 72.00   Median :23.00   Median : 30.5  
# Mean   : 3.845   Mean   :120.9   Mean   : 69.11   Mean   :20.54   Mean   : 79.8  
# 3rd Qu.: 6.000   3rd Qu.:140.2   3rd Qu.: 80.00   3rd Qu.:32.00   3rd Qu.:127.2  
# Max.   :17.000   Max.   :199.0   Max.   :122.00   Max.   :99.00   Max.   :846.0

   #bmi           diabetes           age             test      
# Min.   : 0.00   Min.   :0.0780   Min.   :21.00   Min.   :0.000  
# 1st Qu.:27.30   1st Qu.:0.2437   1st Qu.:24.00   1st Qu.:0.000  
# Median :32.00   Median :0.3725   Median :29.00   Median :0.000  
# Mean   :31.99   Mean   :0.4719   Mean   :33.24   Mean   :0.349  
# 3rd Qu.:36.60   3rd Qu.:0.6262   3rd Qu.:41.00   3rd Qu.:1.000  
# Max.   :67.10   Max.   :2.4200   Max.   :81.00   Max.   :1.000  

pairs(pima) #To create a matrix of scatterplots for exploring pairwise relationships between variables
#Note: The diagonal of the diagram is the histogram for each variable

##--PART B----------------------------------------------------------------------

#Based on the summary, the variables: 'glucose', 'diastolic', 'triceps', 'insulin' and 'bmi' are not supposed to be 0.00
#Therefore, there are missing observations (0.00) for many variables
#The solution is to change the missing values to NA instead of removing errors

attach(pima) #To attach the specific dataset

pima$glucose[pima$glucose == 0] <- NA
pima$diastolic[pima$diastolic == 0] <- NA
pima$bmi[pima$bmi == 0] <- NA
pima$triceps[pima$triceps == 0] <- NA
pima$insulin[pima$insulin == 0] <- NA
#To replace missing values with NA

##--PART C----------------------------------------------------------------------

summary(pima) #With the dataset without missing values

   # pregnant         glucose        diastolic         triceps         insulin      
# Min.   : 0.000   Min.   : 44.0   Min.   : 24.00   Min.   : 7.00   Min.   : 14.00  
# 1st Qu.: 1.000   1st Qu.: 99.0   1st Qu.: 64.00   1st Qu.:22.00   1st Qu.: 76.25  
# Median : 3.000   Median :117.0   Median : 72.00   Median :29.00   Median :125.00  
# Mean   : 3.845   Mean   :121.7   Mean   : 72.41   Mean   :29.15   Mean   :155.55  
# 3rd Qu.: 6.000   3rd Qu.:141.0   3rd Qu.: 80.00   3rd Qu.:36.00   3rd Qu.:190.00  
# Max.   :17.000   Max.   :199.0   Max.   :122.00   Max.   :99.00   Max.   :846.00  
# NA's   :5       NA's   :35       NA's   :227     NA's   :374     
    # bmi           diabetes           age             test      
# Min.   :18.20   Min.   :0.0780   Min.   :21.00   Min.   :0.000  
# 1st Qu.:27.50   1st Qu.:0.2437   1st Qu.:24.00   1st Qu.:0.000  
# Median :32.30   Median :0.3725   Median :29.00   Median :0.000  
# Mean   :32.46   Mean   :0.4719   Mean   :33.24   Mean   :0.349  
# 3rd Qu.:36.60   3rd Qu.:0.6262   3rd Qu.:41.00   3rd Qu.:1.000  
# Max.   :67.10   Max.   :2.4200   Max.   :81.00   Max.   :1.000  
# NA's   :11                                                      

##--PART D----------------------------------------------------------------------

#fitting a model with the test as the response variable and the other variables as predictors
mod <- glm(cbind(test,1-test)~.,family=binomial,data=pima)
summary(mod)

#glm(): Used to fit linear models
#cbind(test, 1-test)~.: Use all other variables as predictor variables to predict 'test' and '1-test' i.e. Diabetes or not diabetes (Check the help(pima))
#Note: Remember that `~.` means all the other variables
#family=binomial: The family of the linear model is success/failure ∴ Binomial

##--PART E----------------------------------------------------------------------

#The variables with less than 0.05 therefore glucose, diabetes and bmi 
#Note: Remember that to state the values first then say whether they are less/more than 0.05 then state whether or not there are statistically significant

##--PART F----------------------------------------------------------------------

install.packages("ResourceSelection")
library(ResourceSelection)

hoslem.test(mod$y,fitted(mod)) #To find out if the model fit the data
#Step 1: State the H0 and H1 
#H0: There is no difference between predicted and observed values (Model is well specified)
#H1: There is a difference between predicted and observed values (Model is not well specified)

#Step 2: Interpret the result of the test 
#Since the p-value = 0.9335 > 0.05, we do not reject the H0 and conclude that there is evidence at the 5% level that the model is well-specified 

##--PART G----------------------------------------------------------------------

#To write down the fitted model: Since we used glm(), we use logit(p) as the response variable followed by the coefficients from the summary(mod)
#logit(p) = -10.04 + 0.082(X1) + 0.038(X2) - 0.0014(X3) + 0.011(X4) - 0.00083(X5) + 0.071(X6) + 1.141(X7) + 0.034(X8) 
#where X1 is pregnant, X2 is glucose, X3 is diastolic, X4 is triceps, X5 is insulin, X6 is bmi, X7 is diabetes and X8 is age

##--PART H----------------------------------------------------------------------

#The predictor values used to predict the outcome
x0 <- c(1,1,99,64,22,76,27,0.25,25) #where the first '1' is for the intercepts as it will not change

#To predict:
eta0 <- sum(x0*coef(mod))
ilogit(eta0)
#As the p value is 0.0457, therefore there is a 4.57% predicted value
#This implies that the predicted probability that the women is positive for diabetes with the predictor variables specified is about 4.57&

#Confidence interval for the prediction:
#Step 1: Use the `predict` function to generate predictions for a new set of predictor variables
pred <- predict(mod, newdata = data.frame(pregnant = 1, glucose = 99, diastolic = 64, triceps = 22,
                                                                                    insulin = 76, bmi = 27, diabetes = 0.25, age = 25), se = T)
pred #contains the predicted values and their standard errors

#Step 2: Calculating the confidence interval for the predicted probability
ilogit(c(pred$fit - 1.96*pred$se.fit, pred$fit + 1.96*pred$se.fit))
# where the pred$fit is the predicted value 
# pred$se.fit is the standard error of the prediction
# Note: Remember that the confidence interval = predicted probability +/- 1.96*standard error

#95% C.I. = (2.50%, 8.21%)

##--QUESTION 2------------------------------------------------------------------
##--PART A----------------------------------------------------------------------
data(wbca)
summary(wbca)
    # Class            Adhes            BNucl            Chrom            Epith       
# Min.   :0.0000   Min.   : 1.000   Min.   : 1.000   Min.   : 1.000   Min.   : 1.000  
# 1st Qu.:0.0000   1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 2.000   1st Qu.: 2.000  
# Median :1.0000   Median : 1.000   Median : 1.000   Median : 3.000   Median : 2.000  
# Mean   :0.6505   Mean   : 2.816   Mean   : 3.542   Mean   : 3.433   Mean   : 3.231  
# 3rd Qu.:1.0000   3rd Qu.: 4.000   3rd Qu.: 6.000   3rd Qu.: 5.000   3rd Qu.: 4.000  
# Max.   :1.0000   Max.   :10.000   Max.   :10.000   Max.   :10.000   Max.   :10.000  
    # Mitos            NNucl            Thick            UShap            USize      
# Min.   : 1.000   Min.   : 1.000   Min.   : 1.000   Min.   : 1.000   Min.   : 1.00  
# 1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 2.000   1st Qu.: 1.000   1st Qu.: 1.00  
# Median : 1.000   Median : 1.000   Median : 4.000   Median : 1.000   Median : 1.00  
# Mean   : 1.604   Mean   : 2.859   Mean   : 4.436   Mean   : 3.204   Mean   : 3.14  
# 3rd Qu.: 1.000   3rd Qu.: 4.000   3rd Qu.: 6.000   3rd Qu.: 5.000   3rd Qu.: 5.00  
# Max.   :10.000   Max.   :10.000   Max.   :10.000   Max.   :10.000   Max.   :10.00  

#Fitting a linear regression model with Class as the response
mod2 <- glm(cbind(Class,1-Class)~.,family=binomial,data=wbca)
summary(mod2)

# Call:
#   glm(formula = cbind(Class, 1 - Class) ~ ., family = binomial, 
#       data = wbca)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -2.48282  -0.01179   0.04739   0.09678   3.06425  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) 11.16678    1.41491   7.892 2.97e-15 ***
#   Adhes       -0.39681    0.13384  -2.965  0.00303 ** 
#   BNucl       -0.41478    0.10230  -4.055 5.02e-05 ***
#   Chrom       -0.56456    0.18728  -3.014  0.00257 ** 
#   Epith       -0.06440    0.16595  -0.388  0.69795    
#   Mitos       -0.65713    0.36764  -1.787  0.07387 .  
#   NNucl       -0.28659    0.12620  -2.271  0.02315 *  
#   Thick       -0.62675    0.15890  -3.944 8.01e-05 ***
#   UShap       -0.28011    0.25235  -1.110  0.26699    
#   USize        0.05718    0.23271   0.246  0.80589    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 881.388  on 680  degrees of freedom
# Residual deviance:  89.464  on 671  degrees of freedom
# AIC: 109.46
# 
# Number of Fisher Scoring iterations: 8

#The residual deviance is 89.464 with degrees of freedom 671
#The residual deviance and degrees of freedom has a very big difference when they have to be balanced

##--PART B----------------------------------------------------------------------

#This subset of variables excludes Usize and Nnucl
mod3<-glm(cbind(Class,1-Class)~Adhes+BNucl+Chrom+Mitos+NNucl+Thick+UShap,binomial,wbca)
mod3

# Call:  glm(formula = cbind(Class, 1 - Class) ~ Adhes + BNucl + Chrom + 
#         Mitos + NNucl + Thick + UShap, family = binomial, data = wbca)
# 
# Coefficients:
# (Intercept)        Adhes        BNucl        Chrom        Mitos        NNucl  
#     11.0333      -0.3984      -0.4192      -0.5679      -0.6456      -0.2915  
#       Thick        UShap  
#     -0.6216      -0.2541  
# 
# Degrees of Freedom: 680 Total (i.e. Null);  673 Residual
# Null Deviance:	    881.4 
# Residual Deviance: 89.66 	AIC: 105.7

#The AIC is now minimized from 109.456 to 105.66

##--PART C----------------------------------------------------------------------

#logit(p) = 11.0333 - 0.3984(X1) - 0.4192(X2) - 0.5679(X3) - 0.6456(X4) - 0.2915(X5) - 0.626(X6) - 0.2541(X7)
#State all the variables

##--PART D----------------------------------------------------------------------
x2 <- c(1,1,1,3,1,1,4,1)
eta0 <- sum(x2*coef(mod3))
ilogit(eta0)
# [1] 0.9921115

pred <- predict(mod3,newdata = data.frame(Adhes = 1, BNucl = 1, Chrom = 3, Mitos = 1, NNucl = 1,
                                          Thick = 4, UShap = 1), se=T)
ilogit(c(pred$fit-1.96*pred$se.fit, pred$fit+1.96*pred$se.fit))
#95% C.I. = (0.9757467, 0.9974629)















