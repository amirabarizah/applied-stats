#Q1 - Input the data (A)
A <- data.frame(
  Pair = c(1, 2, 3, 4, 5, 6),
  Honey = c(19, 12, 9, 17, 24, 24),
  No.honey = c(14, 8, 4, 4, 11, 11)
)

View (A)

#Compute the differences
attach(A)
A$Differences <- Honey - No.honey

#Test the differences for normality 
shapiro.test(A$Differences)

#Since the p-value is 0.01214 < 0.05, reject the null hypothesis (the differences follow a normal distribution)
wilcox.test(Honey,No.honey,paired=T, alternative="greater")

#Conclusion: Since the p-value is 0.01675 < 0.05, reject the null hypothesis. There is a significance difference that on 5% level of significance that honey in a diet raises haemoglobin level

#Q2 - Input the data
B <- data.frame(
  Last = c(73,68,73,71,71,72,68,68,74),
  First = c(66,70,64,71,65,71,71,71,71)
)

View(B)
attach(B)

#Compute the differences
B$Differences <- Last - First
attach(B)

#Test the differences for normality
shapiro.test(B$Differences)

#Since the p-value is 0.3553 > 0.05, do not reject the null hypothesis (The differences do not follow a normal distribution)
t.test(Last,First,paired=T,alternative="greater")

#Conclusion: Since the p-value is 0.1096 > 0.05, do not reject the null hypothesis. There is no significant difference on a 5% level of significance that the population mean score on the last round is higher than that on the first.

#Q1 - Input the data (C)
C <- data.frame (
  Nitrogen.from.chemical.compounds = c(2.30143,2.29890,2.29816,2.30182,2.29869,2.29940,2.29849,2.29889),
  Nitrogen.from.air = c(2.31026,2.31017,2.30986,2.31003,2.31007,2.31024,2.31010,2.31028)                         
)

#Since the samples are independent, use the normality test for both the samples
shapiro.test(Nitrogen.from.air)
shapiro.test(Nitrogen.from.chemical.compounds)

#Since the p-value for "Nitrogen from chemical compounds" is 0.03062, reject the null hypothesis (Not both the samples are following normal distributions)
wilcox.test(Nitrogen.from.chemical.compounds,Nitrogen.from.air,paired=F)

#Conclusion : Since the p-value is 0.0001554 < 0.05, reject the null hypothesis. Since there is a significance difference on a 5% level, therefore the population do not have the same mean

#Important note: "paired=T" is for two samples with a relationship (i.e. paired) & "paired=F" is for two independent samples (i.e. two samples t-test/wilcoxon signed rank)


#Q1 -Input the data (D)
D <- data.frame (Music, No.Music)

Music = c(35.0,36.8,40.2,46.6,50.4,64.2,83.0,87.6,89.2)
No.Music = c(28.2,28.6,33.0,34.8,45.4,50.8,52.6,66.4,67.8)

attach(D)

#Check for equal or unequal variance - Check the p-value (H0: The variance are not significantly different)
var.test(Music,No.Music)
#Conclusion: Since the p-value is 0.3048 (>0.05), there is no evidence that the variance of Music and No Music are significantly different

#Check for the normality of both 
shapiro.test(Music)
shapiro.test(No.Music)

#Since both the p-values are >0.05, do not reject the null hypothesis (Both the samples follow a normal distribution)
t.test(Music,No.Music,var.equal=T)

#Conclusion : There is no significance difference at 5% level that background music affect worker productivity  

#Q2 - Input the data (X)
H0 <- "Political views and marijuana usage level are independent within the population"
H1 <- "Political views and marijuana usage level are not independent within the population"
x <- matrix(c(479,214,172,173,47,45,119,15,85),3,3)
chisq.test(x)

#Conclusion : The p-value is 3.043e-13 < 0.05, reject the null hypothesis. There is a significance difference at 5% level that the political views and marijuana usage level are independent within the population

#Q3 - Input the data (Y)
H0 <- "The presence/absence of hypoglycemia is independent of insulin dosage"
H1 <- "The presence/absence of hypoglycemia is not independent of insulin dosage"
y <- matrix (c(4,40,21,74,28,59,15,26,12,46),2,5)
chisq.test(y)

#Conclusion : The p-value is 0.0148 < 0.05, reject the null hypothesis. There is a significance difference at 5% level that the presence/absence of hypoglycemia is independent of insulin dosage
