rm(list=ls()) ;
library(readxl)
library(tidyr)
library(moments)
library(tidyverse)
library(forcats)

#######################################################################################################################################################

# Problem 1:  Does Confidence Interval work?  
# We'll generate a population, get a sample from it, create a Confidence Interval using this sample, and then check if our Confidence interval has the Population parameter. 

# Use a 4-digit number (nnnn)  of your choice to set the seed using this command: set.seed=nnnn
set.seed(1234);

# Generate a Normal distribution problem with N = 2,500, Mean =180,  and Std dev = 30. Round it off to 1 decimal place
ND <- round(rnorm(2500,180,30),1);

# Find the mean of this population.  
Population_Mean <- mean(ND);Population_Mean

# Now, from this population, after setting the same seed again, 
set.seed(1234);

# Draw a random sample of size n = 30.
Sample <- sample(ND,30);

# a.  Find the mean and the std error of this sample.
Sample_Mean <- mean(Sample);Sample_Mean
Sample_Sd <- sd(Sample);Sample_Sd
Standard_Error <- Sample_Sd/sqrt(30); Standard_Error

# b. Get the proper t-score for a Confidence level of 84.65%.  
n=30
t <- qt(0.07675,n-1,lower.tail = FALSE); t

# c. Find the lower and upper limits of the Confidence interval. 
Lower_Tail <- Sample_Mean - t*Standard_Error; Lower_Tail
Upper_Tail <- Sample_Mean + t*Standard_Error; Upper_Tail
t.test(Sample,  conf.level = .8465)

# d. Use If statement to see if the population mean falls within the Confidence interval.  Get the appropriate output from the R code.    
out <- if(Population_Mean >= Lower_Tail & Population_Mean <= Upper_Tail) 
    {cat("Population Mean is within the Confidence Interval")} else
    {cat("Population Mean is not within the Confidence Interval")}

####################################################################################################################################################

# Problem 2: (Set-1)
# Three anti-bacteria creams were used on three age groups.The number of hours before the medicines started to show a noticeable effect are recorded in the table.Assume α = 0.05

prob_2<-data.frame(read_excel("F22-6359-Test-3.xlsx", sheet="Set-1"))

# Create individual vectors.   rep command rep("Young",30) will repeat Young 30 times.  	 	 
v1<-data.frame(Hours = prob_2[, 2], Medicine = prob_2[, 1], Age=rep("Young",30))
v2<-data.frame(Hours = prob_2[, 3], Medicine = prob_2[, 1], Age=rep("Middle_Age",30))
v3<-data.frame(Hours = prob_2[, 4], Medicine = prob_2[, 1], Age=rep("Senior",30))

# Rename columns	 	 	 
names(v1)[1] <- 'Hours'	 	 
names(v2)[1] <- names(v1)[1]	 	 
names(v3)[1] <- names(v1)[1]	 	 

# Combine everything and create a new dataset	 	 
data1=rbind(v1, v2, v3); data1	 	 
attach(data1)

# a.  Run this as an ANOVA 2-factor  R program.
a1 <- aov(Hours~Medicine+Age+Medicine:Age)
summary(a1)

# QUESTION 1
# For Set 1, the P-value for Age is
P_Age <- summary(a1)[[1]][[2,"Pr(>F)"]]; P_Age

# QUESTION 2
# For Set 1, what is the P-value for Medicine? 
P_Medicine <- summary(a1)[[1]][[1,"Pr(>F)"]]; P_Medicine

# QUESTION 4
# For Set 1, what is the F-stat for medicine ?
F_Medicine <- summary(a1)[[1]][[1,"F value"]]; F_Medicine

# c.  Also draw the interaction graph to show the interaction between the two factors.  
a <- interaction.plot (Medicine, Age, Hours, lwd = 2,  col=1:3, main="Age vs Medicine")
b <- interaction.plot (Age, Medicine, Hours, lwd = 2,  col=1:3, main="Medicine vs Age")

#####################################################################################################################################################

# Problem 3: (Set-2) Two sample t-test
# Automobile Insurance companies consider many factors including the miles driven by a driver and the gender.  
# The dataset consists of the reported miles (in thousands) driven by young drivers (25 years or less)
# in the previous year.  One insurance company wants to know if there are any difference between the two genders.  

prob_3 <-read_excel("F22-6359-Test-3.xlsx", sheet="Set-2")

# a.  Do a variance test to see if the two variances are equal.
var_test <- var.test(Distance~Gender, data = prob_3);var_test

# QUESTION 7
# For Set 2, what is the p-value from the Variance test?
var_test_pval <- var.test(Distance~Gender, data = prob_3)$p.value;var_test_pval

# b.  Do the appropriate t-test at α = 5%. 
t_test <- t.test(Distance~Gender, var.equal= TRUE, data = prob_3);t_test

# QUESTION 10
# For Set 2,  what is the p-value for the t-test?
t_test_pval <- t.test(Distance~Gender, var.equal= TRUE, data = prob_3)$p.value;t_test_pval

# QUESTION 11
# For Set 2, what is the t-statistics?
t_test_tstat <- t.test(Distance~Gender, var.equal= TRUE, data = prob_3)$statistic;t_test_tstat

#####################################################################################################################################################

# Problem 4: (Set-3)
# A bank has collected a sample and is trying to see how various factors impact it's loan approvals.  
# Divide Credit Scores by 10 and incomes by 1000 (in R) and perform Logistics Regression.

prob_4<-read_excel("F22-6359-Test-3.xlsx", sheet = "Set-3")

names(prob_4)[1] <- 'Loan.Approved'
names(prob_4)[2] <- 'Credit.scores'
names(prob_4)[3] <- 'Income'
names(prob_4)[4] <- 'Neighborhood.income'

prob_4$Credit.scores<- prob_4$Credit.scores/10
prob_4$Income<- prob_4$Income/1000
prob_4$Neighborhood.income<- prob_4$Neighborhood.income/1000

attach(prob_4)

# Logistic Regression
Loan <-glm(Loan.Approved ~ Credit.scores + Income + Neighborhood.income, family="binomial")
summary(Loan)

# Maximum likelihood estimates of the parameters
RegOut<-c(coef(Loan));  RegOut

# QUESTION 13
#For Set 3, what are the odds of loan approval for a person who lives in a neighborhood with income of 40192 vs someone with the neighborhood income of 46569 assuming everything else being equal?

# Neighborhood with income of 40192
odd40192<-exp(RegOut[1]+RegOut[2]+RegOut[3]+RegOut[4]*40.192)
# Neighborhood income of 46569 
Odd46569<-exp(RegOut[1]+RegOut[2]+RegOut[3]+RegOut[4]*46.569)
# odds of loan approval for Neighborhood with income of 40192 vs Neighborhood income of 46569 
odd40192/Odd46569

# QUESTION 14
# For Set 3, what is the probability of loan approval for a person whose credit score is 728, income is 61653 and lives in a neighborhood whose income is 35436?

# Loan approval for a person whose credit score is 728, income is 61653 and lives in a neighborhood whose income is 35436
odds<-exp(RegOut[1]+RegOut[2]*72.8+RegOut[3]*61.653+RegOut[4]*35.436)
#Probability
probabbility <- odds/(odds+1); probabbility

# QUESTION 15
# For Set 3, what are the Odds of loan approval for a person whose credit score is 716, income is 56759 and lives in a neighborhood whose income is 40746

# Loan approval for a person whose credit score is 716, income is 56759 and whose income is 40746
odds1<-exp(RegOut[1]+RegOut[2]*71.6+RegOut[3]*56.759+RegOut[4]*40.746); odds1

# QUESTION 16
# For the Logistics Problem in Set 3, what is the coefficient of Income as calculated by R?
RegOut[3]

# QUESTION 17
# For the Logistics Problem in Set 3, what is the coefficient of Credit Scores as calculated by R?
RegOut[2]

# QUESTION 18
# For the Logistics Problem in Set 3, what is the Intercept calculated by R?
RegOut[1]

####################################################################################################################################################

# Problem 5: (Set-4)
# You've picked up a bunch of rocks from a rocky beach and want to estimate the weight of all the 
# rocks at the beach with a Confidence level of  93.47%.  

prob_5 <-read_excel("F22-6359-Test-3.xlsx", sheet="Set-4")
attach(prob_5)

# a.  Plot the qqline and box plot of the data.  Also get the skewness.  What is your conclusion about the distribution being normal?
qqnorm(Weight)     
qqline(Weight)
boxplot(Weight)

# QUESTION 20
# For Set 4, what is the skewness Before log Transformation?
skewness(Weight)

# b.  Do a log transformation (base e) and perform the steps in a.  What's your conclusion?  Use Log transformed data for the following questions.
log_data <- log(prob_5$Weight,base = exp(1))
qqnorm(log_data)     
qqline(log_data)
boxplot(log_data)

# QUESTION 21
# For Set 4, calculate Skewness after log transformation?
skewness(log_data)

# c.  What is the mean, Std dev, and the sample size?

# QUESTION 19
# For Set 4, what is the mean of the log-transformed data?
mean_prob5 <- mean(log_data); mean_prob5

# QUESTION 22
# For Set 4, calculate the standard Deviation after log transformation
sd_prob5 <- sd(log_data); sd_prob5

sample_size_prob5 <- length(log_data); sample_size_prob5

# d.  Find std error using the std error formula we've discussed.

# QUESTION 23
# For Set 4, what is the standard error?
std_error_prob5 <- (sd_prob5/sqrt(sample_size_prob5)); std_error_prob5

# e.  Find the t-score for the 93.47% confidence interval.  
t_score_prob5  <- qt(0.03265,sample_size_prob5-1,lower.tail = FALSE); t_score_prob5

# f.  Use this t-score, sample mean, std error to get the upper and lower limit of the Confidence Interval.  Use the formula we've discussed.  

# QUESTION 24
# For Set 4, what is the lower limit of Confidence Interval for a Confidence level of LCL?
Lower_Tail_prob5 <- mean_prob5 - t_score_prob5*std_error_prob5; Lower_Tail_prob5

Upper_Tail_prob5 <- mean_prob5 + t_score_prob5*std_error_prob5; Upper_Tail_prob5
t.test(log_data,  conf.level = .9347)

# g.  Do reverse transformation to get the Confidence Interval in Ounces.  

# QUESTION 25
# For Set 4, calculate Upper Limit after reverse Transformation?
Upper_Tail_rev <- exp(Upper_Tail_prob5); Upper_Tail_rev

# QUESTION 26
# For Set 4, what is the Lower Limit after reverse Transformation?
Lower_Tail_rev <- exp(Lower_Tail_prob5); Lower_Tail_rev


####################################################################################################################################################

# Problem 6: (Set-5)
# A  random sample of 1100 U.S. adults were questioned regarding their political affiliation and opinion on a tax reform bill. 
# Perform a test to see if the political affiliation and their opinion on a tax reform bill are independent.

prob_6 <- data.frame(read_excel("F22-6359-Test-3.xlsx", sheet="Set-5"))

# Get ChiSq Stats, P-values, etc. as required by the Online test.    
chisq_prob_6 <- prob_6[1:3,2:4]
chisq.test(chisq_prob_6)

# QUESTION 27
# For Set-5, what are the degrees of Freedom?
chisq.test(chisq_prob_6)$parameter

# QUESTION 28
# For Set-5, what is the Chi Sq Critical?  Assume Alpha = 5%
Chi Sq Critical <- qchisq(p =0.05, df = 4, lower.tail =FALSE)

# QUESTION 29
# For Set-5, what is the P-Value?
chisq.test(chisq_prob_6)$p.value

####################################################################################################################################################
print("The data is nnot Normally Distributed. 
      Additionally the box plot consists of outliers which is misleading")
