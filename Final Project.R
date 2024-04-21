#Title: Final Project
#Author: Trinity Miller
#Date: 03/26/2024


# The dataset consists of 17 columns, each representing the following attributes:
#   
# • age: Customer's age.
# 
# • job: Customer's occupation.
# 
# • marital: Customer's marital status.
# 
# • education: Customer's level of education.
# 
# • default: Whether the customer has credit in default.
# 
# • balance: Customer's average yearly balance.
# 
# • housing: Whether the customer has a housing loan.
# 
# • loan: Whether the customer has a personal loan.
# 
# • contact: Type of communication contact with the customer.
# 
# • day: Last contact day of the month.
# 
# • month: Last contact month of the year.
# 
# • duration: Last contact duration in seconds.
# 
# • campaign: Number of contacts performed during this campaign for the customer.
# 
# • pdays: Number of days since the customer was last contacted from a previous campaign.
# 
# • previous: Number of contacts performed before this campaign for the customer.
# 
# • poutcome: Outcome of the previous marketing campaign.
# 
# • deposit: Whether the customer subscribed to a term deposit.


#Set working directories and packages:
data_path = "C:\\Users\\trinm\\Saint Vincent College\\DS 300\\"
deposit_data_file = read.csv(paste0(data_path, "Bank Target Marketing Dataset.csv"))
library(corrplot)
library(tidymodels)
library(tidyverse)
library(ggridges)
library(ggplot2)
library(car)



#Examine file:
head(deposit_data_file)
View(deposit_data_file)

#Add a column that is a binary form of deposit:
#0 = No Deposit, 1 = Deposit
deposit_data_file$deposit_binary=0
deposit_data_file$deposit_binary[which(deposit_data_file$deposit=="yes")]=1
head(deposit_data_file)
View(deposit_data_file)

#Add a column that is a binary form of housing:
#0 = No housing Loan, 1 = Housing Loan
deposit_data_file$housing_loan_binary=0
deposit_data_file$housing_loan_binary[which(deposit_data_file$housing=="yes")]=1

#Add a column that is a binary form of loan:
#0 = No personal Loan, 1 = personal Loan
deposit_data_file$personal_loan_binary=0
deposit_data_file$personal_loan_binary[which(deposit_data_file$loan=="yes")]=1

head(deposit_data_file)



#Model with all variables
linear_model = lm(
  deposit_binary ~.,
  data = deposit_data_file
)
summary(linear_model)
plot(linear_model)

#Linear regression with all variables but those that weren't significant
linear_model2 = lm(
  formula = deposit_binary ~. -month -age -pdays -default -contact -day -housing_loan_binary -personal_loan_binary,
  data = deposit_data_file
)
summary(linear_model2)
plot(linear_model2)

vif(linear_model2)

#              GVIF Df GVIF^(1/(2*Df))
# job       2.659136 11        1.045458
# marital   1.127448  2        1.030443
# education 2.264530  3        1.145943
# balance   1.024645  1        1.012248
# housing   1.141547  1        1.068432
# loan      1.030310  1        1.015042
# duration  1.277427  1        1.130233
# campaign  1.026697  1        1.013261
# previous  1.457097  1        1.207103
# poutcome  1.683507  3        1.090693
# deposit   1.480699  1        1.216840

#FOUND: VIF for job is higher then two, meaning collinearity, going to remove it.

linear_model3 = lm(
  deposit_binary ~. -month -age -pdays -default -contact -day -housing_loan_binary -personal_loan_binary -job,
  data = deposit_data_file
)
summary(linear_model3)

# Call:
#   lm(formula = deposit_binary ~ . - month - age - pdays - default - 
#        contact - day - housing_loan_binary - personal_loan_binary - 
#        job, data = deposit_data_file)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -9.106e-13 -4.000e-17  0.000e+00  5.000e-17  1.054e-14 
# 
# Coefficients:
#                       Estimate Std. Error    t value   Pr(>|t|)    
# (Intercept)         1.254e-15  8.762e-17  1.431e+01  < 2e-16 ***
#   maritalmarried     -2.149e-16  5.210e-17 -4.124e+00 3.72e-05 ***
#   maritalsingle       2.543e-16  5.655e-17  4.497e+00 6.90e-06 ***
#   educationsecondary  2.056e-16  4.821e-17  4.265e+00 2.00e-05 ***
#   educationtertiary   5.609e-16  5.234e-17  1.072e+01  < 2e-16 ***
#   educationunknown    3.681e-16  8.995e-17  4.092e+00 4.29e-05 ***
#   balance             3.603e-20  5.301e-21  6.798e+00 1.07e-11 ***
#   housingyes         -1.264e-15  3.355e-17 -3.767e+01  < 2e-16 ***
#   loanyes            -5.814e-16  4.526e-17 -1.284e+01  < 2e-16 ***
#   duration            6.946e-18  6.484e-20  1.071e+02  < 2e-16 ***
#   campaign           -4.897e-17  5.401e-18 -9.067e+00  < 2e-16 ***
#   previous            1.665e-17  8.469e-18  1.966e+00   0.0493 *  
#   poutcomeother       4.373e-16  9.332e-17  4.686e+00 2.79e-06 ***
#   poutcomesuccess     5.512e-15  9.408e-17  5.859e+01  < 2e-16 ***
#   poutcomeunknown    -8.099e-16  5.842e-17 -1.386e+01  < 2e-16 ***
#   deposityes          1.000e+00  5.015e-17  1.994e+16  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.837e-15 on 56357 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# F-statistic: 3.89e+31 on 15 and 56357 DF,  p-value: < 2.2e-16

plot(linear_model3)

vif(linear_model3)

#No collinearity!!
#            GVIF Df       GVIF^(1/(2*Df))
# marital   1.041714  2        1.010269
# education 1.065556  3        1.010639
# balance   1.021612  1        1.010748
# housing   1.070313  1        1.034559
# loan      1.024004  1        1.011931
# duration  1.273059  1        1.128299
# campaign  1.024531  1        1.012191
# previous  1.456724  1        1.206948
# poutcome  1.677983  3        1.090096
# deposit   1.467508  1        1.211407

#logistic regression model
glm.fits = glm(
  deposit_binary ~ balance + campaign + housing_loan_binary + personal_loan_binary + marital + poutcome,
  data = deposit_data_file, family = binomial
)
summary(glm.fits)
# Call:
#   glm(formula = deposit_binary ~ balance + campaign + housing_loan_binary + 
#         personal_loan_binary + marital + poutcome, family = binomial, 
#       data = deposit_data_file)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.0743  -0.6540  -0.4976  -0.3738   3.2501  
# 
# Coefficients:
#                         Estimate Std. Error z value   Pr(>|z|)    
# (Intercept)          -5.912e-01  4.952e-02 -11.939  < 2e-16 ***
#   balance               2.835e-05  3.340e-06   8.488  < 2e-16 ***
#   campaign             -1.019e-01  6.027e-03 -16.900  < 2e-16 ***
#   housing_loan_binary  -8.001e-01  2.386e-02 -33.540  < 2e-16 ***
#   personal_loan_binary -4.929e-01  3.753e-02 -13.131  < 2e-16 ***
#   maritalmarried       -2.200e-01  3.721e-02  -5.911 3.39e-09 ***
#   maritalsingle         1.748e-01  3.914e-02   4.466 7.99e-06 ***
#   poutcomeother         2.801e-01  5.825e-02   4.808 1.52e-06 ***
#   poutcomesuccess       2.231e+00  5.736e-02  38.891  < 2e-16 ***
#   poutcomeunknown      -4.188e-01  3.561e-02 -11.762  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 54432  on 56372  degrees of freedom
# Residual deviance: 47795  on 56363  degrees of freedom
# AIC: 47815
# 
# Number of Fisher Scoring iterations: 5

plot(glm.fits)

vif(glm.fits)
#No collinearity
#                          GVIF Df GVIF^(1/(2*Df))
# balance              1.010843  1        1.005407
# campaign             1.014152  1        1.007051
# housing_loan_binary  1.024181  1        1.012018
# personal_loan_binary 1.008884  1        1.004432
# marital              1.006606  2        1.001647
# poutcome             1.033475  3        1.005503

#FOUND: Adding the loans makes age insignificant, probably due to the fact that the older you get, the more likely it is you'll have loans

#Let's compare the two models
anova(linear_model3, glm.fits)

# Analysis of Variance Table
# 
# Model 1: deposit_binary ~ (age + job + marital + education + default + 
#                              balance + housing + loan + contact + day + month + duration + 
#                              campaign + pdays + previous + poutcome + deposit + housing_loan_binary + 
#                              personal_loan_binary) - month - age - pdays - default - contact - 
#   day - housing_loan_binary - personal_loan_binary - job
# Model 2: deposit_binary ~ balance + campaign + housing_loan_binary + personal_loan_binary + 
#   marital + poutcome
# Res.Df   RSS Df Sum of Sq          F    Pr(>F)    
# 1  56357     0                                      
# 2  56363 47795 -6    -47795 5.4092e+32 < 2.2e-16 ***

#The Pvalue is significant meaning that the models are different!
#Model One has the lowest RSS, 0, meaning it is the better model

AIC(linear_model3, glm.fits)

#               df      AIC
# linear_model3 17 -3582488.20
# glm.fits      10    47815.13

#Interestingly enough, glm.fits has a lower degree of freedom
#But the AIC for the first model is ALOT lower meaning a better model

#Saving my results:
pdf(paste0(data_path, "Final_project_results.pdf"))
par(mfrow=c(2,2))
plot(linear_model3)
plot(glm.fits)
dev.off()





