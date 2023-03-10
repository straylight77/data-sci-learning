# ###########################################################################
# #                                                                         #
# #                           ASSIGNMENT #3                                 #
# #                                                                         #
# ###########################################################################

# A hypothetical fishery in Florida, USA has 439 fish. The fishery has the 
# record of age (in months) and length (in cm) of each fish. 
#
# 1. What is the fitted regression line
# 2. What proportion of the variability in y(length) explained by the linear 
#    relationship
# 3. What is the slope of the regression line. Does the true slope is equal 
#    to 0? Provide your answer using the hypothesis testing.
# 4. What measures the amount of variability in y(length) about the line at 
#    a given value of x(age). Provide the numeric value from the R output.
# 5. Is there any relationship between the fish age and fish length?
#

setwd("data-sci-learning/dat201/")
df = read.csv("assignment3.csv")


# ---------------------------------------------------------------------------
# 1. What is the fitted regression line
fit = lm(Length~Age, data = df)   
plot(df$Age, df$Length)
abline(reg_1, col="red")

summary(fit)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  65.5272     3.1974   20.49   <2e-16 ***
# Age          30.3239     0.6877   44.09   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# ---------------------------------------------------------------------------
# 2. What proportion of the variability in y(length) explained by the linear 
#    relationship



# ---------------------------------------------------------------------------
# 3. What is the slope of the regression line. Does the true slope is equal 
#    to 0? Provide your answer using the hypothesis testing.

slope = b1 = fit$coefficients[2]    # slope = 30.3

#      null hypothesis H0:  b1 = 0
#       alt hypothesis Ha:  b1 != 0

p.value = summary(fit)$coefficients[2,4]

# a very small value for p implies very strong evidence to reject H0 and 
# accept Ha.  Therefore, we are more than 99.999% confident that the true
# slope is not 0.  

# t-test statistic = b1 / std err(b1)
se = summary(fit)$coefficients[2,2]
tstat = b1 / se




# ---------------------------------------------------------------------------
# 4. What measures the amount of variability in y(length) about the line at 
#    a given value of x(age). Provide the numeric value from the R output.





# ---------------------------------------------------------------------------
# 5. Is there any relationship between the fish age and fish length?
corr = cor(df$Length, df$Age)





