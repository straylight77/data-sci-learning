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
abline(fit, col="red")

summary(fit)



# ---------------------------------------------------------------------------
# 2. What proportion of the variability in y(length) explained by the linear 
#    relationship

# Coefficient of Determination R^2 -> from summary(fit)
r.sq = 0.8165
# This tells us that approximately 81.7% of the observed variations in fish 
# length can be explained by this regression model.  



# ---------------------------------------------------------------------------
# 3. What is the slope of the regression line. Does the true slope is equal 
#    to 0? Provide your answer using the hypothesis testing.

slope = b1 = fit$coefficients[2]           # 30.3

#      null hypothesis H0:  b1 = 0
#       alt hypothesis Ha:  b1 != 0

p.value = summary(fit)$coefficients[2,4]   # <2e-16

# CONCLUSION:
# A very small value for p implies very strong evidence to reject H0 and 
# accept Ha.  Therefore, we are more than 99.999% confident that the true
# slope is not 0.  

# t-test statistic = b1 / std err(b1)
se = summary(fit)$coefficients[2,2]
tstat = b1 / se



# ---------------------------------------------------------------------------
# 4. What measures the amount of variability in y(length) about the line at 
#    a given value of x(age). Provide the numeric value from the R output.

# Residual standard error -> from summary(fit)
# Describes the deviation from the model that the observed data represents. 
rse = 28.65



# ---------------------------------------------------------------------------
# 5. Is there any relationship between the fish age and fish length?

# coefficient of correlation
r = sqrt(r.sq)                   # 0.9036039
corr = cor(df$Length, df$Age)    # 0.9035912

# CONCLUSION:
# The coefficient of correlation, r=0.904, is positive and it is very close to 
# +1, which means that the fish length (y) and age (x) have a very strong 
# linear relationship.








# can also calculate these manually (Devore p. 479)
n = length(df$Age)
S.xy = sum(df$Age * df$Length) - ( sum(df$Age) * sum(df$Length) / n)
S.xx = sum(df$Age ^ 2) - (sum(df$Age)^2)/n
B1.hat = S.xy / S.xx
B0.hat = mean(df$Length) - B1.hat * mean(df$Age)

# the final fitted linear equation as a function 
f = function(x) {
  y = 65.5272 + 30.3239 * x
  return(y)
}

# (optional) Let's calculate and analyze the deviations (residuals)
error = df$Length - f(df$Age)
summary(error)  # matches summary(fit)
sd(error)       # matches "Residual standard error: 28.65 on 437 degrees of freedom"
sum(error)      # should be close to 0 

sigma.hat = sum(df$Length - f(df$Age)) ^ 2 / (n-2)   # not working


