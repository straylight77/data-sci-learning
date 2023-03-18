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
fish.mod = lm(Length~Age, data = df)
fish.res = residuals(fish.mod)
fish.sum = summary(fish.mod)

# pull out some useful parameters
b0 = fish.mod$coefficients[1]   # y-intercept
b1 = fish.mod$coefficients[2]   # slope of the line
se = fish.sum$sigma             # std dev of the residuals
n = nrow(df)                    # number of observations


par(mfrow=c(2,2))

# scatter plot with the fitted regression line 
plot(df$Age, df$Length, 
     xlab="Fish Age", 
     ylab="Fish Length", 
     main="Estimated Line of Regression")
abline(fish.mod, col="steelblue", lwd=2)

# Add the 95% prediction interval
t.val = qt(0.975, n-2)
interval = t.val * se
abline(b0 + interval, b1, col="steelblue", lty=2)
abline(b0 - interval, b1, col="steelblue", lty=2)

# quantile-quantile plot 
qqnorm(fish.res)
qqline(fish.res, col="red", lty=2)

# plot of residuals and their expected mean (=0)
plot(df$Age, fish.res, 
     xlab="Fish Age", 
     ylab="Residuals", 
     main="Plot of Residuals")
abline(h=0, col="red", lty=2)

# histogram of residuals with normal distribution curve 
hist(fish.res, prob=TRUE)
xx = seq(min(fish.res), max(fish.res), length=1000)
lines(xx, dnorm(xx, mean=mean(fish.res), sd=sd(fish.res)), col="red")
#abline(v=mean(fish.res), col="steelblue")

interval2 = qnorm(0.975) * sd(fish.res)
abline(v=interval2, col="red", lty=2)
abline(v=-interval2, col="red", lty=2)

summary(fish.res)    # median=-4.5, mean=0

# Despite the distribution of residuals being rightly skewed (median < mean)
# these values for intervals are close (within 0.39% of each other).
interval - interval2                     # 0.2200583
(interval - interval2) / interval * 100  # 0.39%

# CONCLUSION
# Predicted values for fish length can be predicted with a given age using the
# following linear equation and with the 95% prediction interval
#     Length = 65.5 + 30.3 * Age  (+- 56.3)



# ---------------------------------------------------------------------------
# 2. What proportion of the variability in y(length) explained by the linear 
#    relationship

r.sq = fish.sum$r.squared     #0.8165

# Coefficient of Determination R^2 
# This tells us that approximately 81.7% of the observed variations in fish 
# length can be explained by this regression model.



# ---------------------------------------------------------------------------
# 3. What is the slope of the regression line. Does the true slope is equal 
#    to 0? Provide your answer using the hypothesis testing.

slope = b1 = fish.mod$coefficients[2]    # 30.3

# A true slope of 0 implies that there is no relationship between fish weight
# and age.  Let's set up our null and alternative hypotheses. 

#      null hypothesis H0:  b1 = 0
#       alt hypothesis Ha:  b1 != 0

# First, let's assume H0 is valid.  In this case, we determine the probability 
# that we would see the same results we found in the sample.  Let's also assume
# a 95% confidence interval, meaning if the probability that b1 = 0 is less 
# than 5%, we will reject H0.

p.value = fish.sum$coefficients[2,4]     # 5.515796e-163

# CONCLUSION:
# This very small value for p implies very strong evidence to reject H0 and 
# accept Ha.  Furthermore, it can be stated that we are more than 99.999% 
# confident (1-p.value) that the true slope is not 0.  



# ---------------------------------------------------------------------------
# 4. What measures the amount of variability in y(length) about the line at 
#    a given value of x(age). Provide the numeric value from the R output.

# Residual standard error
# Describes the deviation from the model that the observed data represents. 
rse = fish.sum$sigma      # 28.65

# Using this we can determine a 95% prediction interval. For any prediction of 
# fish length for a given fish age, we can expect to see values above and 
# below the predicted amount by this interval 95% of the time. 
#
#   i.e.  predicted Length = regression(Age) +/- interval
#
# (This was also calculated above, shown on the plots, and mentioned in the 
# equation for the line of regression.)  
t.val = qt(0.975, n-2)
interval = t.val * se     # 56.3


# ---------------------------------------------------------------------------
# 5. Is there any relationship between the fish age and fish length?

# coefficient of correlation
r = sqrt(r.sq)                   # 0.9036039
corr = cor(df$Length, df$Age)    # 0.9035912

# CONCLUSION:
# The coefficient of correlation, r=0.904, is positive and it is very close to 
# +1, which means that the fish length (y) and age (x) have a very strong 
# linear relationship.


