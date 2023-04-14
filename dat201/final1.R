# ###########################################################################
# #                                                                         #
# #                    FINAL ASSIGNMENT - QUESTION #1                       #
# #                                                                         #
# ###########################################################################

# 1. Examining the relation between y = LIFEEXP and x = FERTILITY.
# 2. Fit a linear regression model of LIFEEXP using the explanatory variable 
#    x = FERTILITY.
# 3. The United States has a FERTILITY rate of 2.0. Determine the fitted life 
#    expectancy.
# 4. Now fit a regression model on LIFEEXP using three explanatory variables, 
#    FERTILITY, PUBLICEDUCATION, and lnHEALTH (the natural logarithmic transform
#   of PRIVATEHEALTH).
#    a. Interpret the regression coefficient associated with public education.
#    b. Interpret the regression coefficient associated with health expenditures 
#       without using the logarithmic scale for expenditures.
#    c. Based on the model fit, is PUBLICEDUCATION a statistically significant 
#       variable? To respond to this question, use a formal test of hypothesis. 
#       State your null and alternative hypotheses, decision‐making criterion, 
#       and decision‐making rule.
# 7. Make an overall comment for the non‐statistician audience

# Data can be found here:
# https://instruction.bus.wisc.edu/jfrees/jfreesbooks/regression%20modeling/bookwebdec2010/CSVData/UNLifeExpectancy.csv

setwd("~/data-sci-learning/dat201/")
df.raw = read.csv("UNLifeExpectancy.csv")

df = df.raw[,c("LIFEEXP", "FERTILITY", "PUBLICEDUCATION", "PRIVATEHEALTH")]

summary(df)
df.raw[ is.na(df.raw$FERTILITY), ]
df.raw[ is.na(df.raw$PUBLICEDUCATION), ]

attach(df)
hist(FERTILITY)
hist(PRIVATEHEALTH)
hist(PUBLICEDUCATION)
hist(LIFEEXP)

# ----------------------------------------------------------------------------
# 1. Examining the relation between y = LIFEEXP and x = FERTILITY.
cor1 = cor(df$LIFEEXP, df$FERTILITY, use="complete.obs")         # -0.8067466
plot(LIFEEXP ~ FERTILITY, data = df)


# ----------------------------------------------------------------------------
# 2. Fit a linear regression model of LIFEEXP using the explanatory variable 
#    x = FERTILITY.

m = lm(LIFEEXP ~ FERTILITY, data = df)
abline(m, col="red")
m.summ = summary(m)
m.summ

# RESULTS
#
#       LIFEEXP = 83.7381 + (-5.2735) FERTILITY
#
#                Estimate Std. Error t value Pr(>|t|)    
#    (Intercept)  83.7381     1.0439   80.22   <2e-16 ***
#    FERTILITY    -5.2735     0.2887  -18.27   <2e-16 ***
#    Multiple R-squared:  0.6508,	Adjusted R-squared:  0.6489
#    F-statistic: 89.64 on 3 and 148 DF,  p-value: < 2.2e-16


# ----------------------------------------------------------------------------
# 3. The United States has a FERTILITY rate of 2.0. Determine the fitted life 
#    expectancy.

new_df = data.frame(FERTILITY = 2.0)
pred = predict(m, newdata=new_df, interval="confidence")

round(pred, 2)

# RESULTS:
#       fit   lwr   upr
#     73.19 72.01 74.37
#
# The fitted life expectancy for the United States is 73.2 years with a 95% 
# confidence interval of 72.0-74.4 years.



# ----------------------------------------------------------------------------
# 4. Now fit a regression model on LIFEEXP using three explanatory variables, 
#    FERTILITY, PUBLICEDUCATION, and lnHEALTH (the natural logarithmic transform
#    of PRIVATEHEALTH).

# First, let's examine these explanatory variables including PRIVATEHEALTH,
# before we trasnform it to lnHEALTH

pairs(~ LIFEEXP + FERTILITY + PUBLICEDUCATION + PRIVATEHEALTH,
      lower.panel=NULL, 
      gap=0, 
      data=df)

hist(df$PRIVATEHEALTH)
abline(v=mean(df$PRIVATEHEALTH, na.rm=TRUE), col="red")
plot(LIFEEXP ~PRIVATEHEALTH, data=df)

# OBSERVATION: These plots show PRIVATEHEALTH is somewhat skewed with a few
# upper outliers.  Relating it to LIFEEXP as such will be difficult. Attempting
# a log transform to see if it helps makes sense. 

# Next, calculate a new variable lnHEALTH and examine that before creating our
# regression model

df$lnHEALTH = log(df$PRIVATEHEALTH)
hist(df$lnHEALTH)
plot(LIFEEXP ~ lnHEALTH, data=df)

pairs(~ LIFEEXP + FERTILITY + PUBLICEDUCATION + lnHEALTH,
      lower.panel=NULL, 
      gap=0, 
      data=df)

# OBSERVATION: The plot looks much better for a linear regression.  Let's 
# continue with developing the model.

m2 = lm(LIFEEXP ~ FERTILITY + PUBLICEDUCATION + lnHEALTH, data=df)
m2.summ = summary(m2)
m2.summ

# RESULTS:
#
# y = 85.6264 + (-5.3993)x1 + (-0.1846)x2 + (-1.0296)x3
#
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      85.6264     2.0033  42.742   <2e-16 ***
# FERTILITY        -5.3993     0.3308 -16.324   <2e-16 ***
# PUBLICEDUCATION  -0.1846     0.2685  -0.688    0.493    
# lnHEALTH         -1.0296     0.9431  -1.092    0.277    
# Multiple R-squared:  0.645,	Adjusted R-squared:  0.6378
# F-statistic: 333.7 on 1 and 179 DF,  p-value: < 2.2e-16


# ----------------------------------------------------------------------------
#    a. Interpret the regression coefficient associated with public education.

B2 = coef(m2)["PUBLICEDUCATION"]               # -0.1846076
round(B2, 3)

# According to this model, with each one-unit increase in public education we 
# expect a drop in life expectancy by 0.185 years.


# ----------------------------------------------------------------------------
#    b. Interpret the regression coefficient associated with health expenditures 
#       without using the logarithmic scale for expenditures.

# To restate the generic regression equation:
#            y = B0 + B1*x1 + B2*x2 + B3*log(x3)      where x3=lnHEALTH
#
# A change in y will be calculated as: 
#      delta.y = y.new - y.old
#              = [B3 * log(x3.new)] - [B3 * log(x3.old)]
#              = B3 * [log(x3.new) - log(x3.old)]
#              = B3 * log(x3.new / x3.old)
# We can consider x3.new/x3.old as the percent change in x.  For example, a 1%
# increase in x would result in that ratio becoming 1.01.
#              = B3 * log(1.01)
#              = -0.01024474

B3 = coef(m2)["lnHEALTH"]       # -1.0296
B3 * log(1.01)                  # -0.01024474

# CONCLUSION:
# For a 1% change in health expenditure, if all other variables remain fixed, 
# the predicted change in life expectancy is reduced by 0.01 years.


# ----------------------------------------------------------------------------
#    c. Based on the model fit, is PUBLICEDUCATION a statistically significant 
#       variable? To respond to this question, use a formal test of hypothesis. 
#       State your null and alternative hypotheses, decision‐making criterion, 
#       and decision‐making rule.

# To determine the significance of PUBLICEDUCATION we begin with the assumption
# that it is not significant, that is, there is no relationship between it and 
# LIFEEXP. Therefore the slope B2 is 0.  This forms our null hypothesis.
#
#          Null Hypothesis H0: B2 = 0
#   Alternative Hypothesis Ha: B2 != 0
#
# We will perform this test with a 5% level of significance. 

alpha = 0.05

t.stat = m2.summ$coefficients[3,3]      # -0.688
p.value = m2.summ$coefficients[3,4]     #  0.493
p.value < alpha                         # FALSE

round(c("alpha"=alpha, "T"=t.stat, "P"=p.value), 3)

# Since p.value is greater than alpha, we fail to reject the null
# hypothesis.
#
# CONCLUSION:
# If we fail to reject the null hypothesis this implies that B2 is zero or, in 
# other words, is statistically insignificant and of no use in the model.  


# ----------------------------------------------------------------------------
# 7. Make an overall comment for the non‐statistician audience

# Multiple linear regression was used to test if number of children, years of 
# education and spending on health care significantly predicted life expectancy.
#
# The fitted regression model was: 
#        y = 85.6264 + (-5.3993)x1 + (-0.1846)x2 + (-1.0296)x3
#    where:
#        y = LIFEEXP
#       x1 = FERTILITY
#       x2 = PUBLICEDUCATION
#       x3 = lnHEALTH
#
# The overall regression was statistically significant 
# (R2 = 0.645, F(3, 148) = 89.64, p = 0).
#
# It was found that number of children significantly predicted life expectancy.
# (β = -5.3993, p < 2e-16).
# The model predicts that for every additional child in a family, the life 
# expectancy drops by about 5 years.
#
# It was found that years of education did not significantly predict life 
# expectancy (β = -0.1846, p = 0.493).
# 
# It was found that amount spent on health care did not significantly predict 
# life expectancy (β = -1.0296, p = 0.277).
