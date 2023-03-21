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


setwd("~/data-sci-learning/dat201/")
df = read.csv("UNLifeExpectancy.csv")

# ----------------------------------------------------------------------------
# 1. Examining the relation between y = LIFEEXP and x = FERTILITY.
plot(LIFEEXP ~ FERTILITY, data = df)
cor(df$LIFEEXP, df$FERTILITY, use="complete.obs")         # -0.8067466



# ----------------------------------------------------------------------------
# 2. Fit a linear regression model of LIFEEXP using the explanatory variable 
#    x = FERTILITY.

m = lm(LIFEEXP ~ FERTILITY, data = df)
summary(m)
abline(m, col="red")



# ----------------------------------------------------------------------------
# 3. The United States has a FERTILITY rate of 2.0. Determine the fitted life 
#    expectancy.

f = data.frame(FERTILITY = 2.0)
p = predict(m, newdata=f, interval="confidence")

p          #        fit      lwr      upr
           # 1 73.28309 72.12025 74.44592


# ----------------------------------------------------------------------------
# 4. Now fit a regression model on LIFEEXP using three explanatory variables, 
#    FERTILITY, PUBLICEDUCATION, and lnHEALTH (the natural logarithmic transform
#   of PRIVATEHEALTH).

df$lnHEALTH = log(df$PRIVATEHEALTH)

pairs(~ LIFEEXP + FERTILITY + PUBLICEDUCATION + lnHEALTH,
      lower.panel=NULL, 
      gap=0, 
      data=df)

m2 = lm(LIFEEXP ~ FERTILITY + PUBLICEDUCATION + lnHEALTH, data=df)
summary(m2)



#    a. Interpret the regression coefficient associated with public education.

coef(m2)["PUBLICEDUCATION"]               # -0.1846076

# With each one-unit increase in public education, we expect a drop in life
# expectnacy by 1.85.


#    b. Interpret the regression coefficient associated with health expenditures 
#       without using the logarithmic scale for expenditures.

exp(coef(m2)["lnHEALTH"])                 # 0.357154

# With a one-unit increase in health costs, life expectancy increases by 0.35.
#
# Since we took the log of PRIVATEHEALTH as lnHEALTH, we must exponentiate
# the coefficient to draw any conclusions on health expenditures.

# https://data.library.virginia.edu/interpreting-log-transformations-in-a-linear-model/


#    c. Based on the model fit, is PUBLICEDUCATION a statistically significant 
#       variable? To respond to this question, use a formal test of hypothesis. 
#       State your null and alternative hypotheses, decision‐making criterion, 
#       and decision‐making rule.



# ----------------------------------------------------------------------------
# 7. Make an overall comment for the non‐statistician audience

