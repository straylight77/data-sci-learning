# ###########################################################################
# #                                                                         #
# #                    FINAL ASSIGNMENT - QUESTION #2                       #
# #                                                                         #
# ###########################################################################
#
# GENDER ‐ Gender of the survey respondent
# AGE Age of the survey respondent
# INCOME Annual income of the family
# FACE Amount that the company will pay in the event of the death of the named insured
# Answer following questions (show your work for every steps):
# 1. How gender, age and income (explanatory variables) are related to FACE 
# (dependent variable) and each other.
# a. Use graphs/figures to show the relationships
# 2. Can gender, age and income help explain the FACE
#    a. Run a multiple linear regression
#    b. Find the estimated regression parameters
#    c. Use Rsq, Rsq_Adj, MSE to show performance of explanatory variables
#    d. Use T‐test to show – how important the explanatory variables are
# 3. Use a statistical software to answer this question
# 4. Write down your interpretation as necessary so that a non‐statistician can
# understand your explanation

# Data can be found here:
# https://instruction.bus.wisc.edu/jfrees/jfreesbooks/regression%20modeling/bookwebdec2010/CSVData/TermLife.csv

setwd("~/data-sci-learning/dat201/")
df.raw = read.csv("TermLife.csv")
str(df.raw)

# heavily clustered in bottom-left
plot(FACE ~ INCOME, data=df.raw, main="FACE vs INCOME")
# still clustered around x-axis
plot(FACE ~ log(INCOME), data=df.raw, main="FACE vs log(INCOME)")
# much more useful for regression
plot(log(FACE) ~ log(INCOME), data=df.raw, main="log(FACE) vs log(INCOME)")
summary(df.raw[, c("GENDER", "AGE", "INCOME", "FACE")])
# number of rows with zero values for face 
nrow(df.raw[ df.raw$FACE == 0, ])
# as a percetage of total rows
nrow(df.raw[ df.raw$FACE == 0, ]) / nrow(df.raw) * 100

# INCOME is heavily skewed to the right (median < mean) which explains the 
# clustering in the 1st plot above.  Similarly with FACE.
# FACE also has many 0 values (225 of them or 45%).  We cannot use a log 
# transformation on these since it will result in -Inf which won't work for a 
# linear regression.  
#
# ASSUMPTION: We can drop the rows where FACE == 0.  The assumption is that 
# these are incidents where customers did not buy insurance.  We will
# determine a regression model only for customers who purchased.  

# Create a final data frame (df) that will be used for the remaining analysis
df = df.raw[ df.raw$FACE>0, c("GENDER", "AGE", "INCOME", "FACE")]
df$lnINCOME = log(df$INCOME)
df$lnFACE = log(df$FACE)
str(df)
n = nrow(df)
summary(df)

attach(df)
hist(GENDER)
hist(AGE)
hist(INCOME)
hist(FACE)
hist(lnINCOME)
hist(lnFACE)


# ----------------------------------------------------------------------------
# 1. How gender, age and income (explanatory variables) are related to FACE 
# (dependent variable) and each other.
# a. Use graphs/figures to show the relationships

pairs(df, lower.panel=NULL, gap=0)

plot(lnFACE ~ lnINCOME, data=df)
df.cor = cor(df)
round(df.cor[6,], 2)

pairs(~ lnFACE + GENDER + AGE + lnINCOME, data=df, lower.panel=NULL, gap=0)

# CONCLUSION:
#  - Age does not appear to be linearly correlated to lnFace (-0.05)
#  - Gender appears to have a weak linear relationship with lnFace (0.26)
#  - lnIncome appears to have a strong linear relationship with lnFace (0.48)
# (Pearson correlation given as reference.)


# ----------------------------------------------------------------------------
# 2. Can gender, age and income help explain the FACE
#    a. Run a multiple linear regression

m = lm(lnFACE ~ GENDER + AGE + lnINCOME, data=df)
m.summ = summary(m)
m.summ


# ----------------------------------------------------------------------------
#    b. Find the estimated regression parameters

cf = coef(m)
B0 = cf[1]
B1 = cf[2]  # GENDER
B2 = cf[3]  # AGE
B3 = cf[4]  # lnINCOME

round(cf, 3)

# RESULTS:
#      (Intercept)      GENDER         AGE    lnINCOME 
#            4.502       0.876      -0.010       0.647 
#
# Equation of the fitted line:
#      lnFACE = (4.50) + (0.876)GENDER - (0.010)AGE + (0.647)lnINCOME


# ----------------------------------------------------------------------------
#    c. Use Rsq, Rsq_Adj, MSE to show performance of explanatory variables

rsq     = m.summ$r.squared             # 0.261
rsq.adj = m.summ$adj.r.squared         # 0.253
mse     = mean(m.summ$residuals^2)     # 2.577
se      = m.summ$sigma                 # 1.617

perf    = c(RSQ=rsq, RSQA=rsq.adj, MSE=mse, SE=se)
round(perf, 3)

# RESULTS:
#
#   RSQ  RSQA   MSE    SE 
# 0.261 0.253 2.577 1.617 
#
# CONCLUSION:
#
# MSE => same units as the variables which are log transformed.   Not a good 
# measure in this case.
# The model explains 25.3% of the variance in lnFACE using the three explanatory 
# variables (using RSQ).
# Any predicted value using this model will have a 95% chance to be +/- 3.23 of 
# the line (using 2 * RSE).


# ----------------------------------------------------------------------------
#    d. Use T‐test to show – how important the explanatory variables are

# For this analysis of significance, we will take the threshold to be 95%. 
# Therefore our value for alpha = 0.05.  

alpha = 0.05

# By inspecting the report given by the function summary(), we can simply 
# compare the p-values for each variable to our threshold (0.05).
#
#                Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  4.501821   0.915055   4.920 1.51e-06 ***
#   GENDER       0.875621   0.293576   2.983  0.00312 ** 
#   AGE         -0.010196   0.007952  -1.282  0.20091    
#   lnINCOME     0.647437   0.077576   8.346 3.65e-15 ***

m.pvals = c(GENDER = m.summ$coefficients[2,4],
        AGE = m.summ$coefficients[3,4],
        lnINCOME = m.summ$coefficients[4,4]
)

round(m.pvals, 3)
# P-values that are closer to 0 are relatively more significant.
#    GENDER      AGE lnINCOME 
#     0.003    0.201    0.000


m.pvals < alpha
# P-values that exceed our threshold (alpha) are not contributing to our model
# in a meaningful way.
#   GENDER      AGE lnINCOME 
#     TRUE    FALSE     TRUE       

# CONCLUSION:
# It was found that Gender (p=0.003) and Income (p=0.00) significantly 
# predicted values for Face.  Furthermore, Income was found to be a stronger 
# predictor than Gender.  Lastly, Age (p=0.201) was found to NOT be a 
# significant predictor.


# ----------------------------------------------------------------------------
# 4. Write down your interpretation as necessary so that a non‐statistician can
# understand your explanation

# Multiple linear regression was used to test if the age, gender and income of 
# an individual that purchased life insurance would significantly predict the 
# amount the company would pay in the event of death.  
#
# The fitted regression model was: 
#     lnFACE = 4.50 + (0.876)GENDER - (0.010)AGE + (0.647)lnINCOME
#
# The overall regression was statistically significant (R2 = 0.261, 
# F(3, 271) = 31.91, p < 2.2e-16).
#
# Looking at each of the explanatory variables, we can find a few more 
# insights.  The following statements are made with a 95% threshold, meaning
# we expect less that 5% probability of seeing these results by chance.
#
# Age was found to not be a good predictor of face (p=0.201).
(exp(B2)-1) * 100
#
# Gender was found to significantly predict face (p=0.003).  A value of 1 for 
# gender, if all other variables remain fixed, leads to an increase in the 
# predicted value for face by 140.0%.  It is unknown if this value for gender 
# implies male or female for this data set. 
(exp(B1)-1) * 100
#
# Income was found to significantly predict face (p=0.000).  For a 1% increase 
# in income, if all other variables remain fixed, the model predicts a 64.7% 
# increase in face (the payout amount).
