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


# REF p 70 

setwd("~/data-sci-learning/dat201/")
df.raw = read.csv("TermLife.csv")
str(df.raw)

plot(FACE ~ INCOME, data=df.raw)           # heavily clustered in bottom-left
plot(FACE ~ log(INCOME), data=df.raw)      # still clustered around x-axis
plot(log(FACE) ~ log(INCOME), data=df.raw) # much more useful for regression
summary(df.raw[, c("GENDER", "AGE", "INCOME", "FACE")])
nrow(df.raw[ df.raw$FACE == 0, ])
nrow(df.raw[ df.raw$FACE == 0, ]) / nrow(df.raw) * 100

# INCOME is heavily skewed to the right (median < mean) which explains the 
# clustering in the 1st plot above.  Similarly with FACE.
# FACE also has many 0 values (225 of them or 45%).  We cannot use a log 
# transformation on these since it will result in -Inf which won't work for a 
# linear regression.  
#
# ASSUMPTION: We can drop the rows in df1 where FACE == 0.  The assumption 
# is that these are incidents where customers did not buy insurance.  We will
# determine a regression model only for customers who purchased.  


# Ceate a final data frame (df) that will be used for the remaining analysis
df = df.raw[ df.raw$FACE>0, c("GENDER", "AGE", "INCOME", "FACE")]
df$lnINCOME = log(df$INCOME)
df$lnFACE = log(df$FACE)
str(df)
n = nrow(df)
summary(df)



# ----------------------------------------------------------------------------
# 1. How gender, age and income (explanatory variables) are related to FACE 
# (dependent variable) and each other.
# a. Use graphs/figures to show the relationships

pairs(df, lower.panel=NULL, gap=0)

plot(lnFACE ~ lnINCOME, data=df)
df.cor = cor(df)
round(df.cor[6,], 2)

pairs(~ lnFACE + GENDER + AGE + lnINCOME, data=df, lower.panel=NULL, gap=0)



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
B1 = cf[2]
B2 = cf[3]
B3 = cf[4]

round(cf, 3)

# RESULTS:
#      (Intercept)      GENDER         AGE    lnINCOME 
#            4.502       0.876      -0.010       0.647 
#
#           y =   B0 +    B1 *     x1 +     B2 *  x2 +    B3 *       x3 
#      lnFACE = 4.50 + 0.876 * GENDER + -0.010 * AGE + 0.647 * lnINCOME


# ----------------------------------------------------------------------------
#    c. Use Rsq, Rsq_Adj, MSE to show performance of explanatory variables

rsq     = m.summ$r.squared             # 0.261
rsq.adj = m.summ$adj.r.squared         # 0.253
mse     = mean(m.summ$residuals^2)     # 2.577
se      = m.summ$sigma                 # 1.617

perf    = c(RSQ=rsq, RSQA=rsq.adj, MSE=mse, SE=se)
round(perf, 3)

# RESULTS:
#     RSQ  ARSQ   MSE 
#   0.261 0.253 2.577
#
# CONCLUSION:
#
# MSE => same units as the variables which are log transformed.   Not a good measure 
# in this case. 


# ----------------------------------------------------------------------------
#    d. Use T‐test to show – how important the explanatory variables are





# ----------------------------------------------------------------------------
# 4. Write down your interpretation as necessary so that a non‐statistician can
# understand your explanation








