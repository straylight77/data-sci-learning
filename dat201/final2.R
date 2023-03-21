# ###########################################################################
# #                                                                         #
# #                    FINAL ASSIGNMENT - QUESTION #1                       #
# #                                                                         #
# ###########################################################################
#

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

setwd("~/data-sci-learning/dat201/")
df.csv = read.csv("TermLife.csv")

df = data.frame(
    GENDER = df.csv$GENDER,  
       AGE = df.csv$AGE, 
    INCOME = df.csv$INCOME, 
  lnINCOME = log(df.csv$INCOME),
      FACE = df.csv$FACE,
    lnFACE = log(df.csv$FACE)
  )
attach(df)
pairs(df, lower.panel=NULL, gap=0)
pairs(lnFACE ~ GENDER + AGE + lnINCOME, data=df, lower.panel=NULL)

plot(lnINCOME, lnFACE)
df.cor = cor(df)

library(corrplot)
corrplot(df.cor, method="ellipse")
corrplot(df.cor, method="number")

