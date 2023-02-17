

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

plot(df$Age, df$Length)
 
  