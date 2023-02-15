############################################
#
#             Assignment – 2
#
############################################
#
# Question # 1
#
# Generate 10000 sample from Zero Inflated Negative binomial distribution. 
#   (https://rdrr.io/rforge/countreg/man/zinbinom.html). 
#   Each sample should have 30 observations in it.
# Organize the data into a matrix.
# Create the sampling distribution of sample mean.
# Provide necessary figures and comments


#install.packages("countreg", repos="http://R-Forge.R-project.org")

library(countreg)
#set.seed(27012022)
set.seed(123456)


n = 30      # no. of observations per sample
t = 10      # no. of trials within each observation
p = 0.5     # probability of success for each trial
N = 100000  # no. of samples


# mu: the mean of the population
mu = t * p

# sigma: the std dev of the population 
sigma = sqrt(p*(1-p))



S = matrix(rzinbinom(n*N, mu, t, pi=0), nrow=N)

S.means = rowMeans(S)
hist(S.means, prob=TRUE, xlab="Sample Mean")

# xbar: mean of the sample means
xbar = mean(S.means)
abline(v=xbar, col="red")

#plot the theoretical normal distribution
xx = seq(min(S.means), max(S.means), by=0.01)

lines(xx, dnorm(xx, mu, sigma), col="blue") 



