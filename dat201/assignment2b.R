# Assignment – 2

# Question # 2
# Generate a sample of size 500 from Gamma distribution, using the shape 
#   parameter 2 and scale parameter 3.
# Write a R function for the Likelihood function of the Gamma Distribution
# Find the maximum likelihood estimate for the shape and scale parameter. You 
#   may use the “optim” or “nlm” function to optimize the likelihood function.
# Comment of the estimates that you obtained.

set.seed(123456)

n = 500       # number of observations in our sample
shape = 2     # shape
scale = 3      # scale
S = rgamma(n, shape=shape, scale=scale)  # our sample data 

mu = shape * scale   # population mean

# let's plot the sample along with theoretical gamma pdf to see what we have
hist(S, prob=TRUE)
abline(v = mean(S), col="red")
xx = seq(min(S), max(S), by=0.01)
lines(xx, dgamma(xx, shape=shape, scale=scale), col="blue")

mu; mean(S)

#############################################################################
#
# Probability Density Function for Gamma Distribution
#   f(x;alpha,beta) = (x^(alpha-1) * exp(-x/beta)) / (gamma(alpha)*beta^k)
#
# Log Likelihood formula
#   A = alpha
#   B = beta
#   l(x;A,B) = -n*A*log(B) - n*log(gamma(A)) - sum(x)/B + (A-1)*sum(log(x))
#
# Inspriation taken from:
#   https://www.utstat.toronto.edu/~brunner/oldclass/appliedf12/lectures/2101f12Likelihood1.pdf
#
#############################################################################

# Gamma Minus Log Likelihood
gmll = function(theta, D) {
  a = theta[1]
  b = theta[2]
  n = length(D)
  sumD = sum(D)
  sumlogD = sum(log(D))
  ret = n*a*log(b) + n*lgamma(a) + sumD/b - (a-1)*sumlogD
  ret
}
E = nlm(gmll, c(shape, scale), D=S)
E$estimate

# Gamma Minus Log Likelihood v2
gmll2 <- function(theta, D) {
  ret = -sum(dgamma(D, shape=theta[1], scale=theta[2], log=TRUE))
  ret
}
E2 = nlm(gmll2, c(shape, scale), D=S)
E2$estimate

results = c(
  "alpha"=shape, 
  "alpha.hat1"=E$estimate[1], 
  "alpha.hat2"=E2$estimate[1], 
  "beta"=scale, 
  "beta.hat1"=E$estimate[2],
  "beta.hat2"=E2$estimate[2]
)
round(results, 3)

# ---------------- FINAL RESULTS ----------------
#
#   alpha alpha.hat1 alpha.hat2       beta  beta.hat1  beta.hat2 
#    2.00       1.97       1.97       3.00       3.00       3.00
#
# Both versions of the gmll function yielded the same results.
#   alpha.hat = 1.970140  (98.5% of value for shape)
#   beta.hat = 3.000276  (100.01% of value for scale)
# So, overall ... pretty good estimates!  




####################################################
#               ALTERNATIVE METHOD                 #
####################################################

# Calculate a set of values using the log likelihood function using a sequence 
# of values for alpha given beta (scale) and S (our original sample data).
# Plot them and determine the maximum value for alpha (alpha.hat)
alpha = seq(0.001, 5, by=0.0001)
beta = scale
L = -n*alpha*log(beta) - n*log(gamma(alpha)) - sum(S)/beta + (alpha-1)*sum(log(S))
plot(alpha, L, type="l")
alpha.hat3 = alpha[which.max(L)]
abline(v=alpha.hat3, col="red")

# Repeat the same approach for beta given alpha (shape) to determine beta.hat.
alpha = shape
beta = seq(0.001, 6, by=0.0001)
L2 = -n*alpha*log(beta) - n*log(gamma(alpha)) - sum(S)/beta + (alpha-1)*sum(log(S))
plot(beta, L2, type="l")
beta.hat3 = beta[which.max(L2)]
abline(v=beta.hat3, col="red")

results2 = c(
  "alpha" = shape,
  "alpha.hat3" = alpha.hat3,
  "beta" = scale,
  "beta.hat3" = beta.hat3
)
round(results2, 3)

# ---------------- FINAL RESULTS ----------------
#
# alpha alpha.hat3       beta  beta.hat3 
# 2.000      1.970      3.000      2.956
#
# alpha.hat3 = 1.9703 (98.5% of the value for shape)
# beta.hat3  = 2.9555 (98.5% of the value for scale)

