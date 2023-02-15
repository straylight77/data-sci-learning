# Assignment – 2

# Question # 2
# Generate a sample of size 500 from Gamma distribution, using the shape 
#   parameter 2 and scale parameter 3.
# Write a R function for the Likelihood function of the Gamma Distribution
# Find the maximum likelihood estimate for the shape and scale parameter. You 
#   may use the “optim” or “nlm” function to optimize the likelihood function.
# Comment of the estimates that you obtained.

set.seed(123456)

n = 500
alpha = 2
beta = 3 
S = rgamma(n, shape=alpha, scale=beta)

mu = alpha * beta

hist(S, prob=TRUE)
abline(v = mean(S), col="red")
xx = seq(min(S), max(S), by=0.01)
lines(xx, dgamma(xx, shape=alpha, scale=beta), col="blue")

mu; mean(S)

#############################################################################
#
# Probability Density Function for Gamma Distribution
#   f(x;alpha,beta) = (x^(alpha-1) * exp(-x/beta)) / (gamma(alpha)*beta^k)
#
# Log Likelihood formula
#   l(x;A,B) = -n*A*log(B) - n*log(gamma(A)) - sum(x)/B + (A-1)*sum(log(x))
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
fit = nlm(gmll, c(alpha, beta), D=S)


# https://www.utstat.toronto.edu/~brunner/oldclass/appliedf12/lectures/2101f12Likelihood1.pdf
# Gamma Minus Log Likelihood
gmll2 <- function(theta, D) {
  ret = -sum(dgamma(D, shape=theta[1], scale=theta[2], log=TRUE))
  ret
}
fit2 = nlm(gmll2, c(alpha, beta), D=S)



# ---------------- FINAL ESTIMATE ----------------
alpha.hat = fit$estimate[1]     # 1.970140
beta.hat = fit$estimate[2]      # 3.000276


# ---------------- COMMENTS ----------------
# both versions of the gmll function yielded the same results.
# alpha.hat = 1.970140  (98.5% of value for alpha)
# beta.hat = 3.000276  (100.01% of value for beta)
# So ... pretty good estimates!  





# read this first
# https://machinelearningmastery.com/linear-regression-with-maximum-likelihood-estimation/



# https://stats.stackexchange.com/questions/262871/parameter-estimation-of-gamma-distribution-using-r
# https://www.youtube.com/watch?v=ZDg9uWzDB0A
# https://www.youtube.com/watch?v=GyEYKasQTFg

