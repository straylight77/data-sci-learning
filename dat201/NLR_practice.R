# Non-Linear Regression Practice

#install.packages("minpack.lm")
library(minpack.lm)

f.exponential = function(x, A=0, B=1, C=1, err=NA) {
  if ( is.na(err) ){
    e = 0
  } else {
    e = rnorm(length(x), sd=err)
  }
  A + B * C^x + e
}


# ----------------------------------------------------------------------------
# exponential:  y = A + B * C^x + e
A = 500
B = 0.5
C = 2
err = 50

x = seq(1, 10, by=0.1)
e = rnorm(length(x), sd=err)
y = A + B * C^x + e
plot(x, y)
curve(A+B*C^x, add=TRUE, col="red", lty=2)

m1 = nlsLM(y ~ a+b*c^x, start=c(a=1, b=1, c=1))
summary(m1)
cf = coef(m1)
curve(cf[1]+cf[2]*cf[3]^x, add=TRUE, col="blue")
AIC(m1)


# ----------------------------------------------------------------------------
# logarithmic: y = a + bx^c
A = 500
B = 5
C = 2
err = 50

x = seq(1, 10, by=0.1)
e = rnorm(length(x), sd=err)
y = A+B*x^C + e
plot(x, y)
curve(A+B*x^C, add=TRUE, col="red", lty=2)

m1 = nlsLM(y ~ a+b*x^c, start=c(a=1, b=1, c=1))
summary(m1)
cf = coef(m1)
curve(cf[1]+cf[2]*x^cf[3], add=TRUE, col="blue")
AIC(m1)



# ----------------------------------------------------------------------------
# parabolic:  y = a + b * (x - c)^2
A = 20000
B = -20
C = 30
err = 2000

x = seq(1, 50, by=0.5)
e = rnorm(length(x), sd=err)
y = A+B*(x-C)^2 + e
plot(x, y)
curve(A+B*(x-C)^2, add=TRUE, col="red", lty=2)

m1 = nlsLM(y ~ a+b*(x-c)^2, start=c(a=25000, b=1, c=25))
summary(m1)
cf = coef(m1)
curve(cf[1]+cf[2]*(x-cf[3])^2, add=TRUE, col="blue")
AIC(m1)

# find the value of x that maximizes y?
abline(v=cf[3], col="blue", lty=2)
abline(h=cf[1], col="blue", lty=2)



# ----------------------------------------------------------------------------
# Michaelis Menten:  y = (A*x)/(B+x)
A = 500
B = 20
err = 25

x = seq(1, 50, by=0.5)
e = rnorm(length(x), sd=err)
y = (A*x)/(B+x) + e
plot(x, y)
curve((A*x)/(B+x), add=TRUE, col="red", lty=2)

m1 = nlsLM(y ~ (a*x)/(b+x), start=c(a=0, b=1))
summary(m1)
cf = coef(m1)
curve( (cf[1]*x)/(cf[2]+x), add=TRUE, col="blue")
AIC(m1)

