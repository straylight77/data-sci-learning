# Non-Linear Regression Practice

#install.packages("minpack.lm")
library(minpack.lm)


# ----------------------------------------------------------------------------
# exponential:  y = A + B * C^x + e
A = 500
B = 1
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
err = 80

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
err = 50

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



# ----------------------------------------------------------------------------
A = 5000
B = -3
C = 20
err = 200

x = seq(1, 25, by=0.1)
e = rnorm(length(x), sd=err)
#y = A+B*x^C + e
y = A+B*(x-C)^2 + e
plot(x, y)


# try parabolic first
m.para = nlsLM(y ~ a+b*(x-c)^2, start=c(a=4000, b=1, c=100))
m.para.summ = summary(m.para)
cf = coef(m.para)
curve( cf[1]+cf[2]*(x-cf[3])^2, add=TRUE, col="blue" )
r.para = c(err = m.para.summ$sigma, aic=AIC(m.para))


# MM next
m.mm = nlsLM(y ~ (a*x)/(b+x), start=c(a=5000, b=5))
m.mm.summ = summary(m.mm)
cf = coef(m.mm)
curve( (cf[1]*x)/(cf[2]+x), add=TRUE, col="purple")
r.mm = c(err = m.mm.summ$sigma, aic=AIC(m.mm))


# logarithmic
m.log = nlsLM(y ~ a+b*x^c, start=c(a=5000, b=-2000, c=-1))
m.log.summ = summary(m.log)
cf = coef(m.log)
curve(cf[1]+cf[2]*x^cf[3], add=TRUE, col="orange")
r.log = c(err = m.log.summ$sigma, aic=AIC(m.log))

results = data.frame(PARA=r.para, MM=r.mm, LOG=r.log)
round(results, 2)

curve(A+B*(x-C)^2, add=TRUE, col="red", lty=2, lwd=2)
