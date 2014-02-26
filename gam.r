
# SIMULATE FROM GAM MODEL
n=1000
s=0.01
a=0.1
tid=1:n
x=cumsum(rnorm(n))
y=dnorm((tid-mean(tid))/sd(tid))+a*x+s*rnorm(n)
plot(tid,y)
acf(y)

# FIT GAM MODEL
library(gam)
reg1=gam(y~s(tid))
acf(residuals(reg1))
reg2=gam(y~s(tid)+x)
acf(residuals(reg2))
