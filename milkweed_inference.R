
# The usual packages

library(ProbBayes)
require(mosaic)
library(ggplot2)

# --------------------------------- Mar 30 2026

# Milkweed plant example --- Inference

# Suppose we decide (as before) on a prior on mu
# as Normal(mu0=120,sig0=23.4) centimeters

mu0 <- 120
sig0 <- 23.4

# For now, we assume the standard deviation 
# for individual plant height Y

sig <- 20

# Here are our data based on n height measurements

n <- 4
ybar <- 149

# My posterior calculations 
# (I will check with Bayes package)

phi0 <- 1/sig0^2
phi <- 1/sig^2

weight0 <- phi0/(phi0+n*phi)
weightdata <- n*phi/(phi0+n*phi)
c(weight0,weightdata)
weight0+weightdata

# Posterior mun and sign

mun <- weight0*mu0 + weightdata*ybar
mun

sign <- 1/sqrt(n*phi+phi0)
sign

c(mun,sign)

# Using the normal_update in package ProbBayes

prior <- c(mu0,sig0)
data <- c(ybar,sig/sqrt(n))

normal_update(prior,data)

# Again, let's plot

# define the interval of plotting
x <- seq(50,200,length=500)

plot(x, dnorm(x,mu0,sig0), type='l', col="blue",
     main='Mean Milkweed Height',
     ylab='density',xlab='mean height',
     lwd=2, ylim=c(0,0.06))
lines(x,dnorm(x,mun,sign), type='l', col="magenta", 
      lwd=2)
legend("topleft", c("prior","posterior"), cex=0.8, 
       col=c("blue","magenta"),
       pch=19,bty="y")


# Let's get a 90% equal tails C.I. for mu
# based on the posterior

qnorm(0.05,mun,sign)
qnorm(0.95,mun,sign)
qnorm(c(0.05,0.95),mun,sign)

# For comparison, how about a traditional
# frequentist 90% z-interval

Lz <- ybar-1.645*sig/sqrt(n)
Uz <- ybar+1.645*sig/sqrt(n)

c(Lz,Uz)

# think about the Bayes and Frequentist intervals

plot(x, dnorm(x,mu0,sig0), type='l', col="blue",
     main='Mean Milkweed Height',
     ylab='density',xlab='mean height',
     lwd=2, ylim=c(0,0.06))
lines(x,dnorm(x,mun,sign), type='l', col="magenta", 
      lwd=2)
legend("topleft", 
       c("prior","posterior","Bayes CI","Z CI"), cex=0.8, 
       col=c("blue","magenta","cyan","yellow"),
       pch=19,bty="y")
abline(v=129.4,col="cyan",lwd=2)
abline(v=159.65,col="cyan",lwd=2)
abline(v=132.55,col="yellow",lwd=2)
abline(v=165.45,col="yellow",lwd=2)


# what about the claim that mu is 
# greater than 160?
# find this posterior probability

1-pnorm(160,mun,sign)

# since this probability is small (less than 5%),
# we do not see evidence that supports this claim