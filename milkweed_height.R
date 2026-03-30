# March 30, 2026

#  ... the packages ...

library(ProbBayes)
library(mosaic)
library(ggplot2)

# Milkweed plant example.

# First select a prior based on opinion quantiles

normal.select(list(p=0.10,x=90),list(p=0.90,x=150))

# Input these prior values

mu0 <- 120
sig0 <- 23.4

# Check it!

pnorm(c(90,150),mu0,sig0)

# Standard deviation for individual 
# plant height Y

sig <- 20

# define the interval of plotting
x <- seq(50,200,length=500)

# plot the prior on mu

plot(x, dnorm(x,mu0,sig0), type='l', col="blue",
     main='Prior for Mean Milkweed Height',
     ylab='density',xlab='mean height',
     lwd=2, ylim=c(0,0.02))

# Suppose we collect data

n <- 4
ybar <- 149

# My calculations (I will check with 
# Bayes package)

phi0 <- 1/sig0^2
phi <- 1/sig^2

weight0 <- phi0/(phi0+n*phi)
weightdata <- n*phi/(phi0+n*phi)
c(weight0,weightdata)
weight0+weightdata

mun <- weight0*mu0 + weightdata*ybar
mun

sign <- 1/sqrt(n*phi+phi0)
sign

c(mun,sign)

# Using the normal_update in package ProbBayes

prior <- c(mu0,sig0)
data <- c(ybar,sig/sqrt(n))

normal_update(prior,data)

# plot prior and posterior together

plot(x, dnorm(x,mu0,sig0), type='l', col="blue",
     main='Mean Milkweed Height',
     ylab='density',xlab='mean height',
     lwd=2, ylim=c(0,0.06))
lines(x,dnorm(x,mun,sign), type='l', col="magenta", 
      lwd=2)
legend("topleft", c("prior","posterior"), cex=0.8, 
       col=c("blue","magenta"),
       pch=19,bty="y")

# plot prior and posterior (n=4) and 
# posterior (n=8)

plot(x, dnorm(x,mu0,sig0), type='l', col="blue",
     main='Mean Milkweed Height',
     ylab='density',xlab='mean height',
     lwd=2, ylim=c(0,0.06))
lines(x,dnorm(x,144.52,9.20), type='l', col="magenta", 
      lwd=2)
lines(x,dnorm(x,146.57,6.77), type='l', col="darkgreen", 
      lwd=2)

legend("topleft", c("prior","posterior n=4","posterior n=8"), cex=0.8, 
       col=c("blue","magenta","darkgreen"),
       pch=19,bty="y")





