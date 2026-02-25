# February 25, 2026

# Today we explore the Beta(a,b) distribution as a 
# prior distribution on a Bernoulli success
# probability p

library(ProbBayes)
require(mosaic)
library(ggplot2)

# define the interval of plotting
x <- seq(0,1,length=200)

# plot the Beta distribution

plot(x, dbeta(x,1,1), type='l', col="black",
     main='Effect of a and b on Beta',
     ylab='density',xlab='probability p',
     lwd=2, ylim=c(0,4))
lines(x,dbeta(x,5,5), type='l', col="red", 
      lwd=2)
lines(x, dbeta(x,2,6), type='l', col="navy", 
      lwd=2)
lines(x,dbeta(x,6,2), type='l', col="green", 
      lwd=2)
lines(x,dbeta(x,0.9,0.9), type='l', col="magenta", 
      lwd=2)
lines(x,dbeta(x,0.5,0.5), type='l', col="pink", 
      lwd=2)
lines(x,dbeta(x,0.1,0.1), type='l', col="yellow", 
      lwd=2)
lines(x,dbeta(x,0.5,2), type='l', col="seagreen", 
      lwd=2)
lines(x,dbeta(x,0.1,3), type='l', col="cyan", 
      lwd=2)

# Increasing a does what?

plot(x, dbeta(x,2,2), type='l', col="black",
     main='Effect of Increasing a in Beta',
     ylab='density',xlab='probability p',
     lwd=2, ylim=c(0,6))
legend("topleft", c("a=2 b=2","a=4 b=2","a=6 b=2","a=8 b=2",
                    "a=10 b=2","a=12 b=2","a=14 b=2"), cex=0.8, 
       col=c("black","red","navy","green",
             "magenta","orange","yellow"),
       pch=19,bty="y")
lines(x,dbeta(x,4,2), type='l', col="red", 
      lwd=2)
lines(x, dbeta(x,6,2), type='l', col="navy", 
      lwd=2)
lines(x,dbeta(x,8,2), type='l', col="green", 
      lwd=2)
lines(x,dbeta(x,10,2), type='l', col="magenta", 
      lwd=2)
lines(x,dbeta(x,12,2), type='l', col="orange", 
      lwd=2)
lines(x,dbeta(x,14,2), type='l', col="yellow", 
      lwd=2)

# Mean and Mode Relationships

plot(x, dbeta(x,2,6), type='l', col="navy",
     main='Mean and Mode Relationships',
     ylab='density',xlab='probability p',
     lwd=2, ylim=c(0,4))
legend("top", c("a=2 b=6","a=12 b=3","mean","mode"),
       cex=0.8, 
       col=c("navy","red",
             "magenta","green"),
       pch=19,bty="y")
abline(v=2/(2+6),col="magenta",lwd=2)
abline(v=(2-1)/(2+6-2),col="green",lwd=2)
lines(x,dbeta(x,12,3), type='l', col="red", 
      lwd=2)
abline(v=12/(12+3),col="magenta",lwd=2)
abline(v=(12-1)/(12+3-2),col="green",lwd=2)

# priors and posteriors
# effect of evidence y = number of
# successes in 20 trials

plot(x, dbeta(x,2,2), type='l', col="black",
     main='Effect of Number of Successes y \n in 20 Trials',
     xlab='probability p',ylab='density',
     lwd=2, ylim=c(0,6))
legend("topleft", c("y=10","y=12","y=14","y=16",
                    "y=18","y=19"), cex=0.8, 
       col=c("red","navy","green",
             "magenta","orange","yellow"),
       pch=19,bty="y")
lines(x,dbeta(x,12,12), type='l', col="red", 
      lwd=2)
lines(x, dbeta(x,14,10), type='l', col="navy", 
      lwd=2)
lines(x,dbeta(x,16,8), type='l', col="green", 
      lwd=2)
lines(x,dbeta(x,18,6), type='l', col="magenta", 
      lwd=2)
lines(x,dbeta(x,20,4), type='l', col="orange", 
      lwd=2)
lines(x,dbeta(x,21,3), type='l', col="yellow", 
      lwd=2)

# effect of sample size on posterior
# suppose sample proportion is held at 0.75

plot(x, dbeta(x,2,2), type='l', col="black",
     main='Effect of Sample Size n \n holding sample proportion at 0.75',
     xlab='probability p',ylab='density',
     lwd=2, ylim=c(0,7))
legend("topleft", c("n=4","n=8","n=16","n=28",
                    "n=40","n=60"), cex=0.8, 
       col=c("red","navy","green",
             "magenta","orange","yellow"),
       pch=19,bty="y")
lines(x,dbeta(x,5,3), type='l', col="red", 
      lwd=2)
lines(x, dbeta(x,8,4), type='l', col="navy", 
      lwd=2)
lines(x,dbeta(x,14,6), type='l', col="green", 
      lwd=2)
lines(x,dbeta(x,23,9), type='l', col="magenta", 
      lwd=2)
lines(x,dbeta(x,32,12), type='l', col="orange", 
      lwd=2)
lines(x,dbeta(x,47,17), type='l', col="yellow", 
      lwd=2)


# Suppose we have a sqample size of n=4 and
# the number of successes y is 3, so our
# sample proportion is 3/4 = 0.75

# Equal tails 95% Bayesian Credible Interval
# 2.5% in each tail with n=4 trials

qbeta(0.025,2+3,2+1)
qbeta(0.975,2+3,2+1)

# another way to request this in R

qbeta(c(0.025,0.975),2+3,2+1)

# check the coverage probability (area)
# is indeed 95%

beta_area(0.2904,0.9010,c(2+3,2+1))

# the ProbBayes package can do it all at once

beta_interval(0.95,c(2+3,2+1))

# Equal tails 95% Bayesian Credible Interval
# 2.5% in each tail with n=28 trials

qbeta(c(0.025,0.975),2+21,2+7)
beta_interval(0.95,c(2+21,2+7))

# Equal tails 95% Bayesian Credible Interval
# 2.5% in each tail with n=60 trials

qbeta(c(0.025,0.975),2+45,2+15)
beta_interval(0.95,c(2+45,2+15))

plot(x, dbeta(x,2,2), type='l', col="black",
     main='Effect of Sample Size n \n holding sample proportion at 0.75',
     xlab='probability p',ylab='density',
     lwd=2, ylim=c(0,7))
legend("topleft", c("n=4","n=8","n=16","n=28",
                    "n=40","n=60"), cex=0.8, 
       col=c("red","navy","green",
             "magenta","orange","yellow"),
       pch=19,bty="y")
lines(x,dbeta(x,5,3), type='l', col="red", 
      lwd=2)
#lines(x, dbeta(x,8,4), type='l', col="navy", 
#     lwd=2)
#lines(x,dbeta(x,14,6), type='l', col="green", 
#     lwd=2)
lines(x,dbeta(x,23,9), type='l', col="magenta", 
      lwd=2)
#lines(x,dbeta(x,32,12), type='l', col="orange", 
#      lwd=2)
lines(x,dbeta(x,47,17), type='l', col="yellow", 
      lwd=2)
abline(v=c(0.2904,0.9010),col="red")
abline(v=c(0.554,0.858),col="magenta")
abline(v=c(0.621,0.834),col="yellow")

# The Analog of Hypothesis Testing --- Finding
# probabilities of researcher's claims
# Again a prior of Beta(2,2)
# Researcher's Claim to Test
# "The proportion p is > 0.5"
# Find probabilistic evidence based on posterior

# n=4 : 3 successes and 1 failure

pbeta(0.5,5,3)
1-pbeta(0.5,5,3)
pvaln4 <- 1-pbinom(2,4,0.5)
pvaln4

prop.test(3,4, p=0.5,
          alternative = "greater",
          conf.level = 0.95, correct = TRUE)
prop.test(3, 4, p=0.5,
          alternative = "two.sided",
          conf.level = 0.95, correct = TRUE)

# n=28 : 21 successes and 7 failures

pbeta(0.5,23,9)
1-pbeta(0.5,23,9)
pvaln28 <- 1-pbinom(20,28,0.5)
pvaln28


prop.test(21, 28, p = 0.5,
          alternative = "greater",
          conf.level = 0.95, correct = TRUE)
prop.test(21, 28, p = 0.5,
          alternative = "two.sided",
          conf.level = 0.95, correct = TRUE)


# n=60 : 45 successes and 15 failures

pbeta(0.5,47,17)
1-pbeta(0.5,47,17)
pvaln60 <- 1-pbinom(44,60,0.5)
pvaln60
prop.test(45, 60, p = 0.5,
          alternative = "greater",
          conf.level = 0.95, correct = TRUE)
prop.test(45, 60, p = 0.5,
          alternative = "two.sided",
          conf.level = 0.95, correct = TRUE)
