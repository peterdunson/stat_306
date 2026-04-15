# April 15 2026

# The usual packages

library(ProbBayes)
library(mosaic)
library(ggplot2)

# ------------------------------------------ Apr 15 2026

# Milkweed plant example --- Metropolis Algorithm

# For studying mean milkweed height, recall we used
# (as before) a prior on mu
# of Normal(mu0=120,sig0=23.4) centimeters

# recall, we selected a prior based 
# on opinion quantiles

normal.select(list(p=0.10,x=90),list(p=0.90,x=150))

mu0 <- 120
sig0 <- 23.4

# We assumed the standard deviation 
# for individual plant height Y is sigma=20 centimeters

sig <- 20

# Here is our data based on n=4 height measurements

n <- 4
ybar <- 149

# Now from our in-class worksheet of March 27, 2026
# (equation 3)
# we know that the likelihood L(mu) is 
# proportional to the Normal(ybar,sigma/sqrt(n))
# = Normal(149,20/sqrt(4)) = Normal(149,10)

# And recall the prior on mu is
# Normal(mu0,sig0) = Normal(120,23.4)

# We let the Metropolis algorithm go through
# 10000 iterations.

postproduct <- function(x){
   dnorm(x,149,10)*dnorm(x,120,23.4)
}

metropolis <- function(postproduct,current,C,iter){
   S <- rep(0,iter)
   n_accept <- 0
   for(j in 1:iter){
      candidate <- runif(1,min=current-C,max=current+C)
      ratio <- postproduct(candidate)/postproduct(current)
      accept <- ifelse(runif(1)<ratio,"yes","no")
      current <- ifelse(accept=="yes",candidate,current)
      S[j] <- current
      n_accept <- n_accept + (accept=="yes")
   }
   list(S=S, accept_rate=n_accept/iter)
}

simdata <- data.frame(metropolis(postproduct,130,35.0,10000))
simdata

simgraph <- ggplot(simdata, aes(x=S,y=after_stat(density))) + 
   geom_histogram(binwidth=2,color="navy", fill="magenta")
simgraph

simgraph <- simgraph + geom_line(aes(x=S,
                                     y=dnorm(S,144.52,9.20)))
simgraph

quantile(simdata$S, c(0.05,0.95))

# Recall, here are the theoretical calculations for
# the posterior mean muN and posterior standard
# deviation sigN

# My posterior calculations 
# (I will check with Bayes package)

phi0 <- 1/sig0^2
phi <- 1/sig^2

weight0 <- phi0/(phi0+n*phi)
weightdata <- n*phi/(phi0+n*phi)
c(weight0,weightdata)
weight0+weightdata

# Posterior muN and sigN

muN <- weight0*mu0 + weightdata*ybar
muN

sigN <- 1/sqrt(n*phi+phi0)
sigN

c(muN,sigN)

# Using the normal_update in package ProbBayes

prior <- c(mu0,sig0)
data <- c(ybar,sig/sqrt(n))

normal_update(prior,data)

# Again, let's plot

# and some old code if you want to look further

# define the interval of plotting
x <- seq(50,200,length=500)

plot(x, dnorm(x,mu0,sig0), type='l', col="blue",
     main='Mean Milkweed Height',
     ylab='density',xlab='mean height',
     lwd=2, ylim=c(0,0.06))
lines(x,dnorm(x,muN,sigN), type='l', col="magenta", 
      lwd=2)
legend("topleft", c("prior","posterior"), cex=0.8, 
       col=c("blue","magenta"),
       pch=19,bty="y")


# Let's get a 90% equal tails C.I. for mu
# based on the posterior

qnorm(0.05,muN,sigN)
qnorm(0.95,muN,sigN)
qnorm(c(0.05,0.95),muN,sigN)

# For comparison, how about a traditional
# frequentist z-interval

Lz <- ybar-1.645*sig/sqrt(n)
Uz <- ybar+1.645*sig/sqrt(n)

c(Lz,Uz)

# think about the Bayes and Frequentist intervals

plot(x, dnorm(x,mu0,sig0), type='l', col="blue",
     main='Mean Milkweed Height',
     ylab='density',xlab='mean height',
     lwd=2, ylim=c(0,0.06))
lines(x,dnorm(x,muN,sigN), type='l', col="magenta", 
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

1-pnorm(160,muN,sigN)

# since this probability is small (less than 5%),
# we do not see evidence that supports this claim

