
library(ProbBayes)
library(mosaic)
library(ggplot2)

# -------------------------- April 24 2026

# This is a coding of Gibbs Sampling
# of Normal with unknown mu and sigma

# A Bigger Theoretical Interlude on sampling
# from a (mu,phi | y) distribution ...... 

# Our data model is
# Y|mu,sigma ~ Normal(mu,sigma)

# The prior distribution of mu ~ Normal(mu0,phi0)

# The prior distribution of phi ~ Gamma(a,b)

# We assume a sample of data y1,y2, ..., yn 

# The conditional distribution of mu given
# phi and y1,y2, ..., yn
# is Normal(mun, sigman) 
# (recall from a few weeks ago!)

# The conditional distribution of phi given
# mu and y1,y2, ..., yn
# is Gamma(an, bn)

# We will simulate the joint distribution
# of mu and phi given the data y1,y2, ..., yn

# Milkweed plant example again .....

# First select a prior on mu based on 
# opinion quantiles

normal.select(list(p=0.10,x=90),list(p=0.90,x=150))

# Input these prior values

mu0 <- 120
sig0 <- 23.4

# Check it!

pnorm(c(90,150),mu0,sig0)

# define the interval of plotting
x <- seq(50,200,length=500)

plot(x, dnorm(x,mu0,sig0), type='l', col="blue",
     main='Prior for Mean Milkweed Height',
     ylab='density',xlab='mean height',
     lwd=2, ylim=c(0,0.02))

# Suppose we collect data on heights (n=4)

y <- c(135,142,158,161)
s <- data.frame(y)
mean(s$y) # ybar = 149 as before

# plot the prior on phi (a=1, b=400)

t <- seq(0,0.02,length=500)

plot(t, dgamma(t,1,400), type='l', col="blue",
     main='Prior for phi',
     ylab='density',xlab='phi',
     lwd=2, ylim=c(0,100))

# my Gibbs sampler for the 
# Normal(mu,sigma) problem

my_gibbs_normal <- function(s,phi=0.002,iter){
   ybar <- mean(s$y)
   n <- length(s$y)
   mu0 <- 120
   sig0 <- 23.4
   phi0 <- 1/(sig0*sig0)
   a <- 1
   b <- 400
   x <- matrix(0,iter,2)
   for(k in 1:iter){
      mun <- (phi0*mu0+n*phi*ybar)/(phi0+n*phi)
      sigman <- 1/sqrt(phi0+n*phi)
      mu <- rnorm(1,mean=mun,sd=sigman)
      an <- n/2 + a
      bn <- sum((s$y-mu)^2)/2 +b
      phi <- rgamma(1,shape=an,rate=bn)
      x[k, ] <- c(mu,phi)
   }
   x
}

simjoint <- data.frame(my_gibbs_normal(s,phi=0.002,100000))
head(simjoint)

simjoint$sim_mu <- simjoint$X1
simjoint$sim_phi <- simjoint$X2
head(simjoint)

# I want to graph the distributions of
# mu given y1,y2, ..., yn and
# phi given y1,y2, ..., yn

# First a graph of the distribution of 
# mu given y1,y2, ..., yn

sim_mu_graph <- ggplot(simjoint, 
                       aes(x=sim_mu,y=after_stat(density))) + 
   geom_histogram(binwidth=2,
                  color="navy", fill="cyan")
sim_mu_graph

sim_mu_graph <- sim_mu_graph + geom_line(aes(x=sim_mu,
                                             y=dnorm(sim_mu,144.52,9.20)))

sim_mu_graph <- sim_mu_graph + geom_line(aes(x=sim_mu,
                                             y=dnorm(sim_mu,120,23.4)))
sim_mu_graph


# Now a graph of the distribution of 
# phi given y1,y2, ..., yn

sim_phi_graph <- ggplot(simjoint, 
                        aes(x=sim_phi,y=after_stat(density))) + 
   geom_histogram(binwidth=0.001,
                  color="navy", fill="magenta")

sim_phi_graph <- sim_phi_graph + 
   geom_line(aes(x=sim_phi, y=dgamma(sim_phi,1,400)))

sim_phi_graph

1/var(s$y)

head(simjoint)

simjoint$sim_sigma <- sqrt(1/simjoint$sim_phi)

head(simjoint)

quantile(simjoint$sim_mu,c(0.05,0.95))
qnorm(c(0.05,0.95),144.52,9.20)

quantile(simjoint$sim_sigma,c(0.05,0.95))

# Trace plots for simulated mu and sigma

plot(simjoint$sim_mu,type="l",col="darkgreen",
     main="Trace plot for Simulated mu")

plot(simjoint$sim_sigma,type="l",col="coral",
     main="Trace plot for Simulated sigma")

# Finally, a 2-D plot of the joint values
# of the simulated (mu,sigma)

plot(simjoint$sim_mu,simjoint$sim_sigma,pch=20,col="navy",
     main="Joint Values of (mu,sigma)")