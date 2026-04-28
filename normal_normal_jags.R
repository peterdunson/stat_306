library(ProbBayes)
library(mosaic)
library(ggplot2)

# -------------------------- April 27 2026

# This is a coding of Gibbs Sampling
# of Normal with unknown mu and sigma
# AND we will employ the JAGS software
# which needs to be loaded.
# This might take a little time snooping 
# around for the right package to fit your
# system

library(rjags)
library(runjags)

# We will look at the Milkweed Example
# one last time. My intent has been to gradually
# evolve our analysis from our hand-cranked
# theory TO full-tilt Gibbs Sampling
# Posterior Simulation ... We have arrived at
# the final destination --- JAGS software (Just
# Another Gibbs Sampler)

# Our data model is Y = Milkweed Height
# Y|mu,sigma ~ Normal(mu,sigma)

# The prior distribution of mu ~ Normal(mu0,phi0)

# The prior distribution of phi ~ Gamma(a,b)

# We assume a sample of data y1,y2, ..., yn 

# The conditional distribution of mu given
# phi and y1,y2, ..., yn
# is Normal(mun, sigman) 
# (recall from a few weeks ago! Sec 8.5)

# The conditional distribution of phi given
# mu and y1,y2, ..., yn
# is Gamma(an, bn), where
# an and bn are given in our
# Theoretical Interlude (April 24 2026 class)

# We will simulate the joint distribution
# of mu and phi given the data y1,y2, ..., yn
# FIRST using our own coded Gibbs Sampler
# from April 24 2026

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

# the following function is the zig-zag Gibbs
# Sampler based on the conditional densities
# mu | phi, y
# phi | mu, y

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

# Run my home-grown Gibbs Sampler

simjoint <- data.frame(my_gibbs_normal(s,phi=0.002,10000))
head(simjoint)

simjoint$sim_mu <- simjoint$X1
simjoint$sim_phi <- simjoint$X2
head(simjoint)

# I want to graph the distributions of
# mu given y1,y2, ..., yn and
# phi given y1,y2, ..., yn

# First a graph of the distribution of 
# mu given y1,y2, ..., yn

sim_mu_graph <- ggplot(simjoint, aes(x=sim_mu,y=after_stat(density))) + 
   geom_histogram(binwidth=2,color="navy", fill="cyan")
sim_mu_graph

sim_mu_graph <- sim_mu_graph + geom_line(aes(x=sim_mu,
                                             y=dnorm(sim_mu,144.52,9.20)))

sim_mu_graph <- sim_mu_graph + geom_line(aes(x=sim_mu,
                                             y=dnorm(sim_mu,120,23.4)))
sim_mu_graph

# Now a graph of the distribution of 
# phi given y1,y2, ..., yn

sim_phi_graph <- ggplot(simjoint, aes(x=sim_phi,y=after_stat(density))) + 
   geom_histogram(binwidth=0.001,color="navy", fill="magenta")
sim_phi_graph

head(simjoint)

simjoint$sim_sigma <- sqrt(1/simjoint$sim_phi)

head(simjoint)

# Now a graph of the distribution of
# sigma given y1, y2, ... , yn

sim_sigma_graph <- ggplot(simjoint, aes(x=sim_sigma,y=after_stat(density))) + 
   geom_histogram(binwidth=2,color="navy", fill="magenta")
sim_sigma_graph

# The quantile breakdowns

quantile(simjoint$sim_mu,c(0.025,0.975))
qnorm(c(0.025,0.975),144.52,9.20)

quantile(simjoint$sim_phi,c(0.025,0.975))

quantile(simjoint$sim_sigma,c(0.025,0.975))

# Trace plots for simulated mu and sigma

plot(simjoint$sim_mu,type="l",col="darkgreen",
     main="Trace plot for Simulated mu")

plot(simjoint$sim_sigma,type="l",col="coral",
     main="Trace plot for Simulated sigma")

# Finally, a 2-D plot of the joint values
# of the simulated (mu,sigma)

plot(simjoint$sim_mu,simjoint$sim_sigma,pch=20,col="navy",
     main="Joint Values of (mu,sigma)")

# ... and now we will employ the JAGS software
# JAGS = Just Another Gibbs Sampler (Sec 9.7)

library(rjags)
library(runjags)

modelString = "
model{
## sampling
for (i in 1:N) {
  y[i] ~ dnorm(mu,phi)
}
## priors
mu ~ dnorm(mu0, phi0)
phi ~ dgamma(a, b)
sigma <- sqrt(pow(phi, -1))
}
"

# Define the data

y <- c(135,142,158,161)
N <- length(y)
the_data <- list("y"=y, "N"=N,
                 "mu0"=120, "phi0"=1/(23.4)^2,
                 "a"=1, "b"=400)

posterior <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("mu","sigma"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 5000)


# The posterior results for mu

posterior
plot(posterior, vars = "mu")

# again my coding/plot of Gibbs Sampling for mu

sim_mu_graph <- ggplot(simjoint, aes(x=sim_mu,y=after_stat(density))) + 
   geom_histogram(binwidth=2,color="navy", fill="cyan")
sim_mu_graph

# The posterior results for sigma

plot(posterior,vars="sigma")

# again my coding/plot of Gibbs Sampling for sigma
sim_sigma_graph <- ggplot(simjoint, aes(x=sim_sigma,y=after_stat(density))) + 
   geom_histogram(binwidth=2,color="navy", fill="magenta")
sim_sigma_graph

# Finally, summaries of JAGS

posterior

summary(posterior$mcmc[[1]])

