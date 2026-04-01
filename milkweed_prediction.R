
# The usual packages

library(ProbBayes)
require(mosaic)
library(ggplot2)

# -------------------------------- Mar 30 2026

# Milkweed plant example --- Prediction

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

# Again, let's plot the prior and posterior

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


# We take a theoretical interlude to derive the
# predictive distribution of a new observation

# We want Posterior Predictive Density for Y
# so mu_c = mun and sig_c = sign 
# (our current knowledge/belief)

mupred <- mun
sdpred <- sqrt(sign^2+sig^2)
c(mupred,sdpred)

# Now we simulate the posterior predictive 
# distribution of a new observation Y~

sims <- 10000

# simulate many draws from the current (posterior)
# distribution of mu, which is N(mun,sign)

pred_mu_sim <- rnorm(sims,mun,sign)

# generate many data values y from the data model,
# one for each value of the simulated mu

pred_y_sim <- rnorm(sims,pred_mu_sim,sig)

simulation<-cbind(pred_mu_sim,pred_y_sim)
simdata<-data.frame(simulation)

# make a histogram of our simulations
# overlaid with what theory predicts

simgraph <- ggplot(simdata, 
                   aes(x=pred_y_sim,y=after_stat(density))) + 
   geom_histogram(binwidth=3,
                  color="navy", fill="magenta")
simgraph <- simgraph + geom_line(aes(x=pred_y_sim,
                                     y=dnorm(pred_y_sim,144.5217,22.0127)),
                                 lwd=1)
simgraph

# Now we will generate posterior predictive sample 
# replicates and data ybar~

sig <- 20
mun <- 144.5217
sign <- 9.195511

sims <- 10000
pred_mu_sim <- rnorm(sims, mun, sign)
sim_ytilde <- function(j){
   rnorm(4,pred_mu_sim[j], sig)
}

sapply(1:sims,sim_ytilde)

ytilde <- t(sapply(1:sims,sim_ytilde))

ytilde

pred_ybar_sim <- apply(ytilde,1,mean)
pred_ybar_sim

df <- data.frame(pred_ybar_sim)

ggplot(df,aes(x=pred_ybar_sim,y=after_stat(density))) + 
   geom_histogram(binwidth=5,color="navy", fill="cyan")

# Can we get a posterior predictive density for a new
# sample mean ybar?

# Yes, our current belief on mu 
# is N(mu_c=mun, sig_c=sign)

# Our data point is now ybar with distribution
# N(mu, sig/sqrt(n))
# So our posterior predictive distribution 
# for ybar is
# Normal with mean mun and std dev

sigybar <- sqrt(sign^2+sig^2/4)
c(mun,sigybar)

simgraph <- ggplot(df,aes(x=pred_ybar_sim,y=after_stat(density))) + 
   geom_histogram(binwidth=3,color="navy", fill="cyan")
simgraph <- simgraph + geom_line(aes(x=pred_ybar_sim,
                                     y=dnorm(pred_ybar_sim,144.5217,13.5852)),
                                 lwd=1)
simgraph


# what about prior predictive samples?

sig <- 20
mu0 <- 120
sig0 <- 23.4

sims <- 10000
pred_mu_sim <- rnorm(sims, mu0, sig0)
sim_ytilde <- function(j){
   rnorm(4,pred_mu_sim[j], sig)
}
ytilde <- t(sapply(1:sims,sim_ytilde))

ytilde

pred_ybar_sim <- apply(ytilde,1,mean)
pred_ybar_sim

df <- data.frame(pred_ybar_sim)

# Can we get a prior predictive density for a new
# sample mean ybar?

# Yes, our current belief is thus  
# mu is N(mu_c=mu0, sig_c=sig0)

# Our data point is now ybar with distribution
# N(mu, sig/sqrt(n))
# So our prior predictive distribution for ybar is
# Normal with mean mu0 and std dev

sigybar <- sqrt(sig0^2+sig^2/4)
c(mu0,sigybar)

simgraph_prior <- ggplot(df,aes(x=pred_ybar_sim,y=after_stat(density))) + 
   geom_histogram(binwidth=3,color="navy", fill="cyan")
simgraph_prior <- simgraph_prior + geom_line(aes(x=pred_ybar_sim,
                                                 y=dnorm(pred_ybar_sim,120.0,25.4472)),
                                             lwd=1)
simgraph_prior

sum(pred_ybar_sim>=149)/sims

# and exact calc

1-pnorm(149,120,25.4472)
