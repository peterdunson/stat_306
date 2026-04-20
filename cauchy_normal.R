library(ProbBayes)
library(mosaic)
library(ggplot2)

# -------------------------- April 17-20 2026

# Buffalo Snowfall Example from text

# Prior Information for mean snowfall mu
# We think the quartiles of our prior are 
# 8 and 12

# If we have a normal(mu,sigma) prior, find mu and sigma

normal.select(list(p=0.25,x=8),list(p=0.75,x=12))

mu0 <- 10.00
sig0 <- 2.965204

n <- 20
sig <- 14.4718
ybar <- 26.785

# prior <- c(mu0,sig0)
# data <- c(ybar,sig/sqrt(n))

# normal_update(prior,data)

# The Cauchy(location=10,scale=2) satisfies the same 
# quartiles as above with the normal prior

qcauchy(c(0.25,0.75),location=10,scale=2)
qnorm(c(0.25,0.75),mean=10.0,sd=2.9652)

# Compare graphs of the Normal and Cauchy priors
# having the 8 and 12 quartiles

# define the interval of plotting
x <- seq(0,20,length=500)

plot(x, dnorm(x,mu0,sig0), type='l', col="blue",
     main='Normal vs Cauchy Priors',
     ylab='density',xlab='mean snowfall',
     lwd=2, ylim=c(0,0.2))
lines(x,dcauchy(x,location=10,scale=2), type='l', col="magenta", 
      lwd=2)
legend("topleft", c("Normal","Cauchy","quartile","quartile"), cex=0.8, 
       col=c("blue","magenta","green","green"),
       pch=19,bty="y")
abline(v=8,col="green",lwd=2)
abline(v=12,col="green",lwd=2)

# The Cauchy Model has marked peak information 
# but allows for more flexibility in variation 
# from the peak. In other words, the normal 
# prior concentrates opinion very strongly
# around the point of symmetry 10, but
# the Cauchy allows symmetry around 10 but with 
# thicker tails to allow greater uncertainty in 
# the opinion of the degree
# of concentration about the center 10

# Now from our text example for Buffalo snowfall,
# we know that the likelihood L(mu) is 
# proportional to the Normal(ybar,sigma/sqrt(n))
# = Normal(26.785,14.4718/sqrt(20)) 
# = Normal(26.785,3.236)

# And recall the prior on mu is
# Cauchy(location,scale) = Cauchy(10,2)

# We let the Metropolis algorithm go through
# 10000 iterations.


postproduct <- function(x){
   dnorm(x,26.785,3.236)*dcauchy(x,location=10,scale=2)
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

simdata <- data.frame(metropolis(postproduct,10,15,10000))
simdata

simgraph <- ggplot(simdata, aes(x=S,y=after_stat(density))) + 
   geom_histogram(binwidth=0.5,color="navy", fill="magenta")
simgraph <- simgraph + ggtitle("Posterior Distributions") +
   xlab("Inches of Snowfall in January") + ylab("Density")
simgraph

# Let's make an index plot of the mu
# iterates to look for
# randomness, fill, or autocorrelation

plot(simdata$S,main="Index Plot of mu Values",
     type="l",col="magenta")

# acceptance rate?

tail(simdata$accept_rate)