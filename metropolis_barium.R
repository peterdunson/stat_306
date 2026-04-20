#
# April 17, 2026

library(ProbBayes)
library(mosaic)
library(ggplot2)

# For studying Barium 133 radioactive decay,
# recall we used a prior on lambda of 
# Gamma(alpha0=8,beta0=1)

# Our data was as you recall
# y_particles <- c(3,5,6,4,6,4,5,9,4,4,5,4,6,8,4,8,3,3,7,8)
# with n=20 and sum of yi = 106

# Now from our in-class work of April 3-6, 2026,
# we know that the likelihood L(lambda) is 
# proportional to the Gamma((sum of yi)+1,n) density
# thus proportional to Gamma(107,20)

# We let the Metropolis algorithm go through
# 10000 iterations.

postproduct <- function(x){
   dgamma(x,107,20)*dgamma(x,8,1)
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

simdata <- data.frame(metropolis(postproduct,8,2.5,10000))
simdata

simgraph <- ggplot(simdata, aes(x=S,y=after_stat(density))) + 
   geom_histogram(binwidth=0.1,color="navy", fill="magenta")
simgraph

simgraph <- simgraph + geom_line(aes(x=S,
                                     y=dgamma(S,114,21)))
simgraph

# Let's make an index plot of the mu
# iterates to look for
# randomness, fill, or autocorrelation

plot(simdata$S,main="Index Plot of mu Values",
     type="l",col="magenta")

# final acceptance rate?

tail(simdata$accept_rate)


# Recall, here are the theoretical calculations for
# the posterior Gamma density with posterior values
# of alpha_n and beta_n


# My posterior calculations 

# First we
# look at the raw data, which is the number
# of particles emitted from Barium 133 in a
# specified time interval

y_particles <- c(3,5,6,4,6,4,5,9,4,4,5,4,6,8,4,8,3,3,7,8)
df <- data.frame(y_particles)

rawgraph <- ggplot(df,aes(x=y_particles,y=after_stat(density))) + 
   geom_histogram(binwidth=1,color="navy", fill="cyan")
rawgraph

# Suppose our data is from a 
# Poisson(lambda) distribution and
# the parameter lambda has a gamma prior. 
# Let's suppose our prior is 
# Gamma(alpha0,beta0)

alpha0 <- 8
beta0 <- 1

# Let's calculate the posterior 
# alpha_n and beta_n

sum_of_y <- sum(y_particles)
sum_of_y

alpha_n <- alpha0+sum_of_y
beta_n <- beta0+20
c(alpha_n,beta_n)


# define the interval of plotting
x <- seq(0,15,length=500)

# plot the prior and posterior

plot(x, dgamma(x,alpha0,beta0), type='l', col="blue",
     main='Poisson Parameter Lambda',
     ylab='density',xlab='parameter lambda',
     lwd=2, ylim=c(0,0.6))
lines(x,dgamma(x,alpha_n,beta_n), type='l', col="magenta", 
      lwd=2)
legend("topright", c("prior","posterior"), cex=0.8, 
       col=c("blue","magenta"),
       pch=19,bty="y")

qgamma(c(0.05,0.95),alpha_n,beta_n)

# A maximum likelihood interval based on 
# Fisher Information

ybar <- mean(y_particles)
L <- ybar-1.645*sqrt(ybar/20)
U <- ybar+1.645*sqrt(ybar/20)
c(L,U)

plot(x, dgamma(x,alpha0,beta0), type='l', col="blue",
     main='Poisson Parameter Lambda',
     ylab='density',xlab='parameter lambda',
     lwd=2, ylim=c(0,0.6))
lines(x,dgamma(x,alpha_n,beta_n), type='l', col="magenta", 
      lwd=2)
legend("topright", c("prior","posterior","Bayes CI","MLE methods"), cex=0.8, 
       col=c("blue","magenta","cyan","darkgreen"),
       pch=19,bty="y")
abline(v=4.62,col="cyan",lwd=2)
abline(v=6.29,col="cyan",lwd=2)
abline(v=4.45,col="darkgreen",lwd=2)
abline(v=6.15,col="darkgreen",lwd=2)

# Mathematical Interlude gets us the
# predictive distributions of Y if
# alpha_c and beta_c are the "current"
# knowledge/belief of the lambda parameter

#  ....... T H E O R Y (in a GOOD way) .......



# If we are wanting the prior predictive 
# distribution, then our current opinion
# of alpha and beta
# are alpha0 and beta0

alpha_c <- alpha0
beta_c <- beta0

pred_y <- function(y,alpha_c,beta_c){
   gamma(alpha_c+y)/gamma(alpha_c)*
      (beta_c)^alpha_c/(beta_c+1)^(y+alpha_c)*
      (1/factorial(y))
}

# check
c(alpha_c,beta_c)

pred_probs <- pred_y(0:16,alpha_c,beta_c)
barplot(pred_probs,names.arg=0:16,
        main="Predictive Probablities for Prior",
        col="magenta",xlab="Number of Hits",
        ylab="Probability")

rawgraph <- ggplot(df,aes(x=y_particles,y=after_stat(density))) + 
   geom_histogram(binwidth=1,color="navy", fill="cyan")
rawgraph


# I want to do a skeptic check

ytest <- 6
choose(7+ytest,7)/2^(8+ytest)


# If we are wanting the posterior predictive 
# distribution, then our current alpha and
# beta are the posterior alpha_n and beta_n

alpha_c <- alpha_n
beta_c <- beta_n

pred_y <- function(y,alpha_c,beta_c){
   gamma(alpha_c+y)/gamma(alpha_c)*
      (beta_c)^alpha_c/(beta_c+1)^(y+alpha_c)*
      (1/factorial(y))
}

# check
c(alpha_n,beta_n)

pred_probs <- pred_y(0:16,alpha_c,beta_c)
barplot(pred_probs,names.arg=0:16,
        main="Predictive Probablities for Posterior",
        col="magenta",xlab="Number of Hits",
        ylab="Probability")