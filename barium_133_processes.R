#
# April 3, 2026

library(ProbBayes)
library(mosaic)
library(ggplot2)

# First we
# look at the raw data, which is the number
# of particles emitted from Barium 133 in a
# specified time interval

y_particles <- c(3,5,6,4,6,4,5,9,4,4,5,4,6,8,4,8,3,3,7,8)
df <- data.frame(y_particles)

ggplot(df,aes(x=y_particles,y=after_stat(density))) + 
   geom_histogram(binwidth=1,color="navy", fill="cyan")


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

# Calculate the posterior mean
posterior_mean <- alpha_n / beta_n

# Print the result
posterior_mean


# we find a 90% equal tails credible 
# interval for lambda

qgamma(c(0.05,0.95),alpha_n,beta_n)

# A maximum likelihood confidence interval based 
# on Fisher Information from Math-Stat class

ybar <- mean(y_particles)
L <- ybar-1.645*sqrt(ybar/20)
U <- ybar+1.645*sqrt(ybar/20)
c(L,U)

# plot the 90% credible and 
# confidence intervals

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
# distribution of y, then our current opinion
# of alpha and beta
# are alpha0 and beta0

alpha_c <- alpha0
beta_c <- beta0

pred_y_prob_calc <- function(y,alpha_c,beta_c){
   gamma(alpha_c+y)/gamma(alpha_c)*
      (beta_c)^alpha_c/(beta_c+1)^(y+alpha_c)*
      (1/factorial(y))
}

# check
c(alpha_c,beta_c)

pred_y_probs <- pred_y_prob_calc(0:24,alpha_c,beta_c)
barplot(pred_y_probs,names.arg=0:24,
        main="Prior Predictive Probablities for Y",
        col="magenta",xlab="Number of Hits",
        ylab="Probability")

ggplot(df,aes(x=y_particles,y=after_stat(density))) + 
   geom_histogram(binwidth=1,color="navy", fill="cyan")


# Let's simulate the prior predictive distribution
# of y and see if there is agreement between theory 
# and simulation

numsims <- 100000
lamsample <- rgamma(numsims,shape=alpha_c,rate=beta_c)
y_pred_sim <- rpois(numsims,lamsample)
proportions <- tally(y_pred_sim,format="proportion")
proportions
pred_y_probs

check_prior_pred <- cbind(pred_y_probs,proportions)
check_prior_pred <- check_prior_pred[1:25,]
check_prior_pred


# If we are wanting the posterior predictive 
# distribution for y, then our current alpha and
# beta are the posterior alpha_n and beta_n

alpha_c <- alpha_n
beta_c <- beta_n

pred_y_prob_calc <- function(y,alpha_c,beta_c){
   gamma(alpha_c+y)/gamma(alpha_c)*
      (beta_c)^alpha_c/(beta_c+1)^(y+alpha_c)*
      (1/factorial(y))
}

# check
c(alpha_c,beta_c)

pred_y_probs <- pred_y_prob_calc(0:24,alpha_c,beta_c)
barplot(pred_y_probs,names.arg=0:24,
        main="Posterior Predictive Probablities for Y",
        col="magenta",xlab="Number of Hits",
        ylab="Probability")

# Let's simulate the posterior predictive distribution
# of y and see if there is agreement between theory 
# and simulation

numsims <- 100000
lamsample <- rgamma(numsims,shape=alpha_c,rate=beta_c)
y_pred_sim <- rpois(numsims,lamsample)
proportions <- tally(y_pred_sim,format="proportion")
proportions
pred_y_probs

check_posterior_pred <- cbind(pred_y_probs,proportions)
check_posterior_pred <- check_posterior_pred[1:25,]
check_posterior_pred