# Good Morning! on April 8, 2026

library(ProbBayes)
library(mosaic)
library(ggplot2)

# The transition matrix for the Airport model

# B = Baggage claim      (Location 1)
# C = Concourse          (Location 2)
# T = Ticketing/check-in (Location 3)

P <- matrix(c(0, .7, .3,
              .3, .2, .5,
              .8, .2, 0),
            nrow=3,ncol=3,byrow=TRUE)
P

# Let's suppose that the security guard starts 
# his shift at Baggage claim (location 1), so 
# the distribution of location at time t=0 is 
# p(0) = (1,0,0) 

p0 <- c(0,0.5,0.5)
p0

# Then the probability distribution of location at
# time period t=1 is

print(p0 %*% P, digits=5)

# The probability distribution of location at
# time period t=2 is

print(p0 %*% P %*% P, digits=5)

# The probability distribution of 
# location at time period t=5 is

print(p0 %*% P %*% P %*% P %*% P %*% P, digits=5)

# at time period t=6

print(p0 %*% P %*% P %*% P %*% P %*% P %*% P, digits=5)

# Let's calculate any power of the transition matrix
#

m <- 2
Pm <- diag(rep(1,3))
for(j in 1:m){
   Pm <- Pm %*% P
}

print(Pm, digits=5)
print(P%*%P%*%P, digits=5)

# The "expm" package has a built-in power
# of matrices calculator.

library(expm)
print(P%^%2)

p0 <- c(0, 1, 0)
p0%*%(P%^%24)

# A simulation of this Markov Chain

N <- 10000
location <- vector("numeric",N)
location[1] <- 3
for(j in 2:N){
   location[j] <- sample(1:3, size=1, 
                         prob=P[location[j-1], ])
}  

location

counts <- tally(location)
counts

N

proportions <- counts/N
proportions
