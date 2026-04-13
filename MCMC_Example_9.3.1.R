
# Good Morning! for April 13-15, 2026

library(ProbBayes)
library(mosaic)
library(ggplot2)

# Let's work through Example 9.3.1 in the text

# The following will produce the weights as
# a function of x. I'm using the name pd because it 
# it was chosen by the text

pd <- function(x){
   values <- c(5, 10, 4, 4, 20, 20, 12, 5)
   ifelse(x %in% 1:length(values),values[x],0)
}

# check 'em

pd(1)
pd(2)
pd(0)


# The following function will run this random
# walk for num_steps and start position
# of your choosing

random_walk <- function(pd, start, num_steps){
   y <- rep(0,num_steps)
   current <- start
   for(j in 1:num_steps){
      candidate <- current + sample(c(-1,1),1)
      R <- pd(candidate)/pd(current)
      if (runif(1) < R) current <- candidate
      y[j] <-current
   }
   return(y)
}

# Let's use num_steps = 5, and starting 
# at position 6, which is analogous
# to your work in Exercise 1

random_walk(pd,6,5)

# Does this random_walk have a stationary
# distribution? Explore this.

yvals <- random_walk(pd,4,500000)

counts <- tally(yvals)
counts

proports <- counts/length(yvals)
proports


# Now let's put into R the Transition Matrix Q
# that we have collectively derived for the 
# 1:8 random walk

Q <- matrix(c(1/2, 1/2, 0, 0, 0, 0, 0, 0,
              5/20, 11/20, 4/20, 0, 0, 0, 0, 0,
              0, 1/2, 0, 1/2, 0, 0, 0, 0,
              0, 0, 1/2, 0, 1/2, 0, 0, 0,
              0, 0, 0, 1/10, 4/10, 5/10, 0, 0,
              0, 0, 0, 0, 5/10, 2/10, 3/10, 0,
              0, 0, 0, 0, 0, 12/24, 7/24, 5/24,
              0, 0, 0, 0, 0, 0, 1/2, 1/2),
            nrow=8,ncol=8,byrow=TRUE)

Q

# Let's find the stationary vector using the
# theory of Markov chain transition matrices

Qm <- diag(rep(1,8))
Qm
for(j in 1:500){
   Qm <- Qm %*% Q
}

print(Qm,digits=5)


# Finally, and note that we have intentionally waited to 
# perform this calculation --- find the probability vector
# determined by the weights in the vector w

weights <- c(5,10,4,4,20,20,12,5)
sum(weights)

problist <- weights/sum(weights)
problist