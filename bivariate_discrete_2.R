
library(ProbBayes)
library(mosaic)
library(ggplot2)

# -------------------------- April 22 2026

# This is simulation via Gibbs Sampling
# Algorithm for a
# Bivariate Discrete Distribution

# Theoretical Interlude on sampling
# from an (X,Y) distribution ...... 


# Here is a target joint distribution

p <- matrix(c(1/100, 2/100, 4/100, 8/100,
              6/100, 6/100, 8/100, 8/100,
              16/100, 8/100, 4/100, 2/100,
              6/100, 9/100, 6/100, 6/100),
            nrow=4,ncol=4,byrow=TRUE)
p

# Checking that I have a probability 
# distribution

one4 <- matrix(c(1,1,1,1),nrow=4,ncol=1,byrow=FALSE)
one4
print(p %*% one4)
sum(p %*% one4)

# Looks good (there is an error in text in 
# Example in 9.5.1, the joint probs don't 
# sum to 1)

# We now take a random walk on the bivariate
# space {1,2,3,4}X{1,2,3,4} according to the
# conditional distributions within matrix p

# check it out

p
p[1,] # grabs first row

p[ ,1] # grabs first column

p[2,] # grabs second row

p[ ,2] # grabs second column

# Here is the Gibbs Sampling 
# Discrete Algorithm

my_gibbs_discrete <- function(p, i=1, iter){
   x <- matrix(0,iter,2)  # makes a iter by 2 matrix of 0s
   nX <- dim(p)[1]  # nX is number of rows
   nY <- dim(p)[2]  # nY is the number of columns
   for(k in 1:iter){
      j <- sample(1:nY, 1, prob=p[i, ]) # probs in row i
      i <- sample(1:nX, 1, prob=p[, j]) # probs in col j
      x[k, ] <- c(i,j)
   }
   x
}

# Run the Gibbs Sampler for the bivariate
# distribution of matrix p

simprobs <- data.frame(my_gibbs_discrete(p,i=1,100000))
head(simprobs)
names(simprobs) <- c("X","Y")
head(simprobs)
table(simprobs)/100000

# compare the simulated distribution 
# with the actual

p

# Just to make sure things are running correctly,
# let's suppose X has three levels 1,2,3 and
# Y has five levels 1,2,3,4,5

p <- matrix(c(5/100, 5/100, 5/100, 5/100, 5/100,
              10/100, 10/100, 5/100, 3/100, 2/100,
              2/100, 8/100, 8/100, 12/100, 15/100),
            nrow=3,ncol=5,byrow=TRUE)
p


one5 <- matrix(c(1,1,1,1,1),nrow=5,ncol=1,byrow=FALSE)
one5
print(p %*% one5)
sum(p %*% one5)

my_gibbs_discrete <- function(p, i=1, iter){
   x <- matrix(0,iter,2)  # makes a iter by 2 matrix of 0
   nX <- dim(p)[1]  # nX is number of rows
   nY <- dim(p)[2]  # nY is the number of columns
   for(k in 1:iter){
      j <- sample(1:nY, 1, prob=p[i, ]) # probs in row i
      i <- sample(1:nX, 1, prob=p[, j]) # probs in col j
      x[k, ] <- c(i,j)
   }
   x
}

# Run the Gibbs Sampler for the bivariate
# distribution of matrix p

simprobs <- data.frame(my_gibbs_discrete(p,i=1,100000))
head(simprobs)
names(simprobs) <- c("X","Y")
head(simprobs)
table(simprobs)/100000

# compare the simulated distribution 
# with the actual

p