# February 6, 2026

# Today we will see how we can simulate a sample
# from an arbitrary distribution with probability
# density function f(x) and cumulative distribution
# function F(x).

library(mosaic)
library(ggplot2)

# We start by taking a sample of size n
# from the Uniform(0,1) distribution

# How many times do you want to draw a number 
# uniformly at random from the interval (0,1)? 
# We'll call this value n.

n <- 10000

# We'll give each draw a number, from 
# draw 1 to draw n.

drawnumber <- 1:n

# Draw n numbers uniformly at random from 
# the interval (0,1).

U <- runif(n,0,1)

# Transform the U values to get X by sending
# U through the transformation
# X = F^(-1)(U)

X <- ((64*U)^(1/3))^2

# Let us calculate the running average of 
# the values of X

runningavgX <- cumsum(X)/drawnumber

# make a data frame with our work so far

simdata <- data.frame(drawnumber,U,X,runningavgX)

# make a histogram of the U values 

ggplot(simdata, aes(x=U)) + 
   geom_histogram(aes(y=after_stat(density)), 
                  binwidth=0.01,
                  color="black", fill = "steelblue")

# make a histogram of the X values 

ggplot(simdata, aes(x=X)) + 
   geom_histogram(aes(y=after_stat(density)),
                  binwidth=0.2,
                  color="navy", fill="magenta")

# plot running average of X vs drawnumber

ggplot(simdata, aes(y=runningavgX, x=drawnumber)) +
   geom_point(size=3,pch=19,col="darkorchid") +
   geom_hline(yintercept = 9.6, color = "black",
              linetype = "dashed", linewidth = 1)

# what proportion of X values reside
# between 1 and 9

count1to9 <- sum((1<X) & (X<9))
proportion1to9 <- count1to9/n
proportion1to9

# get the sample moments for X

mean(X)
var(X)
sd(X)




# Now applying this simulation to the
# Cable Guy distribution ..... with
# density f(x) = ? on (0,1)

# How many times do you want to draw a number 
# uniformly at random from the interval (0,1)? 
# We'll call this value n.

n <- 100000

# Draw n numbers uniformly at random from 
# the interval (0,1).

U <- runif(n,0,1)

# Transform the U values to get X by sending
# U through the transformation
# X = F^(-1)(U)

X <- ?
   
   cable_data <- data.frame(U,X)

# make a histogram of the U values 

ggplot(cable_data, aes(x=U)) + 
   geom_histogram(aes(y=after_stat(density)), 
                  binwidth=0.01,
                  color="black", fill = "steelblue")

# make a histogram of the X values 

ggplot(cable_data, aes(x=X)) + 
   geom_histogram(aes(y=after_stat(density)),
                  binwidth=0.01,
                  color="navy", fill="magenta")






16^(7/2)

16384 * (2/7)

4681.143 * (3/128)

109.7143 - (9.6^2)

3072/175


sqrt(17.55429)



