
library(ProbBayes)
library(mosaic)
library(ggplot2)

# -------------------------- April 22 2026

# This is a coding of Gibbs Sampling Algorithm
# of Beta-Binomial Distribution

# Very Brief Theoretical Interlude on sampling
# from an (Y,p) distribution ...... 

# Our data model is
# Y|p ~ Bin(n,p)

# The prior distribution of p ~ Beta(a,b)

# Once a value of Y=y is observed, the conditional 
# distribution of p|Y ~ Beta(a+y,b+n-y)

# We want to simulate the joint distribution
# of Y and p

my_gibbs_betabin <- function(n,a,b,p=0.5,iter){
   x <- matrix(0,iter,2)  # makes a iter by 2 matrix of 0s
   for(k in 1:iter){
      y <- rbinom(1, size=n, prob=p)
      p <- rbeta(1, a+y, b+n-y)
      x[k, ] <- c(y,p)
   }
   x
}

simjoint <- data.frame(my_gibbs_betabin(20,5,5,p=0.5,100000))
head(simjoint)
simjoint$sim_Y <- simjoint$X1
simjoint$sim_p <- simjoint$X2
head(simjoint)

# I want to graph the marginals of Y and p

# First a graph of the marginal 
# distribution of Y

sim_Y_graph <- ggplot(simjoint, 
                      aes(x=sim_Y,y=after_stat(density))) + 
   geom_histogram(binwidth=1,color="navy", fill="cyan")
sim_Y_graph

# Now a graph of the marginal 
# distribution of p

sim_p_graph <- ggplot(simjoint, 
                      aes(x=sim_p,y=after_stat(density))) + 
   geom_histogram(binwidth=0.01,color="navy", fill="magenta")
sim_p_graph

sim_p_graph <- sim_p_graph + geom_line(aes(x=sim_p,
                                           y=dbeta(sim_p,5,5)))
sim_p_graph


x <- seq(0,1,by=0.01)
plot(x,dbeta(x,5,5),type="l",lwd=2,col="blue",
     main="Beta(5,5) Density",
     xlab="p",ylab="density")
