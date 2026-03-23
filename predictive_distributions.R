
# Predictive Distributions --- March 20, 2026

library(ProbBayes)
library(mosaic)
library(ggplot2)

# Restaurant Example from Text

# The prior Beta(a,b) is chosen so that p=0.55 is
# the median of the prior and p=0.8
# is the prior 90th quantile (see page 236)

beta.select(list(x=0.55, p=0.5),
            list(x=0.80, p=0.90))

# You should get that the prescribed prior Beta(a,b)
# distribution has a=3.06, b=2.56


# Theoretical Interlude
#
# Here we derive the Prior Predictive Distribution
# Which is the distribution of the number of 
# successes Y before we take data 
# (so this distribution of Y only reflects 
# the choice of prior on p)

# Now let us apply the theory and calculate and plot
# the Prior Predictive Distribution

a <- 2.07
b <- 7.32
n <- 20

# Give the y range

y <- (0:n)

# Calculate the Prior Predictive Distribution
# print the values and plot them too

priorPD <- choose(n,y)*gamma(a+b)/(gamma(a)*gamma(b))*
   (gamma(a+y)*gamma(b+n-y)/gamma(a+b+n))

priorPDvalues <- data.frame(y,priorPD)

priorPDvalues

barplot(priorPDvalues$priorPD,names.arg=0:20,
        main="Prior Predictive Distribution",
        col="magenta",ylim=c(0,0.12),
        xlab="Number of Successes",ylab="Probability")

# Now suppose an employee of the restaurant
# believes in a prior that has median at p=0.2
# and 90th percentile at p=0.4. This leads to
# a Beta(a,b) prior given as ...

beta.select(list(x=0.2, p=0.50),
            list(x=0.4, p=0.90))

beta_area(0.2,0.4,c(2.07,7.32))

# You should get that the prescribed prior Beta(a,b)
# distribution has a=2.07, b=7.32 ....
# Let's put these in and run it again above



# Calculate the Posterior Predictive Distribution
# print the values and plot them too

yobs <- 12
m <- 20

ytilda <- (0:m)

postPD <- choose(m,ytilda)*gamma(a+b+n)/(gamma(a+yobs)*gamma(b+n-yobs))*
   gamma(a+yobs+ytilda)*gamma(b+n-yobs+m-ytilda)/gamma(a+b+n+m)

postPDvalues <- data.frame(ytilda,postPD)

postPDvalues

barplot(postPDvalues$postPD,names.arg=0:20,
        main="Posterior Predictive Distribution",
        col="magenta",ylim=c(0,0.16),
        xlab="Number of Successes",
        ylab="Probability")

# Now letting the ProbBayes package do it for us

prob <- pbetap(c(a+yobs,b+n-yobs),20,0:20)
prob
prob_plot(data.frame(Y=0:20, Probability=prob),
          Color="navy", Size=4)+
   theme(text=element_text(size=18))