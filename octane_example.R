
# March 25, 2026

library(ProbBayes)
library(mosaic)
library(ggplot2)

# Gasoline Octane Example
# Suppose we have three large tanks of gasoline ---
# the octane levels of these tanks have means
# mu = 87, 90, and 93.

# Suppose we take a collection of n observations
# from a tank at random and record the octane level, 
# which within each tank has natural variation,
# spatial variation, and technical variation of
# measurement. Let ybar be the average of these n
# readings and suppose the underlying variation 
# has value sigsq.

ybar <- 90
n <- 4
sigsq <- 9

# Now it's "All about the Bayes"!
# (collective student groans)

# Likelihoods (proportionals)

E87 <- exp(-n/(2*sigsq)*(ybar-87)^2)
E90 <- exp(-n/(2*sigsq)*(ybar-90)^2)
E93 <- exp(-n/(2*sigsq)*(ybar-93)^2)



# Posterior calculations (recall the prior is uniform,
# with 1/3 probability for each tank)

post87 <- E87/(E87+E90+E93)
post90 <- E90/(E87+E90+E93)
post93 <- E93/(E87+E90+E93)

# List the posterior probabilities

c(post87,post90,post93)

# Plot the posterior distribution

barplot(c(post87,post90,post93),
        names.arg=c(87,90,93),
        main="Posterior Probabilities of Octane Level",
        col="navy",
        xlab="Possible p",ylab="Probability",ylim=c(0,1))


# and we can modify the bayesian_crank in ProbBayes

bayes_table <- data.frame(octane = c(87,90,93),
                          Prior = c(1/3,1/3,1/3))
bayes_table$Likelihood <- 
   exp(-n/(2*sigsq)*(ybar-bayes_table$octane)^2)

bayes_table$Product=NULL
bayes_table$Posterior=NULL

bayes_table

bayes_table <- bayesian_crank(bayes_table)
bayes_table