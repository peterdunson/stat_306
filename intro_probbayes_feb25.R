
# February 23, 2026 --- Discrete Priors and 
# use of the ProbBayes library of packages

# first install our "usuals" mosaic and ggplot2

library(mosaic)
library(ggplot2)

# Let's revisit a problem of last week --- the discrete
# example of our two case studies. The prior distribution
# was uniform on the possible values of p --- 0.4, 0.5, 0.6
#

Prior4 <- 1/3
Prior5 <- 1/3
Prior6 <- 1/3

# If our three coin flips lead to observing 2 heads,
# get the Likelihood for p = 0.4, 0.5, 0.6

L4 <- dbinom(2,3,0.4)
L4

L5 <- dbinom(2,3,0.5)
L5

L6 <- dbinom(2,3,0.6)
L6

# Calculate the joint f(y,p), which is simply the product
# of the Likelihood and Prior

ProductL4p4 <- L4*Prior4
ProductL4p4

ProductL5p5 <- L5*Prior5
ProductL5p5

ProductL6p6 <- L6*Prior6
ProductL6p6

# Calculate the Posterior distribution on p using
# the simplified formula derived today

Posterior4 <- ProductL4p4/(ProductL4p4+ProductL5p5+ProductL6p6)
Posterior5 <- ProductL5p5/(ProductL4p4+ProductL5p5+ProductL6p6)
Posterior6 <- ProductL6p6/(ProductL4p4+ProductL5p5+ProductL6p6)

Posterior4
Posterior5
Posterior6

# Now we are going to have R "do it all". You will need to
# install the author's R library (package) "ProbBayes". This is the
# command that worked with my version of R --- you may have
# to read a bit on the author's website to find commands for
# Macs, etc.

# install.packages("ProbBayes")

# once installed, you can just call it up ....

library(ProbBayes)

# put in the prior distribution

bayes_table <- data.frame(p=c(0.4,0.5,0.6), Prior=c(1/3,1/3,1/3))
bayes_table

# We will suppose we got 2 heads in 3 flips of the coin.
# Now create the likelihood

bayes_table$Likelihood <- dbinom(2,size=3,
                                 prob=bayes_table$p)
bayes_table

# initialize the Product and Posterior columns in our table

bayes_table$Product=NULL
bayes_table$Posterior=NULL

# use the Bayes package to crank out the posterior

bayes_table <- bayesian_crank(bayes_table)
bayes_table

# just a quick graph

barplot(bayes_table$Posterior,
        names.arg=c(0.4,0.5,0.6),
        main="Posterior Probabilities --- 2 of 3",
        col="magenta",
        xlab="Possible p",
        ylab="Probability",ylim=c(0,0.8))


# now look at the same prior below, but suppose
# we got 20 heads out of 30 coin flips

bayes_table_2 <- data.frame(p = c(0.4,0.5,0.6),
                            Prior = c(1/3,1/3,1/3))
bayes_table_2

bayes_table_2$Likelihood <- dbinom(20,size=30,
                                   prob=bayes_table_2$p)
bayes_table_2

bayes_table_2$Product=NULL
bayes_table_2$Posterior=NULL
bayes_table_2 <- bayesian_crank(bayes_table_2)
bayes_table_2

barplot(bayes_table_2$Posterior,
        names.arg=c(0.4,0.5,0.6),
        main="Posterior Probabilities --- 20 of 30",
        col="magenta",
        xlab="Possible p",
        ylab="Probability",ylim=c(0,0.8))


# now look at the prior below, and suppose
# we got 10 heads out of 30 coin flips

bayes_table_3 <- data.frame(p = c(0.3,0.4,0.5,0.6,0.7,0.8),
                            Prior = c(1/8,1/8,1/4,1/4,1/8,1/8))
bayes_table_3

bayes_table_3$Likelihood <- dbinom(10,size=30,
                                   prob=bayes_table_3$p)
bayes_table_3

bayes_table_3$Product=NULL
bayes_table_3$Posterior=NULL
bayes_table_3 <- bayesian_crank(bayes_table_3)
bayes_table_3

barplot(bayes_table_3$Posterior,
        names.arg=c(0.3,0.4,0.5,0.6,0.7,0.8),
        main="Posterior Probabilities --- 10 of 30",
        col="magenta",
        xlab="Possible p",ylab="Probability",
        ylim=c(0,0.7))


# now look at the same prior below, but suppose
# we got 16 heads out of 30 coin flips

bayes_table_4 <- data.frame(p = c(0.3,0.4,0.5,0.6,0.7,0.8),
                            Prior = c(1/8,1/8,1/4,1/4,1/8,1/8))
bayes_table_4

bayes_table_4$Likelihood <- dbinom(16,size=30,
                                   prob=bayes_table_4$p)
bayes_table_4

bayes_table_4$Product=NULL
bayes_table_4$Posterior=NULL
bayes_table_4 <- bayesian_crank(bayes_table_4)
bayes_table_4

barplot(bayes_table_4$Posterior,
        names.arg=c(0.3,0.4,0.5,0.6,0.7,0.8),
        main="Posterior Probabilities --- 16 of 30",
        col="magenta",
        xlab="Possible p",
        ylab="Probability",ylim=c(0,0.7))



# now look at the prior below, which is more
# concentrated around 0.5, and suppose
# we got 10 heads out of 30 coin flips

bayes_table_5 <- data.frame(p = c(0.4,0.45,0.5,0.55,0.6),
                            Prior = c(0.05,0.1,0.7,0.1,0.05))
bayes_table_5

bayes_table_5$Likelihood <- dbinom(10,30,prob=bayes_table_5$p)
bayes_table_5

bayes_table_5$Product=NULL
bayes_table_5$Posterior=NULL
bayes_table_5

bayes_table_5 <- bayesian_crank(bayes_table_5)
bayes_table_5

barplot(bayes_table_5$Posterior,
        names.arg=c(0.4,0.45,0.5,0.55,0.6),
        main="Posterior Probabilities --- 10 of 30",
        col="magenta",
        xlab="Possible p",
        ylab="Probability",ylim=c(0,0.7))


# now look at the same prior below, which is more
# concentrated around 0.5, and suppose
# we got 16 heads out of 30 coin flips

bayes_table_6 <- data.frame(p = c(0.4,0.45,0.5,0.55,0.6),
                            Prior = c(0.05,0.1,0.7,0.1,0.05))
bayes_table_6

bayes_table_6$Likelihood <- dbinom(15,30,prob=bayes_table_6$p)
bayes_table_6

bayes_table_6$Product=NULL
bayes_table_6$Posterior=NULL
bayes_table_6

bayes_table_6 <- bayesian_crank(bayes_table_6)
bayes_table_6

barplot(bayes_table_6$Posterior,
        names.arg=c(0.4,0.45,0.5,0.55,0.6),
        main="Posterior Probabilities --- 16 of 30",
        col="magenta",
        xlab="Possible p",ylab="Probability",ylim=c(0,0.8))