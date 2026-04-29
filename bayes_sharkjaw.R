#
# Wednesday, April 29, 2026
# 

library(ProbBayes)
library(mosaic)
library(ggplot2)
library(rjags)
library(runjags)

# Bayesian Statistics
#
# Review of Simple Linear Regression Basics and R
#
# Let's bring in the Shark Jaws data set. For each of
# of n = 44 sharks, length (in feet) and jawwidth (in inches)
# were measured (carefully!). Bringing data into R ....
setwd("/Users/peterdunson/Desktop/stat_306")
dataone <- read.csv("sharkjaws.csv")

# look at the top of the data set

head(dataone)

# the bottom

tail(dataone)

# make a rough scatterplot

plot(jawwidth~length,
     pch=19,col="navy",
     main="Jaw Width versus Length",
     data=dataone)

# Now the classical least squares method.
# We find the line through the data that
# minimizes the sum of squared deviations
# sum(y - y^)^2

# run a simple linear regression predicting
# jawwidth using length

model1 <- lm(jawwidth~length,data=dataone)
summary(model1)
anova(model1)
abline(model1,lwd=2)

resids <- model1$residuals
fits <- model1$fitted.values

# plot residuals vs fits

plot(resids~fits, pch=19, col="deepskyblue4")

# get confidence intervals for beta coefficients

confint(model1,level=0.95)

# run "diagnostic plots" to assess model assumptions

plot(model1)
hist(resids)

# Anderson-Darling test on residuals

library(nortest)
ad.test(resids)

# p-value not under 0.05 so we conclude
# that our residuals are reasonably normal


# ... and our Theoretical Interlude of the day!
# The Bayesian Viewpoint of Linear Regression

# As we derived in class, the posteriors
# for beta0, beta1, and phi are
# Nasty! Nasty!

# We employ Markov-Chain Monte-Carlo
# (MCMC) techniques such as 
# Gibbs sampling (using JAGS software)

# Model Statement

modelString = "
model{
## sampling
for (i in 1:N) {
  y[i] ~ dnorm(beta0 + beta1*x[i], phi)
}
## priors
beta0 ~ dnorm(mu0, phi0)
beta1 ~ dnorm(mu1, phi1)
phi ~ dgamma(a, b)
sigma <- sqrt(pow(phi, -1))
}
"

# Define the data

y <- dataone$jawwidth
x <- dataone$length
N <- length(y)
the_data <- list("y"=y, "x"=x, "N"=N,
                 "mu0"=0, "phi0"=0.001,
                 "mu1"=0, "phi1"=0.001,
                 "a"=1, "b"=1)

# Give initial values for beta0, beta1, and sigma

InitialValues <- list(
   list(beta0=0, beta1=1, phi=1)
)

# run the run.jags Gibbs sampler to
# generate 10000 iterations of the
# posterior distribution of
# (beta0, beta1, sigma)

posterior <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("beta0","beta1","sigma"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 10000,
                      inits = InitialValues)

# get diagnostic plots for each of our parameters

plot(posterior, vars="beta0")

plot(posterior, vars="beta1")

plot(posterior, vars="sigma")

# take a look at the summary table

print(posterior,digits=4)

# get quantile breakdown of our
# posterior distributions

summary(posterior$mcmc[[1]])

# ... again for comparison, the
# classic least squares 95% 
# confidence intervals

confint(model1,level=0.95)

# this generates the matrix of
# the simulated posterior
# beta0, beta1, sigma values

post <- as.mcmc(posterior)

# take a look

post

# scatterplot of the joint values
# of (beta1,beta0)

plot(beta1~beta0,data=post,pch=20,
     col="darkorchid3")

# make a data frame of the post matrix

post <- data.frame(post)

# correlation of beta1 and beta0 values

cor(post$beta1,post$beta0)

# look at the posterior means of
# our posterior distributions

mean(post$beta0)
mean(post$beta1)
mean(post$sigma)

# plot the original data,
# the least-squares line
# and the Bayes best fit line

plot(jawwidth~length,
     pch=19,col="navy",
     main="Jaw Width versus Length",
     data=dataone)
abline(0.6879,0.9635,col="red")
abline(mean(post$beta0),
       mean(post$beta1),col="blue")


# get posterior means for use in 
# plotting sample of
# posterior lines

post_means <- apply(post,2,mean)
post_means

# plot original data, Bayes best
# fit line and a sample of 10
# posterior Bayes lines

ggplot(dataone, aes(length,jawwidth))+
   geom_point(size=3,col="navy")+
   geom_abline(data=post[1:10, ],
               aes(intercept=beta0,slope=beta1),
               alpha=0.5,col="magenta")+
   geom_abline(intercept=post_means[1],
               slope=post_means[2],
               linewidth=1)+
   ylab("Jaw Width")+xlab("Length")+
   theme_grey(base_size=18,base_family="")

# Simulations of E[Y] = E[jawwidth]
# for 15 foot long sharks

post$EY <- post$beta0+post$beta1*15
head(post)
tail(post)

# let's look at the distribution of the
# posterior mean jaw width for sharks that
# are 15 feet long

sim_EY_graph <- ggplot(post, aes(x=EY,y=after_stat(density))) + 
   geom_histogram(binwidth=0.05,color="navy", fill="cyan")
sim_EY_graph

# let's get 95% credible limits for
# posterior distribution of E[Y]

quantile(post$EY, probs =c(0.025,0.975))

# now using our confidence intervals
# for mean jaw width for the 
# subpopulation of sharks 15 
# feet in length
# do you remember this?

newdata = data.frame(length=15)
predict(model1, newdata, interval="confidence",level=0.95)

# get new observations for jaw width of
# sharks that are 15 feet long 

post$predictedNew <- rnorm(10000,
                           mean=post$EY,sd=post$sigma)
head(post)

# let's look at the distribution of a 
# new observation (posterior predictive
# distribution) of a single shark's jaw width 
# for a shark that is 15 feet long

sim_predNew_graph <- ggplot(post, aes(x=predictedNew,y=after_stat(density))) + 
   geom_histogram(binwidth=0.3,color="navy", fill="cyan")
sim_predNew_graph

# get 95% credible limits for
# posterior predictive distribution of
# jaw width of a new shark that
# is 15 foot long

quantile(post$predictedNew, probs =c(0.025,0.975))

# using classical least squares regression
# what is a 95% Prediction Interval for the
# jaw width of a new shark that is 
# 15 feet long

predict(model1, newdata, interval="prediction",level=0.95)

# now we get replicate samples using the
# posterior distribution of parameters
# (beta0, beta1, sigma)

# take a single observation from
# the posterior distributions

onesample <- post[sample(nrow(post), 1), ]
onesample
onesample$beta0
onesample$beta1
onesample$sigma

# take a replicate sample of 44 sharks
# based on our posteriors

new_sample <- rnorm(44,
                    mean=onesample$beta0+onesample$beta1*dataone$length,
                    sd=onesample$sigma)
new_sample

# let's take yet another replicate sample
# of 44 sharks based on posteriors

onesample2 <- post[sample(nrow(post), 1), ]
onesample2
new_sample2 <- rnorm(44,
                     mean=onesample2$beta0+onesample2$beta1*dataone$length,
                     sd=onesample2$sigma)
new_sample2

# combine these replicated samples
# with original data

dataone <- data.frame(dataone,new_sample,new_sample2)
dataone

# plot replicate samples over the original
# data and cross your fingers!

plot(jawwidth~length,
     pch=20,col="navy",
     main="Jaw Width versus Length",
     data=dataone)
points(new_sample~length,
       pch=17,col="magenta",
       data=dataone)
points(new_sample2~length,
       pch=17,col="green",
       data=dataone)

# It's Miller Time!

# I have enjoyed having you all 
# as students!










# code warehouse below


# Confidence Interval for Mean Jaw Width for 17 foot long sharks
# and Prediction Interval for jaw width a new 17 foot long shark

newdata = data.frame(length=15)
predict(model1, newdata, interval="confidence",level=0.95)
predict(model1, newdata, interval="prediction",level=0.95)

plot(jawwidth~length,col="blue",pch=19,
     main="Jaw Width versus Length",ylim=c(10,22))
abline(model1)
abline(v=17,col="navy")

# Plot the fitted value

abline(h=17.067,col="navy") 

# Plot the Confidence Interval for the Mean 
# jaw width for a 17 ft long shark

abline(h=16.587,col="green")
abline(h=17.546,col="green")

# Plot the Prediction Interval for the  
# jaw width for a "new" 17 ft long shark

abline(h=14.25,col="red")
abline(h=19.88,col="red")

detach(mydata)