# February 16, 2026

# We now look at an example of
# discrete prior and discrete
# data.

# Suppose we have three coins in a
# box, which look identical to the
# eye:

# --- one coin has probability of heads 0.4
# --- one coin has probability of heads 0.5
# --- one coin has probability of heads 0.6

# Suppose a coin is chosen at random from the
# box and flipped three times. The number of
# heads among the three flips is observed.

# Y = the number of heads among the three flips

prior_p <- function(p){
   return(1/3)
}

prior <- c(prior_p(0.4),
           prior_p(0.5),
           prior_p(0.6))
prior

f_Ygivenp <- function(y,p){
   return(dbinom(y,3,p))
}

marginal_Yof0 <- f_Ygivenp(0,0.4)*prior_p(0.4) +
   f_Ygivenp(0,0.5)*prior_p(0.5) +
   f_Ygivenp(0,0.6)*prior_p(0.6)
marginal_Yof0

marginal_Yof1 <- f_Ygivenp(1,0.4)*prior_p(0.4) +
   f_Ygivenp(1,0.5)*prior_p(0.5) +
   f_Ygivenp(1,0.6)*prior_p(0.6)
marginal_Yof1

marginal_Yof2 <- f_Ygivenp(2,0.4)*prior_p(0.4) +
   f_Ygivenp(2,0.5)*prior_p(0.5) +
   f_Ygivenp(2,0.6)*prior_p(0.6)
marginal_Yof2

marginal_Yof3 <- f_Ygivenp(3,0.4)*prior_p(0.4) +
   f_Ygivenp(3,0.5)*prior_p(0.5) +
   f_Ygivenp(3,0.6)*prior_p(0.6)
marginal_Yof3

marginal <- c(marginal_Yof0, 
              marginal_Yof1,
              marginal_Yof2,
              marginal_Yof3)

marginal

sum(marginal)

posterior0 <- c(f_Ygivenp(0,0.4)*prior_p(0.4)/marginal_Yof0,
                f_Ygivenp(0,0.5)*prior_p(0.5)/marginal_Yof0,
                f_Ygivenp(0,0.6)*prior_p(0.6)/marginal_Yof0)

posterior0

sum(posterior0)

barplot(posterior0,names.arg=c(0.4,0.5,0.6),
        main="Posterior for Y=0",
        col="magenta", ylim=c(0,0.6),
        xlab="Probability of Heads",
        ylab="Probability")

posterior1 <- c(f_Ygivenp(1,0.4)*prior_p(0.4)/marginal_Yof1,
                f_Ygivenp(1,0.5)*prior_p(0.5)/marginal_Yof1,
                f_Ygivenp(1,0.6)*prior_p(0.6)/marginal_Yof1)

posterior1

sum(posterior1)

barplot(posterior1,names.arg=c(0.4,0.5,0.6),
        main="Posterior for Y=1",
        col="magenta", ylim=c(0,0.6),
        xlab="Probability of Heads",
        ylab="Probability")

posterior2 <- c(f_Ygivenp(2,0.4)*prior_p(0.4)/marginal_Yof2,
                f_Ygivenp(2,0.5)*prior_p(0.5)/marginal_Yof2,
                f_Ygivenp(2,0.6)*prior_p(0.6)/marginal_Yof2)

posterior2

sum(posterior2)

barplot(posterior2,names.arg=c(0.4,0.5,0.6),
        main="Posterior for Y=2",
        col="magenta", ylim=c(0,0.6),
        xlab="Probability of Heads",
        ylab="Probability")

posterior3 <- c(f_Ygivenp(3,0.4)*prior_p(0.4)/marginal_Yof3,
                f_Ygivenp(3,0.5)*prior_p(0.5)/marginal_Yof3,
                f_Ygivenp(3,0.6)*prior_p(0.6)/marginal_Yof3)

posterior3

sum(posterior3)

barplot(posterior3,names.arg=c(0.4,0.5,0.6),
        main="Posterior for Y=3",
        col="magenta", ylim=c(0,0.6),
        xlab="Probability of Heads",
        ylab="Probability")
