# January 30, 2026

# This program will simulate the rolling of three
# common fair dice.


library(mosaic)
library(ggplot2)

# How many times do you want to roll
# the three dice?

n <- 100000

# Label the rolls 1 to n, so we 
# can identify them.

rollnumber <- 1:n

# roll the red, green, and blue dice

reddie <- sample(1:6,n,replace=TRUE)
greendie <- sample(1:6,n,replace=TRUE)
bluedie <- sample(1:6,n,replace=TRUE)

# calculate the maximum of the three dice

maximum <- pmax(reddie,greendie,bluedie)

# calculate the running average of the
# maximum values

runningavgmax <- cumsum(maximum)/rollnumber

# make a data frame of all the
# variables we've created via simulation

simdata <- data.frame(rollnumber,
                      reddie,greendie,bluedie,
                      maximum,runningavgmax)

# take a look at the first 20 observations

head(simdata,20)

# Pictures are worth a 1000 words, as they say. 
# Check out the following barplot.

ggplot(data=simdata,aes(x=maximum)) +
   geom_bar(color="navy", fill="cyan")

# and the following histogram

ggplot(data=simdata, aes(x=maximum)) + 
   geom_histogram(aes(y=after_stat(count/n)), 
                  binwidth=1.0,
                  color="black", fill = "steelblue")



# let's get counts and proportions of the
# values of the maximum

tally(simdata$maximum,margins=TRUE,format="count")
tally(simdata$maximum,margins=TRUE,format="proportion")

# plot the running average xbar of the maximum
# of the three dice versus roll number

ggplot(simdata, aes(y=runningavgmax, x=rollnumber)) +
   geom_point(size=3,pch=19,col="darkorchid")

tail(simdata)

mean(simdata$maximum)
var(simdata$maximum)
sd(simdata$maximum)


