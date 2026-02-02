#
# February 2, 2026

# We can make what is called the 
# "probability histogram" or 
# "graph of the probability 
# distribution function"
# for the Binomial(n,p)

# Explore this code ...

n <- 20
p <- 0.5
k <- dbinom(0:n,size=n,prob=p)
barplot(k,names.arg=0:n,
        main="Binomial(n,p) Distribution",
        col="magenta",
        xlab="Number of Successes",
        ylab="Probability")


graph <- function(n,p){
   px <- dbinom(0:n,size=n,prob=p)
   return(barplot(px,names.arg=0:n,
                  main="Binomial(n,p) Distribution",
                  col="cornflowerblue",
                  xlab="Number of Successes",
                  ylab="Probability"))
}


graph(20,0.1)
graph(20,0.2)
graph(20,0.3)
graph(20,0.4)
graph(20,0.5)
graph(20,0.6)
graph(20,0.7)
graph(20,0.8)

set.seed()
graph(20,0.999)

graph(50,0.5)
graph(100,0.5)


