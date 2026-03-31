


# 1. Probability Inducing Curves
g <- function(t, alpha, beta) {
   (3 * beta^alpha * t^(alpha - 1) * exp(-beta * t)) / gamma(alpha)
}

t <- seq(0, 20, length.out = 500)

plot(t, g(t, 2, 0.5), type = "l", col = "blue", lwd = 2,
     xlab = "t (weeks)", ylab = "g(t)",
     main = "Probability-Inducing Curves")
lines(t, g(t, 3, 0.5), col = "red", lwd = 2)
legend("topright", legend = c("Sandy (α=2, β=1/2)", "Paul (α=3, β=1/2)"),
       col = c("blue", "red"), lwd = 2)





# 2. Posterior Tables 

library(ProbBayes)

g <- function(t, alpha, beta) {
   (3 * beta^alpha * t^(alpha - 1) * exp(-beta * t)) / gamma(alpha)
}

p_detect <- function(k, alpha, beta) {
   integrate(function(t) g(t, alpha, beta), lower = k, upper = k + 2)$value
}

intervals <- c(0, 2, 4, 6, 8)
p_A_wife    <- sapply(intervals, p_detect, alpha = 2, beta = 0.5)
p_A_partner <- sapply(intervals, p_detect, alpha = 3, beta = 0.5)

likelihood <- function(p_A, data) {
   prod(ifelse(data == 1, p_A, 1 - p_A))
}

# all binary vectors of length 5 with at least 3 ones
all_vecs <- do.call(expand.grid, rep(list(c(0,1)), 5))
vecs_3plus <- all_vecs[rowSums(all_vecs) >= 3, ]

results <- data.frame()

for (i in 1:nrow(vecs_3plus)) {
   dv <- as.numeric(vecs_3plus[i, ])
   label <- paste0(ifelse(dv == 1, "A", "X"), collapse = "")
   
   L_wife    <- likelihood(p_A_wife,    dv)
   L_partner <- likelihood(p_A_partner, dv)
   
   # uniform prior
   bt_unif <- data.frame(suspect = c("Wife", "Partner"),
                         Prior = c(0.5, 0.5),
                         Likelihood = c(L_wife, L_partner))
   bt_unif <- bayesian_crank(bt_unif)
   
   # 60/40 prior
   bt_6040 <- data.frame(suspect = c("Wife", "Partner"),
                         Prior = c(0.6, 0.4),
                         Likelihood = c(L_wife, L_partner))
   bt_6040 <- bayesian_crank(bt_6040)
   
   results <- rbind(results, data.frame(
      vector       = label,
      post_wife_unif    = round(bt_unif$Posterior[bt_unif$suspect == "Wife"], 4),
      post_partner_unif = round(bt_unif$Posterior[bt_unif$suspect == "Partner"], 4),
      post_wife_6040    = round(bt_6040$Posterior[bt_6040$suspect == "Wife"], 4),
      post_partner_6040 = round(bt_6040$Posterior[bt_6040$suspect == "Partner"], 4)
   ))
}

print(results, row.names = FALSE)



# 3. 



# 4. 

library(ggplot2)
library(gridExtra)

g <- function(t, alpha, beta) {
   (3 * beta^alpha * t^(alpha - 1) * exp(-beta * t)) / gamma(alpha)
}

p_detect <- function(k, alpha, beta) {
   integrate(function(t) g(t, alpha, beta), lower = k, upper = k + 2)$value
}

intervals <- c(0, 2, 4, 6, 8)
p_A_wife    <- sapply(intervals, p_detect, alpha = 2, beta = 0.5)
p_A_partner <- sapply(intervals, p_detect, alpha = 3, beta = 0.5)

likelihood <- function(p_A, data) {
   prod(ifelse(data == 1, p_A, 1 - p_A))
}

posterior_plot <- function(vec_string, prior = c(0.5, 0.5)) {
   dv <- ifelse(strsplit(vec_string, "")[[1]] == "A", 1, 0)
   L_wife    <- likelihood(p_A_wife,    dv)
   L_partner <- likelihood(p_A_partner, dv)
   
   df <- data.frame(
      suspect    = c("Wife", "Partner"),
      Prior      = prior,
      Likelihood = c(L_wife, L_partner)
   )
   df <- bayesian_crank(df)
   
   ggplot(df, aes(x = suspect, y = Posterior, fill = suspect)) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_manual(values = c("Wife" = "steelblue", "Partner" = "coral")) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(title = paste("Evidence:", vec_string),
           x = NULL, y = "Posterior Probability") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5))
}

# contrasting pairs
p1 <- posterior_plot("AAAXX")
p2 <- posterior_plot("XXAAA")
p3 <- posterior_plot("AAXAX")
p4 <- posterior_plot("AXAAA")
p5 <- posterior_plot("AAAAA")
p6 <- posterior_plot("AAAAX")

grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)



# 4. (60/40 prior)

p1 <- posterior_plot("AAAXX", prior = c(0.6, 0.4))
p2 <- posterior_plot("XXAAA", prior = c(0.6, 0.4))
p3 <- posterior_plot("AAXAX", prior = c(0.6, 0.4))
p4 <- posterior_plot("AXAAA", prior = c(0.6, 0.4))
p5 <- posterior_plot("AAAAA", prior = c(0.6, 0.4))
p6 <- posterior_plot("AAAAX", prior = c(0.6, 0.4))

grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)




