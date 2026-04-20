library(Stat2Data)
data(Pulse)

n <- sum(!is.na(Pulse$Rest))
xbar <- mean(Pulse$Rest, na.rm = TRUE)
s <- sd(Pulse$Rest, na.rm = TRUE)
se <- s / sqrt(n)
cat("n =", n, "\nxbar =", xbar, "\ns =", s, "\n")

# Histogram
hist(Pulse$Rest,
     main = "Distribution of Resting Pulse Rates",
     xlab = "Resting Pulse Rate (bpm)",
     col = "steelblue",
     breaks = 20)

# Normality check
qqnorm(Pulse$Rest, main = "Normal Q-Q Plot: Resting Pulse Rate")
qqline(Pulse$Rest, col = "red")

# One-sample t-test (H0: mu = 72)
t.test(Pulse$Rest, mu = 72)

# 95% CI manually
ci_lower <- xbar - qt(0.975, df = n - 1) * se
ci_upper <- xbar + qt(0.975, df = n - 1) * se
cat("95% CI: (", round(ci_lower, 3), ",", round(ci_upper, 3), ")\n")





# Confirm posterior calculations
sigma2 <- s^2
mu0 <- 72
tau0_sq <- 100

tau_n_sq <- 1 / (1/tau0_sq + n/sigma2)
mu_n <- tau_n_sq * (mu0/tau0_sq + n*xbar/sigma2)

cat("sigma^2 =", sigma2, "\n")
cat("tau_n^2 =", tau_n_sq, "\n")
cat("mu_n =", mu_n, "\n")

ci_lower_bayes <- mu_n - 1.96*sqrt(tau_n_sq)
ci_upper_bayes <- mu_n + 1.96*sqrt(tau_n_sq)
cat("95% Credible Interval: (", round(ci_lower_bayes, 3), ",", round(ci_upper_bayes, 3), ")\n")










# Prior vs Posterior plot
curve(dnorm(x, mu0, sqrt(tau0_sq)), from = 40, to = 100,
      col = "blue", lwd = 2, lty = 2,
      ylab = "Density", xlab = expression(mu),
      main = "Prior and Posterior Distributions for Mean Pulse Rate",
      ylim = c(0, 0.65))
curve(dnorm(x, mu_n, sqrt(tau_n_sq)), add = TRUE, col = "red", lwd = 2)
abline(v = xbar, col = "darkgreen", lwd = 1.5, lty = 3)
legend("topright", 
       legend = c("Prior: N(72, 100)", "Posterior: N(68.365, 0.425)", "Sample mean"),
       col = c("blue", "red", "darkgreen"), 
       lwd = 2, lty = c(2, 1, 3))

# Prior vs Posterior predictive plot
curve(dnorm(x, mu0, sqrt(sigma2 + tau0_sq)), from = 20, to = 130,
      col = "blue", lwd = 2, lty = 2,
      ylab = "Density", xlab = "Resting Pulse Rate (bpm)",
      main = "Prior and Posterior Predictive Distributions",
      ylim = c(0, 0.045))
curve(dnorm(x, mu_n, sqrt(sigma2 + tau_n_sq)), add = TRUE, col = "red", lwd = 2)
legend("topright",
       legend = c("Prior predictive", "Posterior predictive"),
       col = c("blue", "red"), lwd = 2, lty = c(2, 1))



curve(dnorm(x, mu_n, sqrt(tau_n_sq)), from = 65, to = 73,
      lwd = 2, col = "red",
      xlab = expression(mu), ylab = "Density",
      main = "Posterior Distribution with 95% Credible Interval")

x_shade <- seq(ci_lower_bayes, ci_upper_bayes, length.out = 500)
polygon(c(x_shade, rev(x_shade)),
        c(dnorm(x_shade, mu_n, sqrt(tau_n_sq)), rep(0, 500)),
        col = rgb(1, 0, 0, 0.2), border = NA)

abline(v = mu_n, col = "red", lty = 2, lwd = 1.5)
abline(v = 72, col = "navy", lty = 4, lwd = 1.5)
abline(v = ci_lower_bayes, col = "red", lty = 3, lwd = 1.2)
abline(v = ci_upper_bayes, col = "red", lty = 3, lwd = 1.2)

legend("topright",
       legend = c("Posterior", "95% CI bounds",
                  "Posterior mean (68.365)",
                  "Null value (72)"),
       col = c("red", "red", "red", "navy"),
       lty = c(1, 3, 2, 4), lwd = 1.5)

