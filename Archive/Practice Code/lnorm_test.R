
# Par
par(mfrow=c(2,1))

# Normal distribution approach
mu <- 0.6
cv <- 0.4
sd <- mu * cv
X <- rnorm(n=1000, mean=mu, sd=sd)
hist(X, breaks=20, xlim=c(-0.5, 2.5))

# Log-normal distribution approach
# Equation for the CV of a log distribution:
# https://en.wikipedia.org/wiki/Coefficient_of_variation
# CV = sqrt(exp(sd_log^2) - 1)
# CV^2 = exp(sd_log^2) - 1
# CV^2 + 1 = exp(sd_log^2)
# log(CV^2 + 1) = sd_log^2
# sqrt(log(CV^2 + 1)) = sd_log
mu_log <- log(mu)
sd_log <- sqrt(log(cv^2+1))
Y <- rlnorm(n=1000, meanlog = mu_log, sdlog=sd_log)
hist(Y, breaks=20, xlim=c(-0.5, 2.5))


