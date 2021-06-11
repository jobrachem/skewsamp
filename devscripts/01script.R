nsim <- 200
alpha <- 0.05
power <-  0.9
delta <- 0.2
p_resample <- 1

n <- vector(mode="numeric", nsim)

for (i in seq(nsim)) {
  x <- runif(10)
  y <- runif(10) + delta

  n[i] <- estimate_n_chak(x, y, alpha = alpha, power = power, delta = delta, p_resample = p_resample)
}


par(mfrow=c(1,2))
hist(n)
plot(density(n))
q <- quantile(n, c(0.1, 0.5, 0.9))
abline(v = q, col = "blue")

print(q)
mean(n)
var(n)

saveRDS(n, "devscripts/chakn.Rds")
