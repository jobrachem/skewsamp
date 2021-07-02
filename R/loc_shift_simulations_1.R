
# Sample size estimator matrix
# 200 Simulations for each case (8 cases in total)
res_mat <- matrix(0, nrow = 200, ncol = 8)
res <- matrix(0, nrow = 5, ncol = 8)
rownames(res) <- c("Mean", "Variance", "q_10", "q_50", "q_90")



# Case 1 (Normal distribution, m = 10, delta = 0.5)

for(i in 1:200){
  res_mat[i,1] <- n_locshift(s1 = rnorm(10), s2 = rnorm(10), alpha = 0.05, power = 0.9, delta = 0.5)
}

hist(res_mat[,1], main = "Normal distribution, m = 10, delta = 0.5", xlab = "Sample Size")
res[1,1] <- mean(res_mat[,1])
res[2,1] <- var(res_mat[,1])
res[3,1] <- quantile(res_mat[,1],0.1)
res[4,1] <- quantile(res_mat[,1],0.5)
res[5,1] <- quantile(res_mat[,1],0.9)


# Case 2 (Normal distribution, m = 20, delta = 0.5)

for(i in 1:200){
  res_mat[i,2] <- n_locshift(s1 = rnorm(20), s2 = rnorm(20), alpha = 0.05, power = 0.9, delta = 0.5)
}

hist(res_mat[,2], main = "Normal distribution, m = 20, delta = 0.5", xlab = "Sample Size")
res[1,2] <- mean(res_mat[,2])
res[2,2] <- var(res_mat[,2])
res[3,2] <- quantile(res_mat[,2],0.1)
res[4,2] <- quantile(res_mat[,2],0.5)
res[5,2] <- quantile(res_mat[,2],0.9)


# Case 3 (Uniform distribution, m = 10, delta = 0.2)

for(i in 1:200){
  res_mat[i,3] <- n_locshift(s1 = runif(10), s2 = runif(10), alpha = 0.05, power = 0.9, delta = 0.2)
}

hist(res_mat[,3], main = "Uniform distribution, m = 10, delta = 0.2", xlab = "Sample Size")
res[1,3] <- mean(res_mat[,3])
res[2,3] <- var(res_mat[,3])
res[3,3] <- quantile(res_mat[,3],0.1)
res[4,3] <- quantile(res_mat[,3],0.5)
res[5,3] <- quantile(res_mat[,3],0.9)


# Case 4 (Uniform distribution, m = 20, delta = 0.2)

for(i in 1:200){
  res_mat[i,4] <- n_locshift(s1 = runif(20), s2 = runif(20), alpha = 0.05, power = 0.9, delta = 0.2)
}

hist(res_mat[,4], main = "Uniform distribution, m = 20, delta = 0.2", xlab = "Sample Size")
res[1,4] <- mean(res_mat[,4])
res[2,4] <- var(res_mat[,4])
res[3,4] <- quantile(res_mat[,4],0.1)
res[4,4] <- quantile(res_mat[,4],0.5)
res[5,4] <- quantile(res_mat[,4],0.9)


# Case 5 (Exponential distribution, m = 10, delta = 0.35)

for(i in 1:200){
  res_mat[i,5] <- n_locshift(s1 = rexp(10), s2 = rexp(10), alpha = 0.05, power = 0.9, delta = 0.35)
}

hist(res_mat[,5], main = "Exponential distribution, m = 10, delta = 0.35", xlab = "Sample Size")
res[1,5] <- mean(res_mat[,5])
res[2,5] <- var(res_mat[,5])
res[3,5] <- quantile(res_mat[,5],0.1)
res[4,5] <- quantile(res_mat[,5],0.5)
res[5,5] <- quantile(res_mat[,5],0.9)


# Case 6 (Exponential distribution, m = 20, delta = 0.35)

for(i in 1:200){
  res_mat[i,6] <- n_locshift(s1 = rexp(20), s2 = rexp(20), alpha = 0.05, power = 0.9, delta = 0.35)
}

hist(res_mat[,6], main = "Exponential distribution, m = 20, delta = 0.35", xlab = "Sample Size")
res[1,6] <- mean(res_mat[,6])
res[2,6] <- var(res_mat[,6])
res[3,6] <- quantile(res_mat[,6],0.1)
res[4,6] <- quantile(res_mat[,6],0.5)
res[5,6] <- quantile(res_mat[,6],0.9)


# Case 7 (Logistic distribution, m = 10, delta = 0.8)

for(i in 1:200){
  res_mat[i,7] <- n_locshift(s1 = rlogis(10), s2 = rlogis(10), alpha = 0.05, power = 0.9, delta = 0.8)
}

hist(res_mat[,7], main = "Logistic distribution, m = 10, delta = 0.8", xlab = "Sample Size")
res[1,7] <- mean(res_mat[,7])
res[2,7] <- var(res_mat[,7])
res[3,7] <- quantile(res_mat[,7],0.1)
res[4,7] <- quantile(res_mat[,7],0.5)
res[5,7] <- quantile(res_mat[,7],0.9)

# Case 8 (Logistic distribution, m = 20, delta = 0.8)

for(i in 1:200){
  res_mat[i,8] <- n_locshift(s1 = rlogis(20), s2 = rlogis(20), alpha = 0.05, power = 0.9, delta = 0.8)
}

hist(res_mat[,8], main = "Logistic distribution, m = 20, delta = 0.8", xlab = "Sample Size")
res[1,8] <- mean(res_mat[,8])
res[2,8] <- var(res_mat[,8])
res[3,8] <- quantile(res_mat[,8],0.1)
res[4,8] <- quantile(res_mat[,8],0.5)
res[5,8] <- quantile(res_mat[,8],0.9)

