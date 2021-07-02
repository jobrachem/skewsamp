set.seed(1)


m <- 10
X_samp <- rnorm(m, mean = 1)
X_samp <- sort(X_samp)
X_samp <- c(2*X_samp[1]-X_samp[2], X_samp, 2*X_samp[m]-X_samp[m-1])

delta <- 0

X_samp_star <- X_samp[2:(m+2)] - delta

Z_samp <- c(X_samp, X_samp_star)
Z_samp <- sort(Z_samp)

################ Funktion G ################################

G <- function(x){
  if (x <= X_samp[1]) {
    return(0)
  }
  for(i in 1:(m+1)){
    if(x >= X_samp[i] & x < X_samp[i+1]){
      return((i-1)/(m+1) + (x - X_samp[i])/((m+1)*(X_samp[i+1] - X_samp[i])))
    }
  }
  return(1)
}

############################################################

################ Funktion g ################################

g <- function(x){
  for(i in 1:(m+1)){
    if(x >= X_samp[i] & x < X_samp[i+1]){
      return(1/((m+1)*(X_samp[i+1] - X_samp[i])))
    }
  }
  return(0)
}

############################################################

p_hat <- 0

for(i in 1:(2*m +2)){
  p_hat <- p_hat + g(Z_samp[i]) * (Z_samp[i+1] - Z_samp[i]) *
           (G(Z_samp[i+1] + delta) + G(Z_samp[i] + delta))/2
}


