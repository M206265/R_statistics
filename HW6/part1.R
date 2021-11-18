# Problem №6 
# №1 Random values of normal distribution parameters
# --------------------------------------------------------
# Functions
# --------------------------------------------------------

averageRandom = function(N, n, mean, sd){
  eta1 = rep(0, N)
  for (i in (1:N)){
    for (j in 1:n){
    eta1[i] = eta1[i] + rnorm(n, mean, sd)
    }
    eta1[i] = eta1[i]/n
  }
  return(eta1)
}

standardDevRandom = function(N, n, mean, sd){
  eta2 = rep(0, N)
  for (i in (1:N)){
    eta2[i] = sum((rnorm(n, mean, sd)-mean)^2)/n
  }
  return(eta2)
}

dispRandom = function(N, n, mean, sd){
  eta3 = rep(0, N)
  for (i in (1:N)){
    x = rnorm(n, mean, sd)
    eta3[i] = sum((x-sum(x)/n)^2)/(n-1)
  }
  return(eta3)
}

plotGraphs = function(data, dDistr, pDistr, mean, sd){
    layout (matrix (c (1,2), 1, 2))
    t = seq(0, 1000,0.01)
    
    hist(data, freq = F, breaks = 100, xlim = c(0,10))
    lines(t,dDistr(t, mean, sd), col = "purple", lwd = 2)
    
    plot(ecdf(data))
    lines(t,pDistr(t, mean, sd), col = "purple", lwd = 2)
  }
# --------------------------------------------------------
# Initial data
# --------------------------------------------------------
  
  N = 1000 #quantity of random values
  n = 5 #quantity of distributions
  mean = 1 
  sd = 3
  
# --------------------------------------------------------
# Calculating data and plotting
# --------------------------------------------------------

 data1 = averageRandom(N, n, mean, sd)
 data2 = standardDevRandom(N, n, mean, sd)
 data3 = dispRandom(N, n, mean, sd)
 # --------------------------------------------------------
 dDistr = function(data, mean, sd) dnorm(data, mean, sd)
 pDistr = function(data, mean, sd) pnorm(data, mean, sd)
 
 plotGraphs(data1, dDistr, pDistr, mean, sd/sqrt(n))
 # --------------------------------------------------------
 dDistr = function(data, mean, sd) dchisq(data*n/sd^2, n)*n/sd^2
 pDistr = function(data, mean, sd) pchisq(data*n/sd^2, n)
 
 plotGraphs(data2, dDistr, pDistr, mean, sd)
 # -------------------------------------------------------- 
 dDistr = function(data, mean, sd) dchisq(data*(n-1)/sd^2, (n-1))*(n-1)/sd^2
 pDistr = function(data, mean, sd) pchisq(data*(n-1)/sd^2, (n-1))
 
 plotGraphs(data3, dDistr, pDistr, mean, sd)
 
 