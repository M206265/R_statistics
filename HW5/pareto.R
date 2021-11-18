# №2 Asteroid diameter distribution

# Distribution function and density + plotting function because why not

pDistr = function(x, x0, a){
  res = x
  for (i in 1:length(x)){
  if (x[i] < x0) {
    res[i] = 0
    }
  else {
    res[i] = 1 - (x0/x[i])^a
    }
  }
  return(res)
}

dDistr = function(x, x0, a){
  res = x
  for (i in 1:length(x)){
    if (x[i] < x0){
      res[i] = 0
    }
    else {
      res[i] = a * x0^a / x[i]^(a+1)
    }
  }
  return(res)
}

plotGraphs = function(data, main, xlab, x0, a){
  layout (matrix (c (1,2), 1, 2))
  t = seq(min(data)-0.01, 1000,0.01)
  
  hist(data, freq = F, breaks = 1000, xlim = c(0,25), main = main, xlab = xlab)
  lines(t,dDistr(t,x0,a), col = "purple", lwd = 2)
  
  plot(ecdf(data), main = main)
  lines(t,pDistr(t,x0,a), col = "purple", lwd = 2)
}


setwd("D:\\вуз\\R\\programs\\HW5")
data = as.numeric(read.csv("neowise_mainbelt.csv", header = FALSE)[,12])
pdf(file = "pareto.pdf")

#In case x < x0: F(x) = P(X < x) = 0, so we can use
x0 = min(data)

# ---------------------------------------------------------------------------------------------
# a) Method of moments
# ---------------------------------------------------------------------------------------------

a = mean(data)/(mean(data - x0))
cat("Method of moments:", sprintf("x0 = %f, a = %f", x0, a))

plotGraphs(data,"Method of Moments", "diameters of asteroids, m", x0, a)

# ---------------------------------------------------------------------------------------------
# b) Maximum likelihood method
# ---------------------------------------------------------------------------------------------

a = length(data) / (sum(log(data)) - length(data) * log(x0))                   # Can be obtained from first derivative of log-likelihood function
cat("Maximum likelihood method:", sprintf("x0 = %f, a = %f", x0, a))

plotGraphs(data,"Maximum likelihood method", "diameters of asteroids, m", x0, a)

# ---------------------------------------------------------------------------------------------
# In the maximum likelihood method we are looking for a maximum of the function.
# In cases with x < x0 our values lay lower than with x0 => we can use x0 = min(data) as our "threshold"

a_tmp = c()
x = seq(0, min(data), 0.01)

for (i in (1:length(x))){
  a_tmp[i] = length(data) / (sum(log(data)) - length(data) * log(x[i]))}

plot(x, a_tmp, pch = '.')
lines(x0, a, col = 'red', type = 'p', pch = 'o')

# ---------------------------------------------------------------------------------------------
# b) Least squares method
# ---------------------------------------------------------------------------------------------
# y = Xb + e,  b is an estimated parameter, e is an intercept
Fn = ecdf(data)
x = data.frame(log(data[data < max(data)]))
y = data.frame(log(1 - Fn(data[data < max(data)])))
estimation = lsfit(x,y)$coef
a = - estimation[2]
x0 = exp(estimation[1]/a)
cat("Least squares method:", sprintf("x0 = %f, a = %f", x0, a))
estimation

plotGraphs(data,"Least squares method", "diameters of asteroids, m", x0, a)
layout(1,1)
plot(log(data), log(1-Fn(data)), pch = ".")
lines(log(data), log(data)*estimation[2]+estimation[1], col = "red")

dev.off()

