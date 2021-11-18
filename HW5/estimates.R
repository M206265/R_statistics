# --------------------------------------------------------------------------------------------- 
# №1 Estimates
# --------------------------------------------------------------------------------------------- 
#   E1                 E2
#
#   x  0,7    1        N(1.1,0.3)
#   p   ?     ?
#
# variables:
# sd - standard deviaton
# mean - expected value
# int* - integral
#
# data - values of E1*E2

# ---------------------------------------------------------------------------------------------
# a) Method of Moments
# --------------------------------------------------------------------------------------------- 

setwd("D:\\вуз\\R\\programs\\HW5")
data <- as.numeric(read.csv("data.csv", header = FALSE, row.names = 1)['D',])

E1mean=mean(data)
a1 = 0.7
a2 = 1
#p =
#1 - p = 

E2mean = 1.1
E2dsp = 0.3
E2sd = sqrt(E2dsp)

int1_func = function(x) {
  return(dnorm(x/a1, E2mean, E2sd)*x)
}

int2_func = function(x) {
  return(dnorm(x/a2, E2mean, E2sd)*x)
}

#int1 = integrate(int1_func,-Inf,Inf)$value
#int2 = integrate(int2_func, -Inf, Inf)$value

int1 = 0.49*E1mean #Results correlates with numerical integration, hoor~ay! 
int2 = E2mean

p_a = (E1mean - 1/a2 * int2)/(1/a1 * int1 - 1/a2 * int2)
cat("Method of moments:", sprintf("p = %f", p_a))


# --------------------------------------------------------------------------------------------- 
# b) Maximum likelihood method
# --------------------------------------------------------------------------------------------- 
# Unknown parameter estimate 

res = 0
densityLikelihood = function(t, p){
  res = p/a1 * dnorm(t/a1, E2mean, E2sd) + (1 - p)/a2 * dnorm(t/a2, E2mean, E2sd)
  return(res)
}


lnLikelihood = function(t, data){
  res = t
  for (i in 1:length(t)) {
    res[i] = 0
    for (j in 1:length(data)) {
    res[i] = res[i] + log(densityLikelihood(data[j], t[i]))
    }
  }
  return(res)
}
tmp0 = seq(-1, 1, 0.01);
tmp1 = lnLikelihood(tmp0, data)

p_b = -1 + 0.01 * (which.max(tmp1)- 1)
cat("Maximum likelihood method:", sprintf("p = %f", p_b))


# Plot of likelihood function
pdf( file="likelihoodFunction.pdf")

plot(tmp0, lnLikelihood(tmp0, data),
     type = 'l',
     xlab = 'p',
     ylab = 'log L',
     main = 'Logariphm of Likelihood function')

lines(c(p_b, p_b), c(min(tmp1, na.rm = TRUE), max(tmp1,  na.rm = TRUE)),
      type = 'l',
      col = 'red',
      lty = 2)

dev.off()
# Plot of distribution with estimated parameters

dDistribution = function(t){                                                          # Density of distribution
  res = p/a1 * dnorm(t/a1, E2mean, E2sd) + (1 - p)/a2 * dnorm(t/a2, E2mean, E2sd)     # == densityLikelihood 
  return(res)
}

pDistribution = function(low, up) {                                                   # Distribution function
  for (i in 1:up) {
    low = low+0.01
    res[i] = integrate(dDistribution, -Inf, low)$value
  }
  return(res)}

pdf( file="estimates.pdf")
layout (matrix (c (1,2,3,4), 2, 2))

p = p_a
plot(ecdf(data),
     main = 'Empirical function')
lines(seq(0,2.99,0.01),pDistribution(0,300),
      col = 'red',
      lwd = 3)
hist(data,
     freq = is.null(dDistribution),
     breaks = 10,
     xlim = c(0,3),
     main = 'Histogram')
lines(seq(0,3,0.1),dDistribution(seq(0,3,0.1)),
      col = 'red',
      lwd = 3)

p = p_b
plot(ecdf(data),
     main = 'Empirical function')
lines(seq(0,2.99,0.01),pDistribution(0,300),
      col = 'red',
      lwd = 3)
hist(data,
     freq = is.null(dDistribution),
     breaks = 10,
     xlim = c(0,3),
     main = 'Histogram')
lines(seq(0,3,0.1),dDistribution(seq(0,3,0.1)),
      col = 'red',
      lwd = 3)

dev.off()

