# Estimating mean using maximum likelihood method
# differentiating the likelihood function we get mean = sum(xi/dispi)/(sum(1/dispi)) - that's a weighted mean of x

setwd("D:\\вуз\\R\\HW6")
data = as.numeric(read.csv("data1.csv", header = FALSE)[7,2:133])
disp = as.numeric(read.csv("data1.csv", header = FALSE)[8,2:133])
mean = weighted.mean(data, 1.0 / disp) 
sprintf("mean = %f", mean)
