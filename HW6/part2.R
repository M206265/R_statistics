# Part 2: defining confidence intervals

# ----------------------------------------------
# Initial data:
# N(?, 0.0169)
# Conf. level = 0.95

setwd("D:\\вуз\\R\\programs\\HW6")
data = as.numeric(read.csv("data.csv", header = FALSE, row.names = 1)['D',])

conf_level = 0.95
signif_level = 1 - conf_level
disp = 0.0169
sd = sqrt(disp)

# ----------------------------------------------

quantile_signif = qnorm(1 - signif_level/2)
delta = sd/sqrt(length(data))*quantile_signif # because of the small standard deviation estimate seems to be precise enough

# ----------------------------------------------
# Histogram with confidence interval

hist(data, breaks = 40, )
lines(mean(data,na.rm = TRUE)+delta, 40, type = "h", col = "red", lwd = 1)
lines(mean(data,na.rm = TRUE)-delta, 40, type = "h", col = "red", lwd = 1)

# ----------------------------------------------
# Confidence intervals for n = 5, 10, 100

for (n in c(5, 10, 100)) {
  data_temp = data[1:n]
  quantile_signif_temp = qnorm(1 - signif_level/2)
  delta_temp = sd/sqrt(n)*quantile_signif_temp
  lower = mean(data_temp)-delta_temp
  upper = mean(data_temp)+delta_temp
  length = upper - lower
  print(sprintf("n = %f, lower = %f, upper = %f, length = %f", n, lower, upper, length))
}


