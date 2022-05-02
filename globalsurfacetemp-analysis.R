import = read.csv("GLB.Ts_dSST.csv")

data = import[2:13] # keep only 12-month columns
year = 1880:2020 # year data range
yearly.temp = apply(data, 1, mean) # vector of yearly average temperatures

plot(year, yearly.temp, type = 'l') # time series of average temperatures against the year


###################################
# A general upward trend in yearly average temperatures since 1880 is observed,
# especially within the past 40-50 years.
###################################


## Construct a sum of squares objective function to estimate values of linear regression
## based on yearly.temp and year vector values. This will be our first fitted model.

fun = function(ls.est){
  i = seq(from = 1, to = length(yearly.temp))
  s = (yearly.temp[i] - ls.est[1] - (ls.est[2] * year[i]))^2
  return(sum(s))
}

obj = nlminb(start = c(-10, 0.1), objective = fun) # estimate a and b values in objective function with start values -10, 0.1

a = (obj$par)[1]
b = (obj$par)[2]
ls.est = c(a, b) # appending values of a and b to vector ls.est

resid = yearly.temp - ls.est[1] - (ls.est[2] * year) #calculate residual values


## Model diagnostic checking based on residuals

# Add a line of best fit to observe linear trend:
fitted = ls.est[1] + (ls.est[2] * year) # a is ls.est[1], and b is ls.est[2]
plot(year, yearly.temp, type = 'l') # original yearly temperature plot
abline(ls.est[1], ls.est[2], col = 2) # line of best fit to original plot

# scatterplot residuals to observe if trend is linear or not
plot(year, resid)

# see if residuals are normally distributed based on whether points adhere to the qqline well
qqnorm(resid)
qqline(resid)


###################################
# The points appear to follow more of a cyclical trend rather than linear due to 
# the W-shape pattern the residuals follow. The points are not random, they are 
# relatively normally distributed as seen on the QQ plot but there exists deviation.
###################################


## Implement statistical dummy variables with values 0, 1. This will be used to 
## analyze the strength of the regression results and act as our second fitted model.

dummy = c(rep(0, 85), rep(1, 56))

# The following function is redone from the previous model (function 'fun'), but 
# the summation has changed to include two dummy variables.

fun2 = function(ls.est2){
  i = seq(from = 1, to = length(yearly.temp))
  s = (yearly.temp[i] - ls.est2[1] - (ls.est2[2] * year[i]) - (ls.est2[3] * dummy[i]) - (ls.est2[4] * dummy[i] * year[i]))^2
  return(sum(s))
}

obj2 = nlminb(start = c(-1, 1, -1, 1), objective = fun2)
a = (obj2$par)[1]
b = (obj2$par)[2]
c = (obj2$par)[3]
d = (obj2$par)[4]
ls.est2 = c(a, b, c, d) # estimated a, b, c, d values

# new fitted line
fitted2 = ls.est2[1] + (ls.est2[2] * year) - (ls.est2[3] * dummy) - (ls.est2[4] * dummy * year)

# residuals
resid2 = yearly.temp - ls.est2[1] - (ls.est2[2] * year) - (ls.est2[3] * dummy) - (ls.est2[4] * dummy * year)
plot(year, resid2)

# QQ plot to analyze normality of residuals
qqnorm(resid2)
qqline(resid2)

###################################
# Conclusions are that including the dummy variables in the second model show a 
# closer conformity of the data points to a normal distribution as shown by the 
# QQ plot, and the residual plot shows more randomness compared to the first model. 
# Overall, there appears to be a relatively cyclical pattern with the yearly 
# temperatures rather than the points following a linear or exponential trend.
###################################