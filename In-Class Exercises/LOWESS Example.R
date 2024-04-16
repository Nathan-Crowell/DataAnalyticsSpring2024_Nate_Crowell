
# Fitting a curve to the data 
# Local regression or local polynomial regression, also known as moving regression is a generalization 
# of moving average and polynomial regression. It is one of the most common methods, 
# initially developed for scatterplot smoothing, are LOESS (locally estimated scatterplot smoothing) and 
# LOWESS (locally weighted scatterplot smoothing), 
# LOWESS example using the Cars dataset 
data("cars")
str(cars) # we see 50 observation and 2 variables 
# now we create a plot, speed Vs distance 
plot(speed ~ dist, data = cars) 
# When we look at the plot, we see that there is a positive relationship between these two variables

# Now we will use the lowess() function 
lowess(cars$speed ~ cars$dist) 
# Now we will use the lowess() function along with the line() function 
# to draw the lines 
lines(lowess(cars$speed ~ cars$dist, f=2/3), col="blue") 
# here the f value is the the smoother span. f= 2/3 = 0.666 
# the default value for smoother span is 0.666 in RStudio.

lines(lowess(cars$speed ~ cars$dist, f=0.75), col="red")
lines(lowess(cars$speed ~ cars$dist, f=0.8), col="green")
lines(lowess(cars$speed ~ cars$dist, f=0.9), col="gray")
lines(lowess(cars$speed ~ cars$dist, f=0.1), col=7)
lines(lowess(cars$speed ~ cars$dist, f=0.3), col=5)
