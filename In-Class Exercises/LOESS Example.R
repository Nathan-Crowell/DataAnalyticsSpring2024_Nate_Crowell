
# LOESS Exaplme from: 
# http://r-statistics.co/Loess-Regression-With-R.html 
data(economics, package="ggplot2")  # load data 
economics$index <- 1:nrow(economics)  # create index variable 
economics <- economics[1:80, ]  # retail 80rows for better graphical understanding 
loessMod10 <- loess(uempmed ~ index, data=economics, span=0.10) # 10% smoothing span 
loessMod25 <- loess(uempmed ~ index, data=economics, span=0.25) # 25% smoothing span 
loessMod50 <- loess(uempmed ~ index, data=economics, span=0.50) # 50% smoothing span

# Predict Loess 
smoothed10 <- predict(loessMod10) 
smoothed25 <- predict(loessMod25) 
smoothed50 <- predict(loessMod50) 

# From above plot, you would notice that as the span increases, the smoothing of the curve also increases. 
# Code for Plot 
# Plot it 
plot(economics$uempmed, x=economics$date, type="l", main="Loess Smoothing and Prediction", 
     xlab="Date", ylab="Unemployment (Median)") 
lines(smoothed10, x=economics$date, col="red") 
lines(smoothed25, x=economics$date, col="green") 
lines(smoothed50, x=economics$date, col="blue")

















