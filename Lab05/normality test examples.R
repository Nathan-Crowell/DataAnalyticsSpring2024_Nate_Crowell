# Normality Tests
# Normal Distribution
help(rnorm)
set.seed(10)
data1 = rnorm(50)

set.seed(30)
data2 = rnorm(50)

# Shapiro-Wilk Normality Test
help("shapiro.test")

# As the test returns a p-value less than 0.05, we reject the null-hypothesis 
# and conclude that the data is not normally distributed
shapiro.test(data1)
hist(data1, col='green')
# the histogram shows that the curve is mildly left skewed in nature

shapiro.test(data2)
hist(data2, col = 'steelblue')
# this histogram shows that the curve is normally distributed in nature

set.seed(0)
# create dataset of 100 random values generated from a normal distribution
data = rnorm(100)

shapiro.test(data)

# the p-value of teh test turns out to be 0.6303
# since this value is not less than .05, we fail to reject the null hypothesis that the data is normally distributed

# now lets try it on non-normal data
set.seed(0)
help(rpois)
data = rpois(n=100, lambda = 3)

shapiro.test(data)
hist(data, col = 'yellow')
# the data is clearly skewed right, and doesn't have a bell curve shape


# Anderson-Darling test for normality
install.packages("nortest")
library(nortest)
help("ad.test")

set.seed(1)
x = rnorm(100,0,1) # define vector of 100 values that are normally distributed
# now conduct AD test
ad.test(x)

# Anderson-Darling normality test
# this test returns 2 values, A: the test statistic, p-value: the corresponding p-value of the test statistic
# the null-hypothesis for the A-D test is that the data does follow a normal distribution
# thus, if p-value is less than the significance level, we reject the null hypothesis

set.seed(1)

# The Uniform Distribution
help(runif)
x = runif(100,0,1)

# now do A-D test
ad.test(x)
hist(x, col = 'steelblue')












