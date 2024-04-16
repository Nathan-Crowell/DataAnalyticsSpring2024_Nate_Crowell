
# Logistic Regression Example 
# Introduction to Statistical Learning with R 7th Edition – Chapter 4 Lab: 4.6.1 
library(ISLR) 
data("Smarket") 
head(Smarket) 
# data set consists ofpercentage returns for the S&P 500 stock index over 1, 250 days, from the 
# beginning of 2001 until the end of 2005 names(Smarket) 
# For each date, we have recorded the percentage returns for each of the five previous 
# trading days, "Lag1" through "Lag5". 
# We have also recorded "Volume" (the number of shares traded on the previous day, in billions of dollars. 
# "Today" is the percentage return on the date 
# "Direction" (whether the market was Up or Down on this date)

dim(Smarket) 
summary(Smarket) 
cor(Smarket) 
# cor() function produces a matrix that contains all of the pairwise 
# correlations among the predictors in a data set. The first command below 
# gives an error message because the "Direction" variable is qualitative. cor(Smarket[,-9]) 
# we ommit the 9th column: "Direction" # the correlations between the lag variables and today’s 
# returns are close to zero. In other words, there appears to be little 
# correlation between today’s returns and previous days’ returns. The only 
# substantial correlation is between Year and Volume 
attach(Smarket) 
plot(Volume)

# Now we will fit a Logistic Regression model in order to predict "Direction" 
# using "Lag1" through "Lag5" and "Volume". 
# The glm() function fits generalized glm() linear models, 
# a class of models that includes logistic regression. 
help("glm") # Read the glm() function documentation. 
# glm() function is similar to that of lm(), except that we must pass in 
# the argument family=binomial in order to tell R to run a logistic regression 
# rather than some other type of generalized linear model 
glm.fit.model1 = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial) 
summary(glm.fit.model1)

# The predict() function can be used to predict the probability that the 
# market will go up, given values of the predictors. 
# The The type="response" option tells R to output probabilities of the form P(Y = 1|X), as opposed 
# to other information such as the logit.
# If no data set is supplied to the predict() function, then the probabilities are computed for the training 
# data that was used to fit the logistic regression model. 
# Here we have printed only the first ten probabilities. We know that these values correspond to 
# the probability of the market going up, rather than down, because the 
# contrasts() function indicates that R has created a dummy variable with a 1 for Up. 
glm.probs <- predict(glm.fit.model1, type="response") 
glm.probs[1:10] 
contrasts(Direction)

# In order to make a prediction as to whether the market will go up or 
# down on a particular day, we must convert these predicted probabilities 
# into class labels, Up or Down. 
# The following two commands create a vector of class predictions based on 
# whether the predicted probability of a market increase is greater than or less than 0.5 
help("rep") # Read the documentation for rep() function which replicates elements of vectors. 
glm.pred <- rep("Down", 1250) # this command creates a vector of 1,250 "Down" elements 
glm.pred[glm.probs > 0.5] = "Up" 
# The above command transforms to "Up" all of the elements for which the predicted probability of a market increase exceeds 0.5

# Now we can use the table() function. 
# Given these predictions, the table() function 
# can be used to produce a confusion matrix in order to determine how many 
# observations were correctly or incorrectly classified. 
table(glm.pred, Direction)

# The diagonal elements of the confusion matrix indicate correct predictions, 
# while the off-diagonals represent incorrect predictions 
# Hence our model correctly predicted that the market would go up on 507 days and that 
# it would go down on 145 days, for a total of 507 + 145 = 652 correct predictions (507+145)/1250 
# 0.5216 # Also we can use the mean() function and that can be used to compute the fraction of 
# days for which the prediction was correct. 
# In this case, logistic regression  correctly predicted the movement of the market 52.2% of the time. 
mean(glm.pred == Direction)
# 0.5216

# Next, in order to implement this strategy, we will first create a vector corresponding 
# to the observations from 2001 through 2004. We will then use this vector 
# to create a held out data set of observations from 2005. 
train <- (Year <2005) 
Smarket.2005 = Smarket[!train,] 
dim(Smarket.2005) 
Direction.2005 <- Direction[!train]

# We now we will fit a logistic regression model using only the subset of the observations 
# that correspond to dates before 2005, using the subset argument. 
# We then obtain predicted probabilities of the stock market going up for 
# each of the days in our test set—that is, for the days in 2005. 
glm.fit.model2 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 +Volume, data=Smarket, 
                      family = binomial, subset = train) 
glm.prob2 = predict(glm.fit.model2, Smarket.2005, type="response") 
# Notice that we have trained and tested our model on two completely separate 
# data sets: training was performed using only the dates before 2005, 
# and testing was performed using only the dates in 2005.

# Finally, we compute the predictions for 2005 and compare them to the actual movements 
# of the market over that time period. 
glm.pred2 <- rep("Down", 252) 
glm.pred2[glm.prob2 > 0.5] = "Up" 
table(glm.pred2, Direction.2005)

mean(glm.pred2 == Direction.2005) 
mean(glm.pred2 != Direction.2005) 
# The != notation means not equal to, and so the last command computes 
# the test set error rate. The results are rather disappointing: the test error 
# rate is 52 %, which is worse than random guessing! Of course this result 
# is not all that surprising, given that one would not generally expect to be 
# able to use previous days’ returns to predict future market performance.

glm.fit.model3 <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train) 
glm.probs3 <- predict(glm.fit.model3, Smarket.2005, type = "response") 
glm.pred3 <- rep("Down", 252) 
glm.pred3[glm.probs3 > 0.5] = "Up" 
table(glm.pred3, Direction.2005) 
mean(glm.pred3 == Direction.2005) 
# 0.5595238 approximately 0.56
