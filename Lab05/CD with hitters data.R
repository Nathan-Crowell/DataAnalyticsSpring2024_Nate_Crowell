# Outlier Detection using "Cooks Distance"
# Multivariate Regression using Cook's Distance
# Cook's Distance is an estimate of the influence of a data point
# Cook's Distance is a summary of how much a regression model changes
# when the ith observation is removed from the data
library(ISLR)
library(dplyr)

# Let's look at the baseball hitters dataset in the ISLR package
head(Hitters)
dim(Hitters)
is.na(Hitters)

# Now we will remove the NA using na.omit() fxn
HittersData = na.omit(Hitters)
dim(HittersData)
glimpse(HittersData)
head(HittersData)

# Now we implement a multivariate regression model using all the features in the dataset to
# predict the salary of the Baseball player
SalaryPredictModel1 = lm(Salary ~ ., data = HittersData)
summary(SalaryPredictModel1)
# Multiple R-squared: 0.5461, Adjusted R-Squared: 0.5106

# Cook's Distance
cooksD = cooks.distance(SalaryPredictModel1)
influential = cooksD[(cooksD > 3 * mean(cooksD, na.rm = T))]
influential

# We see that 18 players and rerun the model to see if we have a better fit in our regression model.
names_of_influential = names(influential)
names_of_influential

outliers = HittersData[names_of_influential,]
Hitters_Without_Outliers = HittersData %>% anti_join(outliers)

# Model 2: without the outliers
SalaryPredictModel2 = lm(Salary ~., data = Hitters_Without_Outliers)
summary(SalaryPredictModel2)
# Multiple R-squared: 0.6721, Adjusted R-squared: 0.6445
# we have improved from an adjusted R-squared of 0.5106 to 0.6445 with the removal of only 18 observations