
# Cook's Distance using mtcars
library(ISLR)
mtcars
head(mtcars)
str(mtcars)

model1 = lm(mpg ~ cyl + wt, data = mtcars)
model1

help("cooks.distance")

plot(model1, pch = 18, col = 'red', which = c(4))

# we can use the cooks.distance() function to identify Cook's distance to each observation
cooks.distance(model1)
CooksDistance = cooks.distance(model1)
round(CooksDistance, 5)

sort(round(CooksDistance,5))