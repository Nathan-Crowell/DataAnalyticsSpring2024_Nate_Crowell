# k-Fold Cross Validation
# the cv.glm() function can also be used to implement k-fold CV
# We once again, set a random seed and intialize a vector in which,
# we will store the CV errors corresponding to the polynomial fits of order one to ten
# here the K = 10

install.packages("boot")
library(ISLR)
library(boot)

??cv.glm

set.seed(17)
help("rep")

cv.error.10 = rep(0,10)

for(i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10
# Notice the computation time is much shorter than LOOCV
# This depends on your laptop performance
# We still see little evidence that using cubic or higher-order polynomials terms,
# leads to lower test error than simply using a quadratics fit.