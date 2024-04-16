
# KNN Regressor In-Class Work

set.seed(123) 
# create example data set 
x <- seq(0, 2*pi, length.out=100) 
y <- sin(x) + rnorm(100, sd=0.2) 

# Next, we can split the data into training and testing sets: 
train_index <- sample(1:100, 70) 
train_x <- x[train_index] 
train_y <- y[train_index] 
test_x <- x[-train_index] 
test_y <- y[-train_index]

# we can train a KNN regressor on the training data: 
library(class) 
library(FNN)
knn_model <- knn.reg(train_x, train_y, k = 5) 
# Here, we're using a K value of 5. 
# You can experiment with different values to see how they affect the model. 
# Finally, we can use the trained model to make predictions on the test data: 
predictions <- knn.reg(train_x, train_y, test_x, k = 5) 
plot(test_x, test_y, main = "KNN Regressor Example") 
lines(test_x, predictions, col = "red") 
# This should give you a plot of the actual and predicted values. 
# You can adjust the value of K to try to get better predictions.












