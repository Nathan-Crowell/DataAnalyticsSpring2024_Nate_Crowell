# Install the following libararies/packages in RStudio
library(rpart)
library(rpart.plot)

# use the iris dataset
iris
dim(iris)
str(iris)
head(iris)

# create sample of iris dataset
s_iris = sample(150,100)
s_iris
length(s_iris)

# create testing and training sets
iris_train = iris[s_iris,]
iris_test = iris[-s_iris,]
dim(iris_test)
dim(iris_train)

# generate tree decision model
dectionTreeModel = rpart(Species~., iris_train, method="class")
dectionTreeModel

rpart.plot(dectionTreeModel)





