
# In-Class work 2/16/24

# clustering with iris dataset again

library(ggplot2)
head(iris)
summary(iris)
str(iris)

help("sapply")
sapply(iris[,-5],var)
summary(iris)

# create a copy of the dataset without the 5th row
trim_iris = iris[,1:4]
summary(trim_iris)

# apply kmeans with 1000 iterations
set.seed(300)
k.max = 9

wss = sapply(1:k.max,function(k){kmeans(trim_iris[,3:4],k,nstart=20,iter.max=1000)$tot.withinss})
wss
plot(1:k.max,wss,type="b",xlab="Number of Clusters(k)",ylab="Within cluster sum of squares")
icluster = kmeans(trim_iris[,3:4],3,nstart=20)
table(iris[,5],icluster$cluster)






















