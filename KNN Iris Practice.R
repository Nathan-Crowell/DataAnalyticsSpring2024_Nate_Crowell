
install.packages("ggplot2")

library(ggplot2)
head(iris)
summary(iris)
str(iris)

help("sapply")
sapply(iris[,-5],var)
summary(iris)

ggplot(iris,aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +geom_point()
ggplot(iris,aes(x=Petal.Length, y=Petal.Width, color=Species)) +geom_point()


set.seed(300)
k.max = 12

wss = sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart=20,iter.max=20)$tot.withinss})
wss
plot(1:k.max,wss,type="b",xlab="Number of Clusters(k)",ylab="Within cluster sum of squares")
icluster = kmeans(iris[,3:4],3,nstart=20)
table(icluster$cluster,iris$Species)


