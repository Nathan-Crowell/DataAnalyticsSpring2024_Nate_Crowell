# Trees for the Titanic


install.packages("titanic")
install.packages("partykit")
library(partykit)


data("Titanic")
typeof(Titanic)
print(Titanic)

titanic_df = data.frame(Titanic)


rpT = rpart(Survived ~ ., data = titanic_df)
print(rpT)

cT = ctree(Survived ~ ., data = titanic_df)
print(cT)

hT = hclust(dist(titanic_df))
plot(hT)

help("rpart")
help("ctree")
help("hclust")
