# In-Class Work - Friday, Feb 2

multivariate = read.csv(file.choose(), header = T)
attach(multivariate)
names(multivariate)



# creating scatter plots
plot(Income,Immigrant, main='Scatterplot')
plot(Immigrant, Homeowners)

# fitting linear models
help(lm)
mm = lm(Homeowners ~ Immigrant)
mm
plot(Immigrant, Homeowners)

help(abline)
abline(mm)
abline(mm, col=2, lwd=3)

summary(mm)
attributes(mm)
mm$coefficients

