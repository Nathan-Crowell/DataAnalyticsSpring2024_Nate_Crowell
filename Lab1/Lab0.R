# Lab 0 for Data Analytics
# Nate Crowell
myData = read.csv(file.choose(),header=T)

View(myData)

summary(myData$EPI)
boxplot(myData$EPI)
fivenum(myData$EPI,na.rm=T)

hist(myData$EPI)
stem(myData$EPI)
hist(myData$EPI, seq(30., 95., 1.0), prob=T)
lines(density(myData$EPI,na.rm=T,bw=1.))
rug(myData$EPI)


plot(ecdf(myData$EPI), do.points=F, verticals=T)
par(pty="s")
qqnorm(myData$EPI); qqline(myData$EPI)

x = seq(30,95,1)
qqplot(qt(ppoints(250), df= 5), x, xlab= "Q-Q plot for t dsn")
qqline(x)
