# Lab 1 for Data Analytics
# Nate Crowell
data2010 = read.csv(file.choose(),header=T)
data2016 = read.csv(file.choose(),header=T)

names(data2010) <- data2010[1,]
data2010 = data2010[-1,]
View(data2010)

# all data is formatted as characters, so this formats the EPI column as numeric type
data2010$EPI = as.numeric(as.character(data2010$EPI))
View(data2010)

# Exercise 1: exploring distribution
summary(data2010$EPI)
fivenum(data2010$EPI)
stem(data2010$EPI)
hist(data2010$EPI)
hist(data2010$EPI, seq(30.,95.,1.0),prob=T)
lines(density(data2010$EPI,na.rm=TRUE,bw="SJ"))
lines(density(data2010$EPI,na.rm=TRUE,bw=1.0))
rug(data2010$EPI)

# Exercise 1: fitting a distribution beyond histograms
plot(ecdf(data2010$EPI), do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(data2010$EPI)
qqline(data2010$EPI)

x<-seq(30,95,1)
qqplot(qt(ppoints(250), df= 5), x, xlab= "Q-Q plot for t dsn")
qqline(x)

# Exercise 1: fitting a distribution
# using AIR_H and AIR_E

# cast each column as numerics
data2010$AIR_H = as.numeric(as.character(data2010$AIR_H)) 
data2010$AIR_E = as.numeric(as.character(data2010$AIR_E))
View(data2010)

# give columns var names to make it faster
HAir2010 <- data2010$AIR_H
EAir2010 <- data2010$AIR_E

# now lets compare the two variables
boxplot(HAir2010,EAir2010)
help("boxplot")
qqplot(HAir2010,EAir2010)
help("qqline")
plot(ecdf(HAir2010), do.points=F, verticals=T)
plot(ecdf(EAir2010), do.points=F, verticals=T)


# Exercise 2: filtering (populations)
data2010$Landlock = as.numeric(as.character(data2010$Landlock))
EPILand <- data2010$EPI[!data2010$Landlock]
ELand <- EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)
lines(density(ELand,na.rm=TRUE,bw="SJ"))
lines(density(ELand,na.rm=TRUE,bw=1.0))


# filter AIR_H on high population density
data2010$High_Population_Density = as.numeric(as.character(data2010$High_Population_Density))
hist(data2010$High_Population_Density)
airhHPD <- HAir2010[!data2010$High_Population_Density]
ahHPD = airhHPD[!is.na(airhHPD)]
hist(ahHPD)
hist(ahHPD, seq(0.,100.,1.0),prob=T)
lines(density(ahHPD,na.rm=TRUE,bw="SJ"))
lines(density(ahHPD,na.rm=TRUE,bw=1.0))

# filtering AIR_E on HPD
aireHPD <- EAir2010[!data2010$High_Population_Density]
aeHPD = aireHPD[!is.na(aireHPD)]
hist(aeHPD)
hist(aeHPD, seq(20.,100.,1.0),prob=T)
lines(density(aeHPD,na.rm=TRUE,bw="SJ"))
lines(density(aeHPD,na.rm=TRUE,bw=1.0))


# now compare filtered values
boxplot(ahHPD,aeHPD)
qqplot(ahHPD,aeHPD)
plot(ecdf(ahHPD), do.points=F, verticals=T)
plot(ecdf(aeHPD), do.points=F, verticals=T)

# now lets compare before and after filtering?
boxplot(HAir2010,ahHPD)
boxplot(EAir2010,aeHPD)

# Filter on No_Surface_Water
data2010$No_surface_water = as.numeric(as.character(data2010$No_surface_water))
EPInsw <- data2010$EPI[!data2010$No_surface_water]
ENSW <- EPInsw[!is.na(EPInsw)]
hist(ENSW)
hist(ENSW, seq(30., 95., 1.0), prob=TRUE)
lines(density(ENSW,na.rm=TRUE,bw="SJ"))
lines(density(ENSW,na.rm=TRUE,bw=1.0))

# Filter on Desert
data2010$Desert = as.numeric(as.character(data2010$Desert))
EPId <- data2010$EPI[!data2010$Desert]
ED <- EPId[!is.na(EPId)]
hist(ED)
hist(ED, seq(30., 95., 1.0), prob=TRUE)
lines(density(ED,na.rm=TRUE,bw="SJ"))
lines(density(ED,na.rm=TRUE,bw=1.0))

# Filter on (EPI_regions == "Europe")
View(data2010)
dataEPIEurope <- data2010$EPI[!(data2010$EPI_regions!="Europe")]
EPIEurope <- dataEPIEurope[!is.na(dataEPIEurope)]
hist(EPIEurope)
hist(EPIEurope, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPIEurope,na.rm=TRUE,bw="SJ"))
lines(density(EPIEurope,na.rm=TRUE,bw=1.0))
