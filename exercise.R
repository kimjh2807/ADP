# reshape
install.packages("reshape")
library(reshape)

data(airquality)

head(airquality)
head(airquality, 10)

names(airquality)
names(airquality) = tolower(names(airquality))
names(airquality)

apm = melt(airquality, id = c("month", "day"), na.rm = TRUE)
apm

a <- cast(apm, day ~ month ~ variable)
a

b <- cast(apm, month ~ variable, mean)
b

c <- cast(apm, month ~ . |variable, mean)
c

d <- cast(apm, month ~ variable, mean, margins = c("grand_row", "grand_col"))
d

e <- cast(apm, day ~ month, mean, subset = variable == "ozone")
e

f <- cast(apm, month ~ variable, range)
f

# sqldf
install.packages("sqldf")
library(sqldf)

data(iris)

sqldf("select * from iris")
sqldf("select * from iris limit 10")
sqldf("select * from iris where Species like 'se%' limit 10")

# plyr
install.packages("plyr")
library(plyr)

set.seed(1)
g = data.frame(year = rep(2012:2014, each = 6), count = round(runif(9, 0, 20)))
print(g)

ddply(g, "year", function(x) {
  mean.count = mean(x$count)
  sd.count = sd(x$count)
  cv = sd.count/mean.count
  data.frame(cv.count = cv)
})

ddply(g, "year", summarise, mean.count = mean(count))

ddply(g, "year", transform, total.count = sum(count))

# data.table
install.packages("data.table")
library(data.table)

DT = data.table(x = c("b", "b", "b", "a", "a"), v = rnorm(5))
DT

data(cars)
head(cars)

CARS <- data.table(cars)
head(CARS)

tables()

sapply(CARS, class)
DT

DT[2,]
DT[DT$x == "b", ]

setkey(DT, x)
DT

tables()

DT["b", ]
DT["b", mult = "first"]
DT["b", mult = "last"]

# data explanatory
data(iris)
head(iris)
head(iris, 10)
str(iris)
summary(iris)
cov(iris[, 1:4])
cor(iris[, 1:4])

# missing value
y <- c(1, 2, 3, NA)
is.na(y)

x <- c(1, 2, NA, 3)
mean(x)
mean(x, na.rm = TRUE)

# outlier search
x = rnorm(100)
boxplot(x)

x = c(x, 19, 28, 30)
outwidth = boxplot(x)

outwidth$out

# outliers
install.packages("outliers")
library(outliers)

set.seed(1234)
y = rnorm(100)

outlier(y)
outlier(y, opposite = TRUE)

dim(y) = c(20, 5)
outlier(y)

outlier(y, opposite = TRUE)
boxplot(y)

# statistics
# descriptive statistics
data(iris)
head(iris)
summary(iris)

mean(iris$Sepal.Length)
median(iris$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)
quantile(iris$Sepal.Length, 1/4)
quantile(iris$Sepal.Length, 3/4)
max(iris$Sepal.Length)
min(iris$Sepal.Length)    

install.packages("MASS")
library(MASS)

data(Animals)
head(Animals, 100)

mean(Animals$body)
mean(Animals$brain)
median(Animals$body)
median(Animals$brain)
sd(Animals$body)
sd(Animals$brain)
var(Animals$body)
var(Animals$brain)
quantile(Animals$body)
quantile(Animals$brain)

# regression
set.seed(2)
x = runif(10, 0, 11)
y = 2 + 3*x + rnorm(10, 0, 0.2)
dfrm = data.frame(x, y)
dfrm

lm(y ~ x, data = dfrm)

set.seed(2)
u = runif(10, 0, 11)
v = runif(10, 11, 20)
w = runif(10, 1, 30)
y = 3+0.1*u+2*v-3*w+rnorm(10, 0, 0.1)
dfrm = data.frame(y, u, v, w)
dfrm

lm(y ~ u + v + w)
m <- lm(y ~ u + v + w)
summary(m)

# ChickWeight
head(ChickWeight)
Chick <- ChickWeight[ChickWeight$Diet == 1, ]
Chick

lm(weight ~ Time, data = Chick)
summary(lm(weight ~ Time, data = Chick))

# cars
data(cars)
head(cars)

lm(dist ~ speed, data = cars)
summary(lm(dist ~ speed, data = cars))

x <- c(1,2,3,4,5,6,7,8,9)
y <- c(5,3,2,3,4,6,10,12,18)
df1 <- data.frame(x, y)
df1
plot(df1)
x2 <- x^2
x2

df2 <- cbind(x2, df1)
df2

lm(y ~ x, data = df1)
summary(lm(y ~ x, data = df1))
plot(lm(y ~ x, data = df1))

lm(y ~ x + x2, data = df2)
summary(lm(y ~ x + x2, data = df2))
plot(lm(y ~ x + x2, data = df2))

# backward elimination
X1 <- c(7,1,11,11,7,11,3,1,2,21,1,11,10)
X2 <- c(26,29,56,31,52,55,71,31,54,47,40,66,68)
X3 <- c(6,15,8,8,6,9,17,22,18,4,23,9,8)
X4 <- c(60,52,20,47,33,22,6,44,22,26,34,12,12)
Y <- c(78.5,74.3,104.3,87.6,95.9,109.2,102.7,72.5,93.1,115.9,83.8,113.3,109.4)
df <- data.frame(X1, X2, X3, X4, Y)
head(df)

a <- lm(Y ~ X1+X2+X3+X4, data = df)
summary(a)

b <- lm(Y ~ X1+X2+X4, data = df)
summary(b)

c <- lm(Y ~ X1+X2, data = df)
summary(c)

# step, scope
step(lm(Y ~ 1, df), scope = list(lower= ~ 1, upper = ~ X1+X2+X3+X4),
     direction = "forward")

step(lm(Y ~ 1, df), scope = list(lower= ~ 1, upper = ~ X1+X2+X3+X4),
     direction = "both")

# hills
data(hills)
head(hills)

step(lm(time ~ 1, hills), scope=list(lower= ~ 1, upper= ~ dist + climb),
     direction = "forward")

# multivariate
# correlation analysis
# correlation coefficient
# pearson correlation
# spearman correlation
install.packages("Hmisc")
library(Hmisc)

data(mtcars)
head(mtcars)

drat <- mtcars$drat
disp <- mtcars$disp
plot(drat, disp)
cor(drat, disp)

rcorr(as.matrix(mtcars), type = "pearson")
cov(mtcars)

rcorr(as.matrix(mtcars), type = "spearman")

# score
kor <- c(85,75,65,78,59,60,90,100,99,91,70)
mat <- c(80,60,75,40,50,64,70,78,90,98,50)
eng <- c(80,70,69,79,80,95,98,97,67,80,59)
sci <- c(90,100,50,80,67,89,60,79,89,80,100)
test <- data.frame(kor, mat, eng, sci)
test

rcorr(as.matrix(test), type = "spearman")

# multidimensional scaling, MDS
data(eurodist)
head(eurodist)
eurodist

loc <- cmdscale(eurodist)
loc

x <- loc[, 1]
y <- loc[, 2]

plot(x, y, type = "n", main = "eurodist")
text(x, y, rownames(loc), cex = 0.8)
abline(v = 0, h = 0)

# principal component analysis, PCA
library(datasets)
data(USArrests)
summary(USArrests)

fit <- princomp(USArrests, cor = TRUE)
summary(fit)
loadings(fit)

plot(fit, type = "lines")
fit$scores
biplot(fit)

# time series analysis
# stationary
Nile
summary(Nile)
plot(Nile)

Nile.diff1 <- diff(Nile, differences = 1)
plot(Nile.diff1)

Nile.diff2 <- diff(Nile, differences = 2)
plot(Nile.diff2)

acf(Nile.diff2, lag.max=20)
acf(Nile.diff2, lag.max=20, plot=FALSE)

pacf(Nile.diff2, lag.max=20)
pacf(Nile.diff2, lag.max=20, plot=FALSE)

auto.arima(Nile)

Nile.arima <- arima(Nile, order = c(1,1,1))
Nile.arima

Nile.forecasts <- forecast(Nile.arima, h = 10)
Nile.forecasts

plot(Nile.forecasts)
