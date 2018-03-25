## Lab
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket [,-9])
attach(Smarket)
plot(Volume)

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket, family=binomial)
summary(glm.fit) 
coef(glm.fit)
summary(glm.fit)$coef[,4]
glm.prob <- predict(glm.fit, type = "response")
glm.prob[1:10] 
contrasts(Direction)
?contrasts
?rep
glm.pred <- rep("Down", 1250)
glm.pred[glm.prob > .5] = "Up" 
table(glm.pred,Direction)
mean(glm.pred == Direction)

train <- (Year < 2005)
Smarket.2005 <- Smarket[!train,] 
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")

glm.pred <- rep("Down", 252) 
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction.2005) 
mean(glm.pred == Direction.2005) 

glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
predict(glm.fit, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = "response") 

#LDA
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit, Smarket.2005) # Go over the paragraph about this on 162.
names(lda.pred)

lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

sum(lda.pred$posterior[,1] >= .5)
sum(lda.pred$posterior[,1] < .5)

lda.pred$posterior[1:20,1] 
lda.class[1:20]

sum(lda.pred$posterior[,1] > .9)

#QDA
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005) 
mean(qda.class == Direction.2005)

#KNN
library(class)
train.X <- cbind(Lag1, Lag2)[train,]
train.X 
test.X <- cbind(Lag1, Lag2)[!train,] 
train.Direction <- Direction[train] 

set.seed(1) # R is flipping a coin to decide where a very close observation will fall. We want this to always be the same.
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
(83+43)/252

knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822
names(Caravan)

standardized.X <- scale(Caravan[, -86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])
?var

test <- 1:1000 
test
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k=1)
??knn
?knn.pred
mean(test.Y != knn.pred)
test.Y
mean(test.Y != "No")

table(knn.pred, test.Y)
9/(68+9)

knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
5/26

library(class)
knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
4/15 # When we predict that they will buy insurance, we're correct 27% of the time. Precision! (review this)

knn.pred == test.Y 

glm.fit <- glm(Purchase ~ ., data = Caravan, family = binomial, subset = -test)
glm.probs <- predict(glm.fit, Caravan[test,], type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .5] = "Yes"
table(glm.pred, test.Y)
glm.pred <- rep("No",1000)

glm.pred[glm.probs > .25] = "Yes"
table(glm.pred, test.Y)
11/(22+11)

## Aplied Exercises
# 10a
library(ISLR)
dim(Weekly)
?Weekly
names(Weekly)
cor(Weekly[,-9])
pairs(Weekly[,-9])
plot(Volume) # Yes, there is a correlation between Year and Volume (like the SMarket data), with volume increasing as years increase.

#10b
glm.weekly <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(glm.weekly) # Lag2 appears to be statistically significant. In other words, there appears to be a relationship between the direction of the stock market and the percentage return for 2 weeks prior.

#10c
attach(Weekly)
weekly.prob1 <- predict(glm.weekly, type = "response")
weekly.prob1[1:10]
contrasts(Direction)

weekly.pred <- rep("Down", length(weekly.prob1))
weekly.pred[weekly.prob1 > .5] = "Up"
table(weekly.pred, Direction)
(557+54)/1089
mean(weekly.pred == Direction) # The model is correct 56% of the time. However, the confusion matrix shows that our model tends to drastically over-predict that the stock market will go up when it's actually going down.

#d
attach(Weekly)
summary(Year)
train.weekly <- (Year < 2009)
test.weekly <- Weekly[!train.weekly,]
dim(test.weekly)
test.Direction <- Direction[!train.weekly]

glm.fitweekly <- glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train.weekly)
glm.probsweekly <- predict(glm.fitweekly, test.weekly, type="response")

glm.predweekly <- rep("Down", 104)
glm.predweekly[glm.probsweekly > .5] = "Up"

table(glm.predweekly, test.Direction)
mean(glm.predweekly == test.Direction) # This model predicts 63% of the movements correctly.
mean(glm.predweekly != test.Direction) 

#e
library(MASS)
lda.fitweekly <- lda(Direction ~ Lag2, data = Weekly, subset = train.weekly)
lda.fitweekly
lda.predweekly <- predict(lda.fitweekly, test.weekly)
lda.weeklyclass = lda.predweekly$class
table(lda.weeklyclass, test.Direction)
mean(lda.weeklyclass == test.Direction) # The model predicts 62.5% of the movements correctly. 

#f
qda.fitweekly <- qda(Direction ~ Lag2, data = Weekly, subset = train.weekly)
qda.fitweekly
qda.weeklyclass <- predict(qda.fitweekly, test.weekly)$class
table(qda.weeklyclass, test.Direction)
mean(qda.weeklyclass == test.Direction) # The model predicts 58.7% of the movements correctly.

#g
train.w <- as.matrix(Lag2[train.weekly])
test.w <- as.matrix(Lag2[!train.weekly])
train.weeklyDirection <- Direction[train.weekly]
set.seed(1)
library(ISLR)
knn.predweekly <- knn(train.w, test.w, train.weeklyDirection, k = 1) 
table(knn.predweekly, test.Direction)
(31+21)/(21+30+22+31) # The model predicts 50% of the movements correctly.

#h: It appears that the logistic regression model and the LDA provide similar predictions, better than a QDA and KNN.

#11
#a
library(ISLR)
attach(Auto)
mpg01 <- rep(0, length(mpg))
mpg01[mpg > median(mpg)] <- 1
median(mpg)
Auto <- data.frame(Auto, mpg01)

#b 
cor(Auto[,-9]) 
pairs(Auto)
plot(cylinders, mpg01) 
plot(weight, mpg01) 
plot(displacement, mpg01) 
boxplot(mpg01 ~ cylinders) 
boxplot(weight ~ mpg01)

#c
names(Auto)
summary(year)
train <- (year < 81)
trainAuto <- Auto[train, ]
testAuto <- Auto[!train, ]
testmpg01 <- mpg01[!train]

#d
lda.fitAuto <- lda(mpg01 ~ cylinders + weight + displacement, data = Auto, subset = train)
lda.fitAuto
lda.pred <- predict(lda.fitAuto, testAuto)
table(lda.pred$class, testmpg01)
mean(lda.pred$class != testmpg01) # The test error rate is 12%

#e
qda.fitAuto <- qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
qda.fitAuto
qda.pred <- predict(qda.fitAuto, testAuto)
table(qda.pred$class, testmpg01)
mean(qda.pred$class != testmpg01) # The test error rate is 14%

#f
glm.fitAuto <- glm(mpg01 ~ cylinders + weight + displacement + horsepower, family = binomial, data = Auto, subset = train)
glm.fitAuto
summary(glm.fitAuto)
probs <- predict(glm.fitAuto, testAuto, type = "response")
glm.pred <- rep(0, length(probs))
glm.pred[probs > 0.5] <- 1
table(glm.pred, testmpg01)
mean(glm.pred != testmpg01) # The test error rate is 14%.

#g
train.X <- cbind(cylinders, weight, displacement, horsepower) [train, ]
test.X <- cbind(cylinders, weight, displacement, horsepower) [!train, ]
trainmpg01 <- mpg01[train]
set.seed(1)
knn.pred.Auto <- knn(train.X, test.X, trainmpg01, k = 1)
table(knn.pred.Auto, testmpg01)
mean(knn.pred.Auto != testmpg01) # The test error rate is 19% for k = 1.

knn.pred.Auto5 <- knn(train.X, test.X, trainmpg01, k = 5)
table(knn.pred.Auto5, testmpg01)
mean(knn.pred.Auto5 != testmpg01) # The test error rate is still 19% for k = 5.

knn.pred.Auto10 <- knn(train.X, test.X, trainmpg01, k = 10)
table(knn.pred.Auto10, testmpg01)
mean(knn.pred.Auto10 != testmpg01) # Now the test error rate is 21% for k = 10. I conclude that k = 1 is performing better than k = 10.

#12
#a
Power <- function(){
  list(
    "Hello World",
    2^3
  )
}

Power()
print(Power())

#b
Power2 <- function(x,a) {
  x^a
} 
# It takes 2 arguments and evaluates the body of the function using the values you provide for the arguments.
Power2(3,8) # I'm calling this function with the parameters (or arguments) 3 and 8.

# Now I'm just practicing here...
myfunction <- function(a,b) {
  y <- a + b
  print(y)
  z <- y * b
  print(z)
  q <- z - 1
  return(q)
}
myfunction(1,2)

Panther <- function(a,b) {
  z <- a + b
  print("z is:")
  print(z)
}
Panther(2,3) 

#c
Power2(10,3) #1000
Power2(8,17) #2.2518e+15
Power2(131,3) #2248091

#d
Power3 <- function(x,a) {
  result <- x^a
  return(result)
}

#e
x <- 1:10
plot(x, Power3(x, 2), log = "xy", xlab = "Log of x", ylab = "Log of x^2", main = "Log of x^2 vs Log of x")

#f
PlotPower <- function(x,a) {
  plot(x, Power3(x,a))
}
PlotPower(1:10,3)

#13
library(MASS)
names(Boston)
summary(crim)
attach(Boston)
crimrate <- rep(0, length(crim))
crimrate[crim > median(crim)] <- 1
crimrate
summary(crimrate)
sum(crimrate)
table(crimrate)
dim(Boston)

Boston <- data.frame(Boston, crimrate) #So here I'm creating a data frame and also attaching my new vector to it.

train <- 1:(length(crim) / 2)
test <- (length(crim) / 2 + 1): length(crim)  
Boston.train <- Boston[train,] #Splitting the dataframe [rows,columns]  
Boston.test <- Boston[test,] #Splitting the dataframe
crimrate.test <- crimrate[test] 

fit.glm <- glm(crimrate~. - crimrate - crim, data = Boston, family = binomial, subset = train)
summary(fit.glm)

probs <- predict(fit.glm, Boston.test, type = "response") #Creating a new vector that is predicting whether the probablity (in the test set of observations) of each row is above the median crimrate (which is our response variable)
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, crimrate.test)
mean(pred.glm==crimrate.test) #So the model predicted the test data 82% of the time. And the test error is 18%.

#The next step is refitting the model with only the significant variables.
fit.glm2 <- glm(crimrate~. -crimrate -crim -chas -tax, data=Boston, family = binomial, subset = train)
summary(fit.glm2)

probs2 <- predict(fit.glm2, Boston.test, type = "response")
pred.glm2 <- rep(0, length(probs2))
pred.glm2[probs2 > 0.5] <- 1
table(pred.glm2, crimrate.test) #Interesting: by removing the non-significant predictors, we actually predicted a 1 when the observation point was really 0 (at least once). 
mean(pred.glm2==crimrate.test) #So this model predicted the test data 81% of the time. It's so similar to our last one and it's cleaner so we'd probably use this one for future predictions. 

# Now I will fit an LDA model to predict whether a given suburb has a crime rate above/below the median.
lda.fit <- lda(crimrate ~ . - crimrate - crim, data = Boston, subset = train) 
lda.fit 
lda.pred <- predict(lda.fit, Boston.test) 
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class,crimrate.test) # This model captures more true 0s than the logistic regression (80), so better fit?
mean(lda.class==crimrate.test) # This model predicted the test data correctly 87% of the time. It makes sense that the LDA model is better, as the crim data is well-separated by class.

# Now I will fit a KNN model.
library(class)
names(Boston)
train.Z <- cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv)[train,]
test.Z <- cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv)[test,]
train.crimrate <- crimrate[train]

set.seed(1)
knn.pred <- knn(train.Z, test.Z, train.crimrate, k=1)
table(knn.pred,crimrate.test) 
mean(knn.pred==crimrate.test) # So this model predicts the crime rate correctly 54% of the time. Not so great compared to the other two models.

