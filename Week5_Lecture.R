# Predict the value of iris sepal.length
 # the average sepal length when petal.length is 4 units
 # the sepal length of a randomly chosen iris flower with petal 4 units

ir <- iris
head(ir)
str(ir)

# training vs test
N <- nrow(ir)
t.ind <- sample(1:N, size=0.8*N)

ir.tr <- ir[t.ind,] # training data
ir.ts <- ir[-t.ind, ] # test data

plot(Sepal.Length ~ Petal.Length, data = ir.tr)

# fit the model
mod.l <- lm(Sepal.Length  ~ Petal.Length,
            data = ir.tr)
summary(mod.l)
lcl.a <- 4.37 - 2*0.09
ucl.a <- 4.37 + 2*0.09

lcl.b <- 0.4 - 2*0.02
ucl.b <- 0.4 + 2*0.02

# 1. We observe that the y-intercept (when x=0) is 4.37 (95% CI [4.19,4.55], p<0.05)
# 2. Furthe the slope is is 0.4 (95% CI [0.36,0.44], p<0.05), implying that unit increase
##   in Petal.Length increases Sepal.Length by 0.4 units (constant)
#Thus the prediction equation (supervised learner) is
#Sepal.Length = 4.37 + 0.4 * Petal.Length
#This model explains 74% of variation in Y(Sepal.Length)

# Predict using fitted model
 # Average prediction
# x = 4 units
n.data = data.frame(Petal.Length = 4.0)
predict(mod.l, newdata = n.data, interval = "confidence")

 # individual prediction
predict(mod.l, newdata = n.data, interval = "prediction")

abline(mod.l)

pr.av <- predict(mod.l, newdata = ir.tr, interval = "confidence")

pr.iv <- predict(mod.l, newdata = ir.tr, interval = "prediction")

# average 95% prediction (confidence) bands for training data
lines(ir.tr$Petal.Length,pr.av[,2],col='blue', lty=2)
lines(ir.tr$Petal.Length,pr.av[,3],col='blue', lty=2)
# individual 95% prediction bands for training data
lines(ir.tr$Petal.Length,pr.iv[,2],col='red', lty=2)
lines(ir.tr$Petal.Length,pr.iv[,3],col='red', lty=2)

# Check correlation of all deatures pairs
pairs(ir[,-5])
cor(ir[,-5])

# Import - advertising data set
adv <- read.csv("Advertising.csv", header = T)
str(adv)
install.packages("ISLR2")
library(ISLR2)
data(package = "ISLR2") # show all data in package ISLR2

#In iris data would adding additional features contribute to modelling 
summary(mod.l)
mod.2 <- lm(Sepal.Length ~ 
              Petal.Length
            + Petal.Width,
            data = ir.tr)
summary(mod.2)
# We observe that the multiple R^2 has increased (from 0.7502 to 0.7544) and Petal.Width is also sifnificant
# Is the overall improvement in R^2 significant
# We use ANOVA to compare the two models
anova(mod.l, # smaller model
      mod.2 # larger linear model
      )
# compare the models on test data
 # compute the predicted values
  # compute the prediction intervals
  # Mean squared prediction error 

pr.ts.mod1 <- data.frame(predict(mod.l, newdata = ir.ts))

pr.ts.mod2 <- data.frame(predict(mod.2, newdata = ir.ts))

pred_comp <- data.frame(Sepal = ir.ts$Sepal.Length, # truth
                        Mod1 = pr.ts.mod1, # prediction 1
                        Mod2 = pr.ts.mod2) # prediction 2

mse.1 <- mean((pred_comp$Sepal-pred_comp$predict.mod.l..newdata...ir.ts.)^2)
mse.2 <- mean((pred_comp$Sepal-pred_comp$predict.mod.2..newdata...ir.ts.)^2)

# Adding the extra feature decreases the test MSE.

# Load the Auto dataset from ISLR2 package
 # Fit a linear model of mpg on horsepower
 # check for the appropriateness of the model using residual analysis
at <- Auto
?Auto

mod.h <- lm(mpg ~ horsepower,
            data = at)
summary(mod.h)
# Were the model assumptions correct
e_r <- residuals(mod.h) # extract residuals: y - y_hat
f_t <- fitted(mod.h) # a_hat + b_hat * horsepower

par(mfrow = c(2,2))
plot(x=at$horsepower,
     y=at$mpg)
plot(x=f_t,
     y=e_r)
mod.h1 <- lm(at$mpg ~
               poly(at$horsepower, degree = 2)) # mpg = a + b_1 * horsepower + b_2 * horsepower^2

summary(mod.h1)

e_r.1 <- residuals(mod.h1) # extract residuals: y - y_hat
f_t.1 <- fitted(mod.h1) # a_hat + b_hat * horsepower

plot(x=f_t.1,
     y=e_r.1)
