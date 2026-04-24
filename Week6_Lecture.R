# How we incorpotare categorical features?
ir <- iris
str(ir)
# Do Sepal.Length change with flower species

# Visualization
# boxplot - shows relationship between a numeric and a categorical variable

# count the number of categories
table(ir$Species) # frequency distribution of the categorical variable
boxplot(Sepal.Length ~ Species,
        data = ir)

# We observe that the median of Sepal.Length increase from species setosa to virginica
# The variation is larger for versicolor and virginica
 
# Fit a model for Sepal.Length against Species
# Check that the categorical variable is defined as a factor() class object
mod.c <- lm(Sepal.Length ~ Species, data = ir)
summary(mod.c)
# The fitted model is
# Sepal.Length = 5 + 0.93 * vers + 1.58 vir

# 5(): The average Sepal.length of setosa
# 0.93(): The average increase in Sepal.Length for vers compared to Setosa
# 1.58(): The average increase in Sepal.Length for Vir compared to setosa

# checking propriety of model assumptions on training data
res <- residuals(mod.c)
fit <- fitted(mod.c)

# plot residuals against fitted values
plot(x = fit, y =res)

# Can advertising increase sales?
# which media gives the biggest boost
# shall we advertise in multiple media?
adv <- read.csv("Advertising.csv", header = T)
str(adv)
head(adv)
summary(adv)

mod.adv <- lm(sales ~ TV + radio + newspaper, data = adv)
mod.adv_1 <- lm(sales ~ TV + radio, data = adv)
summary(mod.adv_1)

# res _ analysis
res.adv <- residuals(mod.adv_1)
fit.adv <- fitted(mod.adv_1)
plot(fit.adv, res.adv)

mod.adv_2 <- lm(sales ~ 
                  TV # fixed effect 
                + radio # fixed/main effect
                + TV:radio # interaction term
                , data = adv) 
summary(mod.adv_2)

# sales = 6.75 + 0.02 TV +0.03 Radio + 0.001 TV:Radio
#       = 6.75 + (0.02 + 0.001 Radio) TV
# spend $1000 on TV and $1000 Radio what is the impact on sales
res.3 <- mod.adv_2$residuals
fit.3 <- mod.adv_2$fitted.values
par(mfrow=c(1,2))
# check for non_linearity
# constancy of variance
plot(x = fit.adv, y=res.adv, main="Additive model")
plot(fit.3, res.3, main = "Interection mode")
# check for distribution of residuals: Normal
hist(res.3) # histogram/distribution plot of residuals
qqnorm(res.3) # quantile plot: matches sample and theoretical quantiles
qqline(res.3)

# It is evident that the residual (and hence the response) distribution is 
# potentially non-normal.This calls for alternative probability models.                                                                                                      This calls for alternative probability models.

# Check for multicollinearity: linear dependence among features
head(ir)
pairs(ir[, -c(1,5)])

cor(ir[, -c(1,5)])

mod.l <- lm(Sepal.Length ~ ., # include ALL features
            data = ir[,-5])
library(car)
vif(mod.l)
# Petal len and wid have strong dependency we drop features with higher VIF
mod.cl <- lm(Sepal.Length ~ Sepal.Width
             + Petal.Width,
             data = ir[,-5])
vif(mod.cl)

plot(mod.adv_2)

