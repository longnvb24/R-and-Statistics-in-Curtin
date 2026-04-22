# You'll need to have these libraries installed on whatever computer you're using
require(car)
require(rgl)
require(lattice)
require(leaps)
library(car)
library(rgl)
library(lattice)
library(leaps)

# The data Defects provides data on the average number of defects per 1000 parts (Defective) produced in an industrial process along with
  # the values of other variables (Temperature, Density, and Rate). The production engineer wishes to construct a linear model relating
  # Defective to the potential predictors.
# Load the Defects data set
defects <- read.csv('Defects(1).csv')
head(defects)
str(defects)

# Plot a scatterplot matrix
pairs(defects)
  # There appears to be a curvilinear relationship between Defective and the other variables.
# Produce another scatterplot matrix, but this time transform Defective using a square root transformation
pairs(~ Temperature + Density + Rate + sqrt(Defective), data = defects)

# Construct a linear model relating the transformed number of defects against the other variables
defects.lm1 <- lm(sqrt(Defective) ~ .,
                  data = defects)
summary(defects.lm1)
# Once Temperature and Density are in the model, the Rate appears do not be significant
# Thus, eliminate the Rate value
defects.lm2 <- lm(sqrt(Defective) ~
                    Temperature
                  + Density,
                  data = defects)
summary(defects.lm2)
  # After Rate has been eliminated, there is a little decrease in R-squared but it is acceptable


# Loading the Sim data set
load('sim(1).RData')

# Construct linear model of y and each of variable and both
yx.lm <- lm(y ~ x)
summary(yx.lm)

yx1.lm <- lm(y ~ x1)
summary(yx1.lm)
# The coefficient of x is highly significant, but not surprisingly, the coefficient of x1 is not.

yxx1.lm <- lm(y ~ x + x1)
summary(yxx1.lm)

anova(yx.lm)
anova(yxx1.lm)
# We can see that the residual sum of squares, RSS, has decreased from 628.88 to 536.433; alternatively, R2
  # has increased from 0.912 to 0.925 even though we’ve added a completely irrelevant variable to the regression!

# Loading Highway1 data set
load('Highway1(1).RData')
head(Highway1)

# calculating and displaying the correlation matrix
require(corrplot)
corrplot(cor(Highway1[,-which(names(Highway1)=='hwy')]), # eliminate the categorical variable hwy
         method = 'ellipse')

pairs(Highway1)
# Clearly there are some strong linear associations between the response variable logRate and some of the potential explanatory variables.

# All-subsets selection
Highway1.AllSubsets <- regsubsets(logRate ~.,
                          data = Highway1,
                          nvmax = 10,
                          nbest = 1)
AllSubsets.summary <- summary(Highway1.AllSubsets)
AllSubsets.summary
knitr::kable(AllSubsets.summary$outmat)

# plots the values of the information criteria (adjusted R2, Mallows’ Cp (which yields the same results as AIC), and BIC)
par(mfrow = c(1, 3))
par(cex.axis = 1.5)
par(cex.lab = 1.5)
plot(1:10, AllSubsets.summary$adjr2, xlab="subset size", ylab="adjusted R-squared", type="b")
plot(1:10, AllSubsets.summary$cp, xlab = "subset size", ylab = "Mallows' Cp", type = "b")
plot(1:10, AllSubsets.summary$bic, xlab = "subset size", ylab = "BIC", type = "b")
# We can see that each of the criteria suggests a different compromise between goodness-of-fit (small RSS) 
 # and the number of variables: both Mallows’ Cp and BIC suggest smaller models than does adjusted R2
.
# Set graphical parameters to default values
par(mfrow = c(1, 1))
par(cex.axis = 1)
par(cex.lab = 1.5)

# Stepwise selection methods
  # Forward stepwise model: We begin with the null model
lm.0 <- lm(logRate ~ 1, data = Highway1)
  # Suppress intermediate output with trace = 0
lm.forward <- step(lm.0,
                   scope = ~ logLen + logADT + logTrks + logSigs1 + slim + 
                     shld + lane + acpt + itg + lwid + hwy,
                   direction = "forward", trace = 0)
summary(lm.forward)

  # Backward selection: We begin with the full model
lm.all <- lm(logRate ~ ., data = Highway1)

  # Suppress intermediate output with trace = 0
lm.backward <- step(lm.all, direction = "backward", trace = 0)

# There are differences between the models resulting from the two procedures
library(DAAG)
press(lm.forward)
press(lm.backward)

# Loading BodyMeas.txt data
BodyMeasurements <- read.table('BodyMeast.txt',header=T, sep="\t")
head(BodyMeasurements)
dim(BodyMeasurements)
str(BodyMeasurements)
# Divide the data into training and test sets
# set the seed of the pseudo-random number generator using a large integer 
set.seed(94839)
N <- nrow(BodyMeasurements)
n <- sample(N, N*0.9)
BodyTrain <- BodyMeasurements[n,]
BodyTest <- BodyMeasurements[-n,]

# Exploratory analysis on the training/test data
par(mar = c(7, 4, 1, 1) + 0.1)

# Unscaled and scaled side-by-side boxplots of all the variables.
boxplot(BodyTrain[, -25], xaxt = "n")
axis(side = 1, at = 1:24, labels = FALSE)
text(1:24, par("usr")[3] - 9, srt = 45, adj = 1,
     labels = names(BodyTrain[, -25]), xpd = TRUE)

boxplot(scale(BodyTrain[, -25]), xaxt = "n", ylab = "scaled variables")
axis(side = 1, at = 1:24, labels = FALSE)
text(1:24, par("usr")[3] - .5, srt = 45, adj = 1,
     labels = names(BodyTrain[, -25]), xpd = TRUE)

# Correlation matrix plot
require(corrplot)
corrplot(cor(BodyTrain[, -25]), method = "ellipse") # remove gender from display
  # Not surprisingly with body measurements, they are highly positively correlated

# Scatterplot matrix in term of gender males and females in different colours (blue for females, orange for males)
require(lattice) # install this first
splom(~BodyTrain[, -25], groups = Gender, data = BodyTrain, pscales = 0, varname.cex = 0.5)

# Boxplot by gender
boxplot(Weight ~ Gender, data = BodyTrain, names = c("females", "males"))

# Produce separate plots of weight against the other variables and superimpose separate scatterplot smoothers for males and females
for(i in c(1:22, 24)){
  print(
    xyplot(Weight ~ BodyTrain[, i], groups = !as.logical(Gender), data = BodyTrain,
           xlab = names(BodyTrain)[i], type = c("p", "smooth"))
  )
}

# Subset selection
  # Simplest model, for use with forward regression
lm0 <- lm(Weight ~ 1,
          data = BodyTrain)
  # Most complex model
lmall <- lm(Weight ~ ., data = BodyTrain)
summary(lmall)

# Shorten names of variables to make it easier to display tables, etc.
ShortNames <- c("BiaDia", "BiiDia", "BitDia", "CheDep", "CheDia", "ElbDia", "WriDia", "KneDia",
                "AnkDia", "ShoGir", "CheGir", "WaiGir", "NavGir", "HipGir", "ThiGir", "BicGir",
                "ForGir", "KneGir", "CalGir", "AnkGir", "WriGir", "Age", "Height", "Sex")

# All Subsets
require(leaps)
AllSubsets <- regsubsets(Weight ~ .,
                         nvmax = 20,
                         nbest = 1,
                         data = BodyTrain)
AllSubsets.summary <- summary(AllSubsets)
AllSubsets.outmat <- AllSubsets.summary$outmat
colnames(AllSubsets.outmat) <- ShortNames
require(knitr)
kable(AllSubsets.outmat)

# look at the internal measures - adjusted R2, Cp, and BIC.
par(mfrow=c(1,3))
plot(1:20, AllSubsets.summary$adjr2, xlab="subset size", ylab="Adjusted R-squared", type='b')
plot(1:20, AllSubsets.summary$cp, xlab = "subset size", ylab = "Mallows' Cp", type = "b")
plot(1:20, AllSubsets.summary$bic, xlab = "subset size", ylab = "BIC", type = "b")

# It’s a bit hard to see what’s going on with Cp and BIC, so let’s plot them on a log scale
par(mfrow = c(1, 3))
plot(1:20, AllSubsets.summary$adjr2, xlab = "subset size", ylab = "adjusted R-squared", type = "b", log = "y")
plot(1:20, AllSubsets.summary$cp, xlab = "subset size", ylab = "Mallows' Cp", type = "b", log = "y")
plot(1:20, AllSubsets.summary$bic - min(AllSubsets.summary$bic) + 0.1, xlab = "subset size", ylab = "BIC", type = "b", log = "y")

# From these plots, we can see that adjusted R2 is rather inconclusive, Cp suggests models containing 14 variables, 
# whereas because BIC is a very harsh penalty, it appears to suggest a model with only 13 variables. 
# See what we get with forward and backward regression
lm.forward <- step(object=lm0,
                   scope= formula(lmall),
                   direction = "forward",
                   trace = 0)
summary(lm.forward)

lm.backward <- step(object=lmall,
                    direction="backward",
                    trace=0)
summary(lm.backward)

require(DAAG)
press(lm.forward)
press(lm.backward)
# Base on the press result and the number of variables in both forward and backward,
# forward model is better with less feature
# Now calculate an RMSEP-like statistic
RMSEP.internal <- sqrt(press(lm.forward)/nrow(BodyTrain))
RMSEP.internal

# Now let's see how this full model performs on the test set!
lmfwd.predtest <- predict(lm.forward, newdata=BodyTest)

RMSEP.fwd <- sqrt(sum((lmfwd.predtest - BodyTest$Weight)^2)/nrow(BodyTest))
RMSEP.fwd
# Not surprisingly, the RMSEP on the test set is slightly higher (2.2) than the internal RMSEP (2.14), but it’s still pretty good!

# Usual diagnostic checking for linear regression models
plot(lm.forward)

test.residuals <- BodyTest$Weight - lmfwd.predtest
plot(lmfwd.predtest, test.residuals, 
     xlab = "test set predictions of weight (kg)",
     ylab = "residuals")
abline(h = 0)