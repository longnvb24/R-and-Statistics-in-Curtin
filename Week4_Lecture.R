ir <- iris
str(ir)
head(ir)
summary(ir)

# To construct a linear regression model for
# response (Y): Sepal.Length
# feature (X): Petal.Length

plot(y = ir$Sepal.Length,
     x = ir$Petal.Length,
     xlab = "Petals.L",
     ylab = "Sepal.L",
     pch = 19,
     col = ir$Species)
mtext(side = 3, # text on side 3
      line = -1, # text at line 1 below the margin
      adj = 0.05,
      paste("r: ",round(cor(ir$Sepal.Length,
          ir$Petal.Length),2))
      )
N <- nrow(ir)
n <- sample(1:N, size=0.8*N) # sample indices
ir.ts <- ir[-n,] # test data
ir.tr <- ir[n,] # randomly selected rows

# Model fitting
mod.l <- lm(Sepal.Length ~ Petal.Length,
            data = ir.tr)
summary(mod.l)
# Thus the model equation is,
# Y_hat = 4.32 + 0.4 Petal.Length

a_hat <- 4.32
b_hat <- 0.4
se.a <- 0.09 # std.error of the estimator of a_hat
se.b <- 0.02 # std.error of the estimator of b_hat

lcl_95.a <- a_hat - 2 * se.a
ucl_95.a <- a_hat + 2 * se.a

lcl_95.b <- b_hat - 2 * se.b
ucl_95.b <- b_hat + 2 * se.b

CL_95 <- data.frame(lcl_95.a, ucl_95.a, lcl_95.b, ucl_95.b)
n_2 <- nrow(ir.tr) - 2
t_a <- a_hat/se.a
t_b <- b_hat/se.b

t_nu <- qt(0.05, # level of significance under H_0
           df = n_2,
           lower.tail = F)

# Parameters were estimated using the methods of least squares (reference)
# The fitted line indicates that on average the Sepal.Length are 4.3 units (p<0.05,95% CI:[4.14,4.5])
# For unit increase in Petal.length Sepal.Length increases by 0.4 units (p<0.05,95% CI[0.36,0.44])

# Comparing against the theoretical t-value at level of significance 5% (1.86) we find
# that both parameters(t_a=48, t_b=20) are statistically significant

coef(mod.l)
cor(ir.tr$Sepal.Length,ir.tr$Petal.Length)^2
confint(mod.l)

# MSE - training
1/nrow(ir.tr)*sum(ir$Sepal.Length - mod.l$fitted.values)^2
1/(nrow(ir.tr)-2)*sum(ir$Sepal.Length - mod.l$fitted.values)^2

# confident interval of the loine
p.tr <- predict(object = mod.l, # model object
        newdata = ir.tr,
        interval = "confidence"
        )
p.ts <- predict(object = mod.l,
                newdata = ir.ts,
                interval = "prediction")
MSPE <- mean((ir.ts$Sepal.Length-p.ts[,1])^2)
