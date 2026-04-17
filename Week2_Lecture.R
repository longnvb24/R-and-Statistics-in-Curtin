n <- 1000 # sample size
e <- rnorm(1000) # irreducible error
x <- seq(n) # feature vector
w <- 50/1000
y <- sin(2*pi*w*x) + 0.5*x + e # true relationship
data.sim <- data.frame(y,x)
str(data.sim)

tr.dat <- data.sim[1:100,] # training data
ts.dat <- data.sim[-c(1:100),] # test data
# visualize using scatterplot
plot(y~x, data = tr.dat, pch=19)

mod.l <- lm(y~x,
            data = tr.dat)
tr.mse.l <- mean((fitted.values(mod.l) - tr.dat$y)^2)
abline(mod.l,col="red")
pr.l <- predict(object  = mod.l,
                newdata = as.data.frame(ts.dat)
                )
ts.mse.l <- mean((pr.l - ts.dat$y)^2)

mod.s <- smooth.spline(y = tr.dat$y,
                      x = tr.dat$x,
                      nknots = 75
                      )
tr.mse.s <- mean((mod.s$y - tr.dat$y)^2)
pr.s <- predict(object=mod.s, x=ts.dat$x)
ts.mse.s <- mean((pr.s$y-ts.dat$y)^2)
data.frame(tr.mse.l, ts.mse.l,tr.mse.s,ts.mse.s)
