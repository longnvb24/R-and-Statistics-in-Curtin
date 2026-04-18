data()
# explore the Iris data
  # Relationship between Sepal. Length (Y) and Other features
ir <- iris # assignment of data
# initial data checks
head(ir)
str(ir)
?iris
# Exploration data analysis
#   Desciptive statistical features
summary(ir)

# 1. report the means and medians
# 2. report the presence or absence of missing values

# calculate standard deviation of all numeric features
apply(ir, # data
      MARGIN =2, # apply function across columns
      FUN=sd # the function we are interested in
      )

apply(ir[,-5], # data
      MARGIN =2, # apply function across columns
      FUN=sd # the function we are interested in
     )

# visualize the relationship between Sepal. length (Y) and features
#   Scatterplot - for numeric response-feature relationship
plot(x = ir$Sepal.Width, # x-axis: feature
     y = ir$Sepal.Length,# y-axis: resopnse
     pch = 19, # point symbol
     main = "", # plot title
     xlab = "", # x-axis label
     ylab = "", # y-axis label
     col = "red"
     )
pairs(ir[,-5]) # pairwise scatterplot between all numeric features

# It appears that Sepal.Length has linear association with Pedal.Length and Pedal.Width
# Quantify the strength of this association, we use Pearson's correlation coeeficient

C <- cor(ir[,-5], method="pearson")
corrplot(C)
corrplot.mixed(C,upper="number")

# We model Sepal.Length (Y) against Petal.Length(X1) with highest positive association
# Partition data into training and test 
N <- nrow(ir) # population size
n <- sample(seq(N), # population individuals
            size = 0.8*N # size of training sample
            ) # randomly selecting samples
# 1. Each flower has equal chance of selection
# 2. No flower influences the choice of another flower

ir.tr <- ir[n,] # training data
ir.ts <- ir[-n,] # test data

# linear model fitting
mod.l <- lm(Sepal.Length ~ # response (y)
     Petal.Length, # feature(x)
   data = ir.tr # training data
   ) # A simple linear regression model
summary(mod.l)

# The fitted model equation is
# Sepal.Length = 4.3 + 0.41 Petal.length

beta0 <- mean(ir.tr$Sepal.Length) - beta1*mean(ir.tr$Petal.Length)
