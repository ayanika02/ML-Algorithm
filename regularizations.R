#install.packages("glmnet")
library(glmnet)
set.seed(369)
n <- 1000 #no. of observations
p <- 5000 #no. of predictors
real_p <- 15 #no. of true parameters
#only 15 will help us predict the outcome, the rest will be random noise
#generate data
x <- matrix(rnorm(n*p),nrow=n,ncol=p)
#we create a matrix x with 1000 rows and 5000 columns
#the values in the matrix come from a standard normal dist (mean=0, sd=1)
y <- apply(x[,1:real_p],1,sum) + rnorm(n)
#we will try to predict y with the data in x
#apply() returns a vector of 1000 values that are the sums of 1st 15 columns in x, since x has 1k rows 
#the 1 inside apply() specifies we want to perform a function on each row of data that we have isloated from x
#sum is the function we want to apply on each row
#once we have the vector of sum, we add a lil bit of noise using the rnorm() function, which in this case returns 1k random values from a SND
#x is a matrix of data that we will use ridge, lasso and elastic-net to predict the values in y

#we make vector of indexes that contains the row numbers of the rows we will use in training set
train_rows <- sample(1:n, .66*n)
#66% of the data will be in training set
x.train <- x[train_rows,]
x.test <- x[-train_rows,]
y.train <- y[train_rows]
y.test <- y[-train_rows]

#alpha=0, ridge reg
alpha0.fit <- cv.glmnet(x.train,y.train,type.measure="mse",alpha=0,family="gaussian")
#cv means cross validation, by default is 10 fold
#we want to use x.train to train y.train
#if we were applying elastic-net reg to logistic regression, we would set type.measure to deviance
#in ridge, alpha=0
#family=gaussian tells me to do linear reg, if i want logistic regression, i would say binomial
#the code i wrote in line 29 will fit a linear reg with a ridge reg penalty using 10 fold CV to find optimal values for lambda

alpha0.predicted <- predict(alpha0.fit, s=alpha0.fit$lambda.1se,newx=x.test)
# lambda.1se is the value for lambda stored in alpha0.fit that resulted in the simplest model
# (i.e., the model with the fewest non-zero parameters) and was within 1 standard error of the lambda that had the smallest sum
#lambda.min would be lambda that resulted in smallest sum
#in the statistical sense, .1se is indistinguishable from .min but it results in a model with fewer parameters

#If we wanted to to specify the lambda that results in the
#model with the minimum cross valdiation error, not a model
#within one SE of of the minimum, we would 
#set 's' to "lambda.min".
#Choice of lambda.1se vs lambda.min boils down to this...
#Statistically speaking, the cross validation error for 
#lambda.1se is indistinguisable from the cross validation error
#for lambda.min, since they are within 1 SE of each other. 
#So we can pick the simpler model without
#much risk of severely hindering the ability to accurately
#predict values for 'y' given values for 'x'.
#All that said, lambda.1se only makes the model simpler when
#alpha != 0, since we need some Lasso regression mixed in
#to remove variables from the model. However, to keep things
#consistant when we compare different alphas, it makes sense
#to use lambda.1se all the time.

mean((y.test - alpha0.predicted)^2)

# alpha = 1, Lasso Regression
alpha1.fit <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=1, family="gaussian")
alpha1.predicted <- predict(alpha1.fit, s=alpha1.fit$lambda.1se, newx=x.test)
mean((y.test - alpha1.predicted)^2)

# alpha = 0.5, a 50/50 mixture of Ridge and Lasso Regression
alpha0.5.fit <- cv.glmnet(x.train, y.train, type.measure="mse",alpha=0.5, family="gaussian")
alpha0.5.predicted <- predict(alpha0.5.fit, s=alpha0.5.fit$lambda.1se, newx=x.test)
mean((y.test - alpha0.5.predicted)^2)

#However, the best thing to do is just try a bunch of different values for alpha rather than guess which one will be best.
fit_list <- list()
for (i in 0:10)
{
  fit.name <- paste0("alpha", i/10)
  fit_list[[fit.name]]<-cv.glmnet(x.train,y.train,type.measure = "mse", alpha=i/10,family="gaussian")
}
#seeing which alpha works the best
results <- data.frame()
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  # Use each model to predict 'y' given the Testing dataset
  predicted <- predict(fit_list[[fit.name]],s=fit_list[[fit.name]]$lambda.1se, newx=x.test)
  # Calculate the Mean Squared Error...
  mse <- mean((y.test - predicted)^2)
  # Store the results
  temp <- data.frame(alpha=i/10, mse=mse, fit.name=fit.name)
  results <- rbind(results, temp)
}
results
