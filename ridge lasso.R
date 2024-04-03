library(caret)
library(glmnet)
#install.packages("mlbench")
library(mlbench)
#install.packages("psych")
library(psych)

data("BostonHousing")
data<- BostonHousing
str(data)
windows(width=10,height=8)
pairs.panels(data[c(-4,-14)],cex=1)
ind <- sample(2,nrow(data),replace=T, prob=c(0.7,0.3))
#create 2 independent samples with replacement
#its like rows are given a value of either 1 or 2, and 
train <- data[ind==1,]
test <- data[ind==2,]

custom <- trainControl(method="repeatedcv", number=10,
                       repeats=5, verboseIter = T)
#training data broken into 10 parts and we repeat it 5 times
#verboseiter means we get to see what happens at each stage
#Cross-Validated (10 fold, repeated 5 times) 9 parts to create the model and 1 part to test

lm<-train(medv~.,train,method='lm',trControl = custom)
lm$results
lm
summary(lm)
plot(lm$finalModel)

ridge <- train(medv~.,train,method="glmnet",
               tuneGrid=expand.grid(alpha=0,lambda= seq(0.0001,1,length=5)),
               trControl=custom)
plot(ridge)
ridge
plot(ridge$finalModel,xvar="lambda",label=T)
plot(ridge$finalModel,xvar='dev',label=T)
plot(varImp(ridge,scale=T))
plot(varImp(ridge,scale=F))
#ridge1 <- train(medv~.,train,method="glmnet",tuneGrid=expand.grid(alpha=0,lambda=seq(0.001,0.6,length=3)),
#                trControl=custom)
#plot(ridge1)
#no change????

#BOTH SHRINKAGE AND FEATURE SELECTION
#LASSO
lasso <- train(medv~.,train,method="glmnet",
               tuneGrid=expand.grid(alpha=1,lambda=seq(0.0001,1,length=5))
               ,trControl=custom)
plot(lasso)
lasso
plot(lasso$finalModel,xvar='lambda',scale=T)
plot(lasso$finalModel,xvar="dev",scale=T)
plot(varImp(lasso,scale=T))

#ELASTIC NET
en <- train(medv~.,train,method="glmnet",
               tuneGrid=expand.grid(alpha=seq(0,1,length=10),lambda=seq(0.0001,1,length=5))
               ,trControl=custom)
plot(en)
plot(en$finalModel,xvar='lambda',scale=T)
plot(en$finalModel,xvar="dev",scale=T)
plot(varImp(en,scale=T))

#COMPARE MODELS
model_list <- list(LinearModel=lm, Ridge=ridge, Lasso=lasso, ElasticNet=en)
res <- resamples(model_list)
summary(res)
bwplot(res)
xyplot(res,metric="RMSE")

en$bestTune
best <- en$finalModel
coef(best,en$bestTune$lambda)

#SAVE FINAL MODEL
saveRDS(en,"final_model.rds")
fm <- readRDS("final_model.rds")
print(fm)

#prediction
p1 <- predict(fm,train)
sqrt(mean(train$medv-p1)^2)
p2<-predict(fm,test)
sqrt(mean(test$medv-p2)^2)

#RMSE of only 0.1 on train data 0.33 on test data