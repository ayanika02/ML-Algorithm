library(caret)
library(glmnet)
#install.packages("mlbench")
library(mlbench)
#install.packages("psych")
library(psych)
library(caTools) 
library(randomForest)
library(lime)
library(car)

data <- read.csv("C:/Users/Diya/OneDrive/Documents/india.csv")
str(data)
colnames(data)[2] ="Population"
colnames(data)[3] = "Total_Cases"
colnames(data)[4]= "Pf"
colnames(data)[5]= "Pf_percent"
colnames(data)[7]= "Deaths"
head(data)
windows(width=10,height=8)
pairs.panels(data)
ind <- sample(2,nrow(data),replace=T, prob=c(0.8,0.2))
#create 2 independent samples with replacement
#its like rows are given a value of either 1 or 2, and 
train <- data[ind==1,]
test <- data[ind==2,]

custom <- trainControl(method="repeatedcv", number=10,
                       repeats=5, verboseIter = T)
#training data broken into 10 parts and we repeat it 5 times
#verboseiter means we get to see what happens at each stage
#Cross-Validated (10 fold, repeated 5 times) 9 parts to create the model and 1 part to test

lm1<-train(Deaths~.,train,method='lm',trControl = custom)
lm1$results
lm1
summary(lm1)
#vif(lm1)
#AIC(lm)
plot(lm$finalModel)
ridge <- train(Deaths~.,train,method="glmnet",
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
lasso <- train(Deaths~.,train,method="glmnet",
               tuneGrid=expand.grid(alpha=1,lambda=seq(0.0001,1,length=5))
               ,trControl=custom)
plot(lasso)
lasso
vif(ridge)
vif(lasso)
plot(lasso$finalModel,xvar='lambda',scale=T)
plot(lasso$finalModel,xvar="dev",scale=T)
plot(varImp(lasso,scale=T))

#ELASTIC NET
en <- train(Deaths~.,train,method="glmnet",
            tuneGrid=expand.grid(alpha=seq(0,1,length=10),lambda=seq(0.0001,1,length=5))
            ,trControl=custom)
plot(en)
plot(en$finalModel,xvar='lambda',scale=T)
plot(en$finalModel,xvar="dev",scale=T)
plot(varImp(en,scale=T))

#COMPARE MODELS
model_list <- list(Ridge=ridge, Lasso=lasso, ElasticNet=en)
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
sqrt(mean(train$Deaths-p1)^2)
p2<-predict(fm,test)
sqrt(mean(test$Deaths-p2)^2)

data1<-data[,-1:-2]
head(data1)
split<-sample.split(data1,SplitRatio = 0.8)
train <- subset(data1, split == "TRUE") 
test <- subset(data1, split == "FALSE") 
RF = randomForest(x = train[-5],y = train$Deaths,ntree = 500,mtry=2) 
RF 
y_pred = predict(RF, newdata = test[-5])
mse <- mean((test$Deaths - y_pred)^2)
rmse <- sqrt(mse)
mae <- mean(abs(test$Deaths - y_pred))
rsquared <- 1 - (sum((test$Deaths - y_pred)^2) / sum((test$Deaths - mean(test$Deaths))^2))
plot(RF) 
rmse
importance(RF) 
varImpPlot(RF) 

#bagging
ind2 <-sample(2, nrow(data1), replace = T, prob = c(0.7, 0.3))
train2 <- data1[ind2 == 1,]
test2 <- data1[ind2 == 2,]
cvcontrol <- trainControl(method="repeatedcv",number = 10,repeats = 2,
                          allowParallel=TRUE)
set.seed(1234)
bag <- train(Deaths ~ .,data=train2,method="treebag",trControl=cvcontrol,
             importance=TRUE)
plot(varImp(bag))
ba <- predict(bag,  test2)
plot(ba ~ test2$Deaths, main = 'Predicted Vs Actual Deaths - Test data')
sqrt(mean((test2$Deaths - ba)^2))
cor(test2$Deaths, ba) ^2

#RF
forest <- train(Deaths ~ ., 
                data=train2,
                method="rf",
                trControl=cvcontrol,
                importance=TRUE)
plot(varImp(forest))

# Plot, RMSE, R-square
rf <-  predict(forest,  test2)
plot(rf ~ test2$Deaths, main = 'Predicted Vs Actual Deaths - Test data')
sqrt(mean((test2$Deaths - rf)^2))
cor(test2$Deaths, rf) ^2

# Explain predictions
explainer <- lime(test2[1:3,], forest, n_bins = 5)
explanation <- explain( x = test2[1:3,], 
                        explainer = explainer, 
                        n_features = 5)
plot_features(explanation)
plot_explanations(explanation)
# Boosting
#set.seed(1234)
boo <- train(Deaths ~ ., 
             data=train2,
             method="xgbTree", 
             trControl=cvcontrol,
             tuneGrid = expand.grid(nrounds = 500,
                                    max_depth = 3,
                                    eta = 0.2,
                                    gamma = 3.1,
                                    colsample_bytree = 1,
                                    min_child_weight = 1,
                                    subsample = 1))
plot(varImp(boo))

# Plot, RMSE, R-square
bo <-  predict(boo,  test2)
plot(bo ~ test2$Deaths, main = 'Predicted Vs Actual Deaths - Test data')
sqrt(mean((test2$Deaths - bo)^2))
cor(test2$Deaths, bo) ^2
