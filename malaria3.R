library(caret)
library(randomForest)
library(e1071)
library("corrplot")
library(caTools)
library(broom)
#install.packages("FactoMineR")
library("FactoMineR")

data <- read.csv("C:/Users/Diya/OneDrive/Documents/india.csv")
m<- cor(data)
plot(data)
cor(data$Years,(data$Total.Malaria.Cases..Million.*1000000))
corrplot(m,method="color")
colnames(data)[2] ="Population"
colnames(data)[3] = "Total_Cases"
colnames(data)[4]= "Pf"
colnames(data)[5]= "Pf_percent"
colnames(data)[7]= "Deaths"
M<-scale(data)
m1<-cor(M)
corrplot(m1,method="color")
#from the correlation matrix, pf and pf% have very low correlation.why? theyre literally related to each other

split <- sample.split(data$Deaths,SplitRatio = 8/10)
train <- subset(data,split==TRUE)
test <- subset(data,split==FALSE)
multi_reg <- lm(Deaths~.,data=train)
summary(multi_reg)
multi_reg
y_pred=predict(multi_reg,newdata=train)
summary(y_pred)  #didnt show R-adj and F score???  
#adj R is 81%
vif(multi_reg)
#all of them have vif>>>10n except pf_percent

# Extract the response variable (e.g., 'Deaths') and predictors
response_variable <- data$Deaths
predictors <- data[, !(names(data) %in% c("Deaths","Years"))]
scaled_predictors <- scale(predictors)
scaled_data <- data.frame(Deaths = response_variable, scaled_predictors)
split1 <- sample.split(scaled_data$Deaths, SplitRatio = 0.8)
training_data_scaled <- subset(scaled_data, split1 == TRUE)
testing_data_scaled <- subset(scaled_data, split1 == FALSE)
multi_reg <- lm(Deaths~., data=training_data_scaled)
summary(multi_reg)  #adjusted R is 77%, again 82%
vif(multi_reg)
#high correlation between total cases, pf, api
#here intercept has 3 stars??? what does that mean???
y_pred <- predict(multi_reg, newdata=testing_data_scaled)
summary(y_pred)
mse <- (testing_data_scaled$Deaths-y_pred)^2
mean(mse)
rsquared <- 1 - (sum((testing_data_scaled$Deaths - y_pred)^2) / sum((testing_data_scaled$Deaths - mean(testing_data_scaled$Deaths))^2))
rsquared  #testing data is 70% accurate

predictors <- data[, !(names(data) %in% c("Deaths","Years","Pf"))] #do i exclue pf or pf%????
new_data <- data.frame(Deaths=response_variable,predictors)
split1 <- sample.split(new_data$Deaths, SplitRatio = 0.8)
training_data <- subset(new_data, split1 == TRUE)
testing_data <- subset(new_data, split1 == FALSE)
multi_reg <- lm(Deaths~., data=training_data)
summary(multi_reg) #adjusted r=71% when i did with Pf
#adjusted R is 82.22% with Pf_percent
vif(multi_reg)
#total cases and api high corelation

multi_reg <- lm(Deaths~Total_Cases+Pf+API,data=data)
summary(multi_reg)
vif(multi_reg)
#adj R is 73%
#high vif for total cases and api

split1 <- sample.split(data$Deaths,SplitRatio = 8/10)
train1 <- subset(data,split==TRUE)
test1 <- subset(data,split==FALSE)
multi_reg1 <- lm(Deaths~Pf+API,data=data)
summary(multi_reg1)  #adjusted R is 58%
vif(multi_reg1)
#BOTH HAVE LOW VIF
#SO HERE API HAS A HIGH P VALUE BUT IN THE PREVIOUS CASE, API WAS THE BEST VARIABLE????????
y_pred1=predict(multi_reg1,newdata=train1)
summary(y_pred1)
plot(y_pred1)
abline(multi_reg1)

simple_reg <- lm(Deaths~API, data=data)
summary(simple_reg)
#BUT FOR THIS API HAS A VERY LOW P VALUE???????????
#adj R is 48

simple_reg1 <- lm(Deaths~Pf, data=data)
summary(simple_reg1)
#also very less P value
#adj R is 60%

simple_reg <- lm(Deaths~Pf_percent, data=data)
summary(simple_reg) #adj R is 29%

plot(data$Pf,data$Deaths)
plot(data$API, data$Deaths)

modified <- data[-c(1,2)]

library(glmnet)
x<- data[,!(names(data) %in% c("Deaths","Year","Population"))]
y<-data$Deaths
X <- scale(x)
train_rows <- sample(1:28, .8*10)
x.train <- X[train_rows,]
x.test <- X[-train_rows,]
y.train <- y[train_rows]
y.test <- y[-train_rows]
alpha0.fit <- glmnet(x.train,y.train,type.measure="mse",alpha=0,family="gaussian")
alpha0.predicted <- predict(alpha0.fit, s=alpha0.fit$lambda.1se,newx=x.test)
mean((y.test - alpha0.predicted)^2)


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
plot(alpha0.fit)
plot()

intrain <- createDataPartition(y= data$Deaths, p=0.8, list=FALSE)
training <- data[intrain,]
testing <- data[-intrain,]
dim(training)
dim(testing)
trctrl= trainControl(method="repeatedcv", number=10,repeats=3, verboseIter = TRUE, savePredictions = TRUE)
svm_linear <- train(Deaths~., data=training, method='svmLinear', trControl= trctrl, preProcess=c("center","scale"),tuneLength=10)
test_pred <- predict(svm_linear, newdata=testing)
confusionMatrix(table(test_pred,testing$Deaths))
#hyperparameter tuning
grid <- expand.grid(C=c(seq(0.0,5,length.out=5)))
svmlineargrid <- train(Deaths~., data=training, method="svmLinear", trControl=trctrl, preProcess=c("center","scale"),tuneGrid=grid, tuneLength=5)
plot(svmlineargrid)
test_pred_grid <- predict(svmlineargrid, newdata= testing)
test_pred_grid
confusionMatrix(table(data=test_pred_grid,reference=((testing$Deaths))))
#CONFUSION MATRIX KEEPS SHOWING the table must the same classes in the same order
levels(test_pred_grid)
levels(testing$Deaths)


model <- randomForest(Deaths ~ ., data=data, proximity=TRUE)
model
# %variance = 81.19
model <- randomForest(Deaths ~ ., data=data, ntree=1000, proximity=TRUE)
model
# %var explained is 80.85%

pca <- prcomp(data,scale=TRUE)
plot(pca$x[,1],pca$x[,2])
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100)
barplot(pca.var.per)

split1 <- sample.split(data$Deaths, SplitRatio = 0.8)
training_data <- subset(data, split1 == TRUE)
testing_data <- subset(data, split1 == FALSE)
y<-lm(Deaths~Pf^2+API,training_data)
summary(y)
vif(y)

