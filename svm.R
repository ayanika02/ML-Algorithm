#install.packages('caret')
library(caret)
library(ggplot2)
heart <- read.csv("C:/Users/Diya/Downloads/heart.csv")
str(heart)
head(heart)
 
#Slicing data
set.seed(369)
intrain <- createDataPartition(y= heart$target, p=0.7, list=FALSE)
training <- heart[intrain,]
testing <- heart[-intrain,]
plot(heart)
dim(training)
dim(testing)
anyNA(heart)
#converting to categorical data
training[["target"]]=factor(training[["target"]])
trctrl= trainControl(method="repeatedcv", number=10,repeats=3, verboseIter = TRUE, savePredictions = TRUE)
svm_linear <- train(target~., data=training, method='svmLinear', trControl= trctrl, preProcess=c("center","scale"),tuneLength=10)
test_pred <- predict(svm_linear, newdata=testing)
confusionMatrix(table(test_pred,testing$target))
#hyperparameter tuning
grid <- expand.grid(C=c(seq(0.0,5,length.out=5)))
svmlineargrid <- train(target~., data=training, method="svmLinear", trControl=trctrl, preProcess=c("center","scale"),tuneGrid=grid, tuneLength=10)
plot(svmlineargrid)
test_pred_grid <- predict(svmlineargrid, newdata= testing)
test_pred_grid
confusionMatrix(table(test_pred_grid,testing$target))
