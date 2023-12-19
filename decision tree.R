#Prepare Data

data <- read.csv("C:/Users/Diya/Downloads/Cardiotocographic.csv")
View(data)
str(data)
head(data,5)
data$NSPF <- factor(data$NSP)
head(data,5)

#Partition Data

library(caTools)
set.seed(369)
#install.packages("rsample")
library(rsample)
split<-initial_split(data, prop=0.75)
train_card <- training(split)
test_card <- testing(split)

#Decision tree with part

#install.packages("party")
library(party)
#tree <- ctree(NSPF~LB+AC+FM, data=train_card)
tree <- ctree(NSPF~LB+AC+FM, data=train_card, controls=ctree_control(mincriterion=0.99,minsplit=300))
#0.9= 90% confidence interval that the variable is significant
#minisplit= branch will split into 2 only when a sample size is at least 200
tree
plot(tree)
predict(tree,test_card,type='prob')
#in the output, the 1st number signifies normal, suspect, pathological
predict(tree,test_card)
#just gives which class a patient belongs to

#Decision tree with rpart
#install.packages("rpart")
library(rpart)
tree1 <- rpart(NSPF ~ LB+AC+FM,train_card)
tree1
#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(tree1)
rpart.plot(tree1,extra=2)
predict(tree1,test_card)

#Misclassification error for train dataset
tab <- table(predict(tree),train_card$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)
#Misclassification erorr is about 18% 

#Misclassification using validate data
testPred <- predict(tree,newdata=test_card)
tab <- table(testPred,test_card$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)
#Misclassification error is around 21%