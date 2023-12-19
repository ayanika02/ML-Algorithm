data<-read.csv("C:/Users/Diya/Downloads/Predicting_Salaries.csv")
View(data)
str(data)
head(data)
library(tidyverse)
glimpse(data)
ymean<-mean(data$AnnualSalary)
#Annual Salary is response variable and Years of experience is regressor/predictor variable
xmean<-mean(data$YearsOfExperience)
x2=sum(data$YearsOfExperience^2)
print(x2)
xy=sum(data$YearsOfExperience*data$AnnualSalary)
print(xy)
b1<-(xy-(ymean*xmean*nrow(data)))/(x2-(nrow(data)*xmean^2))
#length(data) returns no. of columns and nrow(data) returns no. of rows
print(b1)
b0<-ymean-(b1*xmean)
print(b0)
#linear equation stands at y = 26970.37 + 9272.426x
#install.packages('caTools')
library(caTools)
set.seed(123)
split<-sample.split(data$AnnualSalary, SplitRatio = 3/4)
split
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)
# Linear regression starts
linear<-lm(formula = AnnualSalary ~ YearsOfExperience, data=train_data)
summary(linear)
y_pred=predict(linear,newdata=train_data)
summary(y_pred)
#Visualization starts
library(ggplot2)
install.packages('scales')
library(scales)
ggplot()+
  geom_point(aes(x=train_data$YearsOfExperience, y=train_data$AnnualSalary),
  color='red') + 
  geom_line(aes(x=train_data$YearsOfExperience, y=predict(linear,newdata = train_data )),
  color='navy') +
  ggtitle("Annual Salaries of Data Scientists VS Experience in Years (Training Set") +
    xlab("Years of Experience") +
    ylab("Annual Salary") +
  scale_x_continuous(limits=c(0,12)) +
  scale_y_continuous((limits=c(0,150000)))

ggplot()+
  geom_point(aes(x=test_data$YearsOfExperience, y=test_data$AnnualSalary),
             color='pink') + 
  geom_line(aes(x=train_data$YearsOfExperience, y=predict(linear,newdata = train_data )),
            color='green') +
  ggtitle("Annual Salaries of Data Scientists VS Experience in Years (Testing Set") +
  xlab("Years of Experience") +
  ylab("Annual Salary") +
  scale_x_continuous(limits=c(0,12)) +
  scale_y_continuous((limits=c(0,150000)))
  