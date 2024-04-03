library(caret)
library(randomForest)
library(e1071)
library("corrplot")
library(caTools)

data <- read.csv("C:/Users/Diya/OneDrive/Documents/MalariaFinal.csv")
no_null <- na.omit(data)
m<- cor(no_null)
corrplot(m,method="color")

just_wb <- no_null[,2:9]
just_ind <- no_null[,10:16]

n <- sample.split(no_null$Total_WB, SplitRatio = 0.80)
train <- subset(no_null,n==TRUE)
test <- subset(no_null, n==FALSE)

multi_reg <- lm(Total_WB~.,data=no_null)
summary(multi_reg)

#comparing total cases- high linearity with Pf and Pv but Pf and Pv technically make up total cases
n1 <- sample.split(just_wb$Total_WB, SplitRatio = 0.80)
train1 <- subset(no_null,n==TRUE)
test1 <- subset(no_null, n==FALSE)

multi_reg <- lm(Total_WB~.,data=just_wb)
summary(multi_reg)
# Coefficients:
# Estimate Std. Error   t value Pr(>|t|)    
# (Intercept) -2.050e+01  1.020e+02    -0.201    0.859    
# Pf_WB        1.000e+00  7.494e-05 13345.923 5.61e-09 ***
#   Pv_WB        1.000e+00  7.817e-05 12794.510 6.11e-09 ***
#   ABER_WB     -2.486e-01  1.713e-01    -1.451    0.284    
# SPR_WB      -4.423e+00  3.855e+00    -1.147    0.370    
# Pf._WB       2.116e-01  1.017e+00     0.208    0.854    
# Pv._WB       2.194e-01  1.023e+00     0.214    0.850    
# Death_WB    -3.394e-03  1.041e-02    -0.326    0.775    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3649 on 2 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# F-statistic: 1.198e+09 on 7 and 2 DF,  p-value: 8.344e-10

#Comparing deaths in just WB data- no linear regression
n1 <- sample.split(just_wb$Death_WB, SplitRatio = 0.80)
train1 <- subset(no_null,n==TRUE)
test1 <- subset(no_null, n==FALSE)

multi_reg <- lm(Death_WB~.,data=just_wb)
summary(multi_reg)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept) -7296.75    4465.44  -1.634    0.244
# Total_WB      -14.88      45.62  -0.326    0.775
# Pf_WB          14.89      45.63   0.326    0.775
# Pv_WB          14.89      45.63   0.326    0.775
# ABER_WB       -12.81      13.49  -0.949    0.443
# SPR_WB       -292.21     255.65  -1.143    0.371
# Pf._WB         72.80      44.54   1.635    0.244
# Pv._WB         73.80      44.42   1.661    0.239
# 
# Residual standard error: 24.16 on 2 degrees of freedom
# Multiple R-squared:  0.735,	Adjusted R-squared:  -0.1924 
# F-statistic: 0.7926 on 7 and 2 DF,  p-value: 0.6595

n1 <- sample.split(just_wb$Death_WB, SplitRatio = 0.80)
train1 <- subset(no_null,n==TRUE)
test1 <- subset(no_null, n==FALSE)

multi_reg <- lm(Death_WB~Pv._WB,data=just_wb)
summary(multi_reg)
#CANT PREDICT DEATH FROM PF,PV,PF%,PV%

n2 <- sample.split(just_ind$Death_IND, SplitRatio = 3/4)
train2 <- subset(just_ind,n==TRUE)
test2 <- subset(just_ind, n==FALSE)

multi_reg <- lm(Death_IND~Pf_IND,data=just_ind)
summary(multi_reg)
#This at least shows regression
plot(multi_reg)

library(glmnet)
x<- no_null[,!(names(no_null) %in% c("Death_WB","Year"))]
y<-no_null$Death_WB
X <- scale(x)
train_rows <- sample(1:10, .9*10)
x.train <- X[train_rows,]
x.test <- X[-train_rows,]
y.train <- y[train_rows]
y.test <- y[-train_rows]
ridge <- cv.glmnet(x.train,y.train,type.measure="mse",alpha=0,family="gaussian")
#wont work as Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold 
