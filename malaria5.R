data <- read.csv("C:/Users/Diya/OneDrive/Documents/india.csv")
str(data)
colnames(data)[2] ="Population"
colnames(data)[3] = "Total_Cases"
colnames(data)[4]= "Pf"
colnames(data)[5]= "Pf_percent"
colnames(data)[7]= "Deaths"
colnames(data)[9]= "GDP"
split <- sample.split(data$Deaths,SplitRatio = 8/10)
train <- subset(data,split==TRUE)
test <- subset(data,split==FALSE)
lm1 <- lm(Deaths~., data=train)
summary(lm1)
AIC(lm1)
BIC(lm1)
car::vif(lm1)
lm2 <- lm(Deaths~Population+Total_Cases+Pf_percent+API+Rainfall, data=train)
summary(lm2)
vif(lm2)
AIC(lm2)
BIC(lm2)
lm3 <- lm(Deaths~Population+Total_Cases+Pf+API, data=train)
summary(lm3)
AIC(lm3)
BIC(lm3)
vif(lm3)
lm4 <- lm(Deaths~log(Total_Cases+API^2), data=train)
summary(lm4)
AIC(lm4)
BIC(lm4)
vif(lm4)
BIC(lm(Deaths~1,train))

lm5 <- lm(Deaths~log(Pf+API), data=train)
summary(lm5)
AIC(lm5)
lm6<-lm(Total_Cases~Rainfall,data=train)
summary(lm6)
plot(data$API, data$Deaths)
plot(data$Pf, data$Deaths)
plot(data$Pf_percent, data$Deaths)
bwplot(data$Pf_percent,data$Deaths,data=data)

lmr <- lm(Deaths~API+Rainfall, data=train)
summary(lmr)

plot(data$API,data$Deaths)

summary(lm(Deaths~Pf_percent,data=train))
