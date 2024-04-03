library(ggplot2)
#install.packages("cowplot")
library(cowplot)
library(randomForest)
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url, header=FALSE)
colnames(data) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak", # ST depression induced by exercise relative to rest
                    "slope","ca","thal","hd")
str(data)
anyNA(data)
data[data == "?"] <- NA
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$ca <- as.integer(data$ca)
anyNA(data)
summary(data)
str(data)
View(data)
data$ca <- as.factor(data$ca)
data$thal <- as.integer(data$thal) # "thal" also had "?"s in it.
data$thal <- as.factor(data$thal)
data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd)
nrow(data[is.na(data$ca) | is.na(data$thal),])
data[is.na(data$ca) | is.na(data$thal),]
data <- data[!(is.na(data$ca) | is.na(data$thal)),] #removing rows w null values
nrow(data)
