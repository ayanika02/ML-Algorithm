library(ggplot2)
install.packages("cowplot")
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

set.seed(42)
data.imputed <- rfImpute(hd ~ ., data = data, iter=6)
#impute values for the NAs in the dataset with rfImpute
# hd~. we want hd column to be predicted by all other columns
model <- randomForest(hd ~ ., data=data.imputed, proximity=TRUE)
model
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Healthy"], 
          model$err.rate[,"Unhealthy"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

model <- randomForest(hd ~ ., data=data.imputed, ntree=1000, proximity=TRUE)
model
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Healthy"], 
          model$err.rate[,"Unhealthy"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))
#not too much difference for when n=500 and n=1000

oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(hd ~ ., data=data.imputed, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
min(oob.values)
## find the optimal value for mtry...
which(oob.values == min(oob.values))

model <- randomForest(hd ~ ., 
                      data=data.imputed,
                      ntree=1000, 
                      proximity=TRUE, 
                      mtry=which(oob.values == min(oob.values)))
distance.matrix <- as.dist(1-model$proximity)
mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data.imputed$hd)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")

                                                                                           
                                                                                           
                                                                                           