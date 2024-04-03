library(caret)
library(randomForest)
#install.packages("randomForest")
#install.packages("e1071")
#install.packages("corrplot")
library(e1071)
library("corrplot")

data <- read.csv("C:/Users/Diya/OneDrive/Documents/MalariaFinal.csv")
no_null <- na.omit(data)
m<- cor(no_null)
corrplot(m,method="color")
cor_df <- sort(as.vector(m), decreasing = TRUE)
# Step 3: Data Splitting
set.seed(123)  # for reproducibility
train_data <- subset(data, Year < 2021)
test_data <- subset(data, Year == 2021)

# Step 2: Feature Selection
features <- c('Pf_WB', 'Pv_WB', 'ABER_WB', 'SPR_WB', 'Pf%_WB', 'Pv%_WB', 'Death_WB', 'API_WB', 'Total_WB')
# Assuming your data frame is named 'data'
for (col_name in colnames(data)) {
  if ('%' %in% col_name) {
    new_col_name <- gsub('%', 'Percent', col_name)  # Replace '%' with 'Percent'
    colnames(data)[colnames(data) == col_name] <- new_col_name
  }
}

# Now the columns with '%' in their names are modified

# Step 4: Choose Models
models <- list(
  "Multiple Linear Regression" = lm(Total_WB ~ ., data = train_data),
  "Polynomial Regression" = lm(Total_WB ~ poly(Pf_WB, degree = 2) + poly(Pv_WB, degree = 2) + poly(ABER_WB, degree = 2) + poly(SPR_WB, degree = 2) + poly(Pf._WB, degree = 2) + poly(Pv._WB, degree = 2) + poly(Death_WB, degree = 2) + poly(API_WB, degree = 2), data = train_data),
  "Decision Tree" = train(Total_WB ~ ., data = train_data, method = "rpart"),
  "Random Forest" = randomForest(Total_WB ~ ., data = train_data),
  "Support Vector Machine" = svm(Total_WB ~ ., data = train_data)
)


# Step 5: Training the Models
for (model_name in names(models)) {
  train(model_name, models[[model_name]])
}

# Step 7: Evaluation
predictions <- lapply(models, function(model) {
  predict(model, newdata = test_data)
})

# Evaluate performance for each model
for (i in seq_along(models)) {
  model_name <- names(models)[i]
  mae <- mean(abs(predictions[[i]] - test_data$Total_WB))
  cat(paste(model_name, "MAE:", mae), "\n")
}

# Step 8: Prediction for 2022
features_2022 <- test_data[features[-length(features)]]
predictions_2022 <- lapply(models, function(model) {
  predict(model, newdata = features_2022)
})

# Display predictions for 2022
for (i in seq_along(models)) {
  model_name <- names(models)[i]
  cat(paste(model_name, "Predicted Total_WB for 2022:", predictions_2022[[i]][1]), "\n")
}
