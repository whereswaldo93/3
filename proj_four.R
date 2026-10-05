# Project 4: Predicting Median Value of Boston Housing Prices
# Using Linear Regression, Random Forest, ARMA and Logistic Regression


# 1. Install and load necessary libraries
install.packages(c("ISLR2", "randomForest"))
library(ISLR2)
library(randomForest)


# 2. Access the data
data("Boston")

# 3. Train-test split (80% train, 20% test)
set.seed(42)
train_idx <- sample(1:nrow(Boston), 0.8 * nrow(Boston))
train_data <- Boston[train_idx, ]
test_data  <- Boston[-train_idx, ]

# 4. Fit two different models
lin_model <- lm(medv ~ ., data = train_data)                          # Linear Regression
rf_model <- randomForest(medv ~ ., data = train_data, ntree = 500) # Random Forest
log_model <- glm(medv > median(medv) ~ ., data = train_data, family = binomial) # Logistic Regression
arma_model <- arima(train_data$medv, order = c(1, 0, 1)) # ARMA model

# 5. Make predictions on test set
lin_preds <- predict(lin_model, newdata = test_data)
rf_preds <- predict(rf_model, newdata = test_data)
arma_preds <- predict(arma_model, n.ahead = nrow(test_data))$pred
log_preds <- predict(log_model, newdata = test_data, type = "response")

# 6. Compare Root Mean Squared Error (RMSE)
calc_rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

cat("Linear Regression RMSE:", calc_rmse(test_data$medv, lin_preds), "\n")
cat("Random Forest RMSE:", calc_rmse(test_data$medv, rf_preds), "\n")
cat("ARMA RMSE:", calc_rmse(test_data$medv, arma_preds), "\n")
log_preds <- predict(log_model, newdata = test_data, type = "response")
cat("Logistic Regression RMSE:", calc_rmse(test_data$medv > median(test_data$medv), log_preds), "\n")