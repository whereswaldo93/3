# 1. Load libraries
library(ISLR2)
library(randomForest)
library(rpart) # Built-in decision trees

# 2. Access data & split (80/20)
data("Boston")
set.seed(42)
train_idx <- sample(1:nrow(Boston), 0.8 * nrow(Boston))
train_data <- Boston[train_idx, ]
test_data  <- Boston[-train_idx, ]

# 3. Fit Regression Models
lm_model <- lm(medv ~ ., data = train_data)                          # Linear Regression
dt_model <- rpart(medv ~ ., data = train_data)                       # Decision Tree
rf_model <- randomForest(medv ~ ., data = train_data, ntree = 500) # Random Forest

# 4. Generate Predictions
results <- data.frame(
  Actual = test_data$medv,
  Linear = predict(lm_model, test_data),
  Tree   = predict(dt_model, test_data),
  RF     = predict(rf_model, test_data)
)

# 5. Calculate RMSE across models
rmse <- function(act, pred) sqrt(mean((act - pred)^2))
sapply(results[-1], function(p) rmse(results$Actual, p))

# Plot Actual vs. Predicted values
par(mfrow = c(1, 3)) # Arrange plots side-by-side

plot(results$Actual, results$Linear, main="Linear Regression", 
     xlab="Actual Price ($1k)", ylab="Predicted Price ($1k)", col="blue", pch=16)
abline(0, 1, col="red", lwd=2)

plot(results$Actual, results$Tree, main="Decision Tree", 
     xlab="Actual Price ($1k)", ylab="Predicted Price ($1k)", col="green", pch=16)
abline(0, 1, col="red", lwd=2)

plot(results$Actual, results$RF, main="Random Forest", 
     xlab="Actual Price ($1k)", ylab="Predicted Price ($1k)", col="purple", pch=16)
abline(0, 1, col="red", lwd=2)

par(mfrow = c(1, 1)) # Reset plot layout