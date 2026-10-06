library(ISLR2)
library(randomForest)
library(rpart)

data("Boston")
n <- nrow(Boston)

# Create containers to store predictions
results <- data.frame(
  Actual = Boston$medv,
  Linear = numeric(n),
  Tree   = numeric(n),
  RF     = numeric(n)
)

# Perform LOOCV Loop
set.seed(42)
for (i in 1:n) {
  train_data <- Boston[-i, ]
  test_data  <- Boston[i, ]
  
  # Fit models
  lm_model <- lm(medv ~ ., data = train_data)
  dt_model <- rpart(medv ~ ., data = train_data)
  rf_model <- randomForest(medv ~ ., data = train_data, ntree = 100) # Reduced ntree for performance
  
  # Predict for the single left-out observation
  results$Linear[i] <- predict(lm_model, test_data)
  results$Tree[i]   <- predict(dt_model, test_data)
  results$RF[i]     <- predict(rf_model, test_data)
}

# Calculate RMSE across models
rmse <- function(act, pred) sqrt(mean((act - pred)^2))
sapply(results[-1], function(p) rmse(results$Actual, p))

# Visualize Actual vs. Predicted values
par(mfrow = c(1, 3))

plot(results$Actual, results$Linear, main="Linear Regression (LOOCV)", 
     xlab="Actual Price ($1k)", ylab="Predicted Price ($1k)", col="blue", pch=16)
abline(0, 1, col="red", lwd=2)

plot(results$Actual, results$Tree, main="Decision Tree (LOOCV)", 
     xlab="Actual Price ($1k)", ylab="Predicted Price ($1k)", col="green", pch=16)
abline(0, 1, col="red", lwd=2)

plot(results$Actual, results$RF, main="Random Forest (LOOCV)", 
     xlab="Actual Price ($1k)", ylab="Predicted Price ($1k)", col="purple", pch=16)
abline(0, 1, col="red", lwd=2)