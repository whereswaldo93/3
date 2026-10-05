# ==============================================================================
# Script: ARMA(p, q) Estimation for Single-Column Input Data
# ==============================================================================

# 1. Load Single-Column Data
raw_data <- read.csv("your_data.csv")
y <- raw_data[[1]]

# 2. Difference to achieve stationarity
z <- diff(y)

# 3. Model Identification (Check ACF/PACF)
par(mfrow = c(1, 2))
acf(z, main = "ACF of Differenced Series")
pacf(z, main = "PACF of Differenced Series")
par(mfrow = c(1, 1))

# 4. Estimate Parameters (e.g., p = 2, q = 3)
p_order <- 2
q_order <- 3
fitted_model <- arima(z, order = c(p_order, 0, q_order))

# 5. Display Parameter Table
coefs <- fitted_model$coef
se <- sqrt(diag(fitted_model$var.coef))
results <- cbind(Estimate = round(coefs, 4), Std_Error = round(se, 4))

print(results)

# 6. Residual Diagnostics
residuals_data <- residuals(fitted_model)
Box.test(residuals_data, lag = 10, type = "Ljung-Box", fitdf = p_order + q_order)