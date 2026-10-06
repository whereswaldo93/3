raw_data <- read.csv("TimeSeries10000.csv", header = FALSE)
y <- raw_data[[1]]

# Difference the series to make it stationary
z <- diff(y)

# Visual check for stationarity
plot(z, type = "l", col = "darkgreen", 
     main = "Stationary Series after First Differencing (z)",
     xlab = "Time", ylab = "Value")

# Parameter Estimation for ARMA(p=2, q=3)
p_order <- 2
q_order <- 3

# Fit the ARMA(2, 3) model on the stationary data
fitted_model <- arima(z, order = c(p_order, 1, q_order), method = "ML")

# Display Results (Estimating the p + q = 5 parameters)
print(fitted_model)

# Extract individual parameter estimates
ar_coefs <- fitted_model$coef[paste0("ar", 1:p_order)]
ma_coefs <- fitted_model$coef[paste0("ma", 1:q_order)]

cat("(phi_1, phi_2) :", round(ar_coefs, 4), "\n")
cat("(theta_1, theta_2, & theta_3):", round(ma_coefs, 4), "\n")