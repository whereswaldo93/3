# ==============================================================================
# Script: ARMA(2, 3) Parameter Estimation
# Objective: Estimate p+q=5 parameters from data that requires differencing 
#            to achieve stationarity.
# ==============================================================================

set.seed(42)

# 1. Generate Non-Stationary / Cumulative Dataset (Simulating given dataset)
N <- 1000
e <- rnorm(N, mean = 0, sd = 1) # i.i.d. standard Normal noise

# True underlying stationary ARMA(2,3) process (5 parameters: 2 AR + 3 MA)
x <- arima.sim(
  model = list(ar = c(0.5, -0.2), ma = c(0.4, -0.3, 0.1)),
  n = N,
  innov = e
)

# Suppose the provided dataset 'y' is non-stationary (e.g., cumulative sum)
y <- cumsum(x)

# ------------------------------------------------------------------------------
# 2. Check & Apply Differencing to Achieve Stationarity
# ------------------------------------------------------------------------------
# Difference the series to make it stationary
z <- diff(y)

# Visual check for stationarity
plot(z, type = "l", col = "darkgreen", 
     main = "Stationary Series after First Differencing (z)",
     xlab = "Time", ylab = "Value")

# ------------------------------------------------------------------------------
# 3. Parameter Estimation for ARMA(p=2, q=3)
# ------------------------------------------------------------------------------
p_order <- 2
q_order <- 3

# Fit the ARMA(2, 3) model on the differenced, stationary data
fitted_model <- arima(z, order = c(p_order, 0, q_order))

# ------------------------------------------------------------------------------
# 4. Display Results (Estimating the p + q = 5 parameters)
# ------------------------------------------------------------------------------
cat("\n================ ARMA(2,3) ESTIMATION RESULTS ================\n")
print(fitted_model)

# Extract individual parameter estimates
ar_coefs <- fitted_model$coef[paste0("ar", 1:p_order)]
ma_coefs <- fitted_model$coef[paste0("ma", 1:q_order)]

cat("\nEstimated AR Parameters (phi_1, phi_2)    :", round(ar_coefs, 4), "\n")
cat("Estimated MA Parameters (theta_1..theta_3):", round(ma_coefs, 4), "\n")