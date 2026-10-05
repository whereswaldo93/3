# ==============================================================================
# Simple ARMA(p, q) estimation with differencing (Box-Jenkins workflow)
# ==============================================================================
# Input : a CSV whose first column is the time series
# Output: estimated ARMA parameters with standard errors, and residual check
# ==============================================================================

# ---- USER SETTINGS -----------------------------------------------------------
file_name <- "TimeSeries10000.csv"  # data file (single column)
d <- 1                              # number of times to difference (0 = none)
p <- 2                              # number of AR parameters (phi)
q <- 3                              # number of MA parameters (theta)
# ------------------------------------------------------------------------------

# STEP 1: Load data
# The first column is taken as the series y.
y <- read.csv(file_name)[[1]]

# STEP 2: Differencing
# ARMA models require a stationary series (constant mean/variance over time).
# Differencing, z[t] = y[t] - y[t-1], removes trends / random-walk behavior.
# d = 1 is enough for a linear trend or random walk; d = 2 for a quadratic trend.
# Over-differencing adds unnecessary MA structure, so use the smallest d that works.
# Check: the plot of z should hover around a constant level, and its ACF should
# decay quickly (not stay high for many lags).
z <- if (d > 0) diff(y, differences = d) else y

# STEP 3: Identification (Box-Jenkins)
# Used to choose p and q:
#   - AR(p): PACF cuts off after lag p, ACF tails off.
#   - MA(q): ACF cuts off after lag q, PACF tails off.
#   - ARMA : both tail off gradually.
par(mfrow = c(1, 2))
acf(z,  main = "ACF of stationary series")
pacf(z, main = "PACF of stationary series")
par(mfrow = c(1, 1))

# STEP 4: Estimation
# arima(order = c(p, 0, q)) fits an ARMA(p, q) by maximum likelihood:
#   z[t] - mu = phi1*(z[t-1] - mu) + ... + phip*(z[t-p] - mu)
#               + e[t] + theta1*e[t-1] + ... + thetaq*e[t-q]
# The middle 0 means no further differencing (already done in Step 2).
# Output names: ar1..arp = phi, ma1..maq = theta, intercept = mean mu.
fit <- arima(z, order = c(p, 0, q), method = "ML")

# STEP 5: Parameter table
# Std_Error comes from the diagonal of the estimated covariance matrix.
# |Estimate / Std_Error| > ~2 means the parameter is significantly nonzero;
# if not, the order may be too high (try smaller p or q).
est <- fit$coef
se  <- sqrt(diag(fit$var.coef))
print(round(cbind(Estimate = est, Std_Error = se, t_ratio = est / se), 4))
cat("sigma^2 (noise variance):", round(fit$sigma2, 4), "\n")
cat("AIC:", round(AIC(fit), 2), " (lower is better when comparing different p, q)\n")

# STEP 6: Diagnostic check
# If the model is adequate, residuals are white noise (no autocorrelation).
# Ljung-Box null hypothesis: residuals are uncorrelated.
#   p-value > 0.05 -> fail to reject -> model is adequate.
#   p-value < 0.05 -> leftover structure -> revise p, q, or d and repeat.
# fitdf = p + q corrects the degrees of freedom for the estimated parameters.
print(Box.test(residuals(fit), lag = 10, type = "Ljung-Box", fitdf = p + q))
