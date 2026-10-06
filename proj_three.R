raw_data <- read.csv("TimeSeries10000.csv", header = FALSE)
y <- raw_data[[1]]

p <- 2
q <- 3

# Fit the ARIMA model using Maximum Likelihood (ML)
fit_ml <- arima(y, order = c(p, 1, q), method = "ML")

# Fit the same ARIMA model using Conditional Sum-of-Squares (CSS)
fit_css <- arima(y, order = c(p, 1, q), method = "CSS")

# Extract the 5 estimated coefficients directly
# (Order in R: ar1, ar2, ma1, ma2, ma3)
ml_coefs  <- fit_ml$coef[1:5]
css_coefs <- fit_css$coef[1:5]

# Create a table to compare results
comparison_table <- rbind(
  "Maximum Likelihood (ML)"  = round(ml_coefs, 4),
  "Conditional Squares (CSS)" = round(css_coefs, 4)
)

print(comparison_table)