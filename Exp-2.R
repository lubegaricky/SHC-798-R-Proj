# Engine Displacement vs Highway Fuel Economy Analysis
# Load required libraries
library(ggplot2)
library(dplyr)
library(corrplot)

# Load the mpg dataset (built-in to ggplot2)
data("mpg")

# Display basic information about the dataset
cat("Dataset Overview:\n")
cat("Number of observations:", nrow(mpg), "\n")
cat("Number of variables:", ncol(mpg), "\n")
cat("\nFirst few rows:\n")
print(head(mpg))

# Summary statistics for displacement and highway mpg
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Engine Displacement (displ):\n")
print(summary(mpg$displ))
cat("\nHighway Fuel Economy (hwy):\n")
print(summary(mpg$hwy))

# Calculate correlation coefficient
correlation_pearson <- cor(mpg$displ, mpg$hwy, use = "complete.obs")
correlation_spearman <- cor(mpg$displ, mpg$hwy, method = "spearman", use = "complete.obs")

cat("\n=== CORRELATION ANALYSIS ===\n")
cat("Pearson correlation coefficient:", round(correlation_pearson, 4), "\n")
cat("Spearman correlation coefficient:", round(correlation_spearman, 4), "\n")

# Interpretation of correlation strength
interpret_correlation <- function(r) {
  abs_r <- abs(r)
  if (abs_r >= 0.7) return("Strong")
  else if (abs_r >= 0.3) return("Moderate")
  else return("Weak")
}

cat("Correlation strength:", interpret_correlation(correlation_pearson), "\n")
cat("Direction:", ifelse(correlation_pearson > 0, "Positive", "Negative"), "\n")

# Create basic scatter plot
cat("\n=== CREATING VISUALIZATIONS ===\n")

# Plot 1: Basic scatter plot
plot1 <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(alpha = 0.6, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  labs(
    title = "Engine Displacement vs Highway Fuel Economy",
    subtitle = paste("Pearson r =", round(correlation_pearson, 3)),
    x = "Engine Displacement (L)",
    y = "Highway Fuel Economy (mpg)",
    caption = "Data source: mpg dataset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

print(plot1)

# Plot 2: Scatter plot colored by vehicle class
plot2 <- ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  labs(
    title = "Engine Displacement vs Highway Fuel Economy by Vehicle Class",
    subtitle = paste("Overall correlation r =", round(correlation_pearson, 3)),
    x = "Engine Displacement (L)",
    y = "Highway Fuel Economy (mpg)",
    color = "Vehicle Class"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2))

print(plot2)

# Plot 3: Hexbin plot for density visualization
pacman::p_load(hexbin)
plot3 <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_hex(bins = 15) +
  geom_smooth(method = "lm", se = TRUE, color = "red", size = 1) +
  labs(
    title = "Density Plot: Engine Displacement vs Highway Fuel Economy",
    subtitle = paste("Pearson r =", round(correlation_pearson, 3)),
    x = "Engine Displacement (L)",
    y = "Highway Fuel Economy (mpg)",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_gradient(low = "lightblue", high = "darkblue")

print(plot3)

# Correlation matrix for related variables
cat("\n=== CORRELATION MATRIX ===\n")
# Select numeric variables related to fuel economy
fuel_vars <- mpg %>% 
  select(displ, year, cyl, cty, hwy) %>%
  cor(use = "complete.obs")

print(round(fuel_vars, 3))

# Create correlation matrix plot
corrplot(fuel_vars, 
         method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 0.8,
         tl.col = "black",
         addCoef.col = "black",
         number.cex = 0.7,
         title = "Correlation Matrix: Fuel Economy Variables",
         mar = c(0,0,2,0))

# Statistical significance test
cor_test <- cor.test(mpg$displ, mpg$hwy)
cat("\n=== STATISTICAL SIGNIFICANCE TEST ===\n")
cat("Pearson correlation test:\n")
cor_test
cat("Pearson correlation coefficient:", round(correlation_pearson, 4), "\n")
cat("Spearman correlation coefficient:", round(correlation_spearman, 4), "\n")
cat("t-statistic:", round(cor_test$statistic, 4), "\n")
cat("p-value:", format(cor_test$p.value, scientific = TRUE), "\n")
cat("95% Confidence Interval: [", round(cor_test$conf.int[1], 4), ", ", round(cor_test$conf.int[2], 4), "]\n")
cat("Significance level: ", ifelse(cor_test$p.value < 0.001, "p < 0.001 (highly significant)", 
                                   ifelse(cor_test$p.value < 0.01, "p < 0.01 (significant)", 
                                          ifelse(cor_test$p.value < 0.05, "p < 0.05 (significant)", "not significant"))), "\n")

# Linear regression analysis
cat("\n=== LINEAR REGRESSION ANALYSIS ===\n")
lm_model <- lm(hwy ~ displ, data = mpg)
print(summary(lm_model))

cat("\n=== CONCLUSION ===\n")
cat("Based on the analysis:\n")
cat("1. Correlation coefficient:", round(correlation_pearson, 3), "\n")
cat("2. Relationship strength:", interpret_correlation(correlation_pearson), "\n")
cat("3. Direction: Negative (as displacement increases, highway mpg decreases)\n")
cat("4. Statistical significance: Highly significant (p < 0.001)\n")
cat("5. R-squared:", round(summary(lm_model)$r.squared, 3), "(", round(summary(lm_model)$r.squared * 100, 1), "% of variance explained)\n")


plot(lm_model)

## Residuals vs. Predictor Plot
plot(mpg$displ, lm_model$residuals, xlab="predictor (displ)", ylab="Residuals", pch=20) +
  title("Residuals vs. Predictor displ") +
  lines(loess.smooth(xx,yy),col="red") +
  abline(h=0, col="grey")


# ++++++++++========================++++++++++++++++++++====

# Load required libraries
library(ggplot2)
library(dplyr)

# Load your data file ('housing.RData')
# load("housing.RData")
str(housing)


data_column <- housing$price
data_col <- housing$size

# Method 1: Using base R with density curve
hist(data_col, 
     freq = FALSE,  # Use density instead of frequency
     breaks = 30,   # Adjust number of bins as needed
     main = " Size Histogram with Density Curve",
     xlab = "Value",
     ylab = "Density",
     col = "lightblue",
     border = "black")

# Add density curve
lines(density(data_col, na.rm = TRUE), 
      col = "red", 
      lwd = 2)

# Add normal distribution curve for comparison
x_seq <- seq(min(data_col, na.rm = TRUE), 
             max(data_col, na.rm = TRUE), 
             length.out = 100)
normal_curve <- dnorm(x_seq, 
                      mean = mean(data_col, na.rm = TRUE), 
                      sd = sd(data_col, na.rm = TRUE))
lines(x_seq, normal_curve, 
      col = "blue", 
      lwd = 2, 
      lty = 2)

# Add legend
legend("topright", 
       legend = c("Kernel Density", "Normal Distribution"), 
       col = c("red", "blue"), 
       lwd = 2, 
       lty = c(1, 2))

# Method 2: Using ggplot2 with multiple distribution fits
p <- ggplot(housing, aes(x = size)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30, 
                 fill = "lightblue", 
                 color = "black", 
                 alpha = 0.7) +
  geom_density(color = "red", 
               size = 1.2, 
               alpha = 0.8) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(housing$size, na.rm = TRUE), 
                            sd = sd(housing$size, na.rm = TRUE)),
                color = "blue", 
                size = 1.2, 
                linetype = "dashed") +
  labs(title = "Size Histogram with Fitted Curves",
       x = "Value",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(p)

# Method 3: Fit multiple distributions and compare
# Install fitdistrplus if not already installed

pacman::p_load(fitdistrplus)

# Remove any missing values
clean_data <- data_column[!is.na(data_column)]

# Fit different distributions
fit_normal <- fitdist(clean_data, "norm")
fit_lognormal <- fitdist(clean_data, "lnorm")
fit_gamma <- fitdist(clean_data, "gamma")
fit_weibull <- fitdist(clean_data, "weibull")

# Plot comparison of fits
plot(fit_normal)
plot(fit_lognormal)
plot(fit_gamma)
plot(fit_weibull)

# Compare fits using AIC
aic_comparison <- data.frame(
  Distribution = c("Normal", "Log-Normal", "Weibull"),
  AIC = c(fit_normal$aic, fit_lognormal$aic, fit_weibull$aic)
)
print(aic_comparison)

# Method 4: Advanced ggplot with multiple distribution overlays
p_advanced <- ggplot(housing, aes(x = price)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30, 
                 fill = "lightblue", 
                 color = "black", 
                 alpha = 0.7) +
  geom_density(color = "red", 
               size = 1.2, 
               alpha = 0.8) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(housing$price, na.rm = TRUE), 
                            sd = sd(housing$price, na.rm = TRUE)),
                color = "blue", 
                size = 1) +
  stat_function(fun = function(x) dlnorm(x, 
                                         meanlog = fit_lognormal$estimate[1], 
                                         sdlog = fit_lognormal$estimate[2]),
                color = "green", 
                size = 1) +
  stat_function(fun = function(x) dgamma(x, 
                                         shape = fit_gamma$estimate[1], 
                                         rate = fit_gamma$estimate[2]),
                color = "purple", 
                size = 1) +
  labs(title = "Histogram with Multiple Distribution Fits",
       x = "Value",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


print(p_advanced)

# Summary statistics
pacman::p_load(moments)
summary_stats <- data.frame(
  Statistic = c("Mean", "Median", "SD", "Skewness", "Kurtosis"),
  Value = c(
    mean(clean_data),
    median(clean_data),
    sd(clean_data),
    moments::skewness(clean_data),
    moments::kurtosis(clean_data)
  )
)
print(summary_stats)


# Smoothers
# Data
hour <- 6:18
vehicles <- c(200, 350, 500, 420, 380, 300, 250, 220, 200, 280, 400, 550, 600)
traffic <- data.frame(hour, vehicles)

# Compute smoother using ksmooth
smooth_result <- ksmooth(x = traffic$hour, y = traffic$vehicles,
                         kernel = "box", bandwidth = 1.5,
                         x.points = traffic$hour)

# Plot scatter plot of original data
plot(traffic$hour, traffic$vehicles, type = "p",
     xlab = "Hour of Day", ylab = "Vehicle Count",
     main = "Running Mean Smoother using ksmooth()",
     pch = 16, col = "blue")

# Add smoother line
lines(smooth_result$x, smooth_result$y, col = "red", lwd = 2)
legend("topleft", legend = c("Original Data", "Running Mean Smoother"),
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))


# **********************************_________-----_____________*******
# Create the data frame
hour <- 6:18
vehicles <- c(200, 350, 500, 420, 380, 300, 250, 220, 200, 280, 400, 550, 600)
traffic <- data.frame(hour, vehicles)

# Compute running mean smoother using ksmooth
# Bandwidth = 1 corresponds to a three-hour window (since hours are spaced by 1)
smoothed <- ksmooth(traffic$hour, traffic$vehicles, kernel = "box", bandwidth = 2)

# Create a data frame with smoothed values
traffic_smoothed <- data.frame(hour = smoothed$x, vehicles_smoothed = smoothed$y)

# Plot scatter plot of original data and overlay the smoothed line
plot(traffic$hour, traffic$vehicles, 
     xlab = "Hour", ylab = "Vehicles", 
     main = "Traffic Data with Running Mean Smoother (3-Hour Window)",
     pch = 16, col = "blue")  # Scatter plot
lines(traffic_smoothed$hour, traffic_smoothed$vehicles_smoothed, 
      col = "red", lwd = 2)  # Smoothed line

legend("topright", legend = c("Original Data", "Smoothed"), 
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1))


##########################
# Data
hour <- 6:18
vehicles <- c(200, 350, 500, 420, 380, 300, 250, 220, 200, 280, 400, 550, 600)
traffic <- data.frame(hour, vehicles)

# Gaussian kernel smoother with bandwidth = 2 (standard deviation = 2 hours)
smoothed <- ksmooth(traffic$hour, traffic$vehicles, kernel = "normal", bandwidth = 2, x.points = 6:18)

# Create data frame for smoothed values
traffic_smoothed <- data.frame(hour = smoothed$x, vehicles_smoothed = smoothed$y)

# Plot original data and smoothed curve
plot(traffic$hour, traffic$vehicles, 
     xlab = "Hour", ylab = "Vehicles", 
     main = "Traffic Data with Gaussian Kernel Smoother (σ = 2 hours)",
     pch = 16, col = "blue")
lines(traffic_smoothed$hour, traffic_smoothed$vehicles_smoothed, 
      col = "red", lwd = 2)
legend("topright", legend = c("Original Data", "Gaussian Smoother"), 
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1))

# Print smoothed values for comparison with Excel
print(traffic_smoothed)


#' @@@@@@@@@@@@@@@@@@################
#' # Data
hour <- 6:18
vehicles <- c(200, 350, 500, 420, 380, 300, 250, 220, 200, 280, 400, 550, 600)
traffic <- data.frame(hour, vehicles)

# Gaussian kernel smoothing with sd = 2 hours
gaussian_smooth <- ksmooth(x = traffic$hour,
                           y = traffic$vehicles,
                           kernel = "normal",
                           bandwidth = 2,
                           x.points = traffic$hour)

# Add smoothed values to data
traffic$gaussian_smooth <- gaussian_smooth$y

# Plot
plot(traffic$hour, traffic$vehicles, pch = 16, col = "blue",
     xlab = "Hour", ylab = "Vehicles",
     main = "Gaussian Kernel Smoother (σ = 2)")
lines(traffic$hour, traffic$gaussian_smooth, col = "red", lwd = 2)
legend("topleft", legend = c("Original", "Gaussian Smoother"),
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))

gaussian_smooth
print(gaussian_smooth)


# Validation in R
# Data
hour <- 6:18
vehicles <- c(200, 350, 500, 420, 380, 300, 250, 220, 200, 280, 400, 550, 600)

# Gaussian smoothing with sd = 2
gaussian <- ksmooth(x = hour, y = vehicles,
                    kernel = "normal",
                    bandwidth = 2,
                    x.points = hour)

# Show smoothed value at hour 6
gaussian$y[1]  # Should be close to 338


# OR
hour <- 6:18
vehicles <- c(200, 350, 500, 420, 380, 300, 250, 220, 200, 280, 400, 550, 600)

smoothed <- ksmooth(x = hour, y = vehicles,
                    kernel = "normal",
                    bandwidth = 2,
                    x.points = hour)

# Show value at hour 6
smoothed$y[1]


# OR
# Manual Gaussian weights using dnorm (like ksmooth does)
hour <- 6:18
vehicles <- c(200, 350, 500, 420, 380, 300, 250, 220, 200, 280, 400, 550, 600)

# Kernel center
x0 <- 6
bw <- 2

# Gaussian weights using dnorm
weights <- dnorm(hour - x0, mean = 0, sd = bw)

# Weighted average
smoothed_value <- sum(weights * vehicles) / sum(weights)
smoothed_value


# Validate the solution in R (Kernel Smoother)
# Data
hour <- 6:18
vehicles <- c(200, 350, 500, 420, 380, 300, 250, 220, 200, 280, 400, 550, 600)

# Manual Gaussian smoother (does not have the normalization constant)
gaussian_manual <- function(x0, x, y, h) {
  weights <- exp(-((x - x0)^2) / (2 * h^2))
  smoothed_value <- sum(weights * y) / sum(weights)
  return(smoothed_value)
}

# Validating at x0 = 6, with h = 2
gaussian_manual(x0 = 6, x = hour, y = vehicles, h = 2)



# =================Validating for x0 = 6,7,8,9,10==============
# Data
hour <- 6:18
vehicles <- c(200, 350, 500, 420, 380, 300, 250, 220, 200, 280, 400, 550, 600)

# Checking the Gaussian kernel smoother (does not have the normalization constant)
gaussian_kernel <- function(xi, x, y, h) {
  weights <- exp(-((x - xi)^2) / (2 * h^2))
  sum(weights * y) / sum(weights)
}

# Compute values
xi_values <- 6:10
kernel_values <- sapply(xi_values, function(xi) gaussian_manual(xi, hour, vehicles, h = 2))

# # Validating at xi = 6,7,8,9,& 10 with h = 2
validation <- data.frame(
  xi = xi_values,
  manual = round(kernel_values, 4)
)

print(validation)


# Using the LOESS Smoother
# Data
hour <- 6:18
vehicles <- c(200, 350, 500, 420, 380, 300, 250, 220, 200, 280, 400, 550, 600)
traffic <- data.frame(hour, vehicles)

# Base scatter plot
plot(traffic$hour, traffic$vehicles, pch = 16, col = "black",
     xlab = "Hour", ylab = "Vehicles", main = "LOESS Smoother on Traffic Data")

# Fit and add LOESS smoothers with different span and degree combinations

# 1. LOESS: span = 0.3, degree = 1 (local linear)
loess_fit1 <- loess(vehicles ~ hour, data = traffic, span = 0.3, degree = 1)
lines(traffic$hour, predict(loess_fit1), col = "red", lwd = 2)

# 2. LOESS: span = 0.5, degree = 2 (local quadratic)
loess_fit2 <- loess(vehicles ~ hour, data = traffic, span = 0.5, degree = 2)
lines(traffic$hour, predict(loess_fit2), col = "green", lwd = 2)

# 3. LOESS: span = 0.8, degree = 1
loess_fit3 <- loess(vehicles ~ hour, data = traffic, span = 0.8, degree = 1)
lines(traffic$hour, predict(loess_fit3), col = "purple", lwd = 2)

# Legend
legend("topleft",
       legend = c("Original Data", "LOESS span=0.3, deg=1", "span=0.5, deg=2", "span=0.8, deg=1"),
       col = c("blue", "red", "green", "purple"),
       pch = c(16, NA, NA, NA), lty = c(NA, 1, 1, 1), lwd = c(NA, 2, 2, 2))


# From GroK
# Create the data frame
hour <- 6:18
vehicles <- c(200, 350, 500, 420, 380, 300, 250, 220, 200, 280, 400, 550, 600)
traffic <- data.frame(hour, vehicles)

# Define LOESS smoothers with varying degree and span
loess_1_03 <- loess(vehicles ~ hour, data = traffic, degree = 1, span = 0.3)
loess_1_075 <- loess(vehicles ~ hour, data = traffic, degree = 1, span = 0.75)
loess_2_03 <- loess(vehicles ~ hour, data = traffic, degree = 2, span = 0.3)
loess_2_075 <- loess(vehicles ~ hour, data = traffic, degree = 2, span = 0.75)

# Predict smoothed values at integer hours
hours <- seq(6, 18, by = 0.1)  # Finer grid for smoother curves
pred_1_03 <- predict(loess_1_03, newdata = data.frame(hour = hours))
pred_1_075 <- predict(loess_1_075, newdata = data.frame(hour = hours))
pred_2_03 <- predict(loess_2_03, newdata = data.frame(hour = hours))
pred_2_075 <- predict(loess_2_075, newdata = data.frame(hour = hours))

# Create scatter plot with original data and LOESS smoothers
plot(traffic$hour, traffic$vehicles, 
     xlab = "Hour", ylab = "Vehicles", 
     main = "Fitting with LOESS Smoothers",
     pch = 16, col = "black", ylim = c(150, 650))

# Add LOESS smoother lines
lines(hours, pred_1_03, col = "red", lwd = 2, lty = 1)
lines(hours, pred_1_075, col = "green", lwd = 2, lty = 2)
lines(hours, pred_2_03, col = "orange", lwd = 2, lty = 3)
lines(hours, pred_2_075, col = "blue", lwd = 2, lty = 4)

# Add legend
legend("topright", 
       legend = c("Degree=1, Span=0.3", 
                  "Degree=1, Span=0.75", 
                  "Degree=2, Span=0.3", 
                  "Degree=2, Span=0.75"), 
       col = c("red", "green", "orange", "blue"), 
       pch = c(NA, NA, NA, NA), 
       lty = c(1, 2, 3, 4), 
       lwd = c(2, 2, 2, 2))

