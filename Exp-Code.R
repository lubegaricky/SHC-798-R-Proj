# library(ggplot2)
# library(dplyr)
library(patchwork)


# Boxplot for city fuel efficiency by cylinders
p1 <- ggplot(mpg, aes(x = factor(cyl), y = cty)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "City Fuel Efficiency by Number of Cylinders",
       x = "Number of Cylinders",
       y = "City mpg") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot for highway fuel efficiency by cylinders
p2 <- ggplot(mpg, aes(x = factor(cyl), y = hwy)) +
  geom_boxplot(fill = "lightcoral", alpha = 0.7) +
  labs(title = "Highway Fuel Efficiency by Number of Cylinders",
       x = "Number of Cylinders",
       y = "Highway mpg") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Display both plots
p1
p2
p1 + p2


# Side-by-side comparison using faceting
library(tidyr)
mpg_long <- mpg %>%
  select(cyl, cty, hwy) %>%
  pivot_longer(cols = c(cty, hwy), names_to = "fuel_econ", values_to = "mpg")

ggplot(mpg_long, aes(x = factor(cyl), y = mpg, fill = fuel_econ)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Fuel Efficiency by Number of Cylinders",
       x = "Number of Cylinders",
       y = "miles per gallon",
       fill = "Fuel Economy") +
  scale_fill_manual(values = c("cty" = "lightblue", "hwy" = "lightcoral"),
                    labels = c("City", "Highway")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




# ---------- Others ----------
# Load ggplot2 for the mpg dataset
# library(ggplot2)

# Set up a 1x2 plot layout for side-by-side boxplots
par(mfrow = c(1, 2))

# Boxplot for city MPG by cylinders
boxplot(cty ~ cyl, data = mpg, 
        main = "City mpg by Number of Cylinders", 
        xlab = "Cylinders", 
        ylab = "City mpg (miles per gallon)")

# Boxplot for highway MPG by cylinders
boxplot(hwy ~ cyl, data = mpg, 
        main = "Highway mpg by Number of Cylinders", 
        xlab = "Cylinders", 
        ylab = "Highway mpg (miles per gallon)")

# Reset plot layout to default
par(mfrow = c(1, 1))

# correlations
# Calculate correlation coefficients
cor_cty <- cor(mpg$cyl, mpg$cty)
cor_hwy <- cor(mpg$cyl, mpg$hwy)

print(paste("City mpg correlation with cylinders:", round(cor_cty, 4)))
print(paste("Highway mpg correlation with cylinders:", round(cor_hwy, 4)))

# You can also get more detailed statistics
summary(lm(cty ~ cyl, data = mpg))
summary(lm(hwy ~ cyl, data = mpg))


# Median Values
# Get actual median values by cylinder count
mpg %>%
  group_by(cyl) %>%
  summarise(
    median_cty = median(cty),
    median_hwy = median(hwy),
    .groups = 'drop'
  )


# Correlation X
# Load required library
# library(ggplot2)

# Load the mpg dataset
# data(mpg)

# Create a scatter plot of displ vs hwy
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  labs(title = "Scatter Plot of Engine Displacement vs Highway Fuel Economy",
       x = "Engine Displacement (liters)",
       y = "Highway Fuel Economy (mpg)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate the Pearson correlation coefficient
correlation <- cor(mpg$displ, mpg$hwy, method = "pearson")
cat("Pearson correlation coefficient:", correlation, "\n")

# Optional: Test the significance of the correlation
cor_test <- cor.test(mpg$displ, mpg$hwy, method = "pearson")
print(cor_test)

# SSD and Speed
# library(ggplot2)
ggplot(cars, aes(speed, dist)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(title = "Linear Model Underpredicts at Extremes",
       x = "Speed (mph)", y = "Stopping Distance (ft)")

# Histogram plot

# Load necessary packages
library(ggplot2)

# Load the R data file (it mayb be a .RData or .rda file)
load(file.choose())  # Choose the file interactively

# Here, our loaded data is a data frame named `housing` with a variable `x`
# If unsure of the name, use ls() after loading to check what was loaded

# For 'price' in the dataset
ggplot(housing, aes(x = price)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red", size = 1.2) +
  labs(title = "Price Histogram with Fitted Density Curve",
       x = "Value", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# For 'size' in the dataset
ggplot(housing, aes(x = size)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red", size = 1.2) +
  labs(title = "Size Histogram with Fitted Density Curve",
       x = "Value", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# For base functions

# Price histogram with density
hist(housing$price, probability = TRUE, col = "lightgray",
     main = "Price Histogram with Density Curve", xlab = "Value")
# Add density curve
lines(density(housing$price), col = "blue", lwd = 2)


# Size histogram with density
hist(housing$size, probability = TRUE, col = "lightgray",
     main = "Price Histogram with Density Curve", xlab = "Value")
# Add density curve
lines(density(housing$size), col = "red", lwd = 2)


