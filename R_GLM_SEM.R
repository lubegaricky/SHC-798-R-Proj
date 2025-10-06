pacman::p_load(tidymodels)
set.seed(123)
n <- 500

freeway_data <- data.frame(
  vehicle_id = 1:n,
  std_speed = abs(rnorm(n, 5, 1.5)),  # speed variability
  gap_var = abs(rnorm(n, 3, 1.2)),   # gap size variation
  lane_density = rnorm(n, 30, 5),
  short_headway = rbinom(n, 1, 0.4),
  speed = rnorm(n, 100, 15),
  accel = rnorm(n, 0.5, 0.2),
  surrounding_gaps = rnorm(n, 2.5, 0.6),
  onramp_distance = runif(n, 50, 300),
  lane_change_freq = rpois(n, lambda = 2)  # target variable
)

str(freeway_data)
glimpse(freeway_data)
summary(freeway_data)

pacman::p_load(lavaan)

model <- '
  # Measurement model
  FI =~ std_speed + gap_var + lane_density
  DU =~ speed + short_headway + accel
  PO =~ surrounding_gaps + onramp_distance

  # Structural model
  lane_change_freq ~ FI + DU + PO
'

fit <- sem(model, data = freeway_data, estimator = "MLM")
summary(fit, fit.measures = TRUE, standardized = TRUE)
