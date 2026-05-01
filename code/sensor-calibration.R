library(readxl)
library(dplyr)
library(hms)
library(ggplot2)
library(GGally)
library(gridExtra)
library(glmnet)
library(mgcv)

##### Data Collection/Prep #####

# Data can be found here 
# https://archive.ics.uci.edu/dataset/360/air+quality

data <- read_excel("5160/ProjectAirQuality/air+quality/AirQualityUCI.xlsx")
head(data) 
summary(data) 
str(data)

data_clean <- data %>%
  mutate(across(where(is.numeric), ~ na_if(., -200))) %>%
  mutate(
    DateTime = as.POSIXct(Date) + as.numeric(as_hms(Time))
  ) %>%
  arrange(DateTime) %>%
  mutate(
    hour = as.numeric(format(DateTime, "%H")),
    day = as.numeric(format(DateTime, "%d")),
    month = as.numeric(format(DateTime, "%m")),
    time_index = as.numeric(difftime(DateTime, min(DateTime), units = "hours")),
    
    #cyclical time features
    hour_sin = sin(2 * pi * hour / 24),
    hour_cos = cos(2 * pi * hour / 24)
  )

summary(data_clean)

##### Data Exploration #####

ggpairs(data_clean[, -c(1, 2, 13, 14, 15, 16)])

### Distributions

p1 <- ggplot(data_clean, aes(x = `CO(GT)`)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of CO (Ground Truth)")

p2 <- ggplot(data_clean, aes(x = `NMHC(GT)`)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of NMHC (Ground Truth)")

p3 <- ggplot(data_clean, aes(x = `NOx(GT)`)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of NOx (Ground Truth)")

p4 <- ggplot(data_clean, aes(x = `NO2(GT)`)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of NO2 (Ground Truth)")

grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

### Sensors vs. GT

p5 <- ggplot(data_clean, aes(x = `PT08.S1(CO)`, y = `CO(GT)`)) +
  geom_point(alpha = 0.4) +
  geom_smooth(color = "red") +
  theme_minimal() +
  labs(title = "Raw Sensor vs CO Ground Truth")

p6 <- ggplot(data_clean, aes(x = `PT08.S2(NMHC)`, y = `NMHC(GT)`)) +
  geom_point(alpha = 0.4) +
  geom_smooth(color = "red") +
  theme_minimal() +
  labs(title = "Raw Sensor vs NMHC Ground Truth")

p7 <- ggplot(data_clean, aes(x = `PT08.S3(NOx)`, y = `NOx(GT)`)) +
  geom_point(alpha = 0.4) +
  geom_smooth(color = "red") +
  theme_minimal() +
  labs(title = "Raw Sensor vs NOx Ground Truth")

p8 <- ggplot(data_clean, aes(x = `PT08.S4(NO2)`, y = `NO2(GT)`)) +
  geom_point(alpha = 0.4) +
  geom_smooth(color = "red") +
  theme_minimal() +
  labs(title = "Raw Sensor vs NO2 Ground Truth")

grid.arrange(p5, p6, p7, p8, nrow = 2, ncol = 2)

### Temperature vs. GT

p9 <- ggplot(data_clean, aes(x = T, y = `CO(GT)`)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  theme_minimal() +
  labs(title = "CO vs Temperature")

p10 <- ggplot(data_clean, aes(x = T, y = `NMHC(GT)`)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  theme_minimal() +
  labs(title = "NMHC vs Temperature")

p11 <- ggplot(data_clean, aes(x = T, y = `NOx(GT)`)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  theme_minimal() +
  labs(title = "NOx vs Temperature")

p12 <- ggplot(data_clean, aes(x = T, y = `NO2(GT)`)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  theme_minimal() +
  labs(title = "NO2 vs Temperature")

grid.arrange(p9, p10, p11, p12, nrow = 2, ncol = 2)

### Humidity vs. GT

p13 <- ggplot(data_clean, aes(x = RH, y = `CO(GT)`)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  theme_minimal() +
  labs(title = "CO vs Humidity")

p14 <- ggplot(data_clean, aes(x = RH, y = `NMHC(GT)`)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  theme_minimal() +
  labs(title = "NMHC vs Humidity")

p15 <- ggplot(data_clean, aes(x = RH, y = `NOx(GT)`)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  theme_minimal() +
  labs(title = "NOx vs Humidity")

p16 <- ggplot(data_clean, aes(x = RH, y = `NO2(GT)`)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  theme_minimal() +
  labs(title = "NO2 vs Humidity")

grid.arrange(p13, p14, p15, p16, nrow = 2, ncol = 2)

### Date vs. GT

p17 <- ggplot(data_clean, aes(x = DateTime, y = `CO(GT)`)) +
  geom_line(alpha = 0.6) +
  theme_minimal() +
  labs(title = "CO over Time")

p18 <- ggplot(data_clean, aes(x = DateTime, y = `NMHC(GT)`)) +
  geom_line(alpha = 0.6) +
  theme_minimal() +
  labs(title = "NMHC over Time")

p19 <- ggplot(data_clean, aes(x = DateTime, y = `NOx(GT)`)) +
  geom_line(alpha = 0.6) +
  theme_minimal() +
  labs(title = "NOx over Time")

p20 <- ggplot(data_clean, aes(x = DateTime, y = `NO2(GT)`)) +
  geom_line(alpha = 0.6) +
  theme_minimal() +
  labs(title = "NO2 over Time")

grid.arrange(p17, p18, p19, p20, nrow = 2, ncol = 2)

### Usable data 

m1 <- ggplot(data_clean, aes(x = DateTime, y = is.na(`CO(GT)`))) +
  geom_point(alpha = 0.3) +
  theme_minimal() +
  labs(title = "Missingness of CO(GT)", y = "Missing")

m2 <- ggplot(data_clean, aes(x = DateTime, y = is.na(`NMHC(GT)`))) +
  geom_point(alpha = 0.3) +
  theme_minimal() +
  labs(title = "Missingness of NMHC(GT)", y = "Missing")

m3 <- ggplot(data_clean, aes(x = DateTime, y = is.na(`NOx(GT)`))) +
  geom_point(alpha = 0.3) +
  theme_minimal() +
  labs(title = "Missingness of NOx(GT)", y = "Missing")

m4 <- ggplot(data_clean, aes(x = DateTime, y = is.na(`NO2(GT)`))) +
  geom_point(alpha = 0.3) +
  theme_minimal() +
  labs(title = "Missingness of NO2(GT)", y = "Missing")

grid.arrange(m1, m2, m3, m4, nrow = 2, ncol = 2)
# Thus NMHC may not be usable as response

usable <- data_clean %>%
  filter(!is.na(`CO(GT)`), !is.na(`PT08.S1(CO)`))

nrow(usable) / nrow(data_clean)

ggplot() +
  geom_histogram(data = data_clean, aes(x = DateTime), fill = "gray", bins = 100, alpha = 0.5) +
  geom_histogram(data = usable, aes(x = DateTime), fill = "red", bins = 100, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Visual of usable data")

### Check bias

ggplot() +
  geom_density(data = data_clean, aes(x = `PT08.S1(CO)`), fill = "gray", alpha = 0.4) +
  geom_density(data = usable,     aes(x = `PT08.S1(CO)`), color = "red") +
  theme_minimal() + labs(title = "PT08.S1(CO) density: all data (gray) vs usable (red)")


##### 1st Modeling #####

# start with CO and consider other gases later

model_data <- data_clean %>%
  select(
    `CO(GT)`,
    `PT08.S1(CO)`,
    `PT08.S2(NMHC)`,
    `PT08.S3(NOx)`,
    `PT08.S4(NO2)`,
    `PT08.S5(O3)`,
    T, RH, AH,
    DateTime,
    hour, 
    time_index, 
    hour_sin, hour_cos
  ) %>%
  na.omit() %>% 
  rename(
    CO_GT = `CO(GT)`, 
    S1_CO = `PT08.S1(CO)`, 
    S2_NMHC = `PT08.S2(NMHC)`, 
    S3_NOx = `PT08.S3(NOx)`, 
    S4_NO2 = `PT08.S4(NO2)`, 
    S5_O3 = `PT08.S5(O3)`
  )
# For future endeavors I now know to rename things with symbols in name before starting eda and modeling
# as GAM was treating `CO(GT)` as a function CO() and not working properly so at the end i had to come 
# back and rename() 


### 70/30 split (chronological)

split_index <- floor(0.7 * nrow(model_data))

train_data <- model_data[1:split_index, ]
test_data  <- model_data[(split_index + 1):nrow(model_data), ]

x_train_raw <- model.matrix(CO_GT ~ . - DateTime, train_data)[, -1]
x_test_raw <- model.matrix(CO_GT ~ . - DateTime, test_data)[, -1]

y_train <- train_data$CO_GT
y_test <- test_data$CO_GT

train_center <- attr(scale(x_train_raw), "scaled:center")
train_scale <- attr(scale(x_train_raw), "scaled:scale")

x_train <- scale(x_train_raw)
x_test <- scale(x_test_raw, center = train_center, scale = train_scale)

### rmse function 

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

### Linear Model without time

lm_CO <- lm(CO_GT ~ . - DateTime - hour - time_index - hour_sin - hour_cos, 
            data = train_data)
summary(lm_CO)
lm_CO_pred <- predict(lm_CO, newdata = test_data)
rmse_simple <- rmse(y_test, lm_CO_pred)

par(mfrow = c(2,2))
plot(lm_CO)
par(mfrow = c(1,1))


test_data$resid_lm <- test_data$'CO_GT' - lm_CO_pred
ggplot(test_data, aes(x = hour, y = resid_lm)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  theme_minimal() +
  labs(
    title = "Residuals of CO Model over Time",
    x = "Time",
    y = "Residuals (Observed - Predicted)"
  )

### Linear Model with Time

lm_CO_time <- lm(CO_GT ~ .- DateTime - hour - time_index, 
                 data = train_data)
lm_CO_time_pred <- predict(lm_CO_time, newdata = test_data)
summary(lm_CO_time)
rmse_lm <- rmse(test_data$'CO_GT', lm_CO_time_pred)


### Ridge
# hour_sin and hour_cos may be distorting due to the penalty

ridge_model <- cv.glmnet(
  x_train, y_train, alpha = 0
)
ridge_model$lambda.min
ridge_pred <- predict(ridge_model, s = "lambda.min", newx = x_test)
rmse_ridge <- rmse(y_test, ridge_pred)

### LASSO
# hour_sin and hour_cos may be distorting due to the penalty

lasso_model <- cv.glmnet(
  x_train,
  y_train,
  alpha = 1
)
lasso_model$lambda.min
lasso_pred <- predict(lasso_model, s = "lambda.min", newx = x_test)
rmse_lasso <- rmse(y_test, lasso_pred)

###

rmse_mean <- rmse(y_test, rep(mean(y_train), length(y_test)))

cat("Mean baseline RMSE:", rmse_mean, "\n")
cat("Linear (no time) RMSE:", rmse_simple, "\n")
cat("Linear + Time RMSE:", rmse_lm, "\n")
cat("Ridge RMSE:", rmse_ridge, "\n")
cat("Lasso RMSE:", rmse_lasso, "\n")

### Baseline Model Linear with Time
baseline_rmse <- rmse(y_test, lm_CO_time_pred)
baseline_rmse

##### 2nd Modeling #####

### PCA
sensor_vars <- model_data %>%
  select(
    S1_CO,
    S2_NMHC,
    S3_NOx,
    S4_NO2,
    S5_O3,
    T, RH, AH
  )

pca <- prcomp(sensor_vars[1:split_index, ], scale. = TRUE)

pca_train <- predict(pca, sensor_vars[1:split_index, ])
pca_test  <- predict(pca, sensor_vars[(split_index+1):nrow(sensor_vars), ])

lm_pca <- lm(y_train ~ pca_train[,1:5])

pca_pred <- cbind(1, pca_test[,1:5]) %*% coef(lm_pca)
rmse_pca <- rmse(y_test, pca_pred)

# Variance explained by each PC
pca_var_explained <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
cat("Variance explained by 5 PCs:", round(pca_var_explained[5], 3), "\n")

### GAM

gam_model <- gam(CO_GT ~ 
                   s(S1_CO) +
                   s(S2_NMHC) + 
                   s(S5_O3) + # drop NO2 due to significance level
                   s(S3_NOx) +
                   s(T) + s(RH) + s(AH) +
                   s(hour, bs = "cc"),
                 data = train_data, 
                 method = "REML", 
                 knots = list(hour = c(0, 24)))


summary(gam_model)
gam.check(gam_model)

gam_pred <- predict(gam_model, newdata = test_data)
rmse_gam <- rmse(y_test, gam_pred)

###

cat("Mean baseline RMSE:", rmse_mean, "\n", 
    "Simple Sensor RMSE:", rmse_simple, "\n", 
    "Linear RMSE:", rmse_lm, "\n", 
    "Ridge RMSE:", rmse_ridge, "\n", 
    "Lasso RMSE:", rmse_lasso, "\n", 
    "PCA RMSE:", rmse_pca, "\n", 
    "GAM RMSE:", rmse_gam, "\n")

### Drift Analysis

best_pred <- lm_CO_time_pred
  
test_data$resid_best <- y_test - as.numeric(best_pred)

ggplot(test_data, aes(x = time_index, y = resid_best)) + 
  geom_point(alpha = 0.3, size = 0.5) + 
  geom_smooth(method = "loess", color = "red") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") + 
  theme_minimal() + 
  labs(title = "Residuals vs. Deployment time", 
       x = "Hours since deployment start", y = "Residual")

# Linear drift test
drift_model <- lm(resid_best ~ time_index, data = test_data)
summary(drift_model)

cat("Drift slope (mg/m3 per hour): ", coef(drift_model)["time_index"], "\n", 
    "Drift per month (~730 hrs): ", coef(drift_model)["time_index"] * 730, "\n")

acf(test_data$resid_best, lag.max = 48, main = "ACF of best model residuals")
pacf(test_data$resid_best, lag.max = 48, main = "PACF of residuals")
# may need to add lag features

test_data$month_label <- format(test_data$DateTime, "%Y-%m")

drift_by_month <- test_data %>%
  group_by(month_label) %>% 
  summarise(
    mean_resid = mean(resid_best, na.rm = TRUE), 
    sd_resid = sd(resid_best, na.rm = TRUE), 
    n = n()
  )

print(drift_by_month)
# we see a seasonal bias

ggplot(drift_by_month, aes(x = month_label, y = mean_resid)) + 
  geom_col(fill = "steelblue") + 
  geom_errorbar(aes(ymin = mean_resid - sd_resid, 
                    ymax = mean_resid + sd_resid), width = 0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_minimal() + 
  labs(title = "Mean residual by month", 
       x = "Month", y = "Mean residual")

### Coefficient stability test (direct drift test)

early <- model_data %>% filter(time_index <= median(time_index))
late <- model_data %>% filter(time_index > median(time_index))

lm_early <- lm(CO_GT ~ S1_CO + S2_NMHC + S3_NOx + 
                 S4_NO2 + S5_O3 + T + RH + AH, data = early)
lm_late <- lm(CO_GT ~ S1_CO + S2_NMHC + S3_NOx + 
                S4_NO2 + S5_O3 + T + RH + AH, data = late)

coef_comparison <- data.frame(
  early = coef(lm_early), 
  late = coef(lm_late), 
  change = coef(lm_late) - coef(lm_early), 
  pct_change = round(100 * (coef(lm_late) - coef(lm_early)) / 
                       abs(coef(lm_early)), 1)
)
print(coef_comparison)

coef_df <- data.frame(
  term = rownames(coef_comparison)[-1], 
  early = coef(lm_early)[-1], 
  late = coef(lm_late)[-1]
) %>% 
  tidyr::pivot_longer(cols = c(early, late), 
                      names_to = "period", 
                      values_to = "estimate") %>% 
  filter(term %in% c("S1_CO", "S2_NMHC", "S3_NOx", "S4_NO2", "S5_O3"))
# exclude T, RH, AH  as the coefficients shift dramatically due to seasonal multicollinearity

ggplot(coef_df, aes(x = term, y = estimate, fill = period)) + 
  geom_col(position = "dodge") + 
  theme_minimal() + 
  labs(title = "Sensor coefficients: Early vs. Late deployment", 
       x = "Predictor", y = "Coefficient estimate", 
       fill = "Period") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### 3rd Modeling #####
# Lag features

model_data_lag <- model_data %>%
  mutate(CO_lag1 = lag(CO_GT, 1)) %>%
  filter(!is.na(CO_lag1))

split_index_lag <- floor(0.7 * nrow(model_data_lag))
train_lag <- model_data_lag[1:split_index_lag, ]
test_lag  <- model_data_lag[(split_index_lag + 1):nrow(model_data_lag), ]
y_test_lag <- test_lag$CO_GT

# linear no time
lm_CO_lag <- lm(CO_GT ~ . - DateTime - hour - time_index - 
                  hour_sin - hour_cos, data = train_lag)

# linear + time
lm_CO_time_lag <- lm(CO_GT ~ . - DateTime - hour - time_index, 
                     data = train_lag)

#Ridge/LASSO
x_train_lag_raw <- model.matrix(CO_GT ~ . - DateTime, train_lag)[, -1]
x_test_lag_raw  <- model.matrix(CO_GT ~ . - DateTime, test_lag)[, -1]

lag_center <- attr(scale(x_train_lag_raw), "scaled:center")
lag_scale  <- attr(scale(x_train_lag_raw), "scaled:scale")
x_train_lag <- scale(x_train_lag_raw)
x_test_lag  <- scale(x_test_lag_raw, center = lag_center, scale = lag_scale)

ridge_lag <- cv.glmnet(x_train_lag, train_lag$CO_GT, alpha = 0)
lasso_lag <- cv.glmnet(x_train_lag, train_lag$CO_GT, alpha = 1)

# PCA
sensor_vars_lag <- model_data_lag %>% 
  select(S1_CO, S2_NMHC, S3_NOx, S4_NO2, S5_O3, T, RH, AH)

pca_lag       <- prcomp(sensor_vars_lag[1:split_index_lag, ], scale. = TRUE)
pca_train_lag <- predict(pca_lag, sensor_vars_lag[1:split_index_lag, ])
pca_test_lag  <- predict(pca_lag, sensor_vars_lag[(split_index_lag+1):nrow(sensor_vars_lag), ])
lm_pca_lag    <- lm(train_lag$CO_GT ~ pca_train_lag[, 1:5])
pca_lag_pred  <- cbind(1, pca_test_lag[, 1:5]) %*% coef(lm_pca_lag)

# GAM
gam_lag <- gam(CO_GT ~ 
                 s(CO_lag1) + 
                 s(S1_CO) + s(S2_NMHC) + s(S3_NOx) + 
                 s(S4_NO2) + s(S5_O3) + 
                 s(T) + s(RH) + s(AH) + 
                 s(hour, bs = "cc"), 
               data = train_lag, method = "REML", 
               knots = list(hour = c(0, 24)))

cat("=== Without Lag===\n", 
    "Mean baseline RMSE:", rmse_mean, "\n", 
    "Simple Sensor RMSE:", rmse_simple, "\n", 
    "Linear RMSE:", rmse_lm, "\n", 
    "Ridge RMSE:", rmse_ridge, "\n", 
    "Lasso RMSE:", rmse_lasso, "\n", 
    "PCA RMSE:", rmse_pca, "\n", 
    "GAM RMSE:", rmse_gam, "\n\n")

cat("=== With Lag1 ===\n", 
    "Linear: ", rmse(y_test_lag, predict(lm_CO_lag,   test_lag)), "\n", 
    "linear + time: ", rmse(y_test_lag, predict(lm_CO_time_lag, test_lag)), "\n", 
    "Ridge: ", rmse(y_test_lag, predict(ridge_lag, s = "lambda.min", newx = x_test_lag)), "\n", 
    "LASSO: ", rmse(y_test_lag, predict(lasso_lag, s = "lambda.min", newx = x_test_lag)), "\n", 
    "PCA: ", rmse(y_test_lag, pca_lag_pred), "\n", 
    "GAM: ", rmse(y_test_lag, predict(gam_lag, test_lag)), "\n")

acf(y_test_lag - predict(lm_CO_lag, test_lag), 
    lag.max = 48, main = "ACF after adding lag1")

### add more lag features

model_data_lag <- model_data %>%
  mutate(
    CO_lag1 = lag(CO_GT, 1),
    CO_lag2 = lag(CO_GT, 2),
    CO_lag3 = lag(CO_GT, 3)
  ) %>%
  filter(!is.na(CO_lag3))

split_index_lag <- floor(0.7 * nrow(model_data_lag))
train_lag <- model_data_lag[1:split_index_lag, ]
test_lag  <- model_data_lag[(split_index_lag + 1):nrow(model_data_lag), ]
y_test_lag <- test_lag$CO_GT

# linear no time
lm_CO_lag <- lm(CO_GT ~ . - DateTime - hour - time_index - 
                  hour_sin - hour_cos, data = train_lag)

# linear + time
lm_CO_time_lag <- lm(CO_GT ~ . - DateTime - hour - time_index, 
                     data = train_lag)

#Ridge/LASSO
x_train_lag_raw <- model.matrix(CO_GT ~ . - DateTime, train_lag)[, -1]
x_test_lag_raw  <- model.matrix(CO_GT ~ . - DateTime, test_lag)[, -1]

lag_center <- attr(scale(x_train_lag_raw), "scaled:center")
lag_scale  <- attr(scale(x_train_lag_raw), "scaled:scale")
x_train_lag <- scale(x_train_lag_raw)
x_test_lag  <- scale(x_test_lag_raw, center = lag_center, scale = lag_scale)

ridge_lag <- cv.glmnet(x_train_lag, train_lag$CO_GT, alpha = 0)
lasso_lag <- cv.glmnet(x_train_lag, train_lag$CO_GT, alpha = 1)

# PCA
sensor_vars_lag <- model_data_lag %>% 
  select(S1_CO, S2_NMHC, S3_NOx, S4_NO2, S5_O3, T, RH, AH)

pca_lag       <- prcomp(sensor_vars_lag[1:split_index_lag, ], scale. = TRUE)
pca_train_lag <- predict(pca_lag, sensor_vars_lag[1:split_index_lag, ])
pca_test_lag  <- predict(pca_lag, sensor_vars_lag[(split_index_lag+1):nrow(sensor_vars_lag), ])
lm_pca_lag    <- lm(train_lag$CO_GT ~ pca_train_lag[, 1:5])
pca_lag_pred  <- cbind(1, pca_test_lag[, 1:5]) %*% coef(lm_pca_lag)

# GAM
gam_lag <- gam(CO_GT ~ 
                 s(CO_lag1) + s(CO_lag2) + s(CO_lag3) + 
                 s(S1_CO) + s(S2_NMHC) + s(S3_NOx) + 
                 s(S4_NO2) + s(S5_O3) + 
                 s(T) + s(RH) + s(AH) + 
                 s(hour, bs = "cc"), 
               data = train_lag, method = "REML", 
               knots = list(hour = c(0, 24)))

cat("=== With 3 Lag features ===\n", 
    "Linear: ", rmse(y_test_lag, predict(lm_CO_lag,   test_lag)), "\n", 
    "linear + time: ", rmse(y_test_lag, predict(lm_CO_time_lag, test_lag)), "\n", 
    "Ridge: ", rmse(y_test_lag, predict(ridge_lag, s = "lambda.min", newx = x_test_lag)), "\n", 
    "LASSO: ", rmse(y_test_lag, predict(lasso_lag, s = "lambda.min", newx = x_test_lag)), "\n", 
    "PCA: ", rmse(y_test_lag, pca_lag_pred), "\n", 
    "GAM: ", rmse(y_test_lag, predict(gam_lag, test_lag)), "\n")

acf(y_test_lag - predict(lm_CO_lag, test_lag), 
    lag.max = 48, main = "ACF after adding lag1, lag2, lag3")

# persistant autocorrelation after 3 lag features. in turn suggests the need for a different, 
# dedicated time series model e.g. ARIMA (future work)




