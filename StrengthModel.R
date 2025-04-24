# Strength Model
library(data.table)
library(xgboost)
library(caret)
library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv("Data/strengthMetrics.csv")  

top_features <- c("net_peak_vertical_force_.n._max_imtp", 
                  "body_weight_.lbs.",
                  "best_rsi_.jump_height.contact_time._.m.s._mean_ht",
                  "force_at_200ms_.n._max_imtp",
                  "jump_height_.imp.mom._.cm._mean_sj",
                  "peak_power_.w._mean_sj",
                  "concentric_peak_force_.n._mean_cmj",
                  "eccentric_peak_force_.n._mean_cmj",
                  "jump_height_.imp.mom._.cm._mean_cmj",
                  "peak_power_.w._mean_cmj"
                  )

#Replace all "pitch_speed_mph" to "bat_speed_mph" for hitters model

df <- df %>% 
  select(pitch_speed_mph, all_of(top_features)) %>%
  drop_na() %>%
  filter(pitch_speed_mph >= 65)
#If bat_speed_mph filter to greater than 50
#If pitch_speed_mph filter to greater than 65

# 5. IQR Outlier Filtering (threshold = 2.5)
filter_iqr <- function(data, cols, thresh = 2.5) {
  for (col in cols) {
    Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower <- Q1 - thresh * IQR
    upper <- Q3 + thresh * IQR
    data <- data %>% filter(.data[[col]] >= lower & .data[[col]] <= upper)
  }
  return(data)
}
df <- filter_iqr(df, top_features, thresh = 2.5)

# 6. Feature/Target Split
X <- df %>% select(-pitch_speed_mph)
y <- df$pitch_speed_mph
X_matrix <- as.matrix(X)

# 7. Train/Test Split
set.seed(42)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X_matrix[train_index, ]
X_test <- X_matrix[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# 8. Convert to DMatrix
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# 9. Cross-Validation for Optimal Rounds
params <- list(
  objective = "reg:squarederror",
  eta = 0.01,
  max_depth = 4,
  subsample = 0.8,
  colsample_bytree = 0.7,
  gamma = 1,
  eval_metric = "rmse"
)

cv <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 1000,
  nfold = 5,
  early_stopping_rounds = 25,
  verbose = 0
)
best_nrounds <- cv$best_iteration

# 10. Final Model Training
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds,
  watchlist = list(train = dtrain, test = dtest),
  print_every_n = 10,
  early_stopping_rounds = 25
)

# 11. Predict & Evaluate
y_pred <- predict(xgb_model, newdata = dtest)
r2 <- cor(y_test, y_pred)^2
rmse <- sqrt(mean((y_test - y_pred)^2))

cat("XGBoost Model Performance:\n")
cat("R-squared: ", round(r2, 3), "\n")
cat("RMSE: ", round(rmse, 3), "mph\n")

saveRDS(xgb_model, file = "pitch_speed_model.rds")

# 12. Plot Actual vs Predicted
plot_data <- data.frame(Actual = y_test, Predicted = y_pred)
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "XGBoost Regression: Actual vs. Predicted Pitch Speed",
       x = "Actual Pitch Speed (mph)", 
       y = "Predicted Pitch Speed (mph)") +
  theme_minimal()

# 13. Feature Importance
importance_matrix <- xgb.importance(feature_names = colnames(X_train), model = xgb_model)
print(importance_matrix)
