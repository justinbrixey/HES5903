#Strength Model
library(data.table)
library(xgboost)
library(caret)
library(ggplot2)

# 1. Load Dataset
df <- fread("Data/strengthMetrics.csv")  # Update file path if needed

# 2. Select Relevant Features
selected_features <- c("pitch_speed_mph",
                       "jump_height_(imp-mom)_[cm]_mean_cmj",
                       "peak_power_[w]_mean_cmj",
                       "rsi-modified_[m/s]_mean_cmj",                                
                       "concentric_peak_force_[n]_mean_cmj",                         
                       "jump_height_(imp-mom)_[cm]_mean_sj",                         
                       "peak_power_[w]_mean_sj",                                     
                       "peak_vertical_force_[n]_max_imtp",                           
                       "force_at_100ms_[n]_max_imtp",                                
                       "force_at_150ms_[n]_max_imtp",                                
                       "force_at_200ms_[n]_max_imtp",                                
                       "body_weight_[lbs]"                  
)

# 3. Filter Data (Remove Missing Values & Exclude Pitch Speeds < 70 mph)
df <- df[, ..selected_features]
df <- df[complete.cases(df) & pitch_speed_mph >= 60]

# 4. Split Data into Features (X) and Target (y)
X <- df[, !"pitch_speed_mph", with = FALSE]  # Remove target variable
y <- df$pitch_speed_mph  # Target variable

# 5. Convert Data to Matrix (Required for XGBoost)
X_matrix <- as.matrix(X)

# 6. Split into Training (70%) and Testing (30%) Sets
set.seed(42)  # For reproducibility
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X_matrix[train_index, ]
X_test <- X_matrix[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# 7. Convert Data to XGBoost DMatrix Format
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# 8. Train XGBoost Model
params <- list(
  objective = "reg:squarederror",  # Regression task
  booster = "gbtree",  # Use tree-based boosting
  eta = 0.1,  # Learning rate
  max_depth = 3,  # Depth of trees
  nrounds = 250,  # Number of boosting rounds
  eval_metric = "rmse"  # Evaluation metric
)

xgb_model <- xgb.train(
  params = params, 
  data = dtrain, 
  nrounds = 250, 
  watchlist = list(train = dtrain, test = dtest),
  print_every_n = 10
)

# 9. Make Predictions on Test Set
y_pred <- predict(xgb_model, newdata = dtest)

# 10. Evaluate Model Performance
r2 <- cor(y_test, y_pred)^2  # R-squared
rmse <- sqrt(mean((y_test - y_pred)^2))  # Root Mean Squared Error

# Print Results
cat("XGBoost Model Performance:\n")
cat("R-squared: ", round(r2, 3), "\n")
cat("RMSE: ", round(rmse, 3), "mph\n")

# 11. Plot Actual vs. Predicted Pitch Speed
plot_data <- data.frame(Actual = y_test, Predicted = y_pred)
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "XGBoost Regression: Actual vs. Predicted Pitch Speed",
       x = "Actual Pitch Speed (mph)", 
       y = "Predicted Pitch Speed (mph)") +
  theme_minimal()


# Compute feature importance
importance_matrix <- xgb.importance(feature_names = colnames(X_train), model = xgb_model)

# Print top features
print(importance_matrix)
