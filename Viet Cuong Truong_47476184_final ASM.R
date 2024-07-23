
# ================ Task 3 =====================

# Load the necessary libraries
library(ggplot2)
library(readr)
library(gridExtra)

# ========== 1. Load and Preprocess Data ==========

# Load the dataset with suppressed column type message
data <- read_csv("UCI_credit_sample.csv", show_col_types = FALSE)

# Remove rows with any NA values
data <- na.omit(data)

# ========== 2. Create Individual Plots ==========

# Box plot for LIMIT_BAL
plot1 <- ggplot(data, aes(x = as.factor(default), y = LIMIT_BAL, fill = as.factor(default))) +
  geom_boxplot() +
  labs(title = "Box Plot of LIMIT_BAL by Default", x = "Default", y = "LIMIT_BAL") +
  scale_fill_discrete(name = "Default")

# Box plot for AGE
plot2 <- ggplot(data, aes(x = as.factor(default), y = AGE, fill = as.factor(default))) +
  geom_boxplot() +
  labs(title = "Box Plot of AGE by Default", x = "Default", y = "AGE") +
  scale_fill_discrete(name = "Default")

# Box plot for BILL_AMT1
plot3 <- ggplot(data, aes(x = as.factor(default), y = BILL_AMT1, fill = as.factor(default))) +
  geom_boxplot() +
  labs(title = "Box Plot of BILL_AMT1 by Default", x = "Default", y = "BILL_AMT1") +
  scale_fill_discrete(name = "Default")

# Box plot for BILL_AMT2
plot4 <- ggplot(data, aes(x = as.factor(default), y = BILL_AMT2, fill = as.factor(default))) +
  geom_boxplot() +
  labs(title = "Box Plot of BILL_AMT2 by Default", x = "Default", y = "BILL_AMT2") +
  scale_fill_discrete(name = "Default")

# Box plot for PAY_AMT1
plot5 <- ggplot(data, aes(x = as.factor(default), y = PAY_AMT1, fill = as.factor(default))) +
  geom_boxplot() +
  labs(title = "Box Plot of PAY_AMT1 by Default", x = "Default", y = "PAY_AMT1") +
  scale_fill_discrete(name = "Default")

# Box plot for PAY_AMT2
plot6 <- ggplot(data, aes(x = as.factor(default), y = PAY_AMT2, fill = as.factor(default))) +
  geom_boxplot() +
  labs(title = "Box Plot of PAY_AMT2 by Default", x = "Default", y = "PAY_AMT2") +
  scale_fill_discrete(name = "Default")

# ========== 3. Arrange and Save Combined Box Plots ==========

# Arrange plots in a 2x3 grid with adjusted height
combined_plot <- grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 2, heights = c(1, 1, 1))

# Save the combined plot with increased height
ggsave("combined_plot.png", plot = combined_plot, width = 10, height = 12)

# ========== 4. Create Individual Bar Charts for Categorical Variables ==========

# Bar chart for SEX
bar_chart_sex <- ggplot(data, aes(x = as.factor(SEX), fill = as.factor(default))) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Chart of SEX by Default", x = "SEX", y = "Count") +
  scale_fill_discrete(name = "Default")

# Bar chart for EDUCATION
bar_chart_education <- ggplot(data, aes(x = as.factor(EDUCATION), fill = as.factor(default))) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Chart of EDUCATION by Default", x = "EDUCATION", y = "Count") +
  scale_fill_discrete(name = "Default")

# Bar chart for MARRIAGE
bar_chart_marriage <- ggplot(data, aes(x = as.factor(MARRIAGE), fill = as.factor(default))) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Chart of MARRIAGE by Default", x = "MARRIAGE", y = "Count") +
  scale_fill_discrete(name = "Default")

# Bar chart for PAY_0
bar_chart_pay_0 <- ggplot(data, aes(x = PAY_0, fill = default)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Chart of PAY_0 by Default", x = "PAY_0", y = "Count") +
  scale_fill_discrete(name = "Default")

# Bar chart for PAY_2
bar_chart_pay_2 <- ggplot(data, aes(x = PAY_2, fill = default)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Chart of PAY_2 by Default", x = "PAY_2", y = "Count") +
  scale_fill_discrete(name = "Default")

# ========== 5. Arrange and Save Combined Bar Charts ==========

# Arrange bar charts in a 2x3 grid
combined_bar_charts <- grid.arrange(
  bar_chart_sex, bar_chart_education, bar_chart_marriage, bar_chart_pay_0, bar_chart_pay_2,
  ncol = 3, nrow = 2
)

# Save the combined bar charts with increased height
ggsave("combined_bar_charts.png", plot = combined_bar_charts, width = 10, height = 8)
# ========== 6. Generate Summary Statistics for Continuous Variables ==========

summary_continuous <- data %>% 
  summarise(
    LIMIT_BAL_min = min(LIMIT_BAL),
    LIMIT_BAL_mean = mean(LIMIT_BAL),
    LIMIT_BAL_median = median(LIMIT_BAL),
    LIMIT_BAL_sd = sd(LIMIT_BAL),
    LIMIT_BAL_max = max(LIMIT_BAL),
    
    AGE_min = min(AGE),
    AGE_mean = mean(AGE),
    AGE_median = median(AGE),
    AGE_sd = sd(AGE),
    AGE_max = max(AGE),
    
    BILL_AMT1_min = min(BILL_AMT1),
    BILL_AMT1_mean = mean(BILL_AMT1),
    BILL_AMT1_median = median(BILL_AMT1),
    BILL_AMT1_sd = sd(BILL_AMT1),
    BILL_AMT1_max = max(BILL_AMT1),
    
    BILL_AMT2_min = min(BILL_AMT2),
    BILL_AMT2_mean = mean(BILL_AMT2),
    BILL_AMT2_median = median(BILL_AMT2),
    BILL_AMT2_sd = sd(BILL_AMT2),
    BILL_AMT2_max = max(BILL_AMT2),
    
    PAY_AMT1_min = min(PAY_AMT1),
    PAY_AMT1_mean = mean(PAY_AMT1),
    PAY_AMT1_median = median(PAY_AMT1),
    PAY_AMT1_sd = sd(PAY_AMT1),
    PAY_AMT1_max = max(PAY_AMT1),
    
    PAY_AMT2_min = min(PAY_AMT2),
    PAY_AMT2_mean = mean(PAY_AMT2),
    PAY_AMT2_median = median(PAY_AMT2),
    PAY_AMT2_sd = sd(PAY_AMT2),
    PAY_AMT2_max = max(PAY_AMT2)
  )

# ========== 7. Generate Summary Statistics for Categorical Variables ==========

summary_categorical <- data %>% 
  group_by(default) %>% 
  summarise(
    SEX_1 = sum(SEX == 1),
    SEX_2 = sum(SEX == 2),
    
    EDUCATION_1 = sum(EDUCATION == 1),
    EDUCATION_2 = sum(EDUCATION == 2),
    EDUCATION_3 = sum(EDUCATION == 3),
    EDUCATION_4 = sum(EDUCATION == 4),
    EDUCATION_5 = sum(EDUCATION == 5),
    EDUCATION_6 = sum(EDUCATION == 6),
    
    MARRIAGE_0 = sum(MARRIAGE == 0),
    MARRIAGE_1 = sum(MARRIAGE == 1),
    MARRIAGE_2 = sum(MARRIAGE == 2),
    MARRIAGE_3 = sum(MARRIAGE == 3),
    
    PAY_0_0 = sum(PAY_0 == 0),
    PAY_0_1 = sum(PAY_0 == 1),
    PAY_0_2 = sum(PAY_0 == 2),
    PAY_0_3 = sum(PAY_0 == 3),
    PAY_0_4 = sum(PAY_0 == 4),
    PAY_0_5 = sum(PAY_0 == 5),
    PAY_0_6 = sum(PAY_0 == 6),
    PAY_0_7 = sum(PAY_0 == 7),
    PAY_0_8 = sum(PAY_0 == 8),
    PAY_0_9 = sum(PAY_0 == 9),
    
    PAY_2_0 = sum(PAY_2 == 0),
    PAY_2_1 = sum(PAY_2 == 1),
    PAY_2_2 = sum(PAY_2 == 2),
    PAY_2_3 = sum(PAY_2 == 3),
    PAY_2_4 = sum(PAY_2 == 4),
    PAY_2_5 = sum(PAY_2 == 5),
    PAY_2_6 = sum(PAY_2 == 6),
    PAY_2_7 = sum(PAY_2 == 7),
    PAY_2_8 = sum(PAY_2 == 8),
    PAY_2_9 = sum(PAY_2 == 9)
  )


# ================ Task 4 =====================

# ==============================================
# 1. Load Necessary Libraries
# ==============================================
library(tidyverse)
library(TTR)
library(quantmod)
library(xts)
library(caret)
library(e1071)
library(nnet)
library(ggplot2)

# ==============================================
# 2. Load and Preprocess the Data
# ==============================================
# Load the data
data_stock_24 <- read.csv("data_stock_24.csv")
data_stock_24$Date <- as.Date(data_stock_24$Date, format="%Y-%m-%d")

# Inspect the column names to ensure correctness
colnames(data_stock_24)

# Subset the data to include only Date, ATVI-US, and sentiment columns
data_subset <- data_stock_24[, c("Date", "ATVI.US", "sentiment")]

# Convert data to xts format
data_xts <- xts(data_subset$`ATVI.US`, order.by=data_subset$Date)

# ==============================================
# 3. Calculate Technical Indicators
# ==============================================
# Calculate Daily Logarithmic Returns
log_returns <- ROC(data_xts, type="continuous", na.pad=FALSE)
colnames(log_returns) <- "log_returns"

# Calculate Moving Average (5-day)
ma_5 <- SMA(data_xts, n=5)
colnames(ma_5) <- "ma_5"

# Calculate Exponential Moving Average (5-day)
ema_5 <- EMA(data_xts, n=5)
colnames(ema_5) <- "ema_5"

# Calculate RSI (5-day)
rsi_5 <- RSI(data_xts, n=5)
colnames(rsi_5) <- "rsi_5"

# Combine all indicators into a single xts object
indicators <- merge(log_returns, ma_5, ema_5, rsi_5, all=FALSE)

# Lag the predictors by one period using xts::lag.xts, excluding log_returns
predictors <- indicators[, -1]  # Exclude the first column (log_returns)
lagged_predictors <- xts::lag.xts(predictors, k=1)

# Combine the non-lagged log_returns with lagged predictors
final_indicators <- merge(indicators$log_returns, lagged_predictors, all=FALSE)

# Remove NA values resulting from the lag operation
final_indicators <- na.omit(final_indicators)

# ==============================================
# 4. Process Sentiment Indicator
# ==============================================
# Convert sentiment data to xts format
sentiment_xts <- xts(data_subset$sentiment, order.by=data_subset$Date)
colnames(sentiment_xts) <- "sentiment"

# Lag the sentiment indicator by one period
lagged_sentiment <- xts::lag.xts(sentiment_xts, k=1)

# Remove NA values resulting from the lag operation
lagged_sentiment <- na.omit(lagged_sentiment)

# ==============================================
# 5. Merge All Indicators and Prepare Final Data
# ==============================================
# Merge the lagged sentiment indicator with final indicators
final_data <- merge(final_indicators, lagged_sentiment, all=FALSE)

# Convert to data frame for easier handling
final_data_df <- data.frame(Date=index(final_data), coredata(final_data))


# ==============================================
# 6. Set Seed and Split Data into Training and Testing Sets
# ==============================================
# Set seed for reproducibility
set.seed(2345)

# Define the prediction window size
pred.window <- 100 # Last 100 days for the testing set

# Determine the indices for training and testing sets
idx <- c(1:(nrow(final_data_df) - pred.window))
d_train <- final_data_df[idx, ]
d_test <- final_data_df[(max(idx) + 1):nrow(final_data_df), ]

# Inspect the training and testing data
head(d_train)
head(d_test)

# ==============================================
# 7. Neural Network Regression Forecasting
# ==============================================
# Define the control parameters for model training
cntrl2 <- trainControl(method = "timeslice",
                       initialWindow = nrow(d_train) - pred.window,
                       horizon = pred.window,
                       fixedWindow = TRUE,
                       savePredictions = TRUE,
                       allowParallel = TRUE)

# Define preprocessing steps
prep1 <- c("center", "scale", "zv") # Include "zv" which removes variables with close to zero variance

# Train the neural network model
set.seed(2345)
nnet_model <- train(log_returns ~ . - Date, 
                    data = d_train, 
                    method = "nnet", 
                    trControl = cntrl2, 
                    preProcess = prep1, 
                    linout = TRUE, 
                    trace = FALSE, 
                    metric = "RMSE", 
                    tuneLength = 5)

# Print the trained model details
print(nnet_model)

# Prediction on the test set
nnet_predictions <- predict(nnet_model, newdata = d_test)

# Calculate performance measures
nnet_performance <- postResample(nnet_predictions, d_test$log_returns)

# Print performance measures
print(nnet_performance)

# Plot actual vs predicted values for Neural Network
data_pred_nnet <- data.frame(Date = d_test$Date, Actual = d_test$log_returns, Predicted = nnet_predictions)
data_pred_long_nnet <- pivot_longer(data_pred_nnet, cols = -Date)

plot_nn <- ggplot(data_pred_long_nnet, aes(x = Date, y = value, color = name)) +
  geom_line() +
  labs(title = "Neural Network Forecasting: Actual vs Predicted", x = "Date", y = "Log Returns") +
  theme_minimal()

# Save the plot using ggsave
ggsave("NN.png", plot = plot_nn, width = 10, height = 5)


# ==============================================
# 8. SVM Regression Forecasting
# ==============================================
# Train the SVM model with radial kernel
set.seed(2345)
svm_model <- train(log_returns ~ . - Date, 
                   data = d_train, 
                   method = "svmRadial", 
                   trControl = cntrl2, 
                   preProcess = prep1, 
                   metric = "RMSE", 
                   tuneLength = 5)

# Print the trained model details
print(svm_model)

# Prediction on the test set
svm_predictions <- predict(svm_model, newdata = d_test)

# Calculate performance measures
svm_performance <- postResample(svm_predictions, d_test$log_returns)

# Print performance measures
print(svm_performance)

# Plot actual vs predicted values for SVM
data_pred_svm <- data.frame(Date = d_test$Date, Actual = d_test$log_returns, Predicted = svm_predictions)
data_pred_long_svm <- pivot_longer(data_pred_svm, cols = -Date)

# Create the plot for SVM
plot_svm <- ggplot(data_pred_long_svm, aes(x = Date, y = value, color = name)) +
  geom_line() +
  labs(title = "SVM Regression Forecasting: Actual vs Predicted", x = "Date", y = "Log Returns") +
  theme_minimal()

# Save the plot using ggsave
ggsave("SVM.png", plot = plot_svm, width = 10, height = 5)



# Print performance measures for SVM
cat("SVM Performance Measures:\n")
print(svm_performance)

cat("Neural Network Performance Measures:\n")
print(nnet_performance)
