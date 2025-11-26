# ==============================================================================
# SECTION 1: Setup and Data Loading
# ==============================================================================
library(tidyverse)  # For data manipulation and plotting
library(caret)      # For machine learning workflow
library(MASS)       # For Stepwise Regression (stepAIC)
library(corrplot)   # For correlation heatmaps
library(leaps)
library(dplyr)
library(tidyverse)
# Load the dataset
# Make sure the file is in your working directory
df <- read.csv("C:/Users/myron/Mental_Health_and_Social_Media_Balance_Dataset.csv")

# ==============================================================================
# SECTION 2: Data Cleaning & Preparation
# ==============================================================================

# 1. Rename columns to remove spaces and special characters (easier coding)
# New names: UserID, Gender, Platform, Age, ScreenTime, Sleep, Stress, DaysDetox
colnames(df) <- c("UserID", "Age", "Gender", "ScreenTime", "Sleep", 
                  "Stress", "DaysWithoutSocialMedia", "Exercise","Platform", "Happyness")


X <- model.matrix(Stress ~ . - 1, data = my_data_clean) 
X <- X[, !apply(X, 2, function(col) sd(col) == 0)]
qr_X <- qr(X, tol = 1e-7)
X <- X[, qr_X$pivot[1:qr_X$rank]]  

y <- my_data_clean$Stress

data_for_regsubsets <- as.data.frame(cbind(Stress = y, X))

regfit.full <- regsubsets(
  Stress ~ ., 
  data = data_for_regsubsets,
  nvmax = 15,
  method = "exhaustive",
  really.big = TRUE   
)

summary_reg <- summary(regfit.full)
summary_reg

reg.summary <- summary(regfit.full)

n_vars   <- 1:length(reg.summary$rsq)       
RSq      <- reg.summary$rsq
AdjRSq   <- reg.summary$adjr2
Cp       <- reg.summary$cp
BIC      <- reg.summary$bic

par(mfrow = c(2,2), mar = c(4.5, 4.5, 3, 1), cex.lab = 1.3, cex.main = 1.4)

# 1. R-squared
plot(n_vars, RSq, type = "b", col = "blue", lwd = 2,
     xlab = "Number of Variables", ylab = "R-squared",
     main = "R-squared")
points(which.max(RSq), RSq[which.max(RSq)], col = "red", cex = 2.5, pch = 19)
text(which.max(RSq), RSq[which.max(RSq)], "Best", pos = 3, col = "red", font = 2)

# 2. Adjusted R-squared
plot(n_vars, AdjRSq, type = "b", col = "darkgreen", lwd = 2,
     xlab = "Number of Variables", ylab = "Adjusted R-squared",
     main = "Adjusted R-squared")
points(which.max(AdjRSq), AdjRSq[which.max(AdjRSq)], col = "red", cex = 2.5, pch = 19)
text(which.max(AdjRSq), AdjRSq[which.max(AdjRSq)], paste(which.max(AdjRSq), "vars"), pos = 3, col = "red", font = 2)

# 3. Mallow's Cp
plot(n_vars, Cp, type = "b", col = "purple", lwd = 2,
     xlab = "Number of Variables", ylab = "Mallow's Cp",
     main = "Mallow's Cp")
abline(a = 0, b = 1, lty = 2, col = "gray", lwd = 2)  
points(which.min(Cp), Cp[which.min(Cp)], col = "red", cex = 2.5, pch = 19)
text(which.min(Cp), Cp[which.min(Cp)], "Best", pos = 4, col = "red", font = 2)

# 4. BIC
plot(n_vars, BIC, type = "b", col = "darkorange", lwd = 2,
     xlab = "Number of Variables", ylab = "BIC",
     main = "BIC (lower is better)")
points(which.min(BIC), BIC[which.min(BIC)], col = "red", cex = 2.5, pch = 19)
text(which.min(BIC), BIC[which.min(BIC)], paste(which.min(BIC), "vars"), pos = 3, col = "red", font = 2)

p_star <- which.min(summary_reg$bic) # Or which.max(summary_reg$adjr2)

# Get the names of the selected variables for that model size
selected_vars <- names(coef(regfit.full, id = p_star))

# The result is a character vector of the variable names (including the intercept)
print(selected_vars)
# 2. Convert Categorical variables (Gender, Platform) into Factors
df$Gender <- as.factor(df$Gender)
df$Platform <- as.factor(df$Platform)

# 3. Check for missing values and remove them (if any)
df <- na.omit(df)

# 4. Remove UserID (It is just an ID, it has no predictive power)
data_mining_df <- df %>% select(-UserID)

# ==============================================================================
# SECTION 3: Building the Multiple Linear Regression Model
# ==============================================================================

# Split data into Training (80%) and Testing (20%) sets
set.seed(123) # Ensures results are reproducible
training_index <- createDataPartition(data_mining_df$Stress, p = 0.8, list = FALSE)
train_data <- data_mining_df[training_index, ]
test_data  <- data_mining_df[-training_index, ]

# MODEL A: The "Full" Model
# We try to predict Stress using ALL other variables (.)
full_model <- lm(Stress ~ ., data = train_data)

# Show the summary statistics of the full model
print("--- Full Model Summary ---")
summary(full_model)

# ==============================================================================
# SECTION 4: Optimization using Stepwise Regression (Requirement 1)
# ==============================================================================
# Stepwise regression automatically adds/removes variables to find the best fit.
# It lowers the AIC (Akaike Information Criterion).

print("--- Running Stepwise Regression ---")
stepwise_model <- stepAIC(full_model, direction = "both", trace = FALSE)

# Show the summary of the optimized model
print("--- Optimized Stepwise Model Summary ---")
summary(stepwise_model)

# Comparison: Which variables survived?
# If 'Age' or 'Gender' were removed, it means they don't affect stress statistically.

# ==============================================================================
# SECTION 5: Prediction & Evaluation
# ==============================================================================

# Make predictions on the Test Data using the optimized model
predictions <- predict(stepwise_model, test_data)

# Calculate Accuracy Metrics
# RMSE: Root Mean Squared Error (Lower is better)
# R-Squared: How much variation is explained (Higher is better, max 1)

rmse_val <- RMSE(predictions, test_data$Stress)
r2_val <- R2(predictions, test_data$Stress)

cat("Model Performance on Test Data:\n")
cat("RMSE:", rmse_val, "\n")
cat("R-Squared:", r2_val, "\n")

# Visualization: Actual vs Predicted Stress Levels
plot_data <- data.frame(Actual = test_data$Stress, Predicted = predictions)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Stress Levels",
       x = "Actual Stress Score",
       y = "Predicted Stress Score") +
  theme_minimal()

