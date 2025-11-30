# ==============================================================================
# SECTION 1: Setup and Data Loading
# ==============================================================================

# Load necessary libraries for all analyses
library(tidyverse)  # For data manipulation and plotting (dplyr, ggplot2, etc.)
library(caret)      # For machine learning workflow (splitting, pre-processing, evaluation)
library(MASS)       # For Stepwise Regression (stepAIC) and LDA (lda)
library(leaps)      # For Subset Selection (regsubsets)

# --- IMPORTANT: Standardize your file path ---
# Use ONE file path that is correct for your system.
# Replace the placeholder path with your correct file path.
file_path <- "C:/Users/myron/Mental_Health_and_Social_Media_Balance_Dataset.csv"

df <- read.csv(file_path)

# ==============================================================================
# SECTION 2: Data Cleaning & Preparation (Standardized)
# ==============================================================================

# 1. Standardize and rename columns (Using the names from the regression script)
# This step is crucial for consistency across all models.
colnames(df) <- c("UserID", "Age", "Gender", "ScreenTime", "Sleep",
                  "Stress", "DaysWithoutSocialMedia", "Exercise",
                  "Platform", "Happyness")

# 2. Convert Categorical variables (Gender, Platform) into Factors
df$Gender <- as.factor(df$Gender)
df$Platform <- as.factor(df$Platform)

# 3. Check for missing values and remove them
df <- na.omit(df)

# 4. Remove UserID for modeling purposes
data_mining_df <- df %>% dplyr::select(-UserID)

# ==============================================================================
# SECTION 3: Model 1: Best Subset Selection for 'Stress'
# ==============================================================================

# Subset Selection requires a design matrix for predictors
# Create X (design matrix for predictors) and y (target)
X <- model.matrix(Stress ~ . - 1, data = data_mining_df)

# Remove zero-variance columns (if any, to prevent errors)
X <- X[, !apply(X, 2, function(col) sd(col) == 0)]
# Ensure matrix is full rank (handle highly correlated or redundant dummy variables)
qr_X <- qr(X, tol = 1e-7)
X <- X[, qr_X$pivot[1:qr_X$rank]]

y <- data_mining_df$Stress

# Combine back into a dataframe for regsubsets
data_for_regsubsets <- as.data.frame(cbind(Stress = y, X))

# Run exhaustive Best Subset Selection
regfit.full <- regsubsets(
  Stress ~ .,
  data = data_for_regsubsets,
  nvmax = 15, # Max number of variables to consider
  method = "exhaustive"
)

# Plotting Metrics
summary_reg <- summary(regfit.full)
reg.summary <- summary(regfit.full)

n_vars   <- 1:length(reg.summary$rsq)
RSq      <- reg.summary$rsq
AdjRSq   <- reg.summary$adjr2
Cp       <- reg.summary$cp
BIC      <- reg.summary$bic

par(mfrow = c(2,2), mar = c(4.5, 4.5, 3, 1), cex.lab = 1.3, cex.main = 1.4)

plot(n_vars, RSq, type = "b", col = "blue", lwd = 2,
     xlab = "Number of Variables", ylab = "R-squared", main = "R-squared")
points(which.max(RSq), RSq[which.max(RSq)], col = "red", cex = 2.5, pch = 19)

plot(n_vars, AdjRSq, type = "b", col = "darkgreen", lwd = 2,
     xlab = "Number of Variables", ylab = "Adjusted R-squared", main = "Adjusted R-squared")
points(which.max(AdjRSq), AdjRSq[which.max(AdjRSq)], col = "red", cex = 2.5, pch = 19)

plot(n_vars, Cp, type = "b", col = "purple", lwd = 2,
     xlab = "Number of Variables", ylab = "Mallow's Cp", main = "Mallow's Cp")
abline(a = 0, b = 1, lty = 2, col = "gray", lwd = 2)
points(which.min(Cp), Cp[which.min(Cp)], col = "red", cex = 2.5, pch = 19)

plot(n_vars, BIC, type = "b", col = "darkorange", lwd = 2,
     xlab = "Number of Variables", ylab = "BIC (lower is better)", main = "BIC")
points(which.min(BIC), BIC[which.min(BIC)], col = "red", cex = 2.5, pch = 19)

# Select the model based on the minimum BIC
p_star <- which.min(summary_reg$bic)

# Get the names of the selected variables for the best model size
selected_vars <- names(coef(regfit.full, id = p_star))
print("--- Best Subset Variables (Minimum BIC) ---")
print(selected_vars)
par(mfrow = c(1,1)) # Reset plotting layout

# ==============================================================================
# SECTION 4: Model 2: Multiple Linear Regression with Stepwise Optimization
# ==============================================================================

# Split data into Training (80%) and Testing (20%) sets
set.seed(123) # Ensures results are reproducible
training_index <- createDataPartition(data_mining_df$Stress, p = 0.8, list = FALSE)
train_data <- data_mining_df[training_index, ]
test_data  <- data_mining_df[-training_index, ]

# 1. The "Full" Model (predict Stress using all predictors)
full_model <- lm(Stress ~ ., data = train_data)
print("--- Full Model Summary ---")
summary(full_model)

# 2. Optimization using Stepwise Regression (Both directions, minimizing AIC)
print("--- Running Stepwise Regression ---")
stepwise_model <- stepAIC(full_model, direction = "both", trace = FALSE)
print("--- Optimized Stepwise Model Summary ---")
summary(stepwise_model)

# 3. Prediction & Evaluation
predictions <- predict(stepwise_model, test_data)

rmse_val <- RMSE(predictions, test_data$Stress)
r2_val <- R2(predictions, test_data$Stress)

cat("\nModel Performance on Test Data:\n")
cat("RMSE:", rmse_val, "\n")
cat("R-Squared:", r2_val, "\n")

# Visualization: Actual vs Predicted Stress Levels
plot_data <- data.frame(Actual = test_data$Stress, Predicted = predictions)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Stress Levels (Stepwise Model)",
       x = "Actual Stress Score",
       y = "Predicted Stress Score") +
  theme_minimal()

# ==============================================================================
# SECTION 5: Model 3: Linear Discriminant Analysis (LDA)
# ==============================================================================

# --- Specific Data Preparation for LDA ---
# LDA uses ScreenTime_Category as the target, which must be created first.
# This assumes the 'ScreenTime' variable in data_mining_df is Daily_Screen_Time.hrs.

lda_df <- data_mining_df %>%
  mutate(ScreenTime_Category =
           case_when(
             ScreenTime <= 4 ~ "Low",
             ScreenTime <= 7 ~ "Medium",
             TRUE ~ "High"
           ),
         ScreenTime_Category = factor(ScreenTime_Category, levels = c("Low", "Medium", "High"))) # Order the factor

# 1. Split into train/test (LDA is a classification model)
set.seed(123)
train_idx <- createDataPartition(lda_df$ScreenTime_Category, p = 0.75, list = FALSE)
train_lda <- lda_df[train_idx, ]
test_lda  <- lda_df[-train_idx, ]

# 2. Preprocess numeric variables (center + scale)
numeric_vars <- train_lda %>% select_if(is.numeric) %>% names()

preproc <- preProcess(train_lda[, numeric_vars], method = c("center", "scale"))

train_p <- train_lda
train_p[, numeric_vars] <- predict(preproc, train_lda[, numeric_vars])

test_p <- test_lda
test_p[, numeric_vars] <- predict(preproc, test_lda[, numeric_vars])

# 3. Final cleanup: Remove original 'ScreenTime' (now categorized) and 'Happyness'
# 'Happyness' is removed because it was not in the original LDA script, and
# we need to keep predictors consistent.
train_p2 <- train_p %>% dplyr::select(-ScreenTime)
test_p2  <- test_p  %>% dplyr::select(-ScreenTime)


# 4. Fit LDA model (predict ScreenTime_Category using all other variables)
lda_model <- lda(ScreenTime_Category ~ ., data = train_p2 %>% dplyr::select(-Happyness)) # Remove Happyness for consistency

print("--- LDA Model Summary (Coefficients of Linear Discriminants) ---")
print(lda_model)

# 5. Predict on test set
pred <- predict(lda_model, newdata = test_p2 %>% dplyr::select(-Happyness))
pred_class <- pred$class

# 6. Confusion Matrix
print("--- LDA Confusion Matrix ---")
cm <- table(test_p2$ScreenTime_Category, pred_class)
confusionMatrix(cm)

# 7. LDA Plots
par(mfrow = c(1,2), mar = c(4, 4, 2, 1))

# Plot 1: LD1 vs LD2 (Only possible if there are 3 or more classes, yielding 2 LDs)
# Note: ScreenTime_Category has 3 levels (Low, Medium, High), so 2 LDs are generated.
plot(pred$x[,1], pred$x[,2],
     col = as.numeric(test_p2$ScreenTime_Category),
     pch = 19,
     xlab = "LD1",
     ylab = "LD2",
     main = "LDA: Screen Time Category Separation")
legend("topright",
       legend = levels(test_p2$ScreenTime_Category),
       col = 1:length(levels(test_p2$ScreenTime_Category)),
       pch = 19)

# Plot 2: LD1 distribution
plot(lda_model, dimen = 1, type = "b",
     main = "LDA Plot (LD1) – Screen Time Category")

par(mfrow = c(1,1)) # Reset plotting layout

