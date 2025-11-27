library(readr)
library(dplyr)
library(caret)
library(MASS)

#--------------------------------------------------
# 1. Load data
#--------------------------------------------------
df <- read.csv("D:/project/hkmu/s460f/Mental_Health_and_Social_Media_Balance_Dataset.csv")



# Data inspection
head(my_data)
str(my_data)
summary(my_data)
dim(my_data)
names(my_data) 

missing_summary <- colSums(is.na(my_data))
print(missing_summary)


#--------------------------------------------------
# 2. Create a target variable (stress category)
#--------------------------------------------------
df <- df %>%
  mutate(ScreenTime_Category =
           case_when(
             Daily_Screen_Time.hrs. <= 4 ~ "Low",
             Daily_Screen_Time.hrs. <= 7 ~ "Medium",
             TRUE ~ "High"
           ),
         ScreenTime_Category = as.factor(ScreenTime_Category))

#--------------------------------------------------
# 3. Split into train/test
#--------------------------------------------------
set.seed(123)
train_idx <- createDataPartition(df$ScreenTime_Category, p = 0.75, list = FALSE)
train <- df[train_idx, ]
test  <- df[-train_idx, ]

#--------------------------------------------------
# 4. Preprocess numeric variables (center + scale)
#--------------------------------------------------
numeric_vars <- train %>% select_if(is.numeric) %>% names()

preproc <- preProcess(train[, numeric_vars], method = c("center", "scale"))

train_p <- train
train_p[, numeric_vars] <- predict(preproc, train[, numeric_vars])

test_p <- test
test_p[, numeric_vars] <- predict(preproc, test[, numeric_vars])

#--------------------------------------------------
# 5. Remove useless variables
#--------------------------------------------------
train_p2 <- train_p %>% select(-User_ID, -Social_Media_Platform)
test_p2  <- test_p  %>% select(-User_ID, -Social_Media_Platform)

#--------------------------------------------------
# 6. Fit LDA model
#--------------------------------------------------
lda_model <- lda(ScreenTime_Category ~ ., data = train_p2)
lda_model

#--------------------------------------------------
# 7. Predict on test set
#--------------------------------------------------
pred <- predict(lda_model, newdata = test_p2)
pred_class <- pred$class

#--------------------------------------------------
# 8. Confusion Matrix
#--------------------------------------------------
cm <- table(test_p2$ScreenTime_Category, pred_class)
confusionMatrix(cm)

#--------------------------------------------------
# 9. LDA PLOTS (Added)
#--------------------------------------------------
par(mar = c(4, 4, 2, 1))
# ---- Plot 1: LD1 vs LD2 (Scatter Plot) ----
plot(pred$x[,1], pred$x[,2],
     col = as.numeric(test_p2$ScreenTime_Category),
     pch = 19,
     xlab = "LD1",
     ylab = "LD2",
     main = "LDA: Daily Screen Time Category Separation (LD1 vs LD2)")
legend("topright",
       legend = levels(test_p2$ScreenTime_Category),
       col = 1:length(levels(test_p2$ScreenTime_Category)),
       pch = 19)

# ---- Plot 2: LD1 Distribution ----
plot(lda_model, dimen = 1, type = "b",
     main = "LDA Plot (LD1) – Daily Screen Time Category")
