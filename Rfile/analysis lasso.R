library(tidyverse)
library(glmnet)
library(ggplot2)

data <- read_csv("C:/Users/myron/Mental_Health_and_Social_Media_Balance_Dataset.csv") %>%
  select(-User_ID) %>%
  rename(
    Stress      = `Stress_Level(1-10)`,
    ScreenTime  = `Daily_Screen_Time(hrs)`,
    Happiness   = `Happiness_Index(1-10)`,
    Sleep       = `Sleep_Quality(1-10)`,
    Exercise    = `Exercise_Frequency(week)`,
    DaysOff     = `Days_Without_Social_Media`
  ) %>%
  mutate(Gender = factor(Gender),
         Platform = factor(`Social_Media_Platform`)) %>%
  na.omit()

x <- model.matrix(Stress ~ . - 1, data = data)    
y <- data$Stress

set.seed(123)
cv.lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 10)

coef_min   <- coef(cv.lasso, s = "lambda.min")      
coef_1se   <- coef(cv.lasso, s = "lambda.1se")      

lasso_best <- glmnet(x, y, alpha = 1, lambda = cv.lasso$lambda.1se)

screen_grid <- seq(min(data$ScreenTime), max(data$ScreenTime), length.out = 200)
pred_df <- data.frame(ScreenTime = screen_grid)

X_pred <- matrix(mean(x[, "ScreenTime"]), nrow = 200, ncol = ncol(x))
colnames(X_pred) <- colnames(x)
X_pred[, "ScreenTime"] <- screen_grid

pred_stress <- predict(lasso_best, newx = X_pred)

ggplot() +
  geom_line(aes(x = screen_grid, y = pred_stress), size = 1.5, color = "#E74C3C") +
  geom_point(data = data, aes(x = ScreenTime, y = Stress), alpha = 0.3, size = 1.2) +
  labs(title = "Lasso regression",
       x = "Daily_Screen_Time(hrs) ",
       y = "Stress_Level(1-10) ") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "darkred"))

