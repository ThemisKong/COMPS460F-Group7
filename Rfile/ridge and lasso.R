suppressPackageStartupMessages({
  library(tidyverse)
  library(glmnet)
  library(ISLR)     
})

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
  na.omit()   

dim(data)   

x <- model.matrix(Stress~ ., data)[, -1]   
y <- data$Stress

grid <- 10^seq(10, -2, length = 100)

ridge_mod <- glmnet(x, y, alpha = 0, lambda = grid)   

ridge_mod$lambda[50]         
coef(ridge_mod)[,50]
sqrt(sum(coef(ridge_mod)[-1,50]^2)) 

ridge_mod$lambda[60]       
coef(ridge_mod)[,60]
sqrt(sum(coef(ridge_mod)[-1,60]^2))  


lasso_mod <- glmnet(x, y, alpha = 1, lambda = grid)   

par(mfrow = c(1,2), mar = c(5,5,4,2))
plot(ridge_mod, xvar = "lambda")
abline(v = log(ridge_mod$lambda[60]), col = "red", lty = 2)

plot(lasso_mod, xvar = "lambda")
abline(v = log(lasso_mod$lambda[60]), col = "red", lty = 2)

set.seed(1)

cv_ridge <- cv.glmnet(x, y, alpha = 0, nfolds = 10)
cv_lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 10)

par(mfrow = c(1,2))
plot(cv_ridge)
plot(cv_lasso)
