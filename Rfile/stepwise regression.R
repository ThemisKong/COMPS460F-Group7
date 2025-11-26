library(leaps)
library(dplyr)
library(tidyverse)
my_data_clean <- read_csv("C:/Users/myron/Mental_Health_and_Social_Media_Balance_Dataset.csv") %>%
  select(-User_ID) %>%
  rename(Stress = `Stress_Level(1-10)`,
         ScreenTime = `Daily_Screen_Time(hrs)`,
         Happiness = `Happiness_Index(1-10)`,
         Sleep = `Sleep_Quality(1-10)`) %>%
  mutate(
    Gender = factor(Gender),
    Platform = factor(`Social_Media_Platform`)
  )

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
