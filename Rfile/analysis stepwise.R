library(tidyverse)
library(MASS)        # stepAIC
library(ggplot2)
library(broom)
df <- read_csv("C:/Users/myron/Mental_Health_and_Social_Media_Balance_Dataset.csv")
df <- df %>%
  select(-User_ID) %>%   # remove ID column
  
  mutate(
    Gender = factor(Gender),
    Social_Media_Platform = factor(Social_Media_Platform),
    
    ScreenTime = as.numeric(`Daily_Screen_Time(hrs)`),
    Sleep = as.numeric(`Sleep_Quality(1-10)`),
    Stress = as.numeric(`Stress_Level(1-10)`),
    Happiness = as.numeric(`Happiness_Index(1-10)`),
    
    Days_Off = as.numeric(Days_Without_Social_Media),
  ) %>%
  
  # remove old messy names
  select(-`Daily_Screen_Time(hrs)`,
         -`Sleep_Quality(1-10)`,
         -`Stress_Level(1-10)`,
         -`Happiness_Index(1-10)`) %>%
  
  drop_na()
# Full model (all variables)
full_model <- lm(Stress ~ ., data = df)

# Stepwise Regression (both directions)
step_model <- stepAIC(full_model, direction = "both", trace = FALSE)

summary(step_model)
ggplot(df, aes(x = Sleep, y = Stress)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Relationship Between Sleep_Quality and Stress",
    subtitle = "Fitted Linear Trend From Stepwise Regression",
    x = "Sleep_Quality(1-10)",
    y = "Stress Level (1–10)"
  ) +
  theme_minimal()




