library(splines)
library(ggplot2)   # for nicer look, or use base R below

# Single beautiful plot – natural spline with df = 4 only
ggplot(my_data, aes(x = Daily_Screen_Time.hrs., y = Sleep_Quality.1.10.)) +
  geom_point(color = "lightblue", alpha = 0.7, size = 1.8) +
  geom_smooth(
    method = "lm",
    formula = y ~ ns(x, df = 4),
    se = TRUE,
    color = "blue",
    fill = "skyblue",
    alpha = 0.25,
    linewidth = 1.8
  ) +
  labs(
    title = "Daily Screen Time vs Sleep Quality",
    subtitle = "Natural Cubic Spline (df = 4) | n = 500",
    x = "Daily Screen Time (hours)",
    y = "Sleep Quality (1–10)",
    caption = "Fitted using splines::ns(df = 4)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(color = "gray50")
  )

