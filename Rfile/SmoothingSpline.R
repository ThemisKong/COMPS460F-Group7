# Load required libraries
library(ggplot2)

# Extract variables from your dataset
screen_time <- my_data$Daily_Screen_Time.hrs.
stress <- my_data$Stress_Level.1.10.

# Remove duplicates
clean_data_screen <- data.frame(
  x = screen_time,
  stress = stress
)
clean_data_screen <- clean_data_screen[!duplicated(clean_data_screen$x), ]

# Fit smoothing splines with cross-validation
smooth_spline_screen <- smooth.spline(clean_data_screen$x, clean_data_screen$stress, cv = TRUE)
smooth_spline_screen_df4 <- smooth.spline(clean_data_screen$x, clean_data_screen$stress, df = 4)

screen_time_seq <- seq(min(screen_time), max(screen_time), length.out = 300)
pred_screen <- predict(smooth_spline_screen, screen_time_seq)
pred_screen_df4 <- predict(smooth_spline_screen_df4, screen_time_seq)

# SMOOTHING SPLINE: Screen Time VS STRESS
screen_plot <- ggplot() +
  geom_point(data = my_data, aes(x = Daily_Screen_Time.hrs., y = Stress_Level.1.10.), 
             alpha = 0.3, color = "red", size = 1.5) +
  geom_line(data = data.frame(x = pred_screen$x, y = pred_screen$y),
            aes(x = x, y = y, color = "CV-optimized"), linewidth = 1.2) +
  geom_line(data = data.frame(x = pred_screen_df4$x, y = pred_screen_df4$y),
            aes(x = x, y = y, color = "DF = 4"), linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("CV-optimized" = "darkred", "DF = 4" = "salmon")) +
  labs(
    title = "Smoothing Spline: Screen Time vs Stress",
    subtitle = paste("Effective DF =", round(smooth_spline_screen$df, 2)),
    x = "Daily Screen Time (hours)",
    y = "Stress Level (1-10)",
    color = "Smoothing Level"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(screen_plot)


# SMOOTHING SPLINE:EXERCISE VS STRESS
exercise <- my_data$Exercise_Frequency.week.
clean_data_exercise <- data.frame(
  x = exercise,
  stress = stress
)
clean_data_exercise <- clean_data_exercise[!duplicated(clean_data_exercise$x), ]


smooth_spline_exercise <- smooth.spline(clean_data_exercise$x, clean_data_exercise$stress, cv = TRUE)
smooth_spline_exercise_df4 <- smooth.spline(clean_data_exercise$x, clean_data_exercise$stress, df = 4)

exercise_seq <- seq(min(exercise), max(exercise), length.out = 300)
pred_exercise <- predict(smooth_spline_exercise, exercise_seq)
pred_exercise_df4 <- predict(smooth_spline_exercise_df4, exercise_seq)

# PLOT 2: EXERCISE FREQUENCY VS STRESS
exercise_plot <- ggplot() +
  geom_point(data = my_data, aes(x = Exercise_Frequency.week., y = Stress_Level.1.10.), 
             alpha = 0.3, color = "blue", size = 1.5) +
  geom_line(data = data.frame(x = pred_exercise$x, y = pred_exercise$y),
            aes(x = x, y = y, color = "CV-optimized"), linewidth = 1.2) +
  geom_line(data = data.frame(x = pred_exercise_df4$x, y = pred_exercise_df4$y),
            aes(x = x, y = y, color = "DF = 4"), linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("CV-optimized" = "darkblue", "DF = 4" = "lightblue")) +
  labs(
    title = "Smoothing Spline: Exercise vs Stress",
    subtitle = paste("Effective DF =", round(smooth_spline_exercise$df, 2)),
    x = "Exercise Frequency (times per week)",
    y = "Stress Level (1-10)",
    color = "Smoothing Level"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(exercise_plot)
