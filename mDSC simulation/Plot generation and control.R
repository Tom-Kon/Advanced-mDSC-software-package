

# ---- 3. Plot the Results Using ggplot2 ----
ggplot() +
  geom_line(data = resampled_points, aes(x = time, y = BaselinecorrMHF), color = "blue", size = 1) +
  geom_line(data = resampled_points, aes(x = time, y = MHF), color = "red", size = 1) +
  labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
       x = "Time",
       y = "Signal / Rolling Mean") +
  theme_minimal()


ggplot() +
  geom_line(data = resampled_points, aes(x = time, y = loessTHF), color = "blue", size = 1) +
  geom_line(data = resampled_points, aes(x = time, y = loessRHF), color = "red", size = 1) +
  geom_line(data = resampled_points, aes(x = time, y = loessNRHF), color = "green", size = 1) +
  labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
       x = "Time",
       y = "Signal / Rolling Mean") +
  theme_minimal() 



ggplot() +
  geom_line(data = resampled_points, aes(x = time, y = loessTHF), color = "blue", size = 1) +
  labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
       x = "Time",
       y = "Signal / Rolling Mean") +
  theme_minimal()

ggplot() +
  geom_line(data = resampled_points, aes(x = time, y = loessRHF), color = "blue", size = 1) +
  labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
       x = "Time",
       y = "Signal / Rolling Mean") +
  theme_minimal()

ggplot() +
  geom_line(data = resampled_points, aes(x = time, y = loessNRHF), color = "blue", size = 1) +
  labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
       x = "Time",
       y = "Signal / Rolling Mean") +
  theme_minimal()