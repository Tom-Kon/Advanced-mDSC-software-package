
RHFplot <- function(RHFdf) {
  
# Plot the results
ggplot(RHFdf, aes(x = meantemp, y = RHF)) +
  geom_line(color = "blue", size = 1.25) +
  labs(
    title = "RHF as a function of temperature",
    x = "Temperature (°C)",
    y = "Normalized Reversing heat flow (W/g)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"), # Center title with bold font
    plot.subtitle = element_text(size = 12),  # Adjust the size as needed
    axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold axis labels
    axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),  # Bold and separated y-axis title
    axis.text = element_text(size = 18, color = "black"),  # Clear and readable axis text
    axis.line = element_line(color = "black", size = 0.5),  # Black axis lines for better contrast
    panel.grid.major = element_line(color = "gray", size = 0.25),  # Light gray grid lines for a clean look
    panel.grid.minor = element_blank(),  # Minor grid lines removed
    plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
    axis.ticks = element_line(color = "black", size = 0.5)  # Ticks for axes
  )
}

THFplot <- function(RHFdf) {

# Plot the results
ggplot(RHFdf, aes(x = meantemp, y = THF)) +
  geom_line(color = "blue", size = 1.25) +
  labs(
    title = "THF as a function of temperature",
    x = "Temperature (°C)",
    y = "Normalized Total heat flow (W/g)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"), # Center title with bold font
    plot.subtitle = element_text(size = 12),  # Adjust the size as needed
    axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold axis labels
    axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),  # Bold and separated y-axis title
    axis.text = element_text(size = 18, color = "black"),  # Clear and readable axis text
    axis.line = element_line(color = "black", size = 0.5),  # Black axis lines for better contrast
    panel.grid.major = element_line(color = "gray", size = 0.25),  # Light gray grid lines for a clean look
    panel.grid.minor = element_blank(),  # Minor grid lines removed
    plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
    axis.ticks = element_line(color = "black", size = 0.5)  # Ticks for axes
  ) 
}

NRHFplot <- function(RHFdf) {
  
# Plot the results
ggplot(RHFdf, aes(x = meantemp, y = NRHF)) +
  geom_line(color = "blue", size = 1.25) +
  labs(
    title = "NRHF as a function of temperature",
    x = "Temperature (°C)",
    y = "Normalized Non-reversing heat flow (W/g)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"), # Center title with bold font
    plot.subtitle = element_text(size = 12),  # Adjust the size as needed
    axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold axis labels
    axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),  # Bold and separated y-axis title
    axis.text = element_text(size = 18, color = "black"),  # Clear and readable axis text
    axis.line = element_line(color = "black", size = 0.5),  # Black axis lines for better contrast
    panel.grid.major = element_line(color = "gray", size = 0.25),  # Light gray grid lines for a clean look
    panel.grid.minor = element_blank(),  # Minor grid lines removed
    plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
    axis.ticks = element_line(color = "black", size = 0.5)  # Ticks for axes
  ) 
}

THFplotFT <- function(fftCalc) {
  
  # Plot the results
  ggplot(fftCalc, aes(x = temperature, y = rollmean)) +
    geom_line(color = "blue", size = 1.25) +
    labs(
      title = "NRHF as a function of temperature",
      x = "Temperature (°C)",
      y = "Normalized Non-reversing heat flow (W/g)"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"), # Center title with bold font
      plot.subtitle = element_text(size = 12),  # Adjust the size as needed
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold axis labels
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),  # Bold and separated y-axis title
      axis.text = element_text(size = 18, color = "black"),  # Clear and readable axis text
      axis.line = element_line(color = "black", size = 0.5),  # Black axis lines for better contrast
      panel.grid.major = element_line(color = "gray", size = 0.25),  # Light gray grid lines for a clean look
      panel.grid.minor = element_blank(),  # Minor grid lines removed
      plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
      axis.ticks = element_line(color = "black", size = 0.5)  # Ticks for axes
    ) 
}

RHFplotFT <- function(fftCalc) {
  
  # Plot the results
  ggplot(fftCalc, aes(x = temperature, y = RHF)) +
    geom_line(color = "blue", size = 1.25) +
    labs(
      title = "NRHF as a function of temperature",
      x = "Temperature (°C)",
      y = "Normalized Non-reversing heat flow (W/g)"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"), # Center title with bold font
      plot.subtitle = element_text(size = 12),  # Adjust the size as needed
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold axis labels
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),  # Bold and separated y-axis title
      axis.text = element_text(size = 18, color = "black"),  # Clear and readable axis text
      axis.line = element_line(color = "black", size = 0.5),  # Black axis lines for better contrast
      panel.grid.major = element_line(color = "gray", size = 0.25),  # Light gray grid lines for a clean look
      panel.grid.minor = element_blank(),  # Minor grid lines removed
      plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
      axis.ticks = element_line(color = "black", size = 0.5)  # Ticks for axes
    ) 
}


