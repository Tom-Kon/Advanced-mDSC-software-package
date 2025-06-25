
RHFplot <- function(calculationMinMaxResults) {
  
# Plot the results
ggplot(calculationMinMaxResults, aes(x = meantemp, y = RHF)) +
  geom_line(color = "blue", linewidth = 1.25) +
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
    axis.line = element_line(color = "black", linewidth = 0.5),  # Black axis lines for better contrast
    panel.grid.major = element_line(color = "gray", linewidth = 0.25),  # Light gray grid lines for a clean look
    panel.grid.minor = element_blank(),  # Minor grid lines removed
    plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
    axis.ticks = element_line(color = "black", linewidth = 0.5)  # Ticks for axes
  )
}

THFplot <- function(calculationMinMaxResults) {

# Plot the results
ggplot(calculationMinMaxResults, aes(x = meantemp, y = THF)) +
  geom_line(color = "blue", linewidth = 1.25) +
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
    axis.line = element_line(color = "black", linewidth = 0.5),  # Black axis lines for better contrast
    panel.grid.major = element_line(color = "gray", linewidth = 0.25),  # Light gray grid lines for a clean look
    panel.grid.minor = element_blank(),  # Minor grid lines removed
    plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
    axis.ticks = element_line(color = "black", linewidth = 0.5)  # Ticks for axes
  ) 
}

NRHFplot <- function(calculationMinMaxResults) {
  
# Plot the results
ggplot(calculationMinMaxResults, aes(x = meantemp, y = NRHF)) +
  geom_line(color = "blue", linewidth = 1.25) +
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
    axis.line = element_line(color = "black", linewidth = 0.5),  # Black axis lines for better contrast
    panel.grid.major = element_line(color = "gray", linewidth = 0.25),  # Light gray grid lines for a clean look
    panel.grid.minor = element_blank(),  # Minor grid lines removed
    plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
    axis.ticks = element_line(color = "black", linewidth = 0.5)  # Ticks for axes
  ) 
}

THFplotFT <- function(calculate_fft) {
  
  # Plot the results
  ggplot(calculate_fft, aes(x = temperature, y = THF)) +
    geom_line(color = "blue", linewidth = 1.25) +
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
      axis.line = element_line(color = "black", linewidth = 0.5),  # Black axis lines for better contrast
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),  # Light gray grid lines for a clean look
      panel.grid.minor = element_blank(),  # Minor grid lines removed
      plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
      axis.ticks = element_line(color = "black", linewidth = 0.5)  # Ticks for axes
    ) 
}

RHFplotFT <- function(calculate_fft) {
  
  # Plot the results
  ggplot(calculate_fft, aes(x = temperature, y = RHF)) +
    geom_line(color = "blue", linewidth = 1.25) +
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
      axis.line = element_line(color = "black", linewidth = 0.5),  # Black axis lines for better contrast
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),  # Light gray grid lines for a clean look
      panel.grid.minor = element_blank(),  # Minor grid lines removed
      plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
      axis.ticks = element_line(color = "black", linewidth = 0.5)  # Ticks for axes
    ) 
}

NRHFplotFT <- function(calculate_fft) {
  
  # Plot the results
  ggplot(calculate_fft, aes(x = temperature, y = NRHF)) +
    geom_line(color = "blue", linewidth = 1.25) +
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
      axis.line = element_line(color = "black", linewidth = 0.5),  # Black axis lines for better contrast
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),  # Light gray grid lines for a clean look
      panel.grid.minor = element_blank(),  # Minor grid lines removed
      plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
      axis.ticks = element_line(color = "black", linewidth = 0.5)  # Ticks for axes
    ) 
}

RHFplotTRIOS <- function(calculationMinMaxResultsTHF) {
  # Plot the results
  ggplot(calculationMinMaxResultsTHF, aes(x = temperature, y = RHF)) +
    geom_line(color = "blue", linewidth = 1.25) +
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
      axis.line = element_line(color = "black", linewidth = 0.5),  # Black axis lines for better contrast
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),  # Light gray grid lines for a clean look
      panel.grid.minor = element_blank(),  # Minor grid lines removed
      plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
      axis.ticks = element_line(color = "black", linewidth = 0.5)  # Ticks for axes
    ) 
}

THFplotTRIOS <- function(calculationMinMaxResultsTHF) {
  
  # Plot the results
  ggplot(calculationMinMaxResultsTHF, aes(x = temperature, y = THF)) +
    geom_line(color = "blue", linewidth = 1.25) +
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
      axis.line = element_line(color = "black", linewidth = 0.5),  # Black axis lines for better contrast
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),  # Light gray grid lines for a clean look
      panel.grid.minor = element_blank(),  # Minor grid lines removed
      plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
      axis.ticks = element_line(color = "black", linewidth = 0.5)  # Ticks for axes
    ) 
}

NRHFplotTRIOS <- function(calculationMinMaxResultsTHF) {
  
  # Plot the results
  ggplot(calculationMinMaxResultsTHF, aes(x = temperature, y = NRHF)) +
    geom_line(color = "blue", linewidth = 1.25) +
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
      axis.line = element_line(color = "black", linewidth = 0.5),  # Black axis lines for better contrast
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),  # Light gray grid lines for a clean look
      panel.grid.minor = element_blank(),  # Minor grid lines removed
      plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
      axis.ticks = element_line(color = "black", linewidth = 0.5)  # Ticks for axes
    ) 
}

RHFplotDSC <- function(CalculationMinMaxResultsDSC) {
  # Plot the results
  ggplot(CalculationMinMaxResultsDSC, aes(x = temperature, y = RHF)) +
    geom_line(color = "blue", linewidth = 1.25) +
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
      axis.line = element_line(color = "black", linewidth = 0.5),  # Black axis lines for better contrast
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),  # Light gray grid lines for a clean look
      panel.grid.minor = element_blank(),  # Minor grid lines removed
      plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
      axis.ticks = element_line(color = "black", linewidth = 0.5)  # Ticks for axes
    ) 
}

THFplotDSC <- function(CalculationMinMaxResultsDSC) {
  
  # Plot the results
  ggplot(CalculationMinMaxResultsDSC, aes(x = temperature, y = THF)) +
    geom_line(color = "blue", linewidth = 1.25) +
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
      axis.line = element_line(color = "black", linewidth = 0.5),  # Black axis lines for better contrast
      panel.grid.major = element_line(color = "gray", linewidth = 0.25),  # Light gray grid lines for a clean look
      panel.grid.minor = element_blank(),  # Minor grid lines removed
      plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
      axis.ticks = element_line(color = "black", linewidth = 0.5)  # Ticks for axes
    ) 
}

NRHFplotDSC <- function(CalculationMinMaxResultsDSC) {
  
    # Plot the results
    ggplot(CalculationMinMaxResultsDSC, aes(x = temperature, y = NRHF)) +
      geom_line(color = "blue", linewidth = 1.25) +
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
        axis.line = element_line(color = "black", linewidth = 0.5),  # Black axis lines for better contrast
        panel.grid.major = element_line(color = "gray", linewidth = 0.25),  # Light gray grid lines for a clean look
        panel.grid.minor = element_blank(),  # Minor grid lines removed
        plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
        axis.ticks = element_line(color = "black", linewidth = 0.5)  # Ticks for axes
      ) 
}


