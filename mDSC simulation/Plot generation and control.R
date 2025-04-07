num_ticks_x <- 10
num_ticks_y <- 5

MHFplots <- function(resampled_points) {
  
  # Create each ggplot object with the specified names
  MHF_and_baselinecorr_MHF <- ggplot(resampled_points) +
    geom_line(data = resampled_points, aes(x = TRef, y = BaselinecorrMHFNotEven), color = "blue", size = 1) +
    geom_line(data = resampled_points, aes(x = TRef, y = MHF), color = "red", size = 1.5) +
    labs(title = "Initial modulated heat flow and baseline-corrected modulated heat flow",
         x = "Temperature (°C)",
         y = "Modulated Heat Flow (W/g)"
    ) +
    theme_minimal(base_size = 18) +  # Larger base font size for better readability
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"), # Center title with bold font
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold axis labels
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),  # Bold and separated y-axis title
      axis.text = element_text(size = 18, color = "black"),  # Clear and readable axis text
      axis.line = element_line(color = "black", size = 0.5),  # Black axis lines for better contrast
      panel.grid.major = element_line(color = "gray", size = 0.25),  # Light gray grid lines for a clean look
      panel.grid.minor = element_blank(),  # Minor grid lines removed
      plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
      axis.ticks = element_line(color = "black", size = 0.5)  # Ticks for axes
    ) +
    scale_x_continuous(expand = c(0.005, 0.005), breaks = scales::pretty_breaks(n = num_ticks_x)) +  # Set the number of x-axis ticks
    scale_y_continuous(
      expand = c(0.0002, 0.0002), # Remove space between plot and y-axis
      breaks = scales::pretty_breaks(n = num_ticks_y)
    )  # This ensures the y-axis covers the full range of your data with extra space at the top
    
  return(MHF_and_baselinecorr_MHF)  
}

overlayplot <- function(resampled_points) {
  
  Overlay <- ggplot(resampled_points) +
    geom_line(data = resampled_points, aes(x = TRef, y = loessTHF, color = "THF"), size = 1.3) +
    geom_line(data = resampled_points, aes(x = TRef, y = loessRHF, color = "RHF"), size = 1.3) +
    geom_line(data = resampled_points, aes(x = TRef, y = loessNRHF, color = "NRHF"), size = 1.3) +
    labs(title = "Overlay of the total, reversing and non-reversing heat flows",
         x = "Time",
         y = "Heat flow (W/g)",
         color = "Legend") +  # Change the legend title here
    scale_color_manual(values = c("THF" = "blue", "RHF" = "red", "NRHF" = "darkgreen")) +
    theme_minimal(base_size = 18) +
    theme(
      plot.subtitle = element_text(size = 12),  # Adjust the size as needed
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"),
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", size = 0.5),
      panel.grid.major = element_line(color = "gray", size = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      axis.ticks = element_line(color = "black", size = 0.5)
    ) +
    scale_x_continuous(expand = c(0.005, 0.005), breaks = scales::pretty_breaks(n = num_ticks_x)) +  # Set the number of x-axis ticks
    scale_y_continuous(
      expand = c(0.0002, 0.0002), # Remove space between plot and y-axis
      breaks = scales::pretty_breaks(n = num_ticks_y)
    )  # This ensures the y-axis covers the full range of your data with extra space at the top
  
  return(Overlay)  
  
  }

smoothedTHFplot <- function(resampled_points) {
  Smoothed_THF <- ggplot(resampled_points) +
    geom_line(data = resampled_points, aes(x = TRef, y = loessTHF), color = "blue", size = 1.3) +
    labs(title = "LOESS smoothed Total heat flow",
         x = "Time",
         y = "Total heat flow (W/g)") +
    theme_minimal(base_size = 18) +  # Larger base font size for better readability
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"), # Center title with bold font
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold axis labels
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),  # Bold and separated y-axis title
      axis.text = element_text(size = 18, color = "black"),  # Clear and readable axis text
      axis.line = element_line(color = "black", size = 0.5),  # Black axis lines for better contrast
      panel.grid.major = element_line(color = "gray", size = 0.25),  # Light gray grid lines for a clean look
      panel.grid.minor = element_blank(),  # Minor grid lines removed
      plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
      axis.ticks = element_line(color = "black", size = 0.5)  # Ticks for axes
    ) +
    scale_x_continuous(expand = c(0.005, 0.005), breaks = scales::pretty_breaks(n = num_ticks_x)) +  # Set the number of x-axis ticks
    scale_y_continuous(
      expand = c(0.0002, 0.0002), # Remove space between plot and y-axis
      breaks = scales::pretty_breaks(n = num_ticks_y)
    )  # This ensures the y-axis covers the full range of your data with extra space at the top
  return(Smoothed_THF)  
  }
  

  
smoothedRHFplot <- function(resampled_points) {
  
  Smoothed_RHF <- ggplot(resampled_points) +
    geom_line(data = resampled_points, aes(x = TRef, y = loessRHF), color = "blue", size = 1.3) +
    labs(title = "LOESS smoothed Reversing heat flow",
         x = "Time",
         y = "Reversing heat flow (W/g)") +
    theme_minimal(base_size = 18) +  # Larger base font size for better readability
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"), # Center title with bold font
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold axis labels
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),  # Bold and separated y-axis title
      axis.text = element_text(size = 18, color = "black"),  # Clear and readable axis text
      axis.line = element_line(color = "black", size = 0.5),  # Black axis lines for better contrast
      panel.grid.major = element_line(color = "gray", size = 0.25),  # Light gray grid lines for a clean look
      panel.grid.minor = element_blank(),  # Minor grid lines removed
      plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
      axis.ticks = element_line(color = "black", size = 0.5)  # Ticks for axes
    ) +
    scale_x_continuous(expand = c(0.005, 0.005), breaks = scales::pretty_breaks(n = num_ticks_x)) +  # Set the number of x-axis ticks
    scale_y_continuous(
      expand = c(0.0002, 0.0002), # Remove space between plot and y-axis
      breaks = scales::pretty_breaks(n = num_ticks_y)
    )  # This ensures the y-axis covers the full range of your data with extra space at the top
  return(Smoothed_RHF)
}  
 
smoothedNRHFplot <- function(resampled_points) {
  Smoothed_NRHF <- ggplot(resampled_points) +
    geom_line(data = resampled_points, aes(x = TRef, y = loessNRHF), color = "blue", size = 1.3) +
    labs(title = "LOESS smoothed non-reversing heat flow",
         x = "Time",
         y = "Non-reversing heat flow (W/g)") +
    theme_minimal(base_size = 18) +  # Larger base font size for better readability
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"), # Center title with bold font
      axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold axis labels
      axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),  # Bold and separated y-axis title
      axis.text = element_text(size = 18, color = "black"),  # Clear and readable axis text
      axis.line = element_line(color = "black", size = 0.5),  # Black axis lines for better contrast
      panel.grid.major = element_line(color = "gray", size = 0.25),  # Light gray grid lines for a clean look
      panel.grid.minor = element_blank(),  # Minor grid lines removed
      plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
      axis.ticks = element_line(color = "black", size = 0.5)  # Ticks for axes
    ) +
    scale_x_continuous(expand = c(0.005, 0.005), breaks = scales::pretty_breaks(n = num_ticks_x)) +  # Set the number of x-axis ticks
    scale_y_continuous(
      expand = c(0.0002, 0.0002), # Remove space between plot and y-axis
      breaks = scales::pretty_breaks(n = num_ticks_y)
    )  # This ensures the y-axis covers the full range of your data with extra space at the top
  return(Smoothed_NRHF)
}  

