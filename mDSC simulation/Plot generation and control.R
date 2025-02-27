
ggplots <- function(resampled_points) {
  
  # Create each ggplot object with the specified names
  MHF_and_baselinecorr_MHF <- ggplot() +
    geom_line(data = resampled_points, aes(x = time, y = BaselinecorrMHF), color = "blue", size = 1) +
    geom_line(data = resampled_points, aes(x = time, y = MHF), color = "red", size = 1) +
    labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
         x = "Time",
         y = "Signal / Rolling Mean") +
    theme_minimal()
  
  Overlay <- ggplot() +
    geom_line(data = resampled_points, aes(x = time, y = loessTHF), color = "blue", size = 1) +
    geom_line(data = resampled_points, aes(x = time, y = loessRHF), color = "red", size = 1) +
    geom_line(data = resampled_points, aes(x = time, y = loessNRHF), color = "green", size = 1) +
    labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
         x = "Time",
         y = "Signal / Rolling Mean") +
    theme_minimal() 
  
  Smoothed_THF <- ggplot() +
    geom_line(data = resampled_points, aes(x = time, y = loessTHF), color = "blue", size = 1) +
    labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
         x = "Time",
         y = "Signal / Rolling Mean") +
    theme_minimal()
  
  Smoothed_RHF <- ggplot() +
    geom_line(data = resampled_points, aes(x = time, y = loessRHF), color = "blue", size = 1) +
    labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
         x = "Time",
         y = "Signal / Rolling Mean") +
    theme_minimal()
  
  Smoothed_NRHF <- ggplot() +
    geom_line(data = resampled_points, aes(x = time, y = loessNRHF), color = "blue", size = 1) +
    labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
         x = "Time",
         y = "Signal / Rolling Mean") +
    theme_minimal()
  
  # Return the plots as a list with the new names
  return(list(MHF_and_baselinecorr_MHF = MHF_and_baselinecorr_MHF, 
              Overlay = Overlay, 
              Smoothed_THF = Smoothed_THF, 
              Smoothed_RHF = Smoothed_RHF, 
              Smoothed_NRHF = Smoothed_NRHF))
}
