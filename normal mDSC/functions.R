# Define the function to locate maxima and minima and output their indices and values
locate_extrema_manual <- function(heat_flow_values, time_values, temperature_values) {
  window_size <- 50  # Number of surrounding points to check
  
  maxima_indices <- c()
  minima_indices <- c()
  maxima_times <- c()
  minima_times <- c()
  maxima_temps <- c()
  minima_temps <- c()
  maxima_HFs <- c()
  minima_HFs <- c()
  
  for (i in (window_size + 1):(length(heat_flow_values) - window_size)) {
    local_window <- heat_flow_values[(i - window_size):(i + window_size)]
    
    if (i == (i - window_size) + which.max(local_window) - 1) {  # Ensure unique max
      maxima_indices <- c(maxima_indices, i)
      maxima_times <- c(maxima_times, time_values[i])
      maxima_temps <- c(maxima_temps, temperature_values[i])
      maxima_HFs <- c(maxima_HFs, heat_flow_values[i])
    } 
    
    if (i == (i - window_size) + which.min(local_window) - 1) {  # Ensure unique min
      minima_indices <- c(minima_indices, i)
      minima_times <- c(minima_times, time_values[i])
      minima_temps <- c(minima_temps, temperature_values[i])  # Fixed this line
      minima_HFs <- c(minima_HFs, heat_flow_values[i])
    }
  }
  
  extrema_df <- data.frame(
    type = c(rep("maxima", length(maxima_indices)), rep("minima", length(minima_indices))),
    index = c(maxima_indices, minima_indices),
    time = c(maxima_times, minima_times),
    temperature = c(maxima_temps, minima_temps),
    heat_flow = c(maxima_HFs, minima_HFs)
  )
  
  return(extrema_df)
}

#Counting functions
count_extrema <- function(extrema_df) {
  maxima_count <- sum(extrema_df$type == "maxima")
  minima_count <- sum(extrema_df$type == "minima")
  counts <- data.frame(maxima_count, minima_count)
  return(counts)
}

#Manual calculation functions
HFcalc <- function(extrema_df, heat_amplitude, heating_rate) {
  
  # Check that the number of rows is even; if not, drop the last row
  if (nrow(extrema_df) %% 2 != 0) {
    extrema_df <- extrema_df[-nrow(extrema_df), ]
  }
  
  # Sort by index (which ensures time order)
  extrema_sorted <- extrema_df %>%
    arrange(index)
  
  # Subtract every even from the next odd (i.e., row n+1 - row n)
  diffsHF <- numeric()
  diffstime <- numeric()
  
  maxima <- extrema_df %>%
    filter(type == "maxima")
  
  minima <- extrema_df %>%
    filter(type == "minima")
  
  diff <- abs(nrow(maxima)-nrow(minima))
  print(maxima)
  
  if(nrow(maxima) > nrow(minima)) {
    maxima <- maxima[-(nrow(maxima)-diff+1:nrow(maxima)),]
  } else if(nrow(maxima) < nrow(minima)) {
    minima <- minima[-(nrow(minima)-diff+1:nrow(minima)),]
  }
  
  maxima <- maxima[-(1:7),]
  minima <- minima[-(1:7),]
  
  diffsHF <- maxima$heat_flow-minima$heat_flow
  THF <- (maxima$heat_flow+minima$heat_flow)/2
  temp <- maxima$temperature+minima$temperature
  
  meantemp <- temp/2
  amplitudes <- diffsHF/2
  RHF <- -amplitudes/heat_amplitude*heating_rate/60
  NRHF <- THF-RHF 
  RHFdf <- data.frame(meantemp, RHF, THF, NRHF)
  
  
  return(RHFdf)
}