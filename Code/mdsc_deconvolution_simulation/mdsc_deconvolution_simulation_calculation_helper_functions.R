locate_extrema <- function(modHeatFlow_values, time_values, temperature_values) {
  window_size <- 50  # Number of surrounding points to check
  
  maxima_indices <- c()
  minima_indices <- c()
  maxima_times <- c()
  minima_times <- c()
  maxima_temps <- c()
  minima_temps <- c()
  maxima_HFs <- c()
  minima_HFs <- c()
  
  for (i in (window_size + 1):(length(modHeatFlow_values) - window_size)) { #Define window
    local_window <- modHeatFlow_values[(i - window_size):(i + window_size)]
    
    if (i == (i - window_size) + which.max(local_window) - 1) {  # Ensure unique max in each window and find it 
      maxima_indices <- c(maxima_indices, i)
      maxima_times <- c(maxima_times, time_values[i])
      maxima_temps <- c(maxima_temps, temperature_values[i])
      maxima_HFs <- c(maxima_HFs, modHeatFlow_values[i])
    } 
    
    if (i == (i - window_size) + which.min(local_window) - 1) {  # Ensure unique min in each window and find it 
      minima_indices <- c(minima_indices, i)
      minima_times <- c(minima_times, time_values[i])
      minima_temps <- c(minima_temps, temperature_values[i])
      minima_HFs <- c(minima_HFs, modHeatFlow_values[i])
    }
  }
  
  extremaDf <- data.frame(                                      # Make the extremaDf dataframe that will be used everywhere later
    type = c(rep("maxima", length(maxima_indices)), rep("minima", length(minima_indices))),
    index = c(maxima_indices, minima_indices),
    time = c(maxima_times, minima_times),
    temperature = c(maxima_temps, minima_temps),
    modHeatFlow = c(maxima_HFs, minima_HFs)
  )
  
  return(extremaDf)
}

count_extrema <- function(extremaDf) {
  maxima_count <- sum(extremaDf$type == "maxima")
  minima_count <- sum(extremaDf$type == "minima")
  counts <- data.frame(maxima_count, minima_count)
  return(counts)
}

calculate_heatflow_min_max <- function(extremaDf, RHFCalcDenominator, heatingRate) {
  
  # Check that the number of rows is even; if not, drop the last row
  if (nrow(extremaDf) %% 2 != 0) {
    extremaDf <- extremaDf[-nrow(extremaDf), ]
  }
  
  # Sort by index (which ensures time order)
  extrema_sorted <- extremaDf %>%
    arrange(index)
  
  # Subtract every even from the next odd (i.e., row n+1 - row n)
  diffsHF <- numeric()
  diffstime <- numeric()
  
  maxima <- extremaDf %>%
    filter(type == "maxima")
  
  minima <- extremaDf %>%
    filter(type == "minima")
  
  diff <- abs(nrow(maxima)-nrow(minima))
  
  if(nrow(maxima) > nrow(minima)) {
    maxima <- maxima[-(nrow(maxima)-diff+1:nrow(maxima)),]
  } else if(nrow(maxima) < nrow(minima)) {
    minima <- minima[-(nrow(minima)-diff+1:nrow(minima)),]
  }
  
  #Calculations
  diffsHF <- maxima$modHeatFlow-minima$modHeatFlow
  THF <- (maxima$modHeatFlow+minima$modHeatFlow)/2
  temp <- maxima$temperature+minima$temperature
  
  meantemp <- temp/2
  amplitudes <- diffsHF/2
  RHF <- -amplitudes/RHFCalcDenominator*heatingRate/60
  NRHF <- THF-RHF 
  
  calculationMinMaxResults <- data.frame(meantemp, RHF, THF, NRHF)
  
  return(calculationMinMaxResults)
}