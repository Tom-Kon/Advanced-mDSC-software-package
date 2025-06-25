#-----------------------------------------------------------------------------------------
#Function to define a vector of time points that will be used in the signal generation
#-----------------------------------------------------------------------------------------

time_generation <- function(reactiveInputs) {
  
  sampling <- reactiveInputs$sampling
  startTemp <- reactiveInputs$startTemp
  endTemp <- reactiveInputs$endTemp
  period <- reactiveInputs$period
  heatRate <- reactiveInputs$heatRate
  
  
  timeSpan <- (endTemp - startTemp) / heatRate
  .nrMods <- ceiling(timeSpan/period)
  
  # Define total number of points
  points_per_mod <- period*sampling  # Points per modulation
  total_points <- .nrMods * (points_per_mod +1) # Total points needed
  
  # Initialize vector with correct length
  times <- numeric(total_points)
  groups <- numeric(total_points)
  
  # Fill the vectors iteratively
  for (i in 1:.nrMods) {
    start_idx <- (i - 1) * (points_per_mod + 1) + 1  # Adjust for extra points
    end_idx <- i * (points_per_mod + 1)  # Include extra point
    times[start_idx:end_idx] <- (i-1) * period + seq(0, period, length.out = (points_per_mod+1))
    groups[start_idx:end_idx] <- i
  }
  
  # Convert to a data frame and filter out duplicates
  timeGen <- data.frame(times, groups) %>%
    distinct(times, .keep_all = TRUE)  # Keep the first occurrence of each time point

  return(timeGen)
}

