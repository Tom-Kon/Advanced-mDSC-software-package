#FUNCTIONS------------------------------------------

# Define a function to filter out rows with duplicate neighbor temperatures
filter_duplicates <- function(data_steps) {
  # Create a new column 'neighbor_temp' which holds the temperature of the previous row
  data_steps <- data_steps %>%
    mutate(neighbor_temp = lag(temperature)) # lag shifts the vector to the previous row
  
  # Remove rows where the temperature is the same as the previous one
  filtered_data <- data_steps %>%
    filter(temperature != neighbor_temp | is.na(neighbor_temp)) %>%
    select(-neighbor_temp) # Remove the 'neighbor_temp' column as it's no longer needed
  
  return(filtered_data)
}

# Function to count maxima and minima manually
count_extrema_manual <- function(temp_values) {
  num_maxima <- sum(temp_values[2:(length(temp_values) - 1)] > temp_values[1:(length(temp_values) - 2)] &
                      temp_values[2:(length(temp_values) - 1)] > temp_values[3:length(temp_values)])
  
  num_minima <- sum(temp_values[2:(length(temp_values) - 1)] < temp_values[1:(length(temp_values) - 2)] &
                      temp_values[2:(length(temp_values) - 1)] < temp_values[3:length(temp_values)])
  
  return(c(num_maxima, num_minima))
}

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

# Define a function to delete data after the last maximum for each pattern
delete_data_after_last_maximum <- function(data_steps_cleaned, extrema_df) {
  # Initialize an empty dataframe to store the filtered results (First Pass)
  data_steps_cleaned_2 <- data.frame()
  
  # Step 1: Apply only the Tref_for_pattern condition
  for (pattern_id in unique(data_steps_cleaned$pattern)) {
    # Filter extrema_df to get minima and maxima for the current pattern
    minima_for_pattern <- extrema_df %>%
      filter(type == "minima" & pattern == pattern_id)
    
    maxima_for_pattern <- extrema_df %>%
      filter(type == "maxima" & pattern == pattern_id) %>%
      arrange(index)  # Ensure maxima are sorted in order
    
    # If there are no minima or maxima, skip this pattern
    if (nrow(minima_for_pattern) == 0 || nrow(maxima_for_pattern) == 0) next
    
    # Get the last minimum
    last_minimum <- minima_for_pattern %>%
      slice_max(order_by = index, n = 1)
    
    # Get the last maximum
    last_maximum <- maxima_for_pattern %>%
      slice(n())  # Last row (most recent maximum)
    
    # Get the time and temperature of the last maximum and last minimum
    max_time <- last_maximum$time
    max_temp <- last_maximum$temperature
    min_time <- last_minimum$time
    min_temp <- last_minimum$temperature
    
    # Calculate the reference temperature Tref for this pattern
    Tref_for_pattern <- step_size * pattern_id + starting_temp
    
    # Apply Tref condition
    if (min_temp > Tref_for_pattern) {
      # Keep data up to last maximum time
      data_up_to_maximum <- data_steps_cleaned %>%
        filter(pattern == pattern_id & time <= max_time)
    } else {
      # Keep all data
      data_up_to_maximum <- data_steps_cleaned %>%
        filter(pattern == pattern_id)
    }
    
    # Append to the new dataframe
    data_steps_cleaned_2 <- bind_rows(data_steps_cleaned_2, data_up_to_maximum)
  }
  
  # Step 2: Apply the 180 points condition
  for (pattern_id in unique(data_steps_cleaned_2$pattern)) {
    # Extract maxima again (now from the cleaned dataset)
    maxima_for_pattern <- extrema_df %>%
      filter(type == "maxima" & pattern == pattern_id) %>%
      arrange(index)  # Ensure maxima are sorted in order
    
    # Extract minima again
    minima_for_pattern <- extrema_df %>%
      filter(type == "minima" & pattern == pattern_id)
    
    # If there are no minima or maxima, skip this pattern
    if (nrow(minima_for_pattern) == 0 || nrow(maxima_for_pattern) <= 1) next  # Need at least two maxima
    
    # Get last maximum and second-to-last maximum
    last_maximum <- maxima_for_pattern %>%
      slice(n())  # Last row (most recent maximum)
    
    second_last_maximum <- maxima_for_pattern %>%
      slice(n() - 1)  # Second-to-last row
    
    # Get the index values
    last_max_index <- last_maximum$index
    last_min_index <- minima_for_pattern %>%
      slice_max(order_by = index, n = 1) %>%
      pull(index)
    
    # Check number of points between last maximum and last minimum
    num_points_between <- abs(last_min_index - last_max_index)
    
    # Apply the 180 points condition
    if (num_points_between < (sampling*period*60)/2 - points_distance_minimum_margin) {
      # Print temperature where this condition is met
      print(paste("Deleting data after second-to-last maximum at temperature:", second_last_maximum$temperature))
      
      # Remove data after the second-to-last maximum
      data_steps_cleaned_2 <- data_steps_cleaned_2 %>%
        filter(!(pattern == pattern_id & time > second_last_maximum$time))
    }
  }
  
  return(data_steps_cleaned_2)
}

# Define a function to delete data after the last minimum for each pattern -UNUSED!!!
# delete_data_after_last_minimum <- function(data_steps_cleaned_2, extrema_df2) {
#   # Initialize an empty dataframe to store the filtered results
#   data_steps_cleaned_3 <- data.frame()
#   
#   # Loop through each pattern to apply the deletion condition
#   for (pattern_id in unique(data_steps_cleaned_2$pattern)) {
#     # Filter extrema_df2 to get minima for the current pattern
#     minima_for_pattern <- extrema_df2 %>%
#       filter(type == "minima" & pattern == pattern_id)
#     
#     # Get the minimum with the highest index (the last minimum)
#     last_minimum <- minima_for_pattern %>%
#       slice_max(order_by = index, n = 1)
#     
#     # Get the time and temperature of the last minimum
#     min_time <- last_minimum$time
#     min_temp <- last_minimum$temperature
#     
#     # Check the condition: if the minimum is larger than Tref
#     data_up_to_minimum <- data_steps_cleaned_2 %>%
#       filter(pattern == pattern_id & time <= min_time)
#     
#     # Add the filtered data to the new dataframe
#     data_steps_cleaned_3 <- bind_rows(data_steps_cleaned_3, data_up_to_minimum)
#     
#   }
#   return(data_steps_cleaned_3)
#   
# }


# Define a function to only keep data after an equilibrium is reached
delete_data_until_equil <- function(data_steps_cleaned_3, extrema_df2) {
  # Initialize an empty dataframe to store the filtered results
  data_steps_cleaned_4 <- data.frame()
  
  # Loop through each pattern to apply the deletion condition
  for (pattern_id in unique(data_steps_cleaned_3$pattern)) {
    # Filter extrema_df2 to get minima for the current pattern
    maxima_for_pattern <- extrema_df2 %>%
      filter(type == "maxima" & pattern == pattern_id)
    
    # Get the minimum with the highest index (the last minimum)
    last_maximum <- maxima_for_pattern %>%
      slice_max(order_by = index, n = 1)
    
    # Get the time and temperature of the last minimum
    max_time <- last_maximum$time
    target_time <- max_time-(period*modulations_back)
    max_temp <- last_maximum$temperature
    
    
    # Find the row in pattern_extrema whose time is closest to the target time
    closest_row <- maxima_for_pattern %>% 
      slice(which.min(abs(time - target_time)))
    
    # Use that row’s temperature as the starting time
    start_time <- closest_row$time
    
    # Check the condition: if the minimum is larger than Tref
    data_from_start <- data_steps_cleaned_3 %>%
      filter(pattern == pattern_id & time >= start_time)
    
    # Add the filtered data to the new dataframe
    data_steps_cleaned_4 <- bind_rows(data_steps_cleaned_4, data_from_start)
    
  }
  return(data_steps_cleaned_4)
  
}

# Define a function to delete data after the last minimum for each pattern in extrema_df2
delete_extrema_until_equil <- function(extrema_df2, data_steps_cleaned_3) {
  # Initialize an empty dataframe to store the filtered results
  extrema_df3 <- data.frame()
  
  # Loop through each pattern to apply the deletion condition
  for (pattern_id in unique(data_steps_cleaned_3$pattern)) {
    # Filter extrema_df2 to get minima and maxima for the current pattern
    extrema_for_pattern <- extrema_df2 %>%
      filter(pattern == pattern_id)
    
    # Get the minimum with the highest index (the last minimum)
    last_maximum <- extrema_for_pattern %>%
      filter(type == "maxima") %>%
      slice_max(order_by = index, n = 1)
    
    # Get the time of the last minimum
    max_time <- last_maximum$time
    start_time <- max_time - (period * modulations_back)
    
    # Filter extrema_df2 to get extrema (minima and maxima) after the last minimum time
    extrema_from_start <- extrema_for_pattern %>%
      filter(time >= start_time)
    
    # Add the filtered extrema to the new dataframe
    extrema_df3 <- bind_rows(extrema_df3, extrema_from_start)
  }
  return(extrema_df3)
}

#------------
