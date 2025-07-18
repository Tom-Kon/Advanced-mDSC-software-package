#FUNCTIONS------------------------------------------
excel_cleaner <- function(Excel, sheet, startingTemp) {
  
  if (length(excel_sheets(Excel)) < sheet) {
    msg <- paste0("Error: you're trying to select a sheet that does not exist. You Excel only has ", 
                  length(excel_sheets(Excel)), " sheet(s), while you're trying to select sheet number ", 
                  sheet)
    return(msg)
  }  
  
  Excel <- read_excel(Excel, sheet, col_names = FALSE)
  row_idx <- match(TRUE, apply(Excel, 1, function(r) any(tolower(r) == "time")))
  headers <- as.vector(unlist(Excel[row_idx, ]))
  Excel <- na.omit(Excel)
  
  temp <- sapply(headers, function(x) strsplit(x, " "))
  
  idxtime <- which(vapply(temp, function(x) any(tolower(x) %in% "time"), 
                          logical(1)) )
  temp[idxtime] <- "time"
  
  idxmodtemp <- which(vapply(temp, function(x) {all(c("temperature", "modulated") %in% tolower(x))}, 
                             logical(1)))
  temp[idxmodtemp] <- "modTemp"
  
  idxtemp <- which(vapply(temp, function(x) any(tolower(x) %in% "temperature"), 
                          logical(1)) )
  temp[idxtemp] <- "temperature"
  
  idxModhf <- which(vapply(temp, function(x) {all(c("heat", "flow", "modulated") %in% tolower(x))}, 
                           logical(1)))
  temp[idxModhf] <- "modHeatFlow"
  
  idxRevhf <- which(vapply(temp, function(x) {all(c("reversing", "heat", "flow") %in% tolower(x))}, 
                           logical(1)))
  temp[idxRevhf] <- "RevmodHeatFlow"
  
  idxNonRevhf <- which(vapply(temp, function(x) {all(c("non-reversing","heat", "flow") %in% tolower(x))}, 
                              logical(1)))
  temp[idxNonRevhf] <- "NonRevmodHeatFlow"
  
  idxhf <- which(vapply(temp, function(x) {all(c("heat", "flow") %in% tolower(x))}, 
                        logical(1)))
  temp[idxhf] <- "heatFlow"
  
  headers <- unlist(temp)
  
  if (length(idxhf) > 1) {
    msg <- "Error: there are multiple columns containing the terms \"heat flow\" in your selected Excel sheet"
    return(msg)
  }
  
  if (length(headers[idxmodtemp]) == 0) {
    msg <- "Error: there is no modulated temperature column in your selected Excel sheet"
    return(msg)
  }
  
  if (length(headers[idxtime]) == 0) {
    msg <- "Error: there is no modulated time column in your selected Excel sheet."
    return(msg)
    
  }
  
  if (length(headers[idxModhf]) == 0) {
    msg <- "Error: there is no modulated heat flow column in your selected Excel sheet"
    return(msg)
  }
  
  suppressWarnings(
    Excel <- Excel %>%
      mutate(across(everything(), ~ {
        # Replace commas with dots, then convert to numeric
        if (is.character(.)) as.numeric(gsub(",", ".", .)) else .
      })) %>%
      setNames(headers) %>%                          # rename columns
      mutate(across(everything(), as.numeric)) %>% # Ensure all columns are numeric
      drop_na()
  )

  
  
  for(i in seq_along(Excel)) {
    tempcol <- Excel[[i]]
    tempcheck <- tempcol[1]
    tempcheck <- gsub("\\.", "", tempcol[1])
    
    if (nchar(tempcheck) < 5) {
      if(i == 1) {errorSigFig <- "Warning: less than 5 significant figures were detected in your time data. This might affect the quality of the results"
      }
      if(i == 2) {errorSigFig <- "Warning: less than 5 significant figures were detected in your modulated temperature data. This might affect the quality of the results"
      }
      if(i == 3) {errorSigFig <- "Warning: less than 5 significant figures were detected in your modulated heat flow data. This might affect the quality of the results"
      }
      attr(Excel, "comment") <- errorSigFig
      break
    } 
  }
  
  if(startingTemp < Excel$modTemp[1]) {
    msg <- "Error: the starting temperature that you entered as input is lower than the first temperature in the Excel"
    return(msg)  
  }
  
  return(Excel)
}




# Define a function to filter out rows with duplicate neighbor temperatures
filter_duplicates <- function(originalDataFilteredUniquePatterns) {
  # Create a new column 'neighbor_temp' which holds the temperature of the previous row
  originalDataFilteredUniquePatterns <- originalDataFilteredUniquePatterns %>%
    mutate(neighbor_temp = lag(modTemp)) # lag shifts the vector to the previous row
  
  # Remove rows where the temperature is the same as the previous one
  filtered_data <- originalDataFilteredUniquePatterns %>%
    filter(modTemp != neighbor_temp | is.na(neighbor_temp)) %>%
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
locate_extrema <- function(heat_flow_values, time_values, temperature_values) {
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
  
  extremaDfIntermediate <- data.frame(
    type = c(rep("maxima", length(maxima_indices)), rep("minima", length(minima_indices))),
    index = c(maxima_indices, minima_indices),
    time = c(maxima_times, minima_times),
    modTemp = c(maxima_temps, minima_temps),
    modHeatFlow = c(maxima_HFs, minima_HFs)
  )
  
  return(extremaDfIntermediate)
}

# Define a function to delete data after the last maximum for each pattern
delete_data_after_last_maximum <- function(isolatedPatterns, extremaDfIntermediate, 
                                           stepSize, startingTemp, sampling, period, 
                                           pointsDistanceMinimumMargin) {
  
  # Initialize an empty dataframe to store the filtered results (First Pass)
  deleteLastMax <- data.frame()
  
  # Step 1: Apply only the Tref_for_pattern condition
  for (pattern_id in unique(isolatedPatterns$pattern)) {
    # Filter extrema_df to get minima and maxima for the current pattern
    minima_for_pattern <- extremaDfIntermediate %>%
      filter(type == "minima" & pattern == pattern_id) %>%
      arrange(index)  # Ensure minima are sorted in order
    
    maxima_for_pattern <- extremaDfIntermediate %>%
      filter(type == "maxima" & pattern == pattern_id) %>%
      arrange(index)  # Ensure maxima are sorted in order
    
    # If there are no minima or maxima, skip this pattern
    if (nrow(minima_for_pattern) == 0 || nrow(maxima_for_pattern) == 0) next
    
    # Get the last minimum
    last_minimum <- minima_for_pattern %>%
      slice(n())  # Last row (most recent maximum)
    
    # Get the last maximum
    last_maximum <- maxima_for_pattern %>%
      slice(n())  # Last row (most recent maximum)
    
    # Get the time and temperature of the last maximum and last minimum
    max_time <- last_maximum$time
    max_temp <- last_maximum$modTemp
    min_time <- last_minimum$time
    min_temp <- last_minimum$modTemp
    
    # Calculate the reference temperature Tref for this pattern
    Tref_for_pattern <- stepSize * pattern_id + startingTemp
    
    # Apply Tref condition
    if (min_temp > Tref_for_pattern) {
      # Keep data up to last maximum time
      data_up_to_maximum <- isolatedPatterns %>%
        filter(pattern == pattern_id & time <= max_time)
    } else {
      # Keep all data
      data_up_to_maximum <- isolatedPatterns %>%
        filter(pattern == pattern_id)
    }
    
    # Append to the new dataframe
    deleteLastMax <- bind_rows(deleteLastMax, data_up_to_maximum)
  }
  
  # Step 2: Apply the 180 points condition
  for (pattern_id in unique(deleteLastMax$pattern)) {
    # Extract maxima again (now from the cleaned dataset)
    maxima_for_pattern <- extremaDfIntermediate %>%
      filter(type == "maxima" & pattern == pattern_id) %>%
      arrange(index)  # Ensure maxima are sorted in order
    
    # Extract minima again
    minima_for_pattern <- extremaDfIntermediate %>%
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
    if (num_points_between < (sampling*period)/2 - pointsDistanceMinimumMargin) {
      # Print temperature where this condition is met
      print(paste("Deleting data after second-to-last maximum at temperature:", second_last_maximum$modTemp))
      
      # Remove data after the second-to-last maximum
      deleteLastMax <- deleteLastMax %>%
        filter(!(pattern == pattern_id & time > second_last_maximum$time))
    }
  }
  
  return(deleteLastMax)
}

# Define a function to delete data after the last minimum for each pattern -UNUSED!!!
# delete_data_after_last_minimum <- function(data_steps_cleaned_2, extremaDfAfterDeleteMax) {
#   # Initialize an empty dataframe to store the filtered results
#   deleteLastMax <- data.frame()
#   
#   # Loop through each pattern to apply the deletion condition
#   for (pattern_id in unique(data_steps_cleaned_2$pattern)) {
#     # Filter extremaDfAfterDeleteMax to get minima for the current pattern
#     minima_for_pattern <- extremaDfAfterDeleteMax %>%
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
#     deleteLastMax <- bind_rows(deleteLastMax, data_up_to_minimum)
#     
#   }
#   return(deleteLastMax)
#   
# }


# Define a function to only keep data after an equilibrium is reached
delete_data_until_equil <- function(extremaDfAfterDeleteMax, deleteLastMax, period, modulationsBack) {
  # Initialize an empty dataframe to store the filtered results
  finalDataForAnalysis <- data.frame()

  # Loop through each pattern to apply the deletion condition
  for (pattern_id in unique(deleteLastMax$pattern)) {
    
    # Filter extremaDfAfterDeleteMax to get minima for the current pattern
    maxima_for_pattern <- extremaDfAfterDeleteMax %>%
      filter(type == "maxima" & pattern == pattern_id)
    
    # Get the minimum with the highest index (the last minimum)
    last_maximum <- maxima_for_pattern %>%
      slice_max(order_by = index, n = 1)
    
    # Get the time and temperature of the last minimum
    max_time <- last_maximum$time
    target_time <- max_time-(period/60*modulationsBack)
    max_temp <- last_maximum$modTemp
    
    
    # Find the row in pattern_extrema whose time is closest to the target time
    closest_row <- maxima_for_pattern %>% 
      slice(which.min(abs(time - target_time)))
    
    # Use that row’s temperature as the starting time
    start_time <- closest_row$time
    
    # Check the condition: if the minimum is larger than Tref
    data_from_start <- deleteLastMax %>%
      filter(pattern == pattern_id & time >= start_time)
    
    # Add the filtered data to the new dataframe
    finalDataForAnalysis <- bind_rows(finalDataForAnalysis, data_from_start)
    
  }
  return(finalDataForAnalysis)
  
}

# Define a function to only keep data after an equilibrium is reached in extremaDfAfterDeleteMax
delete_extrema_until_equil <- function(extremaDfAfterDeleteMax, deleteLastMax, period, modulationsBack) {
  # Initialize an empty dataframe to store the filtered results
  finalAnalysisExtrema <- data.frame()
  
  # Loop through each pattern to apply the deletion condition
  for (pattern_id in unique(deleteLastMax$pattern)) {
    # Filter extremaDfAfterDeleteMax to get minima and maxima for the current pattern
    extrema_for_pattern <- extremaDfAfterDeleteMax %>%
      filter(pattern == pattern_id)
    
    # Get the minimum with the highest index (the last minimum)
    last_maximum <- extrema_for_pattern %>%
      filter(type == "maxima") %>%
      slice_max(order_by = index, n = 1)
    
    # Get the time of the last minimum
    max_time <- last_maximum$time
    start_time <- max_time - (period * modulationsBack)
    
    # Filter extremaDfAfterDeleteMax to get extrema (minima and maxima) after the last minimum time
    extrema_from_start <- extrema_for_pattern %>%
      filter(time >= start_time)
    
    # Add the filtered extrema to the new dataframe
    finalAnalysisExtrema <- bind_rows(finalAnalysisExtrema, extrema_from_start)
  }
  return(finalAnalysisExtrema)
}

#------------
