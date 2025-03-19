processDSC <- function(file, Excel, export, rangesmin, rangesmax, starting_temp, step_size,
           modulations_back, period, setAmplitude, saveNRHFplot, saveRevCpplot, savemanualRevCpplot, 
           saveDatasteps3, saveExtremadf3, saveSummaryFT) {

  #Don't touch this unless you know what you're doing----
  temp_margin_first_cleanup <- 0.05
  tempModAmplitude <- setAmplitude*2*pi/period
  sampling <- 10 # pts/sec
  points_distance_minimum_margin <- (sampling*period*60)/2/5
  subtitle <- paste0("Dataset: ", "test")
  tempModAmplitude <- setAmplitude*2*pi/period
  num_ticks <- 10
  sample_size <- 5.79

  source("detailed functions.R")
  
    # 1. Load and pre‐process the data -------------------------------
  d <- na.omit(read.xlsx(Excel))
  d <- d[-1,]
  d[] <- lapply(d, function(x) if(is.character(x)) as.numeric(gsub(",", ".", x)) else x)
  names(d) <- c("time", "temperature", "heat_flow")
  
  # Define the temperature ranges----
  rangesn <- c(0:100)
  rangesmax <- starting_temp + rangesn * step_size + setAmplitude + temp_margin_first_cleanup
  rangesmin <- starting_temp + rangesn * step_size - setAmplitude - temp_margin_first_cleanup

  
  # Filter based on temperature ranges
  d_filtered <- d %>%
    filter(sapply(temperature, function(temp) any(temp >= rangesmin & temp <= rangesmax)))
  
  # Remove duplicate time points
  d_unique <- d_filtered %>%
    distinct(time, .keep_all = TRUE)
  
  # Assign each data point to a pattern
  d_steps <- d_unique %>%
    mutate(pattern = floor((temperature - starting_temp + setAmplitude + temp_margin_first_cleanup)/ step_size))
  
  # Apply your duplicate-filtering function (already defined in your code)
  d_steps_cleaned <- filter_duplicates(d_steps)
  
  
  #Generate extrema_df1
  extrema_df1 <- d_steps_cleaned %>% 
    group_by(pattern) %>% 
    summarise(extrema_info = list(locate_extrema_manual(heat_flow, time, temperature))) %>% 
    unnest(cols = c(extrema_info))
  
  # Then proceed with your cleaning steps – note that you must run your functions that delete data etc.
  d_steps_cleaned_2 <- delete_data_after_last_maximum(d_steps_cleaned, extrema_df1, step_size, starting_temp, sampling, period, points_distance_minimum_margin)

  
  # Recompute extrema on the cleaned data for the subsequent steps
  extrema_counts2 <- d_steps_cleaned_2 %>%
    group_by(pattern) %>%
    summarise(extrema_info = list(locate_extrema_manual(heat_flow, time, temperature)))
  
  extrema_df2 <- extrema_counts2 %>% unnest(cols = c(extrema_info))
  
  # d_steps_cleaned_3 <- delete_data_after_last_minimum(d_steps_cleaned_2, extrema_df2)
  d_steps_cleaned_3 <- delete_data_until_equil(d_steps_cleaned_2, extrema_df2, period, modulations_back)
  TRef <- d_steps_cleaned_3$pattern*step_size+starting_temp
  d_steps_cleaned_3 <- cbind(d_steps_cleaned_3, TRef)
  
  # 2. Compute the FFT and extract the dc component ----------------------
  
  # Group the data and store the time vector for each group
  ft_averages <- d_steps_cleaned_3 %>%
    group_by(pattern) %>%
    summarise(
      n_points = n(),
      time_vector = list(time),    # store time data (in minutes)
      # FFT for DC component:
      fft_result_dc = list(fft(heat_flow)),
      # For the first harmonic, process the data before FFT:
      fft_result_harm = list({
        n <- length(heat_flow)
        hf_centered <- heat_flow - mean(heat_flow)  # detrend the signal
        
        # Create a Hanning window:
        hanning <- 0.5 - 0.5 * cos(2 * pi * (0:(n - 1)) / (n - 1))
        # Compute the coherent gain (i.e. the average of the window):
        coherent_gain <- mean(hanning)
        
        # Apply the window to the detrended signal:
        windowed_signal <- hf_centered * hanning
        
        # Zero-pad to the next power of 2 for improved frequency resolution:
        padded_length <- 2^(ceiling(log2(n)))
        padded_signal <- c(windowed_signal, rep(0, padded_length - n))
        
        # Return both the FFT result and the coherent gain in a list:
        list(fft = fft(padded_signal), cg = coherent_gain)
      })
    ) %>%
    # Now, compute dt for each group and perform the interpolation:
    mutate(
      # Compute dt (in seconds) from the time vector.
      # Since 'time' is in minutes, multiply the mean difference by 60.
      dt_group = map_dbl(time_vector, ~ mean(diff(.x)) * 60),
      
      # DC component remains unchanged:
      dc_value = map2_dbl(fft_result_dc, n_points, ~ Re(.x[1]) / .y),
      
      # Compute the first harmonic with quadratic (peak) interpolation,
      # and correct for the amplitude reduction due to the Hanning window.
      first_harmonic = pmap_dbl(
        list(fft_result_harm, n_points, dt_group),
        function(fft_h_data, n_points, dt_group) {
          # Extract FFT result and coherent gain:
          fft_h <- fft_h_data$fft
          cg <- fft_h_data$cg
          
          padded_length <- length(fft_h)
          # Build frequency axis using the group-specific dt:
          freqs <- seq(0, padded_length - 1) / (padded_length * dt_group)
          # Target modulation frequency in Hz:
          mod_freq <- 1 / period / 60  
          
          # Find the FFT bin index closest to mod_freq:
          i0 <- which.min(abs(freqs - mod_freq))
          
          # If i0 is at an edge, use that bin's magnitude:
          if (i0 <= 1 || i0 >= padded_length) {
            amp <- Mod(fft_h[i0])
          } else {
            # Get magnitudes of the neighboring bins:
            y1 <- Mod(fft_h[i0 - 1])
            y2 <- Mod(fft_h[i0])
            y3 <- Mod(fft_h[i0 + 1])
            # Perform quadratic interpolation:
            denominator <- (y1 - 2 * y2 + y3)
            delta <- ifelse(denominator == 0, 0, 0.5 * (y1 - y3) / denominator)
            amp <- y2 - 0.25 * (y1 - y3) * delta
          }
          # Multiply by 2 (for symmetric negative frequencies),
          # normalize by original n_points, and correct for the window's coherent gain:
          (2 / cg) * amp / n_points
        }
      ),
      
  
      # Calculate reversing heat flow:
      reversing_heat_flow = first_harmonic / tempModAmplitude,
      TRef = pattern * step_size + starting_temp
    )
  
  
  if (export){
    #3. Manual RevCp calculation
    # Apply the function to your extrema_df2 data
    extrema_df3 <- delete_extrema_until_equil(extrema_df2, d_steps_cleaned_2, period, modulations_back)
    
    # Now average the heat_flow values per pattern
    average_heat_maxima <- extrema_df3 %>%
      filter(type == "maxima") %>%
      group_by(pattern) %>%
      summarise(avg_heat_flow = mean(heat_flow, na.rm = TRUE))

    average_heat_minima <- extrema_df3 %>%
      filter(type == "minima") %>%
      group_by(pattern) %>%
      summarise(avg_heat_flow = mean(heat_flow, na.rm = TRUE))

    average_amplitude <- (average_heat_maxima-average_heat_minima)/2
    
    RevCpManual <- (average_amplitude$avg_heat_flow)/tempModAmplitude
    Tref <- step_size * average_heat_maxima$pattern + starting_temp
    average_heat_flow_per_pattern <- cbind(average_amplitude, RevCpManual, Tref)
    
    
    results <- list(extrema_df1, extrema_df2, extrema_df3,d, d_steps_cleaned, d_steps_cleaned_2, d_steps_cleaned_3, ft_averages, average_heat_flow_per_pattern)
    names(results) <- c("Extrema df1","Extrema df2", "Extrema df3", "Original data", "d_steps_cleaned", "d_steps_cleaned_2", "d_steps_cleaned_3", "ft_averages", "average_heat_flow_per_pattern")

    
    #Save Excels----
    # if (saveExtremadf1 == TRUE) {
    #   write.xlsx(extrema_df, paste0(fileName, " extremadf.xlsx"))
    # }
    # 
    # if(saveExtremadf2 == TRUE) {
    #   write.xlsx(extrema_df2, paste0(fileName, " extremadf2.xlsx"))
    # }
    # 
    # if(saveExtremadf3 == TRUE) {
    #   write.xlsx(extrema_df3, paste0(fileName, " extremadf3.xlsx"))
    # }
    # 
    # if(saveDatasteps == TRUE) {
    #   write.xlsx(d_steps_cleaned, paste0(fileName, " data_steps_cleaned.xlsx"))
    # }
    # 
    # if(saveDatasteps2 == TRUE){
    #   write.xlsx(d_steps_cleaned_2, paste0(fileName, " data_steps_cleaned_2.xlsx"))
    # }
    # 
    # if(saveDatasteps4 == TRUE){
    #   write.xlsx(d_steps_cleaned_3, paste0(fileName, " data_steps_cleaned_4.xlsx"))
    # }  
  }
  
  if (nrow(d_steps) - nrow(d_steps_cleaned) > 1000) {
    signfigerror <<- "⚠️ WARNING: More than 1000 duplicate neighbours removed! Did you ensure your input Excel has enough significant figures?!⚠️"
  }
  
  print("Done!")
  return(results)
}
