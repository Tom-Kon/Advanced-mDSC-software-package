processDSCrecalc <- function(fileName, sample_results, modulations_back, period, setAmplitude, step_size, starting_temp) {
  
  tempModAmplitude <- setAmplitude*2*pi/period
  
  source("../Quasi-Isothermal modulated DSC deconvolution/detailed functions.R")

  # Recompute extrema on the cleaned data for the subsequent steps
  extrema_counts2 <- sample_results$d_steps_cleaned_2 %>%
    group_by(pattern) %>%
    summarise(extrema_info = list(locate_extrema_manual(modHeatFlow, time, modTemp)))
  
  extrema_df2 <- extrema_counts2 %>% unnest(cols = c(extrema_info))
  
  d_steps_cleaned_3 <- delete_data_until_equil(sample_results$d_steps_cleaned_2, extrema_df2, period, modulations_back)
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
      fft_result_dc = list(fft(modHeatFlow)),
      # For the first harmonic, process the data before FFT:
      fft_result_harm = list({
        n <- length(modHeatFlow)
        hf_centered <- modHeatFlow - mean(modHeatFlow)  # detrend the signal
        
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
  
  
    #3. Manual RHF calculation
    # Apply the function to your extrema_df2 data
    extrema_df3 <- delete_extrema_until_equil(extrema_df2, sample_results$d_steps_cleaned_2, period, modulations_back)
    
    # Now average the heat_flow values per pattern
    average_heat_maxima <- extrema_df3 %>%
      filter(type == "maxima") %>%
      group_by(pattern) %>%
      summarise(avg_heat_flow = mean(modHeatFlow, na.rm = TRUE))
    
    average_heat_minima <- extrema_df3 %>%
      filter(type == "minima") %>%
      group_by(pattern) %>%
      summarise(avg_heat_flow = mean(modHeatFlow, na.rm = TRUE))
    
    average_amplitude <- (average_heat_maxima-average_heat_minima)*0.5
    
    RevCpManual <- (average_amplitude$avg_heat_flow)/tempModAmplitude
    Tref <- step_size * average_heat_maxima$pattern + starting_temp
    average_heat_flow_per_pattern <- cbind(average_amplitude, RevCpManual, Tref)
    
    TrefCleaned4 <- as.character(c(step_size * d_steps_cleaned_3$pattern + starting_temp))
    
    sample_results$d_steps_cleaned_3 <- d_steps_cleaned_3
    sample_results$ft_averages <- ft_averages
    sample_results$average_heat_flow_per_pattern <- average_heat_flow_per_pattern
    
    
  print("Done!")
  return(sample_results)
}
