processDSC <- function(fileName, Excel, sheet, export, starting_temp, step_size,
           modulations_back, period, isothermLength, setAmplitude, sampling, saveNRHFplot, saveRevCpplot, savemanualRevCpplot, 
           saveExcel) {

  #Intermediate calculations for later----
  temp_margin_first_cleanup <- 0.05
  tempModAmplitude <- setAmplitude*2*pi/period
  points_distance_minimum_margin <- (sampling*period*60)/2/10
  subtitle <- paste0("Dataset: ", "test")
  tempModAmplitude <- setAmplitude*2*pi/period

  source("../Quasi-Isothermal modulated DSC deconvolution/detailed functions.R")
  
    # 1. Load and pre‐process the data -------------------------------
  d <- excel_cleaner(Excel, sheet)
  if(typeof(d) == "character") {
    return(d)
  }

  # Define the temperature ranges----
  rangesn <- c(0:(((round(d$modTemp[length(d$modTemp)]))/step_size)+10))
  rangesmax <- starting_temp + rangesn * step_size + setAmplitude + temp_margin_first_cleanup
  rangesmin <- starting_temp + rangesn * step_size - setAmplitude - temp_margin_first_cleanup

  
  # Filter based on temperature ranges
  d_filtered <- d %>%
    filter(sapply(modTemp, function(temp) any(temp >= rangesmin & temp <= rangesmax)))
  
  # Remove duplicate time points
  d_unique <- d_filtered %>%
    distinct(time, .keep_all = TRUE)
  
  # Assign each data point to a pattern
  d_steps <- d_unique %>%
    mutate(pattern = floor((modTemp - starting_temp + setAmplitude + temp_margin_first_cleanup)/ step_size))
  
  # Apply your duplicate-filtering function (already defined in your code)
  d_steps_cleaned <- filter_duplicates(d_steps)
  
  
  #Generate extrema_df1
  extrema_df1 <- d_steps_cleaned %>% 
    group_by(pattern) %>% 
    summarise(extrema_info = list(locate_extrema_manual(modHeatFlow, time, modTemp))) %>% 
    unnest(cols = c(extrema_info))
  
  # Then proceed with your cleaning steps – note that you must run your functions that delete data etc.
  d_steps_cleaned_2 <- delete_data_after_last_maximum(d_steps_cleaned, extrema_df1, step_size, starting_temp, sampling, period, points_distance_minimum_margin)

  
  # Recompute extrema on the cleaned data for the subsequent steps
  extrema_counts2 <- d_steps_cleaned_2 %>%
    group_by(pattern) %>%
    summarise(extrema_info = list(locate_extrema_manual(modHeatFlow, time, modTemp)))
  
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
  
  
  
  if (export){
    #3. Manual RevCp calculation
    # Apply the function to your extrema_df2 data
    extrema_df3 <- delete_extrema_until_equil(extrema_df2, d_steps_cleaned_2, period, modulations_back)
    
    # Now average the heat_flow values per pattern
    average_heat_maxima <- extrema_df3 %>%
      filter(type == "maxima") %>%
      group_by(pattern) %>%
      summarise(avg_heat_flow = mean(modHeatFlow, na.rm = TRUE))

    average_heat_minima <- extrema_df3 %>%
      filter(type == "minima") %>%
      group_by(pattern) %>%
      summarise(avg_heat_flow = mean(modHeatFlow, na.rm = TRUE))

    average_amplitude <- (average_heat_maxima-average_heat_minima)/2
    
    RevCpManual <- (average_amplitude$avg_heat_flow)/tempModAmplitude
    Tref <- step_size * average_heat_maxima$pattern + starting_temp
    average_heat_flow_per_pattern <- cbind(average_amplitude, RevCpManual, Tref)
    
    
    results <- list(extrema_df1, extrema_df2, extrema_df3,d, d_steps_cleaned, d_steps_cleaned_2, d_steps_cleaned_3, ft_averages, average_heat_flow_per_pattern)
    names(results) <- c("Extrema df1","Extrema df2", "Extrema df3", "Original data", "d_steps_cleaned", "d_steps_cleaned_2", "d_steps_cleaned_3", "ft_averages", "average_heat_flow_per_pattern")

    
    if (saveExcel == TRUE) {
      ft_averagesexport <- data.frame("Step number" = ft_averages$pattern, "Temperature at that modulation" = ft_averages$TRef, "Non-reversing heat flow" = ft_averages$dc_value, "Reversing heat flow" = ft_averages$reversing_heat_flow)
      
      
      config <- data.frame(
        Parameter = c("Starting temperature (°C)", 
                      "Period (sec)",
                      "Isotherm length (min)",
                      "Step size (°C)", 
                      "Number of modulations used in calculation",
                      "Temperature modulation amplitude (°C)", 
                      "Margin for the first cleanup (in between steps) - user input (°C)", 
                      "Margin for the first cleanup (in between steps) - calculated (°C)", 
                      "Calculated amplitude of the derived temperature function (°C)", 
                      "Sampling interval (pts/sec)"),
        
        Value = c(starting_temp, 
                  period*60,
                  isothermLength,
                  step_size, 
                  modulations_back, 
                  setAmplitude, 
                  temp_margin_first_cleanup, 
                  points_distance_minimum_margin, 
                  tempModAmplitude, 
                  sampling)
      )
      
      
      fileName <- unlist(strsplit(fileName, "\\."))[1]
      fileName <- paste0(fileName, " ", modulations_back, " modulations analysed.xlsx")
      wb <- createWorkbook(fileName)

      addWorksheet(wb, "0.Settings")
      writeData(wb, sheet = "0.Settings", config)
      
      addWorksheet(wb, "1.Analysed results")
      writeData(wb, sheet = "1.Analysed results", ft_averagesexport)
      
      addWorksheet(wb, "2.Non-FT calc. RevCp")
      writeData(wb, sheet = "2.Non-FT calc. RevCp", average_heat_flow_per_pattern)
      
      addWorksheet(wb, "3.Data with isolated patterns")
      writeData(wb, sheet = "3.Data with isolated patterns", d_steps_cleaned)
      
      addWorksheet(wb, "4.Extrema of sheet 3")
      writeData(wb, sheet = "4.Extrema of sheet 3", extrema_df1)
      
      addWorksheet(wb, "5.Delete last max of sheet 3")
      writeData(wb, sheet = "5.Delete last max of sheet 3", d_steps_cleaned_2)
      
      addWorksheet(wb, "6.Extrema of sheet 5")
      writeData(wb, sheet = "6.Extrema of sheet 5", extrema_df2)
      
      addWorksheet(wb, "7.Data used in final analysis")
      writeData(wb, sheet = "7.Data used in final analysis", d_steps_cleaned_3)
      
      addWorksheet(wb, "8.Extrema of sheet 7")      
      writeData(wb, sheet = "8.Extrema of sheet 7", extrema_df3)
      
      
      saveWorkbook(wb, fileName, overwrite = TRUE)
    }

  }
  
  if(!is.null(attr(d, "comment"))) {
    attr(results, "comment") <- attr(d, "comment")
  }
  
  return(results)
}
