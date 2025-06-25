processDSCrecalc <- function(fileName, results, modulationsBack, period, setAmplitude, stepSize, startingTemp) {
  
  RevCpDenominator <- setAmplitude*2*pi/period
  
  source("Quasi-Isothermal modulated DSC deconvolution/detailed functions.R")

  # Recompute extrema on the cleaned data for the subsequent steps
  extrema_counts2 <- results$deleteLastMax %>%
    group_by(pattern) %>%
    summarise(extrema_info = list(locate_extrema(modHeatFlow, time, modTemp)))
  
  extremaDfAfterDeleteMax <- extrema_counts2 %>% unnest(cols = c(extrema_info))
  
  finalDataForAnalysis <- delete_data_until_equil(results$deleteLastMax, extremaDfAfterDeleteMax, period, modulationsBack)
  TRef <- finalDataForAnalysis$pattern*stepSize+startingTemp
  finalDataForAnalysis <- cbind(finalDataForAnalysis, TRef)
  
  # 2. Compute the FFT and extract the dc component ----------------------
  
  # Group the data and store the time vector for each group
  resultsFT <- finalDataForAnalysis %>%
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
      NRHF = map2_dbl(fft_result_dc, n_points, ~ Re(.x[1]) / .y),
      
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
      RevCp = first_harmonic / RevCpDenominator,
      TRef = pattern * stepSize + startingTemp
    )
  
  
    #3. Manual RHF calculation
    # Apply the function to your extremaDfAfterDeleteMax data
    finalAnalysisExtrema <- delete_extrema_until_equil(extremaDfAfterDeleteMax, results$deleteLastMax, period, modulationsBack)
    
    # Now average the heat_flow values per pattern
    averageHeatMaxima <- finalAnalysisExtrema %>%
      filter(type == "maxima") %>%
      group_by(pattern) %>%
      summarise(avg_heat_flow = mean(modHeatFlow, na.rm = TRUE))
    
    averageHeatMinima <- finalAnalysisExtrema %>%
      filter(type == "minima") %>%
      group_by(pattern) %>%
      summarise(avg_heat_flow = mean(modHeatFlow, na.rm = TRUE))
    
    averageAmplitude <- (averageHeatMaxima-averageHeatMinima)*0.5
    
    RevCpManual <- (average_amplitude$avg_heat_flow)/RevCpDenominator
    Tref <- step_size * averageHeatMaxima$pattern + startingTemp
    resultsNoFT <- cbind(averageAmplitude, RevCpManual, Tref)
    
    TrefCleaned4 <- as.character(c(step_size * finalDataForAnalysis$pattern + startingTemp))
    
    results$finalDataForAnalysis <- finalDataForAnalysis
    results$resultsFT <- resultsFT
    results$resultsNoFT <- resultsNoFT
    
  return(results)
}
