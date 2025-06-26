processDSC <- function(fileName, Excel, sheet, export, startingTemp, stepSize,
           modulationsBack, period, isothermLength, setAmplitude, sampling) {

  #Intermediate calculations for later----
  tempMarginFirstCleanup <- 0.05
  pointsDistanceMinimumMargin <- (sampling*period)/2/10
  subtitle <- paste0("Dataset: ", "test")
  RevCpDenominator <- setAmplitude*2*pi/period

  source("quasi-isothermal_mdsc_deconvolution/quasi-isothermal_mdsc_deconvolution_detailed_functions.R")
  
  # ---- 1. Load and pre-process the data -------------------------------
  originalData <- excel_cleaner(Excel, sheet, startingTemp)
  if (is.character(originalData))           # excel_cleaner already returned a message
    return(originalData)
  
  #Perform a series of checks mainly to ensure that the user picked the right startingTemp
  temps <- originalData$modTemp
  n_max <- 0.5 * isothermLength * sampling * 60   # cap at available rows
  
  ok <- FALSE                                                # flag we'll set to TRUE if all checks pass
  
  for (i in seq_len(n_max)) {
    
    ## slice *by index*, not by value
    window <- temps[i:n_max]
    
    win_min  <- min(window, na.rm = TRUE)                    # the minimum *value*
    in_band <- all(window >= win_min) &&                                 # allow the min itself
               all(window <= win_min + 1.05 * setAmplitude)               #   ″

    if (in_band) {
      window <- temps[i+(0.1 * isothermLength * sampling * 60): n_max+(0.1 * isothermLength * sampling * 60)]
      avgtemp <- mean(window, na.rm = TRUE)

      ## is avgtemp within ± stepSize/4 of startingTemp?
      if (abs(avgtemp - startingTemp) <= 0.20) {
        ok <- TRUE                                           # looks good – stop checking
        break
      }
    }
  }
  
  if (!ok) {
    msg <- paste0("There is a problem with your starting temperature, amplitude, or step size. 
          The first stabilised temperature is not what you claim it is. 
          Plot your data in external software and confirm the true starting temperature. The detected temperature was ", avgtemp, " °C. You could also try adding your step size to your starting temperature.")
    return(msg)
  }
  
  
  # Define the temperature ranges----
  rangesn <- c(0:(((round(originalData$modTemp[length(originalData$modTemp)]))/stepSize)+10))
  rangesmax <- startingTemp + rangesn * stepSize + setAmplitude + tempMarginFirstCleanup
  rangesmin <- startingTemp + rangesn * stepSize - setAmplitude - tempMarginFirstCleanup

  
  # Filter based on temperature ranges
  originalDataFiltered <- originalData %>%
    filter(sapply(modTemp, function(temp) any(temp >= rangesmin & temp <= rangesmax)))
  
  # Remove duplicate time points
  originalDataFilteredUnique <- originalDataFiltered %>%
    distinct(time, .keep_all = TRUE)
  
  # Assign each data point to a pattern
  originalDataFilteredUniquePatterns <- originalDataFilteredUnique %>%
    mutate(pattern = floor((modTemp - startingTemp + setAmplitude + tempMarginFirstCleanup)/ stepSize))
  
  # Apply your duplicate-filtering function (already defined in your code)
  isolatedPatterns <- filter_duplicates(originalDataFilteredUniquePatterns)
  
  #Generate extremaDfIntermediate
  extremaDfIntermediate <- isolatedPatterns %>% 
    group_by(pattern) %>% 
    summarise(extrema_info = list(locate_extrema(modHeatFlow, time, modTemp))) %>% 
    unnest(cols = c(extrema_info))
  
  # Then proceed with your cleaning steps – note that you must run your functions that delete data etc.
  deleteLastMax <- delete_data_after_last_maximum(isolatedPatterns, extremaDfIntermediate, 
                                                  stepSize, startingTemp, sampling, period, 
                                                  pointsDistanceMinimumMargin)

  # Recompute extrema on the cleaned data for the subsequent steps
  extrema_counts2 <- deleteLastMax %>%
    group_by(pattern) %>%
    summarise(extrema_info = list(locate_extrema(modHeatFlow, time, modTemp)))
  
  extremaDfAfterDeleteMax <- extrema_counts2 %>% unnest(cols = c(extrema_info))
  
  # finalDataForAnalysis <- delete_data_after_last_minimum(deleteLastMax, extremaDfAfterDeleteMax)
  finalDataForAnalysis <- delete_data_until_equil(extremaDfAfterDeleteMax, deleteLastMax, 
                                                  period, modulationsBack)
  
  TRef <- finalDataForAnalysis$pattern*stepSize+startingTemp
  finalDataForAnalysis <- cbind(finalDataForAnalysis, TRef)

  # Apply the function to your extremaDfAfterDeleteMax data
  finalAnalysisExtrema <- delete_extrema_until_equil(extremaDfAfterDeleteMax, deleteLastMax, period, modulationsBack)
  
  #Some more error handling regarding modulationsBack
  extrema_counts <- finalAnalysisExtrema %>%
    filter(type == "maxima") %>%
    count(pattern)
  
  # Check if any pattern has fewer maxima than modulationsBack
  if (any(extrema_counts$n < modulationsBack)) {
    msg <- "You want to go back more modulations than you have extrema in at least one of your patterns, which is impossible. Reduce your modulationsBack."
    return(msg)
  }
  
    
  
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
          mod_freq <- 1 / period 
          
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
  
  
    #3. Manual RevCp calculation
    
    # Now average the heat_flow values per pattern
    average_heat_maxima <- finalAnalysisExtrema %>%
      filter(type == "maxima") %>%
      group_by(pattern) %>%
      summarise(avg_heat_flow = mean(modHeatFlow, na.rm = TRUE))

    average_heat_minima <- finalAnalysisExtrema %>%
      filter(type == "minima") %>%
      group_by(pattern) %>%
      summarise(avg_heat_flow = mean(modHeatFlow, na.rm = TRUE))

    average_amplitude <- (average_heat_maxima-average_heat_minima)/2
    
    RevCpManual <- (average_amplitude$avg_heat_flow)/RevCpDenominator
    Tref <- stepSize * average_heat_maxima$pattern + startingTemp
    resultsNoFT <- cbind(average_amplitude, RevCpManual, Tref)
    
    
    results <- list(extremaDfIntermediate, extremaDfAfterDeleteMax, finalAnalysisExtrema, 
                    originalData, isolatedPatterns, deleteLastMax, finalDataForAnalysis, 
                    resultsFT, resultsNoFT)
    names(results) <- c("Extrema df1","Extrema df2", "Extrema df3", "Original data", 
                        "isolatedPatterns", "deleteLastMax", "finalDataForAnalysis", 
                        "resultsFT", "resultsNoFT")

  
  if(!is.null(attr(originalData, "comment"))) {
    attr(results, "comment") <- attr(originalData, "comment")
  }
  
  return(results)
}
