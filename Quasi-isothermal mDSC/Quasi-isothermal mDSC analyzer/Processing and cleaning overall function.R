source("detailed functions.R")
source("config.R")
library(crayon)
library(rstudioapi)



processDSC <- function(file, export = FALSE, rangesmin, rangesmax, starting_temp, step_size, temp_margin_first_cleanup,
                       modulations_back, period, setAmplitude, tempModAmplitude) {
  library(readxl)  
  library(dplyr)   
  library(ggplot2)
  library(openxlsx)
  library(tidyr)  # For unnest
  library(purrr)
  
  # 1. Load and pre‐process the data -------------------------------
  d <- na.omit(read_excel(paste0(file, ".xlsx")))
  d <- d[-1,]
  d[] <- lapply(d, function(x) if(is.character(x)) as.numeric(gsub(",", ".", x)) else x)
  names(d) <- c("time", "temperature", "heat_flow")
  
  # Define the temperature ranges----
  rangesn <- c(0:100)
  rangesmax <- starting_temp + rangesn * step_size + temp_margin_first_cleanup
  rangesmin <- starting_temp + rangesn * step_size - temp_margin_first_cleanup

  
  # Filter based on temperature ranges
  d_filtered <- d %>%
    filter(sapply(temperature, function(temp) any(temp >= rangesmin & temp <= rangesmax)))
  
  # Remove duplicate time points
  d_unique <- d_filtered %>%
    distinct(time, .keep_all = TRUE)
  
  # Assign each data point to a pattern
  d_steps <- d_unique %>%
    mutate(pattern = floor((temperature - starting_temp + temp_margin_first_cleanup)/ step_size))
  
  # Apply your duplicate-filtering function (already defined in your code)
  d_steps_cleaned <- filter_duplicates(d_steps)
  
  
  
  # Then proceed with your cleaning steps – note that you must run your functions that delete data etc.
  d_steps_cleaned_2 <- delete_data_after_last_maximum(d_steps_cleaned, 
                                                      # Compute extrema (using your function)
                                                      d_steps_cleaned %>% 
                                                        group_by(pattern) %>% 
                                                        summarise(extrema_info = list(locate_extrema_manual(heat_flow, time, temperature))) %>% 
                                                        unnest(cols = c(extrema_info))
  )
  
  # Recompute extrema on the cleaned data for the subsequent steps
  extrema_counts2 <- d_steps_cleaned_2 %>%
    group_by(pattern) %>%
    summarise(extrema_info = list(locate_extrema_manual(heat_flow, time, temperature)))
  
  extrema_df2 <- extrema_counts2 %>% unnest(cols = c(extrema_info))
  
  # d_steps_cleaned_3 <- delete_data_after_last_minimum(d_steps_cleaned_2, extrema_df2)
  d_steps_cleaned_4 <- delete_data_until_equil(d_steps_cleaned_2, extrema_df2)
  
  # 2. Compute the FFT and extract the dc component ----------------------
  
  # Group the data and store the time vector for each group
  ft_averages <- d_steps_cleaned_4 %>%
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
        windowed_signal <- hf_centered * hanning
        # Zero-pad to the next power of 2 for improved frequency resolution:
        padded_length <- 2^(ceiling(log2(n)))
        padded_signal <- c(windowed_signal, rep(0, padded_length - n))
        fft(padded_signal)
      })
    ) %>%
    # Now, compute dt for each group and perform the interpolation:
    mutate(
      # Compute dt (in seconds) from the time vector.
      # Since 'time' is in minutes, multiply the mean difference by 60.
      dt_group = map_dbl(time_vector, ~ mean(diff(.x)) * 60),
      
      # DC component remains unchanged:
      dc_value = map2_dbl(fft_result_dc, n_points, ~ Re(.x[1]) / .y),
      
      # Compute the first harmonic with quadratic (peak) interpolation.
      first_harmonic = pmap_dbl(
        list(fft_result_harm, n_points, dt_group),
        function(fft_h, n_points, dt_group) {
          padded_length <- length(fft_h)
          # Build frequency axis using the group-specific dt:
          freqs <- seq(0, padded_length - 1) / (padded_length * dt_group)
          # Target modulation frequency in Hz:
          mod_freq <- 1 / period / 60  
          
          # Find the FFT bin index closest to mod_freq:
          i0 <- which.min(abs(freqs - mod_freq))
          
          # If i0 is at an edge, return that bin's magnitude:
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
          # Multiply by 2 (for the symmetric negative frequencies) and normalize by original n_points:
          2 * amp / n_points
        }
      ),
      
      # Calculate reversing heat flow:
      reversing_heat_flow = first_harmonic / tempModAmplitude,
      TRef = pattern * step_size + starting_temp
    )

  
  if (export){
    #3. Manual RHF calculation
    # Apply the function to your extrema_df2 data
    extrema_df3 <- delete_extrema_until_equil(extrema_df2, d_steps_cleaned_2)
    
    # Now average the heat_flow values per pattern
    average_heat_maxima <- extrema_df3 %>%
      filter(type == "maxima") %>%
      group_by(pattern) %>%
      summarise(avg_heat_flow = mean(heat_flow, na.rm = TRUE))
    
    average_heat_minima <- extrema_df3 %>%
      filter(type == "minima") %>%
      group_by(pattern) %>%
      summarise(avg_heat_flow = mean(heat_flow, na.rm = TRUE))
    
    average_amplitude <- (average_heat_maxima-average_heat_minima)*0.5
    
    RevCpManual <- (average_amplitude$avg_heat_flow)/tempModAmplitude/sample_size
    Tref <- step_size * average_heat_maxima$pattern + starting_temp
    average_heat_flow_per_pattern <- cbind(average_amplitude, RevCpManual, Tref)
    
    TrefCleaned4 <- as.character(c(step_size * d_steps_cleaned_4$pattern + starting_temp))
    data_steps_cleaned_4 <<- data.frame(d_steps_cleaned_4, TrefCleaned4)
    average_heat_flow_per_pattern <<- average_heat_flow_per_pattern
    
    
    #Save Excels----
    if (saveExtremadf1 == TRUE) {
      write.xlsx(extrema_df, paste0(fileName, " extremadf.xlsx"))
    }
    
    if(saveExtremadf2 == TRUE) {
      write.xlsx(extrema_df2, paste0(fileName, " extremadf2.xlsx"))
    }
    
    if(saveExtremadf3 == TRUE) {
      write.xlsx(extrema_df3, paste0(fileName, " extremadf3.xlsx"))
    }
    
    if(saveDatasteps == TRUE) {
      write.xlsx(d_steps_cleaned, paste0(fileName, " data_steps_cleaned.xlsx"))
    }
    
    if(saveDatasteps2 == TRUE){
      write.xlsx(d_steps_cleaned_2, paste0(fileName, " data_steps_cleaned_2.xlsx"))
    }
    
    # if(saveDatasteps3 == TRUE) {
    #   write.xlsx(d_steps_cleaned_3, paste0(fileName, "data_steps_cleaned_3.xlsx"))
    # }
    # 
    if(saveDatasteps4 == TRUE){
      write.xlsx(d_steps_cleaned_4, paste0(fileName, " data_steps_cleaned_4.xlsx"))
    }  
  }
  
  if (nrow(d_steps) - nrow(d_steps_cleaned) > 1000) {
    signfigerror <<- "⚠️ WARNING: More than 1000 duplicate neighbours removed! Did you ensure your input Excel has enough significant figures?!⚠️"
  }
  
  return(ft_averages)
}
