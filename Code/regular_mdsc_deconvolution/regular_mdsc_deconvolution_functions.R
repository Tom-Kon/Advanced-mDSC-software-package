#-----------------------------------------------------------------------------------------
#This first function identifies the appropriate columns in the Excel file and assigns names to them that can be used later. It also contains 
#some error hanlding in case the columns are not present. 
#-----------------------------------------------------------------------------------------

excel_cleaner <- function(Excel, sheet, HFcalcextra, compare, import) {
  sheet <- as.numeric(sheet)
  
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
  
  idxtime <- which(vapply(temp, function(x) any(tolower(x) %in% "time"), logical(1)) )
  temp[idxtime] <- "time"
  
  idxmodtemp <- which(vapply(temp, function(x) {all(c("temperature", "modulated") %in% tolower(x))}, logical(1)))
  temp[idxmodtemp] <- "modTemp"
  
  idxtemp <- which(vapply(temp, function(x) any(tolower(x) %in% "temperature"), logical(1)) )
  temp[idxtemp] <- "temperature"
  
  idxModhf <- which(vapply(temp, function(x) {all(c("heat", "flow", "modulated") %in% tolower(x))}, logical(1)))
  temp[idxModhf] <- "modHeatFlow"
  
  idxRevhf <- which(vapply(temp, function(x) {all(c("reversing", "heat", "flow") %in% tolower(x))}, logical(1)))
  temp[idxRevhf] <- "RevmodHeatFlow"
  
  idxNonRevhf <- which(vapply(temp, function(x) {all(c("non-reversing","heat", "flow") %in% tolower(x))}, logical(1)))
  temp[idxNonRevhf] <- "NonRevmodHeatFlow"
  
  idxhf <- which(vapply(temp, function(x) {all(c("heat", "flow") %in% tolower(x))}, logical(1)))
  temp[idxhf] <- "heatFlow"
  
  headers <- unlist(temp)
  
  
  if (length(idxhf) > 1) {
    msg <- "Error: there are multiple columns containing the terms \"heat flow\" in your selected Excel sheet."
    return(msg)
  }
  
  if(import == 1) {
    if(HFcalcextra) {
      
      if (length(headers[idxhf]) == 0) {
        msg <- "Error: there is no total heat flow column in your Excel sheet, or you may have selected the wrong sheet."
        return(msg)
      }
      
      if (length(headers[idxtemp]) == 0) {
        msg <- "Error: there is no temperature column in your Excel sheet, or you may have selected the wrong sheet."
        return(msg)
      }      
    
    }
  }
  
  if(import == 2) {
    if(compare) {
     if (length(headers[idxhf]) == 0) {
        msg <- "Error: there is no total heat flow column in your Excel sheet, or you may have selected the wrong sheet."
        return(msg)
      }
      
      if (length(headers[idxtemp]) == 0) {
        msg <- "Error: there is no temperature column in your Excel sheet, or you may have selected the wrong sheet."
        return(msg)
      } 
    }    
  }
  
  if (length(headers[idxtime]) == 0) {
    msg <- "Error: there is no time column in your selected Excel sheet, or you may have selected the wrong sheet."
    return(msg)
  }
  
  if (length(headers[idxModhf]) == 0) {
    msg <- "Error: there is no modulated heat flow column in your selected Excel sheet, or you may have selected the wrong sheet."
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
  
  if (HFcalcextra) {
    if (import == 1) {
      Excel <- data.frame(
        time = Excel$time,
        temperature = Excel$temperature,
        modHeatFlow = Excel$modHeatFlow,
        heatFlow = Excel$heatFlow
      )
    }
  } else {
    if (import == 1) {
      Excel <- data.frame(
        time = Excel$time,
        temperature = Excel$temperature,
        modHeatFlow = Excel$modHeatFlow
      )
    }
  }

  
  if (import == 2) {
    Excel <- data.frame(
      time = Excel$time,
      temperature = Excel$temperature,
      heatFlow = Excel$heatFlow
    )
  }
  
  
  errorSigFig <- NULL
  
  for(i in seq_along(Excel)) {
    tempcol <- Excel[[i]]
    tempcheck <- tempcol[1]
    tempcheck <- gsub("\\.", "", tempcol[1])
    
    if (nchar(tempcheck) < 5) {
      if(i == 1) {errorSigFig <- "Warning: less than 5 significant figures were detected in your time data. This might affect the quality of the results"
      }
      if(i == 2) {errorSigFig <- "Warning: less than 5 significant figures were detected in your (modulated) temperature data. This might affect the quality of the results"
      }
      if(i == 3) {errorSigFig <- "Warning: less than 5 significant figures were detected in your modulated heat flow data. This might affect the quality of the results"
      }
      attr(Excel, "comment") <- errorSigFig
      break
    } 
  }
  
  return(Excel)
}

#-----------------------------------------------------------------------------------------
# Define the function to locate maxima and minima and output their indices and values
#-----------------------------------------------------------------------------------------

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

#-----------------------------------------------------------------------------------------
#Counting function
#-----------------------------------------------------------------------------------------
count_extrema <- function(extremaDf) {
  maxima_count <- sum(extremaDf$type == "maxima")
  minima_count <- sum(extremaDf$type == "minima")
  counts <- data.frame(maxima_count, minima_count)
  return(counts)
}

#-----------------------------------------------------------------------------------------
#Function defining the calculation based on the maxima and minima
#-----------------------------------------------------------------------------------------
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

  if (nrow(maxima) > nrow(minima)) {
    maxima <- maxima[-(nrow(maxima)-diff+1:nrow(maxima)),]
  } else if (nrow(maxima) < nrow(minima)) {
    minima <- minima[-(nrow(minima)-diff+1:nrow(minima)),]
  }
  
  #Remove the first few columns because they're generally just noise
  maxima <- maxima[-(1:7),]
  minima <- minima[-(1:7),]
  
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


#-----------------------------------------------------------------------------------------
#Function defining the calculation based on comparing extremaDf and THF
#-----------------------------------------------------------------------------------------
calculate_heatflow_min_max_THF <- function(extremaDf, RHFCalcDenominator, heatingRate, Excel) {
  
  maxima <- extremaDf %>%
    filter(type == "maxima")
  
  filtered <- Excel %>% 
    filter(time %in% maxima$time)
  
  THF <- filtered$heatFlow
  
  amplitudes <- filtered$modHeatFlow - THF
  RHF <- -amplitudes/RHFCalcDenominator*heatingRate/60
  NRHF <- filtered$heatFlow-RHF 
  temperature <- filtered$temperature
  calculationMinMaxResultsTHF <- data.frame(temperature, RHF, THF, NRHF)
  
  return(calculationMinMaxResultsTHF)
}


#-----------------------------------------------------------------------------------------
#Function defining the calculation based on the Fourier transform
#-----------------------------------------------------------------------------------------
calculate_fft <- function(period, Excel, RHFCalcDenominator, heatingRate) {
  
  dt <- mean(diff(Excel$time*60))
  freq <- 1/period
  
  
  # output <- fftfunc(period, dt, resampled_points)
  window_size <- period/dt
  calculate_fft <- data.frame(time = Excel$time, temperature = Excel$temperature)
  calculate_fft$THF <- rollmean(Excel$modHeatFlow, k = window_size, fill = NA, 
                                align = "center")
  calculate_fft$baselinecorrHF <- Excel$modHeatFlow - calculate_fft$THF

  
  # Perform rolling FFT and extract the amplitude at the user-defined frequency
  calculate_fft$amplitude <- rollapply(calculate_fft$baselinecorrHF, 
                                       width = window_size, FUN = function(x) {
    
    # Perform the Fast Fourier Transform (FFT) on the window
    fft_result <- fft(x)
    
    # Compute the frequencies corresponding to the FFT result
    n <- length(x)
    frequencies <- (0:(n - 1)) * (1 / (n * dt))
    
    # Calculate the amplitude (modulus) of the FFT result
    amplitude_spectrum <- Mod(fft_result)
    
    # Find the index of the closest frequency to the user-defined frequency
    bin <- which.min(abs(frequencies - freq))
    
    # Extract AC component as the amplitude at the first harmonic
    amplitude_at_user_freq <- 2 * Mod(fft_result[bin]) / n  # Normalize
    
    return(amplitude_at_user_freq)
  }, by = 1, fill = NA, align = "center")
  
  calculate_fft$RHF <- calculate_fft$amplitude/RHFCalcDenominator*(-heatingRate/60)
  calculate_fft$NRHF <- calculate_fft$THF - calculate_fft$RHF
  
  return(calculate_fft)
}


#-----------------------------------------------------------------------------------------
#Function defining the calculation based on comparing extremaDf and unmodulated DSC
#-----------------------------------------------------------------------------------------
calculate_heatflow_min_max_DSC <- function(DSC, extremaDf, RHFCalcDenominator, heatingRate) {
  
  matchingDSCmDSC <- extremaDf %>%
    filter(type == "maxima") %>%
    rowwise() %>%
    mutate(
      closest_index = which.min(abs(DSC$temperature - temperature)),
      heat_flowDSC = DSC$heatFlow[closest_index],
      temperatureDSC = DSC$temperature[closest_index],
      amplitudes =  modHeatFlow - DSC$heatFlow[closest_index]
    ) %>%
    ungroup()
  
  THF <- matchingDSCmDSC$heat_flowDSC
  RHF <- -matchingDSCmDSC$amplitudes/RHFCalcDenominator*heatingRate/60
  NRHF <- matchingDSCmDSC$heat_flowDSC-RHF 
  temperature <- matchingDSCmDSC$temperatureDSC
  
  CalculationMinMaxResultsDSC <- data.frame(temperature, THF, RHF, NRHF)
  
  return(CalculationMinMaxResultsDSC)
}
