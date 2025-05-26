finalcalc <- function(sampling, startTemp, endTemp, period, heatRate, Atemp, resampled_points, loessAlpha, df1, df2){
  
  ws <- period*sampling*1 #ws is window size, the factor 1 should be changed to user input to change how many periods should be used for calculations
  user_frequency <- 1/period  
  
  # Compute rolling mean for the equally spaced (in y) data
  
  finaldf <- resampled_points

  finaldf$rollmean <- rollmean(finaldf$MHF, k = ws, fill = NA, align = "center")
  
  BaselinecorrMHF <- df2$MHF - finaldf$rollmean
  finaldf$BaselinecorrMHFNotEven <- BaselinecorrMHF
  
  # Perform rolling FFT and extract the amplitude at the user-defined frequency
  rolling_amplitude <- rollapply(finaldf$BaselinecorrMHFNotEven, width = ws, FUN = function(x) {
    
    # Perform the Fast Fourier Transform (FFT) on the window
    fft_result <- fft(x)
    
    # Compute the frequencies corresponding to the FFT result
    n <- length(x)
    sampling_rate <- 1 / (df1$times[2] - df1$times[1])  # assuming `times` is the time vector in seconds
    frequencies <- seq(0, sampling_rate / 2, length.out = n / 2 + 1)
    
    # Calculate the amplitude (modulus) of the FFT result
    amplitude_spectrum <- Mod(fft_result)
    
    # Find the index of the closest frequency to the user-defined frequency
    bin <- which.min(abs(frequencies - user_frequency))
    
    # Extract AC component as the amplitude at the first harmonic
    amplitude_at_user_freq <- 2 * Mod(fft_result[bin]) / n  # Normalize
    
    return(amplitude_at_user_freq)
  }, by = 1, fill = NA, align = "center")
  
  finaldf$TRef <- finaldf$time*heatRate + startTemp
  
  source("../normal mDSC/functions.R")
  
  Atemp <- 2*pi*Atemp/period
  extrema_df <-locate_extrema_manual(finaldf$MHF, finaldf$time, finaldf$TRef)
  counts <- count_extrema(extrema_df)
  noFTcalc <- HFcalc(extrema_df, Atemp, heatRate)
  

  # Add the rolling amplitude to the data frame
  RHF <- rolling_amplitude/Atemp*(-heatRate)
  finaldf$RHF <- RHF
  NRHF <- finaldf$rollmean - finaldf$RHF
  finaldf$NRHF <- NRHF
  names(finaldf)[names(finaldf) == "rollmean"] <- "THF"
  
  loess_model_RHF <- loess(RHF ~ time, data = finaldf, span = loessAlpha)
  finaldf$loessRHF <- predict(loess_model_RHF, newdata = finaldf$time)

  loess_model_THF <- loess(THF ~ time, data = finaldf, span = loessAlpha)
  finaldf$loessTHF <- predict(loess_model_THF, newdata = finaldf$time)

  loess_model_NRHF <- loess(NRHF ~ time, data = finaldf, span = loessAlpha)
  finaldf$loessNRHF <- predict(loess_model_NRHF, newdata = finaldf$time)
  
  results <- list(finaldf, noFTcalc)
  return(results)
}