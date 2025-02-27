# Compute rolling mean for the equally spaced (in y) data
resampled_points$rollmean <- rollmean(resampled_points$MHF, k = ws, fill = NA, align = "center")

smoothedTHF <- sgolayfilt(resampled_points$rollmean, p = 3, n = 1201)  # p = polynomial order, n = window size
resampled_points <- cbind(resampled_points, smoothedTHF)

extractBef <- na.omit(resampled_points$rollmean)
extractAft <- na.omit(resampled_points$smoothedTHF)
timeBef <- resampled_points$time[length(extractBef)]
timeAft <- resampled_points$time[length(extractAft)]
CorrTime <- resampled_points$time+ (timeBef-timeAft)/2
resampled_points$Corrtime <- CorrTime

BaselinecorrMHF <- df$MHF - resampled_points$rollmean
resampled_points$BaselinecorrMHFNotEven <- BaselinecorrMHF

# Perform rolling FFT and extract the amplitude at the user-defined frequency
rolling_amplitude <- rollapply(resampled_points$BaselinecorrMHFNotEven, width = ws, FUN = function(x) {
  
  # Perform the Fast Fourier Transform (FFT) on the window
  fft_result <- fft(x)
  
  # Compute the frequencies corresponding to the FFT result
  n <- length(x)
  sampling_rate <- 1 / (times[2] - times[1])  # assuming `times` is the time vector in seconds
  frequencies <- seq(0, sampling_rate / 2, length.out = n / 2 + 1)
  
  # Calculate the amplitude (modulus) of the FFT result
  amplitude_spectrum <- Mod(fft_result)
  
  # Find the index of the closest frequency to the user-defined frequency
  bin <- which.min(abs(frequencies - user_frequency))
  
  # Extract AC component as the amplitude at the first harmonic
  amplitude_at_user_freq <- 2 * Mod(fft_result[bin]) / n  # Normalize
  
  return(amplitude_at_user_freq)
}, by = 1, fill = NA, align = "center")

# Add the rolling amplitude to the data frame
RHF <- rolling_amplitude/(2*pi*Atemp/period)*(-heatRate)
resampled_points$RHF <- RHF
NRHF <- resampled_points$rollmean - resampled_points$RHF
resampled_points$NRHF <- NRHF
names(resampled_points)[names(resampled_points) == "rollmean"] <- "THF"
resampled_points$TRef <- resampled_points$time*heatRate + startTemp


loess_model_RHF <- loess(RHF ~ time, data = resampled_points, span = loessAlpha)
resampled_points$loessRHF <- predict(loess_model_RHF, newdata = resampled_points$time)

loess_model_THF <- loess(THF ~ time, data = resampled_points, span = loessAlpha)
resampled_points$loessTHF <- predict(loess_model_THF, newdata = resampled_points$time)

loess_model_NRHF <- loess(NRHF ~ time, data = resampled_points, span = loessAlpha)
resampled_points$loessNRHF <- predict(loess_model_NRHF, newdata = resampled_points$time)

#Export as global variable
resampled_points <<- resampled_points