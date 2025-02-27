library(ggplot2)
library(dplyr)
library(openxlsx)
library(grid)
library(signal)
library(zoo)
library(stats)


#NOTE: one of the main issues is the requirements of a moving window and how
#those requirements depend on what you're actually trying to achieve. 
#Basically, for a simple time-average, you want equally-spaced y-values. 
#However, for a FT, you want equally-spaced time-values. That's why THF and 
#RHF calculations use different inputs. 

#Hello hello test test :)))

rm(list = ls())
graphics.off()


#**SETTINGS*#
savetitle <- "20s period 4 °C_min HR"


#General EVERYTHING IN SECOND - EVERYTHING!!!!!!!!!!!!!!!!!!!!!!
sampling <- 10 #in pts/sec
startTemp <- 20      # in °C
endTemp <- 180       # in °C
period <- 40        # in modulations/sec
periodSignal <- 40    # in sec
heatRate <- 2/60        # in °C/sec
phase <- -0.4 # in rad
ws <- period*sampling*1
loessAlpha <- 0.05

#Temperature mod:
Atemp <- 0.212 # in °C

#Reversing Heat flow:
deltaRHFPreTg <- -0.000137  # in W/(g*°C)
deltaRHFPostTg <- -0.000120  # in W/(g*°C)
StartRHFPreTg <- -0.040 # in J/(g*°C)

deltaRevCpTempPreTg <- -deltaRHFPreTg/heatRate
deltaRevCpTempPostTg <- -deltaRHFPostTg/heatRate
StartRevCpTempPreTg <- -StartRHFPreTg/heatRate

#Baseline total heat flow
deltaHFPreTg <- -0.000286 # in W/(g*°C)
deltaHFPostTg <- -0.000068 # in W/(g*°C)
StartHFTempPreTg <- -0.035 # in W/g



#Tg
locationTgTHF <- c(28.46, 39.13, 33.77) # in °C
locationTgRHF <- c(35, 45, 40) # in °C
deltaCpTg <- 0.268   # in J/(g*°C)
deltaHFTg <- -0.268*heatRate  # in W/g


#Small melting peaks
MeltEnth <- -0.04
phase_melt <- 0
locationMelt <- c(134, 154, 144)   # in °C


#Crystallisation
Crystalenth <- 0.005
locationcrystal <-c(80,100,90) # in °C


#Enthalpy recovery
locationEnthRec <-c(30,45,37.5) # in °C
EnthrecEnth <- -0.002


#**CODE*#

# Basic definitions
freq <- 1/period
timeSpan <- (endTemp - startTemp) / heatRate

.nrMods <- ceiling(timeSpan/period)

# Define total number of points
points_per_mod <- period*sampling  # Points per modulation
total_points <- .nrMods * (points_per_mod +1) # Total points needed

# Initialize vector with correct length
times <- numeric(total_points)
groups <- numeric(total_points)

# Fill the vectors iteratively
for (i in 1:.nrMods) {
  start_idx <- (i - 1) * (points_per_mod + 1) + 1  # Adjust for extra points
  end_idx <- i * (points_per_mod + 1)  # Include extra point
  times[start_idx:end_idx] <- (i-1) * period + seq(0, period, length.out = (points_per_mod+1))
  groups[start_idx:end_idx] <- i
}

# Convert to a data frame and filter out duplicates
df1 <- data.frame(times, groups) %>%
  distinct(times, .keep_all = TRUE)  # Keep the first occurrence of each time point
df1 <- df1[-1,]
times <- df1$times
groups <- df1$groups


N <- length(times)
modTemp <- Atemp * sin(2*pi/period * times) + heatRate * times
modTempnoRamp <- Atemp * sin(2*pi/period * times)
TRef <- startTemp + heatRate * times
modTempderiv <- Atemp * 2*pi/period * cos(2*pi/period * times)
modTempdervPhase <- Atemp * 2*pi/period * cos(2*pi/period * times + phase)

FinalRevCpPreTg <- StartRevCpTempPreTg + deltaRevCpTempPreTg * locationTgRHF[1]
StartRevCpTempPostTg <- FinalRevCpPreTg + deltaCpTg

FinalHFPreTg <- StartHFTempPreTg + deltaHFPreTg*locationTgTHF[1]
StartHFTempPostTg <- FinalHFPreTg + deltaHFTg

# Determine indices for the RHF Tg region
idx_Tg1RHF <- which.min(abs(TRef - locationTgRHF[1]))

# Determine indices for the THF Tg region
idx_Tg1THF <- which.min(abs(TRef - locationTgTHF[1]))


# Create a sequence for the gradual change in RevCp within the Tg region
k <- 1
RevCpTg <- FinalRevCpPreTg + (StartRevCpTempPostTg - FinalRevCpPreTg) / (1 + exp(-k * (TRef - locationTgRHF[3])))
HfTg <- FinalHFPreTg + (StartHFTempPostTg - FinalHFPreTg) / (1 + exp(-k * (TRef - locationTgTHF[3])))


# Create a tibble and assign MHF with proper indexing for whole thermogram without latent effects-------------
df <- tibble(
  times = times,
  TRef = TRef,
  modTemp = modTemp,
  modTempderiv = modTempderiv,
  modTempnoRamp = modTempnoRamp,
  groups = groups
) %>%
  # Identify rows in the Tg region and compute a relative index
  mutate(
    isTg = TRef >= locationTgRHF[1] & TRef <= locationTgRHF[2],
    tg_index = if_else(isTg, row_number() - idx_Tg1RHF + 1, NA_integer_)
  ) %>%
  mutate(
    MHF = case_when(
      TRef < locationTgRHF[1] ~ (StartRevCpTempPreTg + deltaRevCpTempPreTg * TRef) * modTempdervPhase,
      isTg ~ RevCpTg * modTempdervPhase,
      TRef > locationTgRHF[2] ~ (StartRevCpTempPostTg + deltaRevCpTempPostTg * (TRef - locationTgRHF[2])) * modTempdervPhase
    )
  ) %>%
  select(-isTg, -tg_index)

#Add baseline to MHF
df <- df %>%
  # Identify rows in the Tg region and compute a relative index
  mutate(
    isTg = TRef >= locationTgTHF[1] & TRef <= locationTgTHF[2],
    tg_index = if_else(isTg, row_number() - idx_Tg1THF + 1, NA_integer_)
  ) %>%
  mutate(
    MHF = case_when(
      TRef < locationTgTHF[1] ~ MHF + StartHFTempPreTg + deltaHFPreTg * TRef,
      isTg ~ MHF + HfTg,  # Ensure 'isTg' is correctly referenced
      TRef > locationTgTHF[2] ~ MHF + FinalHFPreTg + deltaHFTg + deltaHFPostTg * (TRef - locationTgTHF[2])
    )
  ) %>%
  select(-isTg, -tg_index)


# Track already reached temperatures
reachedTemps <- numeric(0)

# Initialize signal vector
signal_vecmelt <- numeric(nrow(df))
sigma <- 3.33  # Assuming FWHM-based estimate
meltAmplitude <- MeltEnth * exp(-((TRef - locationMelt[3])^2) / (2 * sigma^2))


for (i in seq_along(df$modTemp)) {
  if (df$modTemp[i] %in% reachedTemps) {
    signal_vecmelt[i] <- 0  # No new signal
  } else {
    # Add new temperature to reached list
    reachedTemps <- c(reachedTemps, df$modTemp[i])
    
    # Compute signal
    if (df$TRef[i] >= locationMelt[1] && df$TRef[i] <= locationMelt[2]) {
      signal_vecmelt[i] <- min(meltAmplitude[i] * sin((2*pi/periodSignal*df$times[i]) +phase_melt), 0)
    } else {
      signal_vecmelt[i] <- 0
    }
  }
}

# Add signal and update MHF
df <- df %>%
  mutate(
    signal_vecmelt = signal_vecmelt,
    MHF = if_else(
      TRef >= locationMelt[1] & TRef <= locationMelt[2],
      (StartRevCpTempPostTg + deltaRevCpTempPostTg * (TRef - locationTgRHF[2])) * modTempdervPhase + signal_vecmelt + FinalHFPreTg + deltaHFTg + deltaHFPostTg*(TRef-locationTgTHF[2]),
      MHF
    )
  )

sigmacrystal <- 3.33
signal_vec <- numeric(nrow(df))


for (i in seq_along(df$TRef)) {
  # Compute crystallisation signal
  
  if (df$TRef[i] >= locationcrystal[1] && df$TRef[i] <= locationcrystal[2]) {
    signal_vec[i] <- Crystalenth * exp(-((TRef[i] - locationcrystal[3])^2) / (2 * sigmacrystal^2))
  } else {
    signal_vec[i] <- 0
  }
}

# Add signal and update MHF
df <- df %>%
  mutate(
    signal = signal_vec,
    MHF = MHF + signal
  )

sigmaEnthRec <- 3.33


for (i in seq_along(df$TRef)) {
  # Compute crystallisation signal
  
  if (df$TRef[i] >= locationEnthRec[1] && df$TRef[i] <= locationEnthRec[2]) {
    signal_vec[i] <- EnthrecEnth * exp(-((TRef[i] - locationEnthRec[3])^2) / (2 * sigmaEnthRec^2))
  } else {
    signal_vec[i] <- 0
  }
}

# Add signal and update MHF
df <- df %>%
  mutate(
    signal = signal_vec,
    MHF = MHF + signal
  )


MHF <- df$MHF
time <- df$times


# ---- 2. Resample Data to be Equally Spaced in Y over Full Cycles ----
# Because sin(x) is not monotonic over a full cycle, we split it into monotonic segments.
ds <- diff(MHF)
sgn <- sign(ds)

# Detect turning points where the monotonicity changes
change_idx <- which(diff(sgn) != 0) + 1

# Define segment boundaries (include start and end)
seg_boundaries <- sort(unique(c(1, change_idx, length(MHF))))

# Initialize a data frame to hold resampled points
resampled_points <- data.frame(time = numeric(), MHF = numeric())

# Process each monotonic segment separately
for (i in 1:(length(seg_boundaries) - 1)) {
  seg_idx <- seg_boundaries[i]:seg_boundaries[i + 1]
  x_seg <- times[seg_idx]
  y_seg <- MHF[seg_idx]
  
  # Create equally spaced y-values for the segment
  y_equally <- seq(min(y_seg), max(y_seg), length.out = length(y_seg))
  
  # Invert the relation: interpolate x-values for these equally spaced y-values
  x_equally <- approx(x = y_seg, y = x_seg, xout = y_equally)$y
  
  seg_df <- data.frame(time = x_equally, MHF = y_equally)
  resampled_points <- rbind(resampled_points, seg_df)
}

resampled_points <- resampled_points[order(resampled_points$time), ]
resampled_points <- unique(resampled_points)
rownames(resampled_points) <- NULL  # Ensures default row numbering




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
  user_frequency <- 1/period
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


loess_model_RHF <- loess(RHF ~ time, data = resampled_points, span = loessAlpha)
resampled_points$loessRHF <- predict(loess_model_RHF, newdata = resampled_points$time)

loess_model_THF <- loess(THF ~ time, data = resampled_points, span = loessAlpha)
resampled_points$loessTHF <- predict(loess_model_THF, newdata = resampled_points$time)

loess_model_NRHF <- loess(NRHF ~ time, data = resampled_points, span = loessAlpha)
resampled_points$loessNRHF <- predict(loess_model_NRHF, newdata = resampled_points$time)



  
# ---- 3. Plot the Results Using ggplot2 ----
# The plot shows:
# # - The original sine wave (blue line) with its rolling mean (darker blue).
# # - The resampled points (red dots) with their rolling mean (darker red line).
# ggplot() +
#   # Original data and its rolling mean
#   # geom_line(data = data_original, aes(x = time, y = signal), color = "blue", alpha = 0.5, size = 1) +
#   # geom_line(data = data_original, aes(x = time, y = rollmean), color = "darkblue", size = 1) +
#   # Resampled (y-equally spaced) data and its rolling mean
#   # geom_point(data = resampled_points, aes(x = time, y = signal), color = "red", size = 0.5) +
#   # geom_line(data = resampled_points, aes(x = CorrTime, y = smoothedTHF), color = "darkred", size = 1) +
#   # geom_line(data = resampled_points, aes(x = time, y = rollmean), color = "blue", size = 1) +
#   geom_line(data = resampled_points, aes(x = time, y = BaselinecorrMHF), color = "blue", size = 1) +
#   geom_line(data = resampled_points, aes(x = time, y = MHF), color = "red", size = 1) +
#   labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
#        x = "Time",
#        y = "Signal / Rolling Mean") +
#   theme_minimal()

testdf <- data.frame(times1 = df$times, times2 = resampled_points$time, MHF1 = df$MHF, MHF2 = resampled_points$MHF)

# ggplot() +
#   geom_point(data = testdf, aes(x = times1, y = MHF1), color = "blue", size = 1) +
#   geom_line(data = testdf, aes(x = times2, y = MHF2), color = "red", size = 1) +
#   labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
#        x = "Time",
#        y = "Signal / Rolling Mean") +
#   theme_minimal()
# 
# 
# ggplot() +
#   geom_line(data = resampled_points, aes(x = time, y = THF), color = "blue", size = 1) +
#   geom_line(data = resampled_points, aes(x = time, y = RHF), color = "red", size = 1) +
#   geom_line(data = resampled_points, aes(x = time, y = NRHF), color = "green", size = 1) +
#     labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
#        x = "Time",
#        y = "Signal / Rolling Mean") +
#   theme_minimal()

ggplot() +
  geom_line(data = resampled_points, aes(x = time, y = loessTHF), color = "blue", size = 1) +
  geom_line(data = resampled_points, aes(x = time, y = loessRHF), color = "red", size = 1) +
  geom_line(data = resampled_points, aes(x = time, y = loessNRHF), color = "green", size = 1) +
  labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
       x = "Time",
       y = "Signal / Rolling Mean") +
  theme_minimal() 



ggplot() +
  geom_line(data = resampled_points, aes(x = time, y = loessTHF), color = "blue", size = 1) +
  labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
       x = "Time",
       y = "Signal / Rolling Mean") +
  theme_minimal()

ggplot() +
  geom_line(data = resampled_points, aes(x = time, y = loessRHF), color = "blue", size = 1) +
  labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
       x = "Time",
       y = "Signal / Rolling Mean") +
  theme_minimal()

ggplot() +
  geom_line(data = resampled_points, aes(x = time, y = loessNRHF), color = "blue", size = 1) +
  labs(title = "Rolling Means for Original and Y-Equally Spaced Sine Data",
       x = "Time",
       y = "Signal / Rolling Mean") +
  theme_minimal()

