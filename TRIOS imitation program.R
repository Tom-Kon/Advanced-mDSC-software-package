#TRIOS always has multiple times that are the same in the excel file. I suppose this is to create more data points, which are removed by the averaging technique used here. Recuperating these points might result in better results. 
library(readxl)
library(ggplot2)
library(dplyr)
library(pls)


# Read data from Sheet1 (earlier time points) and Sheet2
data_sheet1 <- read_excel("DSC data.xls", sheet = "Sheet1")
data_sheet2 <- read_excel("DSC data.xls", sheet = "Sheet2")

# Combine the data frames by row, with Sheet1 rows appearing before Sheet2 rows
data <- rbind(data_sheet1, data_sheet2)


# Extract time, heat flow, and heating rate data
time <- data$time  # Now in minutes
heat_flow <- data$heat_flow
heating_rate <- data$heating_rate

# Create a modified time column with small increments for duplicate time points
data_clean <- data %>%
  group_by(time) %>%
  mutate(
    count = n(),               # Count of duplicates for each unique time
    index = row_number(),       # Position of each duplicate in the group
    time_adjusted = ifelse(count > 1, time + (index - 1) / count, time)  # Adjust time for duplicates
  ) %>%
  ungroup() %>%
  select(time_adjusted, heat_flow, heating_rate)  # Only keep necessary columns

# Extract cleaned time, heat flow, and heating rate data
time <- data_clean$time_adjusted
heat_flow <- data_clean$heat_flow
heating_rate <- data_clean$heating_rate

# Parameters
baseline_heating_rate <- 1  # Convert baseline heating rate to °C/min
heating_rate_amplitude <- 0.212 * 1.5 * pi * 2  # Given heating rate amplitude in °C

# Calculate the sampling interval in minutes
sampling_interval <- 0.01

# Window size for heat flow analysis
window_size_small <- ceiling(0.75 / sampling_interval)  # Small window for Fourier transform (0.2s window)

# Initialize vectors to store results
rev_cp_values <- numeric(length(time) - window_size_small + 1)
rev_heat_flow_values <- numeric(length(time) - window_size_small + 1)
tot_heat_flow_values <- numeric(length(time) - window_size_small + 1)
window_times <- numeric(length(time) - window_size_small + 1)

# Moving window Fourier analysis for heat flow (without modulation component)
for (i in seq_len(length(time) - window_size_small + 1)) {
  # Subset data for the current window (small window for heat flow analysis)
  window_heat_flow_small <- heat_flow[i:(i + window_size_small - 1)]
  
  # Perform Fourier Transform on the small windowed data for heat flow analysis
  FT_heat_flow_small <- fft(window_heat_flow_small)
  
  # Frequency vector for small window Fourier transform
  n_small <- length(window_heat_flow_small)
  freq_small <- (0:(n_small - 1)) * (1 / (n_small * (time[2] - time[1])))
  
  # Define the frequency range to remove (between 1.49 and 1.51 cycles/min)
  lower_freq_cutoff <- 1.49  # Lower frequency of the range to remove
  upper_freq_cutoff <- 1.51  # Upper frequency of the range to remove
  
  # Identify indices of frequencies that fall within the range 1.49 - 1.51 cycles/min
  band_remove_indices <- which(freq_small >= lower_freq_cutoff & freq_small <= upper_freq_cutoff)
  
  # Apply a bandstop filter by zeroing out Fourier coefficients in the range
  FT_heat_flow_small[band_remove_indices + 1] <- 0  # Remove positive frequencies in range
  FT_heat_flow_small[n_small - band_remove_indices + 1] <- 0  # Remove symmetric negative frequencies
  
  # Perform the inverse Fourier transform to recover the filtered heat flow
  filtered_heat_flow <- Re(fft(FT_heat_flow_small, inverse = TRUE) / n_small)
  
  # Store the total heat flow (filtered)
  tot_heat_flow_values[i] <- max(filtered_heat_flow)  # You can modify this to suit your desired calculation
  
  # Calculate RevCp using the filtered heat flow (similar to before)
  rev_cp_values[i] <- max(Mod(FT_heat_flow_small[2:(n_small / 2 + 1)])) / heating_rate_amplitude
  
  # Record Reversing Heat Flow
  rev_heat_flow_values[i] <- -rev_cp_values[i] * baseline_heating_rate
  
  # Record the midpoint time of the small window
  window_times[i] <- mean(time[i:(i + window_size_small - 1)])
}

# Data frame with time, RevCp, and Reversing Heat Flow values
results_df <- data.frame(time = window_times, RevCp = rev_cp_values, Rev_heat_flow = rev_heat_flow_values, THF = tot_heat_flow_values)

# Remove rows where time = 0 in results_df
results_df <- results_df %>%
  filter(time != 0)
#
# # Plot RevCp and Reversing Heat Flow over time
# ggplot(results_df, aes(x = time)) +
#   geom_line(aes(y = RevCp), color = "blue", size = 1) +
#   labs(title = "Reversing Heat Capacity (RevCp) over time",
#        x = "Time (min)",
#        y = "RevCp") +
#   theme_minimal()
#
# ggplot(results_df, aes(x = time)) +
#   geom_line(aes(y = Rev_heat_flow), color = "green", size = 1) +
#   labs(title = "Reversing Heat Flow over Time",
#        x = "Time (min)",
#        y = "Rev Heat Flow") +
#   theme_minimal()
#
# ggplot(results_df, aes(x = time)) +
#   geom_line(aes(y = THF), color = "red", size = 1) +
#   labs(title = "Total Heat Flow over Time",
#        x = "Time (min)",
#        y = "Total Heat Flow") +
#   theme_minimal()

# Now, separate modulation frequency calculation
# Set modulation frequency directly for the plot (fixed to 1.5 cycles/min as requested)
# modulation_results_df <- data.frame(time = results_df$time, Modulation_Frequency = rep(1.5, length(results_df$time)))
#
# ggplot(modulation_results_df, aes(x = time)) +
#   geom_line(aes(y = Modulation_Frequency), color = "purple", size = 1) +
#   labs(title = "Detected Modulation Frequency of Heat Flow over Time",
# #        x = "Time (min)",
# #        y = "Modulation Frequency (cycles/min)") +
# #   theme_minimal()
#
# # Perform Fourier Transform on the entire heat flow data
# FT_heat_flow <- fft(heat_flow)
#
# # Frequency vector for the Fourier transform
# n <- length(heat_flow)
# freq <- (0:(n - 1)) * (1 / (n * (time[2] - time[1])))
#
# # Compute the power spectrum (squared magnitude of the Fourier coefficients)
# power_spectrum <- Mod(FT_heat_flow)^2
# dfcheck <- data.frame(Frequency = freq, Power = power_spectrum)
#
# # Plot the power spectrum
# ggplot(data.frame(Frequency = freq, Power = power_spectrum), aes(x = Frequency, y = Power)) +
#   geom_line(color = "blue") +
#   labs(title = "Power Spectrum of Heat Flow",
#        x = "Frequency (cycles/min)",
#        y = "Power") +
#   theme_minimal()

# Define the finer time grid for interpolation (adjust length.out as needed)
fine_time_grid <- seq(min(time), max(time), length.out = 10000)

# Interpolate RevCp values onto the finer time grid
interpolated_revcap <- approx(window_times, rev_cp_values, xout = fine_time_grid)$y

# Check for NA or infinite values in the interpolated RevCp data and remove them
valid_indices_revcap <- which(is.finite(interpolated_revcap))
fine_time_grid_clean_revcap <- fine_time_grid[valid_indices_revcap]
interpolated_revcap_clean <- interpolated_revcap[valid_indices_revcap]

# Fit a smooth spline to the cleaned interpolated RevCp data
spline_fit_revcap <- smooth.spline(fine_time_grid_clean_revcap, interpolated_revcap_clean)

# Get the fitted values from the spline for RevCp
fitted_revcap <- spline_fit_revcap$y

# Plot the original and spline-fitted RevCp
ggplot() +
  geom_line(aes(x = fine_time_grid_clean_revcap, y = fitted_revcap), color = "blue", size = 1) +  # Spline fitted data
  labs(title = "Spline Fit of Interpolated RevCp over Time",
       x = "Time (min)", 
       y = "Reversing Heat Capacity (RevCp)") +
  theme_minimal()

# Interpolate Rev_heat_flow values onto the finer time grid
interpolated_revheatflow <- approx(window_times, rev_heat_flow_values, xout = fine_time_grid)$y

# Check for NA or infinite values in the interpolated Rev_heat_flow data and remove them
valid_indices_revheatflow <- which(is.finite(interpolated_revheatflow))
fine_time_grid_clean_revheatflow <- fine_time_grid[valid_indices_revheatflow]
interpolated_revheatflow_clean <- interpolated_revheatflow[valid_indices_revheatflow]

# Fit a smooth spline to the cleaned interpolated Rev_heat_flow data
spline_fit_revheatflow <- smooth.spline(fine_time_grid_clean_revheatflow, interpolated_revheatflow_clean)

# Get the fitted values from the spline for Rev_heat_flow
fitted_revheatflow <- spline_fit_revheatflow$y

# Plot the original and spline-fitted Rev_heat_flow
ggplot() +
  geom_line(aes(x = fine_time_grid_clean_revheatflow, y = fitted_revheatflow), color = "green", size = 1) +  # Spline fitted data
  labs(title = "Spline Fit of Interpolated Rev Heat Flow over Time",
       x = "Time (min)", 
       y = "Reversing Heat Flow") +
  theme_minimal()

# Define the finer time grid for interpolation (you can adjust the length.out as needed)
fine_time_grid <- seq(min(time), max(time), length.out = 10000)  # More time points

# Interpolate the total heat flow values onto the finer time grid
interpolated_tot_heat_flow <- approx(window_times, tot_heat_flow_values, xout = fine_time_grid)$y

# Plot the interpolated total heat flow
ggplot(data.frame(Time = fine_time_grid, Interpolated_THF = interpolated_tot_heat_flow), aes(x = Time, y = Interpolated_THF)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Interpolated Total Heat Flow over Time",
       x = "Time (min)", 
       y = "Interpolated Total Heat Flow") +
  theme_minimal()

# Define the finer time grid for interpolation (you can adjust the length.out as needed)
fine_time_grid <- seq(min(time), max(time), length.out = 10000)  # More time points

# Interpolate the total heat flow values onto the finer time grid
interpolated_tot_heat_flow <- approx(window_times, tot_heat_flow_values, xout = fine_time_grid)$y

# Check for NA or infinite values in the interpolated data and remove them
valid_indices <- which(is.finite(interpolated_tot_heat_flow))
fine_time_grid_clean <- fine_time_grid[valid_indices]
interpolated_tot_heat_flow_clean <- interpolated_tot_heat_flow[valid_indices]

# Fit a smooth spline to the cleaned interpolated total heat flow data
spline_fit <- smooth.spline(fine_time_grid_clean, interpolated_tot_heat_flow_clean)

# Get the fitted values from the spline
fitted_tot_heat_flow <- spline_fit$y

# Plot the original and spline-fitted total heat flow
ggplot() +
  geom_line(aes(x = fine_time_grid_clean, y = fitted_tot_heat_flow), color = "blue", size = 1) +  # Spline fitted data
  labs(title = "Spline Fit of Interpolated Total Heat Flow over Time",
       x = "Time (min)", 
       y = "Total Heat Flow") +
  theme_minimal()

# Define the length for the zero-padded Fourier Transform (choose a large enough value for desired resolution)
padded_length <- 10000 # This sets the same frequency resolution for each interval

# Calculate the common frequency vector based on the padded length and sampling interval
sampling_interval <- mean(diff(fine_time_grid_clean))  # Average time step between points (assuming it's fairly consistent)
common_freq <- (0:(padded_length - 1)) * (1 / (padded_length * sampling_interval))

# Initialize a data frame to store modulation frequency results for each interval
modulation_freq_df <- data.frame(Interval_Start = numeric(), Interval_End = numeric(), Modulation_Frequency = numeric())

# Define the interval boundaries
intervals <- seq(0, 160, by = 5)

# Loop through each interval and calculate the modulation frequency using the common frequency vector
for (i in seq_along(intervals[-length(intervals)])) {
  
  # Define start and end times for the current interval
  interval_start <- intervals[i]
  interval_end <- intervals[i + 1]
  
  # Filter the data within the current interval
  interval_data <- data.frame(
    Time = fine_time_grid_clean,
    Total_Heat_Flow = interpolated_tot_heat_flow_clean
  ) %>%
    filter(Time >= interval_start & Time < interval_end)
  
  # Perform Fourier Transform on the zero-padded total heat flow in the interval
  FT_interval <- fft(c(interval_data$Total_Heat_Flow, rep(0, padded_length - nrow(interval_data))))
  
  # Compute the power spectrum (squared magnitude of the Fourier coefficients)
  power_spectrum_interval <- Mod(FT_interval)^2
  dfcheck <- data.frame(Frequency = common_freq, Power = power_spectrum_interval)
  
  # Plot the power spectrum for the current interval and use print() to ensure it displays
  p <- ggplot(dfcheck, aes(x = Frequency, y = Power)) +
    geom_line(color = "blue") +
    xlim(1, 2) +
    scale_y_continuous(limits = c(0, 60)) +
    labs(title = paste("Power Spectrum of Heat Flow for Interval", interval_start, "-", interval_end, "min"),
         x = "Frequency (cycles/min)",
         y = "Power") +
    theme_minimal()
  
  print(p)  # Use print() to display the plot for each interval
  
  # Identify the dominant frequency (excluding the zero frequency component)
  dominant_freq_index <- which.max(power_spectrum_interval[2:(padded_length / 2 + 1)]) + 1  # Offset by 1 for R indexing
  modulation_frequency <- common_freq[dominant_freq_index]
  
  # Append the result for the current interval
  modulation_freq_df <- rbind(modulation_freq_df, data.frame(
    Interval_Start = interval_start,
    Interval_End = interval_end,
    Modulation_Frequency = modulation_frequency
  ))
}

# Print the calculated modulation frequencies for each interval
print(modulation_freq_df)

# Plot the modulation frequency over intervals
ggplot(modulation_freq_df, aes(x = Interval_Start, y = Modulation_Frequency)) +
  geom_line(color = "purple", size = 1) +
  labs(title = "Calculated Modulation Frequency of Total Heat Flow over Intervals with Common Frequency Vector",
       x = "Interval Start Time (min)",
       y = "Modulation Frequency (cycles/min)") +
  theme_minimal()

