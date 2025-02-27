library(readxl)
library(ggplot2)
library(dplyr)

# Parameters
start_time <- 75.24    # Starting time for the first maximum (in minutes)
period <- 2/3 
heating_rate <- 2
heat_amplitude <- 0.212*2*pi/period
starttemp <- 10
endtemp <- 160


# Read data from Excel sheets
setwd("C:/Users/u0155764/Desktop")
data <- read_excel("HC1 17_09_24_PProx_60C_1W_1_extradata.xls", sheet = "Step 1")

# Extract relevant columns
data_clean <- data %>%
  mutate(time_adjusted = time, heat_flow = heat_flow, deconv_heat_flow = deconv_heat_flow) %>%
  select(time_adjusted, heat_flow, deconv_heat_flow)

# Generate maxima and minima timepoints
max_times <- seq(start_time, by = period, length.out = 10+(endtemp-starttemp)/(2*period))  # Maxima timepoints, length.out is basically number of modulations in the time frame
min_times <- max_times + (period / 2)                      # Minima timepoints

# Match maxima to closest points in data
maxima <- tibble(
  type = "max",
  time_adjusted = max_times
) %>%
  rowwise() %>%
  mutate(
    closest_idx = which.min(abs(data_clean$time_adjusted - time_adjusted)),
    closest_time = data_clean$time_adjusted[closest_idx],
    heat_flow = data_clean$heat_flow[closest_idx]
  ) %>%
  ungroup()

# Match maxima in the modulated THF to points in the deconvoluted THF
maximadeconv <- tibble(
  type = "max",
  time_adjusted = max_times
) %>%
  rowwise() %>%
  mutate(
    closest_idx = which.min(abs(data_clean$time_adjusted - time_adjusted)),
    closest_time = data_clean$time_adjusted[closest_idx],
    heat_flow = data_clean$deconv_heat_flow[closest_idx]
  ) %>%
  ungroup()

# Match minima to closest points in data
minima <- tibble(
  type = "min",
  time_adjusted = min_times
) %>%
  rowwise() %>%
  mutate(
    closest_idx = which.min(abs(data_clean$time_adjusted - time_adjusted)),
    closest_time = data_clean$time_adjusted[closest_idx],
    heat_flow = data_clean$heat_flow[closest_idx]
  ) %>%
  ungroup()

# Pair maxima and minima
paired <- maxima %>%
  mutate(
    min_time = minima$closest_time,
    min_heat_flow = minima$heat_flow,
    deconvRHF = maximadeconv$heat_flow-maxima$heat_flow,
    amplitude = abs(heat_flow - min_heat_flow) / 2,
    avg_time = (closest_time + min_time) / 2
  )
  
# Plot the results
ggplot(paired, aes(x = avg_time, y = amplitude)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 1) +
  labs(
    title = "paired raw",
    x = "Time (average of max and min timepoints)",
    y = "amplitude raw"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 14)
  )


# Plot RHF against time instead
ggplot(paired, aes(x = closest_time, y = deconvRHF)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 1) +
  labs(
    title = "different method",
    x = "Time (average of max and min timepoints)",
    y = "deconvRHF"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 14)
  )


#reversing heat flow instead of amplitude
paired <- paired %>%
  mutate(amplitude = -amplitude * heating_rate / heat_amplitude) %>%
  filter(amplitude < -0.025)

# Filter the data for the desired interval
filtered_data <- paired %>%
  filter(avg_time >= 120 & avg_time <= 160)

 # Plot the filtered data
 ggplot(filtered_data, aes(x = avg_time, y = amplitude)) +
   geom_line(color = "blue", size = 1) +
   geom_point(color = "red", size = 1) +
   labs(
     title = "b = 2 °C/min, A = 0.212 °C, T = 40s",
     x = "Time (average of max and min timepoints)",
     y = "Reversing heat flow"
  ) +
 theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 14)
   )

#
# Plot the results
ggplot(paired, aes(x = avg_time, y = amplitude)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 1) +
  labs(
    title = "b = 2 °C/min, A = 0.212 °C, T = 40s",
    x = "Time (average of max and min timepoints)",
    y = "Reversing heat flow"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 14)
  )
