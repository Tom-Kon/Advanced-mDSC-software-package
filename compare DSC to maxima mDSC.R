library(readxl)
library(ggplot2)
library(dplyr)
library(signal)
library(openxlsx)

#Settings
Location <- "C:/Users/u0155764/Desktop"
excelName <- "DSC vs mDSC.xlsx"
sheetName_DSC <- "DSC 2"
sheetName_mDSC <- "mDSC 2"
plotTitle <- "2°C_min, 0.212°C, 40s vs. unmodulated DSC 2"


# Parameters
# start_timemDSC <- 2.25   # Starting time for the first maximum (in minutes)
period <- 2/3
heating_rate <- 2
starttemp <- 10
endtemp <- 180
sample_interval <- 0.1
set_amplitude <- 0.212




freq <- 2*pi/period
temp_amplitude <- set_amplitude*freq



# Read data from Excel sheets
setwd(Location)
dataDSC <-  na.omit(read_excel(excelName, sheet = sheetName_DSC))
datamDSC <- na.omit(read_excel(excelName, sheet = sheetName_mDSC))

dataDSC <- dataDSC[-1,]
datamDSC <- datamDSC[-1,]

dataDSC <- as.data.frame(sapply(dataDSC, as.numeric))
datamDSC <- as.data.frame(sapply(datamDSC, as.numeric))

namevector <- c("time", "temperature", "heat_flow")
names(dataDSC) <- namevector
names(datamDSC) <- namevector 

# Automatically determine location of the 1st maximum
datamDSCtemp <- data.frame(heat_flow = datamDSC$heat_flow[0:(1.5*period*60/sample_interval)])

# Find the index of the first maximum heat flow
max_value <- datamDSCtemp$heat_flow[which.max(datamDSCtemp$heat_flow)]
max_indices <- which(datamDSCtemp$heat_flow == max_value)
middle_index <- max_indices[ceiling(length(max_indices) / 2)]


# Store the corresponding time as start_timemDSC
start_timemDSC <- datamDSC$time[middle_index]



# Generate maxima and minima timepoints
max_times <- seq(start_timemDSC, by = period, length.out = 10+(endtemp-starttemp)/(heating_rate*period))  # Maxima timepoints, length.out is basically number of modulations in the time frame
min_times <- max_times + (period / 2)                      # Minima timepoints


# Match maxima to closest points in data and generate maxima df
maxima <- tibble(
  type = "max",
  time_adjusted = max_times,
) %>%
  rowwise() %>%
  mutate(
    closest_index = which.min(abs(datamDSC$time - time_adjusted)),
    closest_time = datamDSC$time[closest_index],
    closest_temperature = datamDSC$temperature[closest_index],
    heat_flowmDSC = datamDSC$heat_flow[closest_index]
  ) %>%
  ungroup()

# Match maxima in the modulated THF to points in the deconvoluted THF
maximadeconv <- tibble(
  type = "max",
  temp_adjusted = maxima$closest_temperature
) %>%
  rowwise() %>% 
  mutate(
    closest_index = which.min(abs(dataDSC$temperature - temp_adjusted)),
    heat_flowDSC = dataDSC$heat_flow[closest_index]
  ) %>%
  ungroup()

#Paired dataframe
paired <- data.frame(maxima$closest_temperature, maxima$heat_flowmDSC, maximadeconv$heat_flowDSC)
names(paired) <- c("Temperatures","Maxima mDSC", "Matched DSC")
AmodHF <- paired$`Maxima mDSC` - paired$`Matched DSC` 
RevCp <- AmodHF/temp_amplitude
reversingHF <- -heating_rate*RevCp
paired <- cbind(paired, AmodHF, RevCp, reversingHF)



# Apply Savitzky-Golay smoothing
window_size <- 9  # Choose an odd number (e.g., 5, 7, 9) for the smoothing window
poly_order <- 2   # Polynomial order (typically 2 or 3)

paired <- paired %>%
  mutate(smoothed_reversingHF = sgolayfilt(reversingHF, p = poly_order, n = window_size))



threshold <- -0.04  # Set your threshold value
paired <- paired %>%
  mutate(smoothed_reversingHF = ifelse(Temperatures < 20 & smoothed_reversingHF < -0.05, NA, smoothed_reversingHF))
paired <- paired %>%
  mutate(reversingHF = ifelse(reversingHF > threshold | (Temperatures < 20 & reversingHF < -0.05), NA, reversingHF))


# Remove the last 10 points from the smoothed_reversingHF column
paired <- paired %>%
  mutate(smoothed_reversingHF = replace(smoothed_reversingHF, (n() - 20):n(), NA))


# Define the number of ticks you want for the x-axis
num_ticks <- 20  # Change this number as needed
y_min <- min(paired$smoothed_reversingHF, na.rm = TRUE) - 0.0005
y_max <- max(paired$smoothed_reversingHF, na.rm = TRUE) + 0.0005

# Plot smoothed data with custom y-axis limits
ggplot(paired, aes(x = Temperatures)) +
  geom_line(aes(y = smoothed_reversingHF), color = "black", size = 1.5) +     # Smoothed data with thicker line
  labs(
    title = plotTitle,
    x = "Temperature (°C)",
    y = "Reversing Heat Flow (W/g)"
  ) +
  theme_minimal(base_size = 18) +  # Larger base font size for better readability
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "black"), # Center title with bold font
    axis.title.x = element_text(size = 18, face = "bold", color = "black"),  # Bold axis labels
    axis.title.y = element_text(size = 18, face = "bold", color = "black", margin = margin(r = 10)),  # Bold and separated y-axis title
    axis.text = element_text(size = 18, color = "black"),  # Clear and readable axis text
    axis.line = element_line(color = "black", size = 0.5),  # Black axis lines for better contrast
    panel.grid.major = element_line(color = "gray", size = 0.25),  # Light gray grid lines for a clean look
    panel.grid.minor = element_blank(),  # Minor grid lines removed
    plot.margin = margin(20, 20, 20, 20),  # Increase space around the plot
    axis.ticks = element_line(color = "black", size = 0.5)  # Ticks for axes
  ) +
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = num_ticks)) +  # Set the number of x-axis ticks
  scale_y_continuous(
    expand = c(0, 0),  # Remove space between plot and y-axis
    limits = c(y_min, y_max)  # Set y-axis limits with additional space
  )  # This ensures the y-axis covers the full range of your data with extra space at the top

# Save the plot with higher resolution and larger dimensions
saveName <- paste0(plotTitle, ".png")
ggsave(saveName, dpi = 600, width = 10, height = 10)


#Export to Excel for further analysis 
# write.xlsx(paired, "Deconvoluted data without FT2.xlsx")
