library(zoo)

fftfunc <- function(period, dt, points) {
  ac <- c()
  dc <- c()
  time_out <- c()
  freq <- 1/period
  
  window_size <- round(period/dt)
  mean <- rollmean(points$heatFlow, window_size)
  
  
  for (i in (window_size + 1):(length(points$heatFlow) - window_size)) {
    window_low <- as.integer(i - (window_size / 2))
    window_high <- as.integer(i + (window_size / 2))
    
    data <- points[window_low:window_high, ]
    fft_result <- fft(data$heatFlow)
    f_s <- 1 / dt
    freqs <- (0:(length(data$heatFlow)-1) * (f_s / length(data$heatFlow))) 
    bin <- which.min(abs(freqs - freq))
    
    # dcResult <- Mod(fft_result[1]) / length(data$heatFlow)
    acResult <- 2 * Mod(fft_result[bin]) / length(data$heatFlow)  # Normalize
    
    # dc <- c(dc, dcResult)
    ac <- c(ac, acResult)
    time_out <- c(time_out, points$time[i])
  }
  output <- data.frame(mean = mean, time = time_out, ac = ac)
  
}


t <- seq(0, 1000, length.out = 10001)       # time from 0 to 1 second
dt <- 1000/(10001-1)
A <- 2
k <- 4
period <- 40
d <- data.frame(time = t, heatFlow = A*sin(2*pi*1/period*t) + k)

ds <- diff(d$heatFlow)
sgn <- sign(ds)

# Detect turning points where the monotonicity changes
change_idx <- which(diff(sgn) != 0) + 1

# Define segment boundaries (include start and end)
seg_boundaries <- sort(unique(c(1, change_idx, length(d$heatFlow))))

# Initialize a data frame to hold resampled points
resampled_points <- data.frame(time = numeric(), heatFlow = numeric())

# Process each monotonic segment separately
for (i in 1:(length(seg_boundaries) - 1)) {
  seg_idx <- seg_boundaries[i]:seg_boundaries[i + 1]
  x_seg <- d$time[seg_idx]
  y_seg <- d$heatFlow[seg_idx]
  
  # Create equally spaced y-values for the segment
  y_equally <- seq(min(y_seg), max(y_seg), length.out = length(y_seg))
  
  # Invert the relation: interpolate x-values for these equally spaced y-values
  x_equally <- approx(x = y_seg, y = x_seg, xout = y_equally)$y
  
  seg_df <- data.frame(time = x_equally, heatFlow = y_equally)
  resampled_points <- rbind(resampled_points, seg_df)
}

resampled_points <- resampled_points[order(resampled_points$time), ]
resampled_points <- unique(resampled_points)
rownames(resampled_points) <- NULL  # Ensures default row numbering

output <- fftfunc(period, dt, resampled_points)


plot(output$time, output$dc, type = "l", col = "blue", ylab = "DC component", xlab = "Time")
plot(output$time, output$ac, type = "l", col = "blue", ylab = "AC component", xlab = "Time")

