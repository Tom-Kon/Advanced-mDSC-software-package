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