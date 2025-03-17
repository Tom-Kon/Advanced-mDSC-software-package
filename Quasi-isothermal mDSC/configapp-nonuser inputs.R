#Don't touch this unless you know what you're doing----
temp_margin_first_cleanup <- 0.25
sampling <- 10 # pts/sec
points_distance_minimum_margin <- (sampling*period*60)/2/5
plotTitleTHF <-paste0("NRHF based on FT (frequency = 0), ", modulations_back, " modulations")
plottitleRHF <- paste0("RevCp based on FT (1st harmonic), ", modulations_back, " modulations")
plottitleRHFmanual <- paste0("RevCp calculated manually, ", modulations_back, " modulations")
subtitle <- paste0("Dataset: ", "test")
tempModAmplitude <- setAmplitude*2*pi/period
num_ticks <- 10
sample_size <- 5.79

#-----------------------------------------------------

