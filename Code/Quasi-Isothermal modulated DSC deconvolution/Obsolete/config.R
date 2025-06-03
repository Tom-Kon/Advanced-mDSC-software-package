#What to print, save, calculate? ------------
plot_datasteps4 <- FALSE
saveplotdatasteps4 <- FALSE
saveNRHFplot <- FALSE
saveRHFplot <- FALSE
savemanualRHFplot <- FALSE
saveDatasteps <- FALSE
saveDatasteps2 <- FALSE
saveDatasteps3 <- FALSE
saveDatasteps4 <- FALSE
saveExtremadf1 <- FALSE
saveExtremadf2 <- FALSE
saveExtremadf3 <- FALSE
saveSummaryFT <- FALSE

#----------------------------------


#Change when changing method----
period <- 2/3
step_size <- 3
isotherm_length <- 20
starting_temp <- 13
setAmplitude <- 0.212
#------------------------------

#Don't touch this unless you know what you're doing----
temp_margin_first_cleanup <- 0.045
sampling <- 10 # pts/sec
points_distance_minimum_margin <- (sampling*period*60)/2/5
plotTitleTHF <-paste0("NRHF based on FT (frequency = 0), ", modulations_back, " modulations")
plottitleRHF <- paste0("RevCp based on FT (1st harmonic), ", modulations_back, " modulations")
plottitleRHFmanual <- paste0("RevCp calculated manually, ", modulations_back, " modulations")
subtitle <- paste0("Dataset: ", fileName)
tempModAmplitude <- setAmplitude*2*pi/period
num_ticks <- 10
#-----------------------------------------------------