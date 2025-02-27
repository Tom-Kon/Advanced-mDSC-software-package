rm(list = ls())
graphics.off()

source("mDSC simulation/Libraries.R")
source("mDSC simulation/config.R")
source("mDSC simulation/Time point generation.R")
source("mDSC simulation/Signal generation.R")
source("mDSC simulation/Equally-spaced y-values.R")
source("mDSC simulation/Final calculations.R")
source("mDSC simulation/Plot generation and control.R")
source("mDSC simulation/Shiny.R")

plots <- ggplots(resampled_points)

plots$Overlay
plots$MHF_and_baselinecorr_MHF
plots$Smoothed_THF
plots$Smoothed_RHF
plots$Smoothed_NRHF