signalgeneration <- function(sampling, startTemp, endTemp, period, heatRate, Atemp, phase, deltaRHFPreTg, deltaRHFPostTg, StartRHFPreTg, deltaCpPreTg, deltaCpPostTg, StartCpTempPreTg, locationTgTHF, locationTgRHF, deltaCpTg,MeltEnth,phase_melt,locationMelt, Crystalenth,locationcrystal,EnthrecEnth,locationEnthRec, periodSignal, df1){
  
  times <- df1$times
  groups <- df1$groups
  
  deltaHFPreTg <- -deltaCpPreTg*heatRate
  deltaHFPostTg <- -deltaCpPostTg*heatRate
  StartHFTempPreTg <- -StartCpTempPreTg*heatRate
  deltaHFTg <- -deltaCpTg*heatRate  # in W/g
  
  
  deltaRevCpTempPreTg <- -deltaRHFPreTg/heatRate
  deltaRevCpTempPostTg <- -deltaRHFPostTg/heatRate
  StartRevCpTempPreTg <- -StartRHFPreTg/heatRate
  
  
  locationMelt[3] <- (locationMelt[2]+locationMelt[1])/2
  locationcrystal[3] <- (locationcrystal[2]+locationcrystal[1])/2
  locationEnthRec[3] <- (locationEnthRec[2]+locationEnthRec[1])/2
  
  
  modTemp <- Atemp * sin(2*pi/period * times) + heatRate * times
  modTempnoRamp <- Atemp * sin(2*pi/period * times)
  TRef <- startTemp + heatRate * times
  modTempderiv <- Atemp * 2*pi/period * cos(2*pi/period * times) + heatRate
  modTempdervPhase <- Atemp * 2*pi/period * cos(2*pi/period * times + phase) + heatRate
  
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
  # This part only takes into account the oscillatory component, so heatRate is not used in the generation of the signal.  
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
        TRef < locationTgRHF[1] ~ (StartRevCpTempPreTg + deltaRevCpTempPreTg * TRef) * (modTempdervPhase-heatRate),
        isTg ~ RevCpTg * (modTempdervPhase-heatRate),
        TRef > locationTgRHF[2] ~ (StartRevCpTempPostTg + deltaRevCpTempPostTg * (TRef - locationTgRHF[2])) * (modTempdervPhase-heatRate)
      )
    ) %>%
    select(-isTg, -tg_index)
  
  #Add baseline to MHF
  # This is nonsensical, physically speaking. Both the oscillatory component and the baseline component should be generated at the same time and with the same Cp values. However, since the aim of this code is to replicate experimental data, the two are split up to allow for different heat capacities to be used.  
  
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
  reachedTemps <- c(0)

  
  # Initialize signal vector
  signal_vecmelt_raw <- numeric(nrow(df))  # store unscaled signal
  smallGauss <- numeric(nrow(df))
  sigmamelt <- (locationMelt[2] - locationMelt[3]) / sqrt(2 * log(1000))  # Gaussian sigma based on FWHM logic
  sharpness <- 0.1
  sigmasmallmelt <- (pi-2*asin(0.5))/(2*pi/period*sqrt(8*log(2)))*sharpness
  
  # We'll start with amplitude = 1 and scale later
  #TO DO LATER: how to include phase melt?????????
  amplitude <- -1  
  
  for (i in seq_along(df$modTemp)) {
    if (df$TRef[i] >= locationMelt[1] && df$TRef[i] <= locationMelt[2]) {
      if (df$modTemp[i] < reachedTemps[length(reachedTemps)]) {
        signal_vecmelt_raw[i] <- 0
      } else {
        reachedTemps <- c(reachedTemps, df$modTemp[i])
        # gauss_weight <- (1 / (sigmamelt * sqrt(2 * pi))) * exp(-((df$TRef[i] - locationMelt[3])^2) / (2 * sigmamelt^2))
        smallGauss <- (1 / (sigmasmallmelt * sqrt(2 * pi))) * exp(-((df$times-df$times[i])^2) / (2 * sigmasmallmelt^2))
        signal_vecmelt_raw <- signal_vecmelt_raw + amplitude * 1 * smallGauss
        } 
    } else {
      signal_vecmelt_raw[i] <- 0
    }
  }
  
  # Compute area under the curve using trapezoidal integration (non-uniform spacing)
  area_raw <- trapz(df$times, signal_vecmelt_raw)
  
  # Scale factor to match desired melting enthalpy
  scaling_factor <- MeltEnth / area_raw
  
  # Final scaled signal
  signal_vecmelt <- signal_vecmelt_raw * 1
  
  # Add signal and update MHF
  df <- df %>%
    mutate(
      signal_vecmelt = signal_vecmelt,
      MHF = if_else(
        TRef >= locationMelt[1] & TRef <= locationMelt[2],
        MHF + signal_vecmelt,
        MHF
      )
    )
  
  sigmacrystal <- (locationcrystal[2]-locationcrystal[3])/sqrt(2*log(1000))
  signal_vec <- numeric(nrow(df))
  
  
  for (i in seq_along(df$TRef)) {
    # Compute crystallisation signal
    
    if (df$TRef[i] >= locationcrystal[1] && df$TRef[i] <= locationcrystal[2]) {
      signal_vec[i] <- Crystalenth/sqrt(2*pi*sigmacrystal^2) * exp(-((TRef[i] - locationcrystal[3])^2) / (2 * sigmacrystal^2))
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
  
  sigmaEnthRec <- (locationEnthRec[2]-locationEnthRec[3])/sqrt(2*log(1000))
  
  
  for (i in seq_along(df$TRef)) {
    # Compute crystallisation signal
    
    if (df$TRef[i] >= locationEnthRec[1] && df$TRef[i] <= locationEnthRec[2]) {
      signal_vec[i] <- EnthrecEnth/sqrt(2*pi*sigmaEnthRec^2) * exp(-((TRef[i] - locationEnthRec[3])^2) / (2 * sigmaEnthRec^2))
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
  
  df2 <- df
  
return(df2)
}

