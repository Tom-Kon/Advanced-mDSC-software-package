signalgeneration <- function(sampling, startTemp, endTemp, period, heatRate, Atemp, phase, deltaRHFPreTg, deltaRHFPostTg, StartRHFPreTg, deltaHFPreTg, deltaHFPostTg, StartHFTempPreTg, locationTgTHF, locationTgRHF, deltaCpTg,MeltEnth,phase_melt,locationMelt, Crystalenth,locationcrystal,EnthrecEnth,locationEnthRec, periodSignal, df1){
  
  times <- df1$times
  groups <- df1$groups
  
  
  deltaRevCpTempPreTg <- -deltaRHFPreTg/heatRate
  deltaRevCpTempPostTg <- -deltaRHFPostTg/heatRate
  StartRevCpTempPreTg <- -StartRHFPreTg/heatRate
  
  deltaHFTg <- -0.268*heatRate  # in W/g
  
  locationMelt[3] <- (locationMelt[2]+locationMelt[1])/2
  locationcrystal[3] <- (locationcrystal[2]+locationcrystal[1])/2
  locationEnthRec[3] <- (locationEnthRec[2]+locationEnthRec[1])/2
  
  
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
  sigmamelt <- (locationMelt[2]-locationMelt[3])/sqrt(2*log(1000))  # Assuming FWHM-based estimate
  meltAmplitude <- MeltEnth/sqrt(2*pi*sigmamelt^2) * exp(-((TRef - locationMelt[3])^2) / (2 * sigmamelt^2))
  
  
  for (i in seq_along(df$modTemp)) {
    if (df$modTemp[i] %in% reachedTemps) {
      signal_vecmelt[i] <- 0  # No new signal
    } else {
      # Add new temperature to reached list
      reachedTemps <- c(reachedTemps, df$modTemp[i])
      
      # Compute signal
      if (df$TRef[i] >= locationMelt[1] && df$TRef[i] <= locationMelt[2]) {
        signal_vecmelt[i] <- min(meltAmplitude[i] * sin((2*pi/periodSignal*df$times[i]) + phase_melt), 0)
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

