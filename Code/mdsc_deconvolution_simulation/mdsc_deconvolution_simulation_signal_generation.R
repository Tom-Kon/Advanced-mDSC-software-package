#-----------------------------------------------------------------------------------------
#Function to create the baseline MHF signal, add Tgs, and add Gaussian curves on top
#-----------------------------------------------------------------------------------------

signal_generation <- function(reactiveInputs, timeGen) {
  
  sampling <- reactiveInputs$sampling
  startTemp <- reactiveInputs$startTemp
  endTemp <- reactiveInputs$endTemp
  period <- reactiveInputs$period
  heatRate <- reactiveInputs$heatRate
  Atemp <- reactiveInputs$Atemp
  phase <- reactiveInputs$phase
  deltaRHFPreTg <- reactiveInputs$deltaRHFPreTg
  deltaRHFPostTg <- reactiveInputs$deltaRHFPostTg
  StartRHFPreTg <- reactiveInputs$StartRHFPreTg
  deltaCpPreTg <- reactiveInputs$deltaCpPreTg
  deltaCpPostTg <- reactiveInputs$deltaCpPostTg
  StartCpTempPreTg <- reactiveInputs$StartCpTempPreTg
  
  gaussianNumber <- reactiveInputs$gaussianNumber

  locationTgTHF <- reactiveInputs$locationTgTHF
  locationTgRHF <- reactiveInputs$locationTgRHF
  deltaCpTg <- reactiveInputs$deltaCpTg
  
  
  MeltEnth <- reactiveInputs$MeltEnth
  phase_melt <- reactiveInputs$phase_melt
  locationMelt <- reactiveInputs$locationMelt
  Crystalenth <- reactiveInputs$Crystalenth
  locationcrystal <- reactiveInputs$locationcrystal
  EnthrecEnth <- reactiveInputs$EnthrecEnth
  locationEnthRec <- reactiveInputs$locationEnthRec
  periodSignal <- reactiveInputs$periodSignal

  
  times <- timeGen$times
  groups <- timeGen$groups
  
  
  deltaRevCpTempPreTg <- -deltaRHFPreTg/heatRate
  deltaRevCpTempPostTg <- -deltaRHFPostTg/heatRate
  StartRevCpTempPreTg <- -StartRHFPreTg/heatRate
  
  deltaHFPreTg <- -deltaCpPreTg*heatRate
  deltaHFPostTg <- -deltaCpPostTg*heatRate
  StartHFTempPreTg <- -StartCpTempPreTg*heatRate
  deltaHFTg <- -deltaCpTg*heatRate  # in W/g

  
  modTemp <- Atemp * sin(2*pi/period * times) + heatRate * times
  modTempnoRamp <- Atemp * sin(2*pi/period * times)
  TRef <- startTemp + heatRate * times
  modTempderiv <- Atemp * 2*pi/period * cos(2*pi/period * times) + heatRate
  modTempdervPhase <- Atemp * 2*pi/period * cos(2*pi/period * times + phase) + heatRate
  modTempdervPhaseNoHR <- Atemp * 2*pi/period * cos(2*pi/period * times + phase)
  
  FinalRevCpPreTg <- StartRevCpTempPreTg + deltaRevCpTempPreTg * locationTgRHF[1]
  StartRevCpTempPostTg <- FinalRevCpPreTg + deltaCpTg
  
  
  FinalCpPreTg <- StartCpTempPreTg + deltaCpPreTg*locationTgTHF[1]
  StartCpPostTg <- FinalCpPreTg + deltaCpTg
  
  # Determine indices for the RHF Tg region
  idx_Tg1RHF <- which.min(abs(TRef - locationTgRHF[1]))
  
  # Determine indices for the THF Tg region
  idx_Tg1THF <- which.min(abs(TRef - locationTgTHF[1]))
  
  
  # Create a sequence for the gradual change in RevCp within the Tg regions
  epsilon <- 0.001
  kRHF <- log((1 - epsilon)/epsilon) / ((locationTgRHF[2] - locationTgRHF[1])/2)
  kTHF <- log((1 - epsilon)/epsilon) / ((locationTgTHF[2] - locationTgTHF[1])/2)
  
  RevCpTg <- 1/ (1 + exp(-kRHF * (TRef - locationTgRHF[3])))
  
  SinebeforeTg <- (StartRevCpTempPreTg + deltaRevCpTempPreTg * TRef) * modTempdervPhaseNoHR
  SineafterTg <- (StartRevCpTempPostTg + deltaRevCpTempPostTg * TRef) * modTempdervPhaseNoHR
  
  TRef1 <- TRef[TRef <= locationTgTHF[1]]
  BaseBeforeTgShort <- -(StartCpTempPreTg + deltaCpPreTg * TRef1) * heatRate
  BaseBeforeTg <- -(StartCpTempPreTg + deltaCpPreTg * TRef) * heatRate
  
  TRef2 <- TRef[TRef >= locationTgTHF[2]]
  BaseAfterTgShort <- -(StartCpPostTg + deltaCpPostTg * TRef2) * heatRate
  BaseAfterTg <- -(StartCpPostTg + deltaCpPostTg * TRef)*heatRate
  

  HfTg <- (BaseAfterTgShort[1] - BaseBeforeTgShort[length(BaseBeforeTgShort)]) / (1 + exp(-kTHF * (TRef - locationTgTHF[3])))

  
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
      isTg = TRef >= locationTgTHF[1] & TRef <= locationTgTHF[2],
      tg_index = if_else(isTg, row_number() - idx_Tg1THF + 1, NA_integer_)
    ) %>%
    
    #Generate the MHF in a way that the Tg location has a gradual change in amplitude and baseline
    mutate(
      MHF = case_when(
        TRef < locationTgTHF[1] ~ BaseBeforeTg,
        isTg ~ BaseBeforeTgShort[length(BaseBeforeTgShort)] + HfTg,  # Ensure 'isTg' is correctly referenced
        TRef > locationTgTHF[2] ~ BaseAfterTg
      )
    ) %>%
    select(-isTg, -tg_index)
  
  
  #Add baseline to MHF
  df$MHF <- df$MHF + SinebeforeTg*(1-RevCpTg)+SineafterTg*RevCpTg

  
  # Track already reached temperatures
  # reachedTemps <- numeric(0)
  # 
  # # Initialize signal vector
  # signal_vecmelt <- numeric(nrow(df))
  # sigmamelt <- (locationMelt[2]-locationMelt[3])/sqrt(2*log(1000))  # Assuming FWHM-based estimate
  # meltAmplitude <- MeltEnth/sqrt(2*pi*sigmamelt^2) * exp(-((TRef - locationMelt[3])^2) / (2 * sigmamelt^2))
  # 
  # 
  # for (i in seq_along(df$modTemp)) {
  #   if (df$modTemp[i] %in% reachedTemps) {
  #     signal_vecmelt[i] <- 0  # No new signal
  #   } else {
  #     # Add new temperature to reached list
  #     reachedTemps <- c(reachedTemps, df$modTemp[i])
  #     
  #     # Compute signal
  #     if (df$TRef[i] >= locationMelt[1] && df$TRef[i] <= locationMelt[2]) {
  #       signal_vecmelt[i] <- min(meltAmplitude[i] * sin((2*pi/periodSignal*df$times[i]) + phase_melt), 0)
  #     } else {
  #       signal_vecmelt[i] <- 0
  #     }
  #   }
  # }
  # 
  # # Add signal and update MHF
  # df <- df %>%
  #   mutate(
  #     signal_vecmelt = signal_vecmelt,
  #     MHF = if_else(
  #       TRef >= locationMelt[1] & TRef <= locationMelt[2],
  #       MHF + signal_vecmelt,
  #       MHF
  #     )
  #   )
  
  signalVec <- numeric(nrow(df))
  
  if(gaussianNumber == 0) {
    NULL
    
    
  } else if (gaussianNumber == 1){
    signalToAdd <- reactiveInputs$gaussianList[[1]]
    onset <- signalToAdd[1]
    endset <- signalToAdd[2]
    midpoint <- (signalToAdd[1]+signalToAdd[2])/2
    enthalpy <- signalToAdd[3]/heatRate       #Normalize by heating rate because here the signal is being generated with respect to temperature whereas the enthalpy is normally calculated with respect to time. In other words, the AUC of a heat flow vs temperature graph is not the enthalpy; the AUC of a heat flow vs time is. 
    sigma <- (endset-onset)/(2*sqrt(2*log(1000)))
    
    
    for (i in seq_along(df$TRef)) {
      signalVec[i] <- enthalpy/sqrt(2*pi*sigma^2) * exp(-((df$TRef[i] - midpoint)^2) / (2 * sigma^2))
    }
    

    # Add signal and update MHF
    df <- df %>%
      mutate(
        MHF = MHF + signalVec
      )
    
    
  } else {
    for(i in 1:gaussianNumber) {
      signalToAdd <- reactiveInputs$gaussianList[[i]]
      onset <- signalToAdd[1]
      endset <- signalToAdd[2]
      midpoint <- (signalToAdd[1]+signalToAdd[2])/2
      enthalpy <- signalToAdd[3]
      sigma <- (endset-onset)/(2*sqrt(2*log(1000)))
      
      for (j in seq_along(df$TRef)) {
        signalVec[j] <- enthalpy/sqrt(2*pi*sigma^2) * exp(-((TRef[j] - midpoint)^2) / (2 * sigma^2))
      }
      
      # Add signal and update MHF
      df <- df %>%
        mutate(
          MHF = MHF + signalVec
        )
    }
  }

  signalGen <- df

return(signalGen)
}

