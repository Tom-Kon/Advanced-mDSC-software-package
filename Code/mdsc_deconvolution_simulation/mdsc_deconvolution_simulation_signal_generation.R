#-----------------------------------------------------------------------------------------
#Function to create the baseline MHF signal, add Tgs, and add Gaussian curves on top
#-----------------------------------------------------------------------------------------

signal_generation <- function(reactiveInputs, timeGen) {
  
  specialMelt <- reactiveInputs$specialMelt
  specialMeltCheck <- reactiveInputs$specialMeltCheck
  sharpnessLinkPeriod <- reactiveInputs$sharpnessLinkPeriod
  
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
  deltaRevCpPreTg <- reactiveInputs$deltaRevCpPreTg
  deltaRevCpPostTg <- reactiveInputs$deltaRevCpPostTg
  startRevCpPreTg <- reactiveInputs$startRevCpPreTg
  
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

  
  # deltaHFPreTg <- -deltaCpPreTg*heatRate
  # deltaHFPostTg <- -deltaCpPostTg*heatRate
  # StartHFTempPreTg <- -StartCpTempPreTg*heatRate
  # deltaHFTg <- -deltaCpTg*heatRate  # in W/g

  
  modTemp <- Atemp * sin(2*pi/period * times) + heatRate * times + startTemp
  modTempnoRamp <- Atemp * sin(2*pi/period * times)
  TRef <- startTemp + heatRate * times
  modTempderiv <- Atemp * 2*pi/period * cos(2*pi/period * times) + heatRate
  modTempdervPhase <- Atemp * 2*pi/period * cos(2*pi/period * times + phase) + heatRate
  modTempdervPhaseNoHR <- Atemp * 2*pi/period * cos(2*pi/period * times + phase)
  
  FinalRevCpPreTg <- startRevCpPreTg + deltaRevCpPreTg * locationTgRHF[1]
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
  
  SinebeforeTg <- (startRevCpPreTg + deltaRevCpPreTg * TRef) * modTempdervPhaseNoHR
  SineafterTg <- (StartRevCpTempPostTg + deltaRevCpPostTg * TRef) * modTempdervPhaseNoHR
  
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
    modTempnoRamp = modTempnoRamp
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
    enthalpy <- signalToAdd[3]      #No normalization for heating rate since it's already being defined in the time domain. 
    sigma <- (endset-onset)/(2*sqrt(2*log(1000)))
    sigmaTime <- sigma/heatRate
    
    
    for (i in seq_along(df$TRef)) {
      signalVec[i] <- enthalpy/sqrt(2*pi*sigmaTime^2) * exp(-((df$times[i] - (midpoint-startTemp)/heatRate)^2) / (2 * sigmaTime^2))
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
      sigmaTime <- sigma/heatRate
      
      
      for (j in seq_along(df$TRef)) {
        signalVec[j] <- enthalpy/sqrt(2*pi*sigmaTime^2) * exp(-((df$times[j] - (midpoint-startTemp)/heatRate)^2) / (2 * sigmaTime^2))
      }
      
      # Add signal and update MHF
      df <- df %>%
        mutate(
          MHF = MHF + signalVec
        )
    }
  }
  
  if (specialMeltCheck) {
    signalToAdd <- specialMelt
    onset <- signalToAdd[1]
    endset <- signalToAdd[2]
    midpoint <- (onset + endset) / 2
    enthalpy <- signalToAdd[3] #VERY IMPORTANT!!! Since this time signal is generated ifo times, enthalpy is not multiplied by HR!!
    sharpness <- reactiveInputs$sharpness
    offset <- reactiveInputs$offset
    sigmasmallperiod <- reactiveInputs$sigmasmallperiod
    firstpointSwitch <- reactiveInputs$firstpointSwitch

    
    sigma <- (endset - onset) /(2*sqrt(2 * log(1000)))
    sigmaTime <- sigma/heatRate
    
    if (sharpnessLinkPeriod) {
      sigmaSmall <- period/(6*sqrt(2*log(2))) * sharpness
    } else {sigmaSmall <- sigmasmallperiod/(6*sqrt(2*log(2))) * sharpness}
    
    #Checking if FWHMs are equal
    # print(paste0("SineFWHM = ",(period/3)))
    # print(paste0("smallGaussianFWHM = ",(2*sigmaSmall*(sqrt(2*log(2))))))
    
    
    # Overlaying Gaussian (main signal)
    overlayingGaussian <- 1 / sqrt(2 * pi * sigmaTime^2) * exp(-((df$times - (midpoint-startTemp)/heatRate)^2) / (2 * sigmaTime^2))

    # Find index of temperature closest to onset.
    onsetWindow <- which.min(abs(modTemp - onset))

    # Define window safely
    delta <- sampling * period * 1.1

    # Extract windowed times and modTemp values
    windowTimes <- times[onsetWindow: (onsetWindow + delta)]
    windowmodTemp <- modTemp[onsetWindow: (onsetWindow + delta)]

    # Find the time where modTemp deviates least from linear expectation
    removeRamp <- windowmodTemp - ((windowTimes-windowTimes[1]) * heatRate)

    if(firstpointSwitch == "min") {
      firstpoint <- windowTimes[which.min(removeRamp)] + offset
    } else if(firstpointSwitch == "max") {
      firstpoint <- windowTimes[which.max(removeRamp)] + offset
    } else if(firstpointSwitch == "zero") {
      firstpoint <- windowTimes[which.min(abs(removeRamp))] + offset
    }
    
    #Make sure that firstpoint (and multiples hereoff) can actually be found in df$time
    firstpoint <- round(firstpoint, 1)

    # Calculate number of full periods (integer)
    numberPeriods <- floor((endset - onset) / heatRate / period)
    
    # Pre-allocate timeList vector
    timeList <- numeric(numberPeriods + 1)
    
    # Build list of times at each period starting from firstpoint
    for (i in 0:numberPeriods) {
      timeList[i + 1] <- firstpoint + i * period  # R is 1-indexed
    }

    # # Extract corresponding TRef values from df for these times
    # tempList <- df$TRef[df$times %in% timeList]

    
    # Small signal: sum of additional gaussians centered on tempList
    smallSignal <- rep(0, length(df$times))  # initialize vector
    smallSignalDf <- data.frame(times = times, modTemp = modTemp)

    #Here I multiply overlayingGaussian by a scaling factor in order to be consistent with the total enthalpy and to make sure that the integration results in an enthalpy in J/g. 
    weights <- c()

    for (time in timeList) {
      i <- which.min(abs(df$times - time))
      weights <- c(weights, overlayingGaussian[i])
    }
    
    factor <- enthalpy/sum(weights)
    currentMagnitude <- weights*factor
    i <- 1

    for (time in timeList) {
      currentPeak <- (currentMagnitude[i]/sqrt(2*pi*sigmaSmall^2)*exp(-((df$times - time)^2) / (2 * sigmaSmall^2)))
      smallSignalDf <- cbind(smallSignalDf, currentPeak)
      i <- i+1
    }
    
    smallSignal <- rowSums(smallSignalDf[, 3:ncol(smallSignalDf), drop = FALSE])
    
    # Rename the individual Gaussian columns clearly
    n_signals <- length(smallSignalDf) - 2
    names(smallSignalDf) <- c("times", "modTemp", paste0("signal_", seq_len(n_signals)))
    
    
    # Add to MHF column but also save the old MHF
    df <- df %>%
      mutate(
        MHFnomelt = MHF,
        MHF = MHF + smallSignal
      )
    
    df$overlayingGaussian <- overlayingGaussian
    
    df$smallSignal <- smallSignal
    
    df <- cbind(df, smallSignalDf[, -(1:2)])  # drop TRef and modTemp from smallSignalDf
  }
  
  signalGen <- df

return(signalGen)
}

