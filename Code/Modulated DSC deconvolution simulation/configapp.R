configUIsim1 <- function(ns) {
  tagList(
    column(6,
           textInput(ns("sampling"), "Sampling rate in points per second", "10"),
           textInput(ns("startTemp"), "Starting temperature of the mDSC run (°C)", "20"),
           textInput(ns("endTemp"), "Final temperature of the mDSC run (°C)", "180"),
           textInput(ns("period"), "Period of the modulations in seconds", "40"),
           textInput(ns("heatRate"), "Heating rate of the mDSC runs in °C/min", "2"),
           textInput(ns("Atemp"), "Amplitude of the temperature modulation (°C)", "0.212"),
           textInput(ns("phase"), "Phase of the modulated heat flow with respect to the temperature modulation (rad)", "-0.2"),
           textInput(ns("loessAlpha"), "Degree of smoothing (higher = more smoothing)", "0.05")
    )
  )
}

configUIsim2 <- function(ns) {
  tagList(
    column(4,
           textInput(ns("deltaRHFPreTg"), "What is the slope of the reversing heat flow before the Tg (J/(g*°C))", "-0.000137"),
           textInput(ns("deltaRHFPostTg"), "What is the slope of the reversing heat flow after the Tg (J/(g*°C))", "-0.000120"),
           textInput(ns("StartRHFPreTg"), "What value does your reversing heat flow start at (J/g)", "-0.040"),
           textInput(ns("deltaCpPreTg"), "What is the slope of the total heat capacity before the Tg (J/(g*°C))", "0.00858"),
           textInput(ns("deltaCpPostTg"), "What is the slope of the total heat capacity after the Tg (J/(g*°C))", "0.00204"),
           textInput(ns("StartCpTempPreTg"), "What value does your total heat capacity start at (J/g)", "1.05")
    )
  )
}

configUIsim3 <- function(ns) {
  tagList(
    column(4,
           textInput(ns("locationTgTHF"), "Where is the Tg on the total heat flow? Input start, end, and midpoint separated by commas (°C)", "28.46, 39.13, 33.77"),
           textInput(ns("locationTgRHF"), "Where is the Tg on the reversing heat flow? Input start, end, and midpoint separated by commas (°C)", "35, 45, 40"),
           textInput(ns("deltaCpTg"), "What is the jump in heat capacity at the Tg (J/(g*°C))", "0.268"),
           textInput(ns("MeltEnth"), "What is the melting enthalpy (in J/g)", "-0.2"),
           textInput(ns("phase_melt"), "What is the phase of the melting with respect to the temperature modulation (rad)", "0"),
           textInput(ns("periodSignal"), "Period of the melting modulations in seconds", "40"),
           textInput(ns("locationMelt"), "Where does the melting start and stop? Two inputs separated by a comma, in °C", "134, 154")
    )
  )
}

configUIsim4 <- function(ns) {
  tagList(
    column(4,
           textInput(ns("Crystalenth"), "What is the crystallisation enthalpy (J/g)", "0.05"),
           textInput(ns("locationcrystal"), "Where does the crystallisation start and stop? Two inputs separated by a comma, in °C", "80,100"),
           textInput(ns("EnthrecEnth"), "What is the enthalpy recovery enthalpy (J/g)", "-0.02"),
           textInput(ns("locationEnthRec"), "Where does the enthalpy recovery start and stop? Two inputs separated by a comma, in °C", "30,45")
    )
  )
}

configUIsim5 <- function(ns) {
  tagList(
    HTML("<br>"),
    HTML("<br>"),
    tags$div(
      style = "text-align: center;",
      actionButton(ns("calculate"), "Calculate", class = "btn-primary btn-lg")
    )
  )
}
