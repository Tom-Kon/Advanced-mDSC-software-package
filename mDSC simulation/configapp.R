configUI1<- function() {
  tagList(
    column(6,
           textInput("savetitle", "Title of the picture that will be saved", "test"),
           checkboxInput("saveplots", "Do you want to save the plots?", value= FALSE),
           textInput("sampling", "Sampling rate in points per second", "10"),
           textInput("startTemp", "Starting temperature of the mDSC run (°C)", "20"),
           textInput("endTemp", "Final temperature of the mDSC run (°C)", "180"),
           textInput("period", "Period of the modulations in seconds", "40"),
           textInput("heatRate", "Heating rate of the mDSC runs in °C/min", "2"),
           textInput("Atemp", "Amplitude of the temperature modulation (°C)", "0.212"),
           textInput("phase", "Phase of the modulated heat flow with respect to the temperature modulation (rad)", "-0.2"), 
           textInput("loessAlpha", "Degree of smoothing (higher = more smoothing)", "0.05"),    
    )
  )
}

configUI2<- function() {
  tagList(
    column(4,
           textInput("deltaRHFPreTg", "What is the slope of the reversing heat flow before the Tg (J/(g*°C))", "-0.000137"), 
           textInput("deltaRHFPostTg", "What is the slope of the reversing heat flow after the Tg (J/(g*°C))", "-0.000120"),
           textInput("StartRHFPreTg", "What value does your reversing heat flow start at (J/g)", "-0.040"),
           textInput("deltaCpPreTg", "What is the slope of the total heat capacity before the Tg (J/(g*°C))", "0.00858"),
           textInput("deltaCpPostTg", "What is the slope of the total heat capacity after the Tg (J/(g*°C))", "0.00204"),
           textInput("StartCpTempPreTg", "What value does your total heat capacity start at (J/g)", "1.05"),
    )
  )
}

configUI3<- function() {
  tagList(
    column(4,
           textInput("locationTgTHF", "Where is the Tg on the total heat flow? Input start, end, and midpoint separated by commas (°C)", "28.46, 39.13, 33.77"), 
           textInput("locationTgRHF", "Where is the Tg on the reversing heat flow? Input start, end, and midpoint separated by commas (°C)", "35, 45, 40"),
           textInput("deltaCpTg", "What is the jump in heat capacity at the Tg (J/(g*°C))", "0.268"),
           textInput("MeltEnth", "What is the melting enthalpy (in J/g)", "-0.2"),
           textInput("phase_melt", "What is the phase of the melting with respect to the temperature modulation (rad)", "0"),
           textInput("periodSignal", "Period of the melting modulations in seconds", "40"),
           textInput("locationMelt", "Where does the melting start and stop? Two inputs separated by a comma, in °C", "134, 154"),
    )
  )
}

configUI4<- function() {
  tagList(
    column(4,
           textInput("Crystalenth", "What is the crystallisation enthalpy (J/g)", "0.05"), 
           textInput("locationcrystal", "Where does the crystallisation start and stop? Two inputs separated by a comma, in °C", "80,100"),
           textInput("EnthrecEnth", "What is the enthalpy recovery enthalpy (J/g)", "-0.02"),
           textInput("locationEnthRec", "Where does the enthalpy recovery start and stop? Two inputs separated by a comma, in °C", "30,45"),
    )
  )
}

configUI5<- function() {
  tagList(
    HTML("<br>"),
    HTML("<br>"),
    tags$div(
      style = "text-align: center;",
      actionButton("calculate", "Calculate", class = "btn-primary btn-lg")
    )
  )
}



