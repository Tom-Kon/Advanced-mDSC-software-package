#**SETTINGS*#
savetitle <- "20s period 4 °C_min HR"


#General EVERYTHING IN SECOND - EVERYTHING!!!!!!!!!!!!!!!!!!!!!!
sampling <- 10 #in pts/sec
startTemp <- 20      # in °C
endTemp <- 180       # in °C
period <- 40        # in modulations/sec
periodSignal <- 40    # in sec
heatRate <- 2/60        # in °C/sec
phase <- -0.4 # in rad
ws <- period*sampling*1
loessAlpha <- 0.05

#Temperature mod:
Atemp <- 0.212 # in °C

#Reversing Heat flow:
deltaRHFPreTg <- -0.000137  # in W/(g*°C)
deltaRHFPostTg <- -0.000120  # in W/(g*°C)
StartRHFPreTg <- -0.040 # in J/(g*°C)

deltaRevCpTempPreTg <- -deltaRHFPreTg/heatRate
deltaRevCpTempPostTg <- -deltaRHFPostTg/heatRate
StartRevCpTempPreTg <- -StartRHFPreTg/heatRate

#Baseline total heat flow
deltaHFPreTg <- -0.000286 # in W/(g*°C)
deltaHFPostTg <- -0.000068 # in W/(g*°C)
StartHFTempPreTg <- -0.035 # in W/g



#Tg
locationTgTHF <- c(28.46, 39.13, 33.77) # in °C
locationTgRHF <- c(35, 45, 40) # in °C
deltaCpTg <- 0.268   # in J/(g*°C)
deltaHFTg <- -0.268*heatRate  # in W/g


#Small melting peaks
MeltEnth <- -0.04
phase_melt <- 0
locationMelt <- c(134, 154, 144)   # in °C


#Crystallisation
Crystalenth <- 0.005
locationcrystal <-c(80,100,90) # in °C


#Enthalpy recovery
locationEnthRec <-c(30,45,37.5) # in °C
EnthrecEnth <- -0.002