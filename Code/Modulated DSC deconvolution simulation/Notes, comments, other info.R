#NOTE: one of the main issues is the requirements of a moving window and how
#those requirements depend on what you're actually trying to achieve. 
#Basically, for a simple time-average, you want equally-spaced y-values. 
#However, for a FT, you want equally-spaced time-values. That's why THF and 
#RHF calculations use different inputs. 