# Tutorial for Modulated DSC deconvolution simulation 
## Function
The mDSC simulation app is different when compared to the others in the sense that it does not require input documents. It does however require manual input of thermal events that occur in the sample. Its goal is also different. Where the other apps are meant to streamline data analysis, this software can be used to gain a better understanding of the sample and of the effect of modifying certain parameters. For example, once can input the details of where a melting peak occurs and study how using different mDSC parameters affects the shape of the melting peak by running several simulations. 

It must be said that this app is not a physical simulation. It is strictly a mathematical tool, that generated a modulated heat flow and deconvolutes it using a Fourier transform.

## Input
The exact input required depends on the events to be modeled. All the required input is stated on the respective tab and should require no further explanation. 

## Mathematical and theoretical background

### Signal generation

First, the modulated heat flow is generated as an oscillating sine wave based on the heat capacity of the sample: 

$$
\frac{dQ}{dt}=C_p Aω cos⁡(ωt),
$$

where t is a data frame containing a sequence of time points. A baseline $(C_pβ)$ is also added to it through a simple addition. 

Following this, the glass transition is modeled through a sigmoid curve. FinalRevCpPreTg, StartRevCpTempPostTg, FinalRevCpPreTg, Tg onset, Tg endset and Tg midpoint are user inputs.  Here, the following equation is used: 

$$
C_p(T)= FinalRevCpPreTg+ \frac{\Delta C_p}{1+e^{-k(T-T_g midpoint)}} 
$$

$$
\Delta C_p= T_g endset- T_g onset  \quad \quad \text{and} \quad \quad   k=1
$$

Melting events, crystallization events, solvent evaporation events and enthalpy recoveries are modeled through Gaussian curves and are added to the signal that was generated previously by simple addition. The melting enthalpy, peak temperature, peak endset and peak onset are all user inputs. These are the equations used to determine the shape of the Gaussians: 

$$
f(T)=\frac{melting enthalpy}{\sqrt{2π σ}} e^{-(\frac{(T-µ)^2}{2σ^2})}  
$$

$$
µ=peak temperature  \quad \quad \text{and} \quad \quad σ= \frac{peak endset-peak onset}{\sqrt(2log⁡(1000))}.
$$


The end result of adding the oscillation, the baseline, the Tg(s), and the other events is essentially the equation that was presented in the overarching theoretical background:

$$
\frac{dQ}{dt}= C_p Aω cos⁡(ωt) + C_p β + f(t,T)
$$

### Signal deconvolution
The goal is to take a rolling average to calculate the total heat flow and to extract the amplitude of the signal to calculate the reversing heat flow. The non-reversing heat flow is then easily determined based on the other two signals. 

#### Total heat flow 
The cosine transformation required to transform the list of timepoints into a modulated heat flow is not a linear transformation. In other words, even if a list of time points is equally spaced (such as 1, 2, 3, 4, 5, etc.), the cosine transform of this list might not have equally spaced values. Hence, performing a rolling average on cosine-transformed data yields another oscillating signal due to the uneven spacing of points. Hence, the points making up the modulated heat flow signal must be transformed to ensure consistent y-spacing between them.

To make sure that y-values are spaced equally, they are resamples after fully initializing the signal through linear interpolation. The approx() function is used for this in R. After this, the total heat flow is simply calculated through this equation: 

$$
THF= 〈\frac{dQ}{dt}〉.
$$

#### Reversing heat flow 
The reversing heat flow is easy to calculate because this signal does have periodicity since it is generated mathematically in this case. It is calculated using 

$$
RHF= -β \frac{A_{MHF}}{\frac{2π  A_{temp}}{T}},
$$

where $A_{MHF}$ is determined using a fast Fourier transform (FFT). In short, the signal is transformed using an FFT, and the y-value of the frequency bin corresponding to the user input frequency is extracted. This signal is multiplied by two to take into account symmetrical negative frequencies, and is then normalized by dividing by the number of points $n$. 


#### Non-reversing heat flow
The NRHF is computed through: 

$$
NRHF=THF-RHF.
$$

## 	Details on how the software works

1. First, a vector of timepoints is generated. Its length and interval depend on the user-input sampling rate, heat rate, and start and end temperatures. 
2. Based on the list of timepoints, a vector of modulated temperatures is generated. 
3. The vector with the timepoints is then used to generate the initial modulated heat   flow: 
$$
\frac{dQ}{dt}= C_p Aω cos⁡(ωt) + C_p β 
$$

4. The $f(t,T)$ term, which is still missing from the equation above, is then added progressively. For instance, if there is a melting event between temperatures 1 and 2 with a certain melting enthalpy, a Gaussian centered on the average temperature is generated and added to $\frac{dQ}{dt}$. 
5. Point 4 is repeated for all additional signals. 
6. The deconvolution procedure is carried out as detailed in the previous section. 
7. Plotly is used to plot the results. 




