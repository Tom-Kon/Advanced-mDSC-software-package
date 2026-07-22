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
\Delta C_p= StartRevCpTempPostTg-FinalRevCpPreTg
$$

The challenge lies in determining the constant k. Since a glass transition is a sigmoid curve, taking its derivative results in a peak-shape. k is defined based on the magnitude of the signal of the derivative at the Tg onset and end. The derivative of the sigmoid expressed above is:

$$
\frac{dC_p(T)}{dT} = \frac{\Delta C_p k e^{-k(T-T_{g,\mathrm{midpoint}})}}
{\left(1+e^{-k(T-T_{g,\mathrm{midpoint}})}\right)^2}
$$

Which is maximal at $T=Tg midpoint$. Thus, the maximal value of $\frac{dC_p(T)}{dT}$ is $\frac{\Delta C_pk}{4}$. 

Now, we define the value $\epsilon$ as the fraction of the maximum of the derivative $\frac{\Delta C_pk}{4}$ at the start or onset of the Tg. Thus, if $\epsilon$ is 0.01 for instance, $k$ needs to be defined such that $\frac{dC_p(Tg onset)}{dT}= 0.01 \frac{dC_p(Tg midpoint)}{dT}$. 

Hence 

$$
\epsilon \frac{\Delta C_p k}{4} =
\frac{\Delta C_p k e^{-k(T_{g,\mathrm{onset}}-T_{g,\mathrm{midpoint}})}}
{\left(1+e^{-k(T_{g,\mathrm{onset}}-T_{g,\mathrm{midpoint}})}\right)^2}
$$

$$
\therefore \quad \quad
1+2e^{-k(T_{g,\mathrm{onset}}-T_{g,\mathrm{midpoint}})} + e^{-2k(T_{g,\mathrm{onset}}-T_{g,\mathrm{midpoint}})} = 4\frac{e^{-k(T_{g,\mathrm{onset}}-T_{g,\mathrm{midpoint}})}}{\epsilon}
$$

$$
\therefore \quad \quad
e^{-2k(T_{g,\mathrm{onset}}-T_{g,\mathrm{midpoint}})}
+
\left(2-\frac{4}{\epsilon}\right)
e^{-k(T_{g,\mathrm{onset}}-T_{g,\mathrm{midpoint}})}
+1
=0
$$

$$
\therefore \quad \quad
k=\frac{1}{T_{g,\mathrm{onset}}-T_{g,\mathrm{midpoint}}}\ln\left(\frac{\frac{4}{\epsilon}-2\mp\sqrt{\left(\frac{4}{\epsilon}-2\right)^2-4}}{2}\right)
$$

where the negative result for $k$ can of course be discarded. The limitation with this approach is that only the Tg onset is taken into account. Indeed, a very similar derivation can be performed using Tg endset rather than onset. In order to solve this problem, the distance between Tg onset and Tg midpoint as well between Tg endset and Tg midpoint can be assumed to be very similar. As a result, 

$$
T_{g,\mathrm{onset}}-T_{g,\mathrm{midpoint}}= \frac{T_{g,\mathrm{endset}}-T_{g,\mathrm{onset}}}{2}
$$

, and thus:

$$
k=\frac{2}{T_{g,\mathrm{endset}}-T_{g,\mathrm{onset}}}\ln\left(\frac{\frac{4}{\epsilon}-2\mp\sqrt{\left(\frac{4}{\epsilon}-2\right)^2-4}}{2}\right)
$$

$ϵ$ is hardcoded to be equal to 0.1, but the user is of course free to change this in the code if this is absolutely required. 

Melting events, crystallization events, solvent evaporation events and enthalpy recoveries are modeled through Gaussian curves and are added to the signal that was generated previously by simple addition. The melting enthalpy, peak temperature, peak endset and peak onset are all user inputs. These are the equations used to determine the shape of the Gaussians: 

$$
f(t)=\frac{\mathrm{melting\ enthalpy}}{\sqrt{2\pi}\sigma}\exp\left(-\frac{(t-\mu)^2}{2\sigma^2}\right)
$$

$$
\mu=\mathrm{time\ at\ peak\ temperature},\qquad\sigma=\frac{1}{\beta}\frac{\mathrm{peak\ endset}-\mathrm{peak\ onset}}{\sqrt{2\ln(1000)}}.
$$


The end result of adding the oscillation, the baseline, the Tg(s), and the other events is essentially the equation that was presented in the overarching theoretical background:

$$
\frac{dQ}{dt}= C_p Aω cos⁡(ωt) + C_p β + f(t,T)
$$
### Signal deconvolution
The goal is to take a rolling average to calculate the total heat flow and to extract the amplitude of the signal to calculate the reversing heat flow. The non-reversing heat flow is then easily determined based on the other two signals. 

#### Total heat flow 
The cosine transformation required to transform the list of timepoints into a modulated heat flow is not a linear transformation. In other words, even if a list of time points is equally spaced (such as 1, 2, 3, 4, 5, etc.), the cosine transform of this list might not have equally spaced values. Hence, performing a rolling average on cosine-transformed data yields another oscillating signal due to the uneven spacing of points. Hence, the points making up the modulated heat flow signal must be transformed to ensure consistent y-spacing between them.

To make sure that y-values are spaced equally, they are resampled after fully initializing the signal through linear interpolation. The approx() function is used for this in R. After this, the total heat flow is simply calculated through this equation: 

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




