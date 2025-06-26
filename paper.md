---
title: 'Advanced mDSC software package: helping people unfamiliar with programming with unraveling complex mDSC methods'
tags:
  - R
  - RShiny
  - Differential scanning calorimetry
  - Modulated differential scanning calorimetry
  - Quasi-isothermal modulated differential scanning calorimetry

authors:
  - name: Tom Konings
    orcid: 0000-0003-1256-6557
    equal-contrib: true
    affiliation: 1
    corresponding: true # (This is how to denote the corresponding author)
  - name: Julia Bandera
    orcid: 0009-0000-1104-7232
    equal-contrib: false
    affiliation: 2

affiliations:
 - name: Tom Konings, Drug Delivery and Disposition, KU Leuven, Department of Pharmaceutical and Pharmacological Sciences, Campus Gasthuisberg ON2, Herestraat 49 b921, 3000 Leuven, Belgium.
   index: 1
 - name: Julia Bandera, XXX, Belgium
   index: 2

date: xx July 2025
bibliography: paper.bib
---

# Summary
The Advanced mDSC software package is a collection of four user-friendly RShiny apps that helps users without programming knowledge to decode their modulated differntial scanning calorimetry (mDSC) data. Differential scanning calorimetry (DSC) is a thermal analysis technique that is commonly used in pharmaceutical and material science to detect the different transitions and thermal events undergone by a material when it is heated. mDSC is an improved version of DSC in the sense that it can be used to deconvolute different events, but it also presents additional complexity and technical difficulty. Moreover, the data generated using this method must be analyzed by computational methods, since it generally involves datasets with hundreds of thousands of datapoints.  

# Statement of need 
Differential scanning calorimetry (DSC) is one of the most common methods to study the thermal properties of materials. [@Knopp2016; @LeyvaPorras2019] It is of crucial importance in polymer chemistry and physics, material science, pharmaceutical science, and so forth. It allows the user to characterize material properties such as glass transitions, crystallization and melting events, solvent evaporation, degradation, or any other detectable event that involves a change in enthalpy or heat capacity. A significant upgrade with respect to unmodulated DSC is modulated DSC, which allows for deconvoluting different signals. Where unmodulated DSC uses a simple constant heating rate, mDSC superimposes a sinusoidal signal. Certain thermal events (such as glass transitions) can react to this faster heating rate, but others (such as most crystallisation events) can not, allowing the user to deconvolute the signal. [@Reading1993; @Rabel1999; @Royall1998; @Craig1998] The Advanced mDSC software package groups several sub-apps that help the user gain a better understanding not only of their mDSC data, but also of the technique in general. 

The problem with mDSC is that since the technique is more complex, blind interpretation of the results can be risky. A Fourier transform is generally used to devonvolute the data, but this can, in some cases, lead to artifacts in the deconvoluted signals. [@Thomas2005] This is where the "Regular modulated DSC deconvolution" application comes in. It offers different tools and methods for deconvoluting the data, allowing the user to avoid performing a Fourier transform. 

Additionally, it may be useful to predict the result of the deconvolution procedure when different sets of parameters are used. The "Modulated DSC deconvolution simulation" app helps with this. It is merely a mathematical tool (as opposed to a physical model) in the sense that it requires complete knowledge of the thermal events in a material, but it can predict what the deconvoluted signals will look like. This can be used to solve complex problems, but can also be used as an educational "look under the hood" to see what really happens in an mDSC deconvolution procedure. 

An important assumption in mDSC is the steady-state assumption, namely that the material is at complete equilibrium at any point during the heating procedure. However, this assumption is generally not met; since the material is being heated continuously and thermal events occur continuously, it is almost a given that the material is not in equilibrium. It is for this reason that the user might choose to perform a quasi-isothermal mDSC analysis, where only the sinusoidal temperature modulation is applied for extended periods of time at a fixed average temperature. [@2005] This results in complex analyses containing hundreds of thousands of data points, but not all commercial mDSC software is able to analyze this type of data. This is where the "Quasi-Isothermal modulated DSC deconvolution" app offers a solution by offering a user-friendly alternative. 

Finally, TRIOS® by TA Instruments is a commonly used software package for the analysis of mDSC results. In order to speed up the analysis of data generated using TRIOS®, the "DSC descriptive statistics" app was included into the overal package as well. It simply combines several tables in an interactive manner, allowing the user to very quickly calculate averages, standard deviations, and relative standard deviations. 

All aforementioned applications are currently being used to fully understand very elusive thermal behaviour of a polymer (more precisely, a polyoxazoline). All mDSC runs of this material have consistently produced an impossible peak in the resulting thermograms, but the development of the different applications (especially the first three mentioned above) has allowed us to start to understand its otherwise unexplainable and unique behaviour. This is why the software package was developed; to allow users to quickly and efficiently analyze their mDSC data without any coding knowledge, all while allowing for exporting results in .xlsx format and diffent image formats. This software package is, to the best of our knowledge, unique in the sense that it is completely open source, user-friendly, and combines powerful calculations that absolutely require code to excute. 

# Mathematics 
The detailed mathematics used within the software package are described in the documentation, as it would be too extensive to give a full overview here. However, three equations are crucial for all applications and are thus mentioned here. When deconvoluting an mDSC signal (also called the modulated heat flow, MHF), this normally results in a total heat flow (THF), a reversing heat flow (RHF), and a non-reversing heat flow (NRHF). The equations used to calculate these three quantities are, respectively, 

$$THF = \langle MHF \rangle  = \langle \frac{dQ}{dt} \rangle, $$
$$RHF =  -\beta \frac{A_{MHF}}{\frac{2 \pi}{T}A_{Temp}}, $$
$$NRHF = THF-RHF, $$

where $\frac{dQ}{dt}$ is the modulated heat flow, $\beta$ is the underlying (constant) heating rate, $T$ is the period of the temperature modulation, and $A_{Temp}$ is the amplitude of the temperature modulation.

# References

# Acknowledgments
The authors would like to acknowledge the help from Els Verdonck (TA Instruments) and Guy Van Assche (Vrije Universiteit Brussel) for providing help with the theoretical background of the software. Additionally, this research was funded through an FWO grant (1SH0S24N). 





