---
title: 'Advanced mDSC software package: helping people unfamiliar with programming to unravel complex mDSC data'
tags:
  - R
  - RShiny
  - Differential scanning calorimetry
  - Modulated differential scanning calorimetry
  - Quasi-isothermal modulated differential scanning calorimetry

authors:
  - name: Tom Konings
    orcid: 0000-0003-1256-6557
    affiliation: 1
    corresponding: True
  - name: Julia Bandera
    orcid: 0009-0000-1104-7232
    affiliation: 2
  - name: Guy Van den Mooter
    orcid: 0000-0001-9166-6075
    affiliation: 1

affiliations:
 - name: Drug Delivery and Disposition, KU Leuven, Department of Pharmaceutical and Pharmacological Sciences, Campus Gasthuisberg ON2, Herestraat 49 b921, 3000 Leuven, Belgium.
   index: 1
   
 - name: VIB-KU Leuven Center for Brain & Disease Research, Herestraat 49 Box 602, Leuven, 3000, Belgium; Department of Neurosciences, Leuven Brain Institute, KU Leuven, Herestraat 49 Box 602, Leuven, 3000, Belgium.
   index: 2

date: 30 June 2025
bibliography: paper.bib
---

# Summary
The advanced mDSC software package is an open-source piece of software composed of four modular R Shiny applications for the deconvolution and analysis of modulated differential scanning calorimetry (mDSC) data. Indeed, mDSC experiments generate large datasets that require computational methods to deconvolute the resulting modulated heat-flow signal. This software provides tools for the deconvolution of quasi-isothermal and non-isothermal mDSC datasets using both Fourier-based and non-Fourier-based approaches, the simulation of modulated heat-flow signals to investigate the resulting deconvoluted heat flows, and the calculation of descriptive statistics. By providing these capabilities through interactive graphical applications that facilitate figure and data export, the software makes advanced mDSC data-processing methods accessible to researchers without programming expertise and facilitates the analysis of complex thermal datasets in materials science, pharmaceutical science, and related fields. It fills a gap left by existing software, which is either proprietary, limited in scope, or requires programming expertise.

# Background
Differential scanning calorimetry (DSC) is one of the most common methods to study the thermal properties of materials. [@Knopp2016; @LeyvaPorras2019] It is of crucial importance in polymer chemistry and physics, material science, pharmaceutical science, and so forth. It allows the user to characterize material properties such as glass transitions, crystallization and melting events, solvent evaporation, degradation, or any other detectable event that involves a change in enthalpy or heat capacity. A similar but more advanced method is modulated DSC, which allows for deconvoluting different signals. Where unmodulated DSC uses a simple constant heating rate, mDSC superimposes a sinusoidal signal. Certain thermal events (such as glass transitions) can react to this faster heating rate, but others (such as most crystallisation events) can not, allowing the user to deconvolute the signal. [@Reading1993; @Rabel1999; @Royall1998; @Craig1998] The Advanced mDSC software package groups several sub-apps that help the user gain a better understanding not only of their mDSC data, but also of the technique in general. 

Despite the potential of mDSC, the technique has some limitations. Since the technique is more complex, blind interpretation of the results can be risky. A Fourier transform is generally used to deconvolute the data, but this can, in some cases, lead to artifacts in the deconvoluted signals. [@Schawe1998] Thus, the "Regular mDSC deconvolution" sub-app proposes alternatives to using a Fourier transformation to deconvolute the modulated heat flow. Furthermore, an important assumption in mDSC is the steady-state assumption, namely that the material is at complete equilibrium at any point during the heating procedure. However, this assumption is generally not met; since the material is being heated continuously and thermal events occur continuously, it is almost a given that the material is not in equilibrium. It is for this reason that the user might choose to perform a quasi-isothermal mDSC analysis, where only the sinusoidal temperature modulation is applied for extended periods of time at a fixed average temperature. [@Wunderlich2005] This results in complex analyses containing hundreds of thousands of data points. 

# Statement of need
The software presented here tackles the two problems presented above. First, the "Regular modulated DSC deconvolution" application offers different tools and methods for deconvoluting mDSC data, allowing the user to avoid performing a Fourier transform, as is shown in Figure 1. Furthermore, the "Quasi-Isothermal modulated DSC deconvolution" app helps users to analyse complex quasi-isothermal mDSC data by performing automatic segmentation and heat capacity calculations, as is shown in Figure 2. Additionally, it may be useful to predict the result of the deconvolution procedure when different sets of parameters are used. The "Modulated DSC deconvolution simulation" app helps with this, as is shown in Figure 3. It is merely a mathematical tool (as opposed to a physical model) in the sense that it requires complete knowledge of the thermal events in a material, but it can predict what the deconvoluted signals will look like. This can be used to solve complex problems but can also be used as an educational "look under the hood" to see what really happens in an mDSC deconvolution procedure. Because deconvolution of mDSC data requires substantial computational processing, software tools are essential for data analysis. The State of the field Section goes into more detail about these packages. 

Finally, TRIOS® by TA Instruments is a commonly used software package for the analysis of mDSC results. In order to speed up the analysis of data generated using TRIOS®, the "DSC descriptive statistics" app was included into the overall package as well. It simply combines several tables that can be automatically generated through TRIOS®, allowing the user to efficiently calculate averages, standard deviations, and relative standard deviations. 

![A demonstration of the regular mDSC deconvolution sub-app using ethyl cellulose. A: Reversing heat flow obtained after deconvolution of the modulated heat flow signal using Fourier analysis. B: Reversing heat flow obtained after deconvolution of the modulated heat flow using  neighbouring minima and maxima (the envelope method). The envelope method is not perfect, as it leads to some additional noise, but it does show the same event, namely the glass transition of ethyl cellulose.](minmaxenvelope.png)

![A demonstration of an application of the quasi-isothermal mDSC deconvolution sub-app applied to an ethyl cellulose sample. A: modulated heat flow prior to deconvolution. The different isothermal segments characteristic of quasi-isothermal mDSC are clearly visible. B: Result of the deconvolution procedure showing the reversing heat capacity. This temperature segment was chosen because it clearly shows the glass transition of ethyl cellulose, which occurs at ca. 128 °C.](QI_mDSC.png)

![A demonstration of the modulated DSC deconvolution simulation sub-app. A: Modulated heat flow generated based on user input. B: Overlay of the different heat flows obtained after deconvolution. It is clear that the deconvolution procedure performs well, since overlapping signals (the enthalpy recovery and the glass transition) are properly deconvoluted in the non-reversing and reversing heat flows respectively. It was also verified that user input enthalpies precisely match the enthalpies of the deconvoluted signals, and that these events occur precisely at user input values.](mDSCsim_overlay.png)


# State of the field
Proprietary software is available (TRIOS® by TA instruments, STARe® by Mettler-Toledo, Pyris® by PerkinElmer, and so forth), but these packages are not open source and a license is required to use them. Moreover, they do not necessarily offer the same capabilities as the Advanced mDSC software package. Indeed, quasi-isothermal mDSC data analysis might not be as automated in proprietary software as in the "Quasi-Isothermal modulated DSC deconvolution" sub-app, as it sometimes requires manual data segmentation. Instead, the "Quasi-Isothermal modulated DSC deconvolution" sub-app detects isothermal segments automatically and performs the analysis based on this, without requiring manual input beyond basic experiment parameters. Furthermore, the "DSC descriptive statistics" app was specifically built to cover a shortcoming of the TRIOS® software, as it did not offer a straightforward way of rapidly and at least semi-automatically generating descriptive statistics of data pertaining to thermal events. 

A larger gap is filled by the regular mDSC deconvolution and the "Modulated DSC deconvolution simulation" sub-apps. Indeed, we are unaware of any published software, open-source or otherwise, that deconvolutes the modulated heat flow as is done by the regular mDSC deconvolution sub-app, and are similarly unaware of any software that offers any of the capabilities of the "Modulated DSC deconvolution simulation" sub-app.

Thus, the software package presented here not only has the advantage of being open source, it also has some capabilities that even licensed software does not have. It is worth mentioning that a piece of open source software called pyDSC [@pyDSC] exists. However, this package focuses on integrating and correcting mDSC data instead of deconvoluting mDSC data, and thus the scope is not the same as the Advanced mDSC software package. 

# Software design
The software was designed to be accessible to users without programming experience while maintaining a modular structure that facilitates future development. A graphical user interface was implemented using the Shiny [@shiny] framework, enabling users to upload data, perform analyses, visualize results, and export results through a browser-based workflow. Documentation is provided both within the application and in the GitHub repository, with user guidance and theoretical background organised according to the four available modules.

The software comprises four functionally independent sub-applications that are connected through a navigation system implemented using the shiny.router package (Figure 4). [@shinyRouter] Although presented as a single software package, each module maintains its own analysis workflow, dependencies, functions, and export routines. The corresponding codebase is organised such that user interface components, reactive server logic, and computational routines are separated into dedicated scripts. Data processing algorithms, plotting functions, and numerical methods are implemented in helper files that are called by the main application scripts. This organisation improves maintainability and facilitates the incorporation of additional analysis functionality without requiring major modifications to the user interface. Although the modules are functionally independent, they are packaged together to allow distribution as a single Electron executable. This provides users with access to all functionalities through a unified interface rather than requiring multiple task-specific applications.

![Schematic of the software architecture. The different sub-apps are functionally independent, with their own libraries, functions, data analysis workflows, and export handlers. Visually, they are connected via a shiny.router navigation menu. The whole software package is either executed as an Electron app, or via R.:architecture](architecture.png)

The software was developed in R, an open-source language with a large user community and extensive package ecosystem. The computational requirements of the implemented analyses are modest, making R a suitable platform while also facilitating community contributions. Key dependencies include shiny [@shiny] for the web interface, shiny.router [@shinyRouter] for navigation between modules, ggplot2 [@ggplot2] for data visualization, and plotly [@plotly] for interactive graphics.

All sub-apps except the DSC descriptive statistics sub-app focus on the deconvolution of modulated heat-flow data obtained from mDSC experiments or simulations. Supported data sources include conventional mDSC experiments, quasi-isothermal mDSC experiments, and simulated modulated heat-flow signals. Fourier analysis is the primary method used for deconvolution, although the regular modulated DSC deconvolution sub-app additionally implements an alternative approach based on neighbouring minima and maxima of the modulated signal. A complete mathematical description of the implemented algorithms is provided in the software documentation. Nevertheless, all deconvolution modules share the same fundamental quantities, namely the total, reversing, and non-reversing heat flows. These quantities are calculated according to the following equations:

$$THF = \langle MHF \rangle = \langle \frac{dQ}{dt} \rangle, $$ 

$$RHF = -\beta \frac{A_{MHF}}{\frac{2 \pi}{T}A_{Temp}}, $$ 

$$NRHF = THF-RHF, $$

where $\frac{dQ}{dt}$ is the modulated heat flow, $\beta$ is the underlying heating rate, $T$ is the period of the temperature modulation, $A_{MHF}$​ is the amplitude of the modulated heat-flow signal, and $A_{Temp}$​ is the amplitude of the temperature modulation.

# Research impact statement
The software was originally developed to investigate a reproducible but apparently physically impossible exothermic peak observed in the reversing heat flow of a polymer during modulated differential scanning calorimetry (mDSC) analysis. This phenomenon was consistently observed across multiple experiments, instruments, thermal histories, and experimental conditions, suggesting that it could not be dismissed as a measurement artifact. This probed further investigation into the signal through the use of the software. Indeed, three applications included in the package played a central role in this investigation. The regular modulated DSC deconvolution sub-app demonstrated that the exothermic peak was not merely an artifact arising from the Fourier-transform-based deconvolution procedure normally used in mDSC. The quasi-isothermal deconvolution app enabled investigation of the material under quasi-isothermal conditions in order to verify whether a truly reversible signal did occur, while the simulation app was used to explore which underlying thermal events could generate the experimentally observed response. The insights obtained using the software form the basis of a manuscript being prepared for submission to Macromolecules. In fact, further research into the phenomenon would have been impossible without the software package.

Beyond this specific application, the methodology has proven useful for investigating broader questions in mDSC analysis. For example, the simulation app has been used to quantify the well-known frequency effect, whereby differences in glass-transition midpoint temperatures between the total and reversing heat flows lead to artifacts in the non-reversing heat flow. The software therefore not only enabled the investigation of a previously unexplained thermal phenomenon, but also provides a platform for studying fundamental aspects of mDSC data interpretation across a wide range of materials and applications. The software is useful for all quasi-isothermal mDSC data analysis, any time where the exclusion of artifacts caused by the Fourier transform is necessary, and any time where it is necessary to study the effect of a particular signal occurring in the modulated heat flow on the deconvoluted signals. The capability of dealing with some descriptive statistics is an additional feature that can also speed up data analysis when commercial TRIOS® software is sufficient for the deconvolution procedure.

# AI usage disclosure
No generative AI was used in the design, the mathematics (which are based on literature), the testing, and most of the code writing for the app. Some generative AI was used to speed up the writing of small blocks of code, to help find relevant documentation, and to improve writing quality in the manuscript. 

# Acknowledgments
The authors would like to acknowledge the help from Els Verdonck (TA Instruments) and Guy Van Assche (Vrije Universiteit Brussel) for providing help with the theoretical background of the software. Additionally, this research was funded through an FWO grant (1SH0S24N). 

# References
