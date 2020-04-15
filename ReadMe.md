---
title: "EpiMod: COVID-19"
author: "Beccuti Marco, Castagno Paolo, Pernice Simone"
output: 
  md_document:
    toc: true
    toc_depth: 3
bibliography: biblio.bib  
vignette: >
  %\VignetteIndexEntry{my-vignette}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
---
  
```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  
  )
```
  
  
  
# COVID-19 model


# Starting

# Model Calibration

The calibration phase was performed to fit the model outcomes with  the surveillance  Piedmont infection and death data (from  February 24\textsuperscript{st}  to April 1\textsuperscript{st})  using  squared  error  estimator  via  trajectory matching.  Hence, a global optimization algorithm, based on  \emph{generalized simulated annealing method}~\cite{GenSA}, was exploited to estimate 23 model parameters characterized by a high uncertainty due to their difficulty  of  being  empirically  measured.   
In particular, 15 parameters represent the infection rates (i.e., $\beta_{i,j}$), three parameters reflect the governmental action strength and the related population response at time epoch $t$ (i.e., $\alpha(t)$ with $t\in \{\ \text{February 25\textsuperscript{th}},\text{March 8\textsuperscript{th}},  \text{March 21\textsuperscript{st}}\}$),
%$\alpha(\text{February 25\textsuperscript{th}})=0.211, \, \alpha(\text{March 8\textsuperscript{th}})=0.549$ and $\alpha(\text{March %21\textsuperscript{st}})=0.738$)}, one parameter describes the intensity of the population response (i.e., $k$), and the remainders four parameters represent the  death rate for the hospitalized patients (i.e., $\sigma_i$, fixing $\sigma_1=0$).
 
Therefore, the model calibration was carried out considering the proportion between undetected and detected infected individuals (i.e., given by the sum of the quarantined and hospitalized infected individuals)  to be  one-to-one on average as reported in \cite{DiamondPrincess} (refer to Section S2 in the Supplementary Material for the definition of the  distribution of infected categories by age).
According to this scenario and to \cite{IstatPiedmont}, we assumed that the initial system state is:  $S_1=733130, \, S_2=881208, \, S_3=1340552, \, S_4=1038395, \, S_5=363121, Iq_2=1, \, Iq_3=1,\,Iq_4=1, \, Iu_2=1, \, Iu_3=1, \, Iu_4=1$, while all the other compartments are set to zero.
Moreover, the availability of social interaction for the infectious sub-classes is fixed to $\eta_u=1$, $\eta_q=0.3$ and $\eta_h=0.1$. 
Finally,  the rates at which an exposed individual with age $i$ becomes  an infected individual in sub-class $\nu$ (i.e., $\lambda_{i,\nu}$) are derived by the surveillance data as described in supplementary material.

<object data="http://yoursite.com/the.pdf" type="application/pdf" width="700px" height="700px">
    <embed src="http://yoursite.com/the.pdf">
        <p>This browser does not support PDFs. Please download the PDF to view it: <a href="http://yoursite.com/the.pdf">Download PDF</a>.</p>
    </embed>
</object>


# References
  
<div id="refs"></div>
  