-   [COVID-19 model](#covid-19-model)
-   [Model Calibration](#model-calibration)
    -   [Stochastic simulation](#stochastic-simulation)
    -   [Studying the effects of different percentages of asyntomatic
        individuals.](#studying-the-effects-of-different-percentages-of-asyntomatic-individuals.)
-   [References](#references)

COVID-19 model
==============

<img src="./Figures/SEIR-Model.png" alt="\label{fig:Pertussis_PN}"  />
<p class="caption">
</p>

SEIR model and surveillance data on Piedmont region.: (A) The
age-dependent SEIR model. (B) The total infected cases distributed in
the counties of the Piedmont region. (C) Distribution of quarantine
infected (Iq), hospitalized infected (Ih) and deaths (D) from February
24th to March 28th. The control strategies are reported below the bar
graph. \# Starting

Model Calibration
=================

The calibration phase was performed to fit the model outcomes with the
surveillance Piedmont infection and death data (from February 24 to
April 1) using squared error estimator via trajectory matching. Hence, a
global optimization algorithm, based on ~, was exploited to estimate 23
model parameters characterized by a high uncertainty due to their
difficulty of being empirically measured.  
In particular, 15 parameters represent the infection rates (i.e.,
*β*<sub>*i*, *j*</sub>), three parameters reflect the governmental
action strength and the related population response at time epoch *t*
(i.e., *α*(*t*) with
$t\\in \\{\\ \\text{February 25\\textsuperscript{th}},\\text{March 8\\textsuperscript{th}}, \\text{March 21\\textsuperscript{st}}\\}$),
%$\\alpha(\\text{February 25\\textsuperscript{th}})=0.211, \\, \\alpha(\\text{March 8\\textsuperscript{th}})=0.549$
and $\\alpha(\\text{March %21\\textsuperscript{st}})=0.738$)}, one
parameter describes the intensity of the population response (i.e.,
*k*), and the remainders four parameters represent the death rate for
the hospitalized patients (i.e., *σ*<sub>*i*</sub>, fixing
*σ*<sub>1</sub> = 0).

Therefore, the model calibration was carried out considering the
proportion between undetected and detected infected individuals (i.e.,
given by the sum of the quarantined and hospitalized infected
individuals) to be one-to-one on average as reported in (refer to
Section S2 in the Supplementary Material for the definition of the
distribution of infected categories by age). According to this scenario
and to , we assumed that the initial system state is:
*S*<sub>1</sub> = 733130, *S*<sub>2</sub> = 881208, *S*<sub>3</sub> = 1340552, *S*<sub>4</sub> = 1038395, *S*<sub>5</sub> = 363121, *I**q*<sub>2</sub> = 1, *I**q*<sub>3</sub> = 1, *I**q*<sub>4</sub> = 1, *I**u*<sub>2</sub> = 1, *I**u*<sub>3</sub> = 1, *I**u*<sub>4</sub> = 1,
while all the other compartments are set to zero. Moreover, the
availability of social interaction for the infectious sub-classes is
fixed to *η*<sub>*u*</sub> = 1, *η*<sub>*q*</sub> = 0.3 and
*η*<sub>*h*</sub> = 0.1. Finally, the rates at which an exposed
individual with age *i* becomes an infected individual in sub-class *ν*
(i.e., *λ*<sub>*i*, *ν*</sub>) are derived by the surveillance data as
described in supplementary material.

<div class="figure" style="text-align: center">
<img src="./Figures/Comulatives-1.png" alt="\"  />
<p class="caption">
&lt;/p&gt;
</div>
<div class="figure" style="text-align: center">
<img src="./Figures/InfectsHistALL-1.png" alt="\"  />
<p class="caption">
&lt;/p&gt;
</div>

### Stochastic simulation

<img src="./Figures/StochMedianDiff-1.png" alt="\label{fig:DailyInfect}"  />
<p class="caption">
</p>

The daily evolution of infected individuals computed by the stochastic
simulation. The stack bars report the undetected infected individuals
(orange), the quarantine infected individuals(light blue), and
hospitalized infected (blue). The red line outlines the trend of the
infected cases from surveillance data. The surveillance data contains
measurements error for two days, causing a negative infect count for
such days. \#\# Studying the effects of different population responses
to government actions.

<img src="./Figures/FutureComulativeInfects-1.png" alt="\label{fig:AlphaVar}"  />
<p class="caption">
</p>

One-month forecast of the cumulative infected cases
($\\sum\_{i=1}^5 Iq\_{i}$ + *I**h*<sub>*i*</sub>) obtained by
deterministic simulation. The traces were divided in four ranges based
on the $\\alpha(\\text{March 21\\textsuperscript{st}})$ values. The blue
trace corresponds to the cumulative evolution of the infected cases
obtained considering the three *α* estimated by the surveillance data
collected.

<img src="./Figures/StochFutComDistribution-1.png" alt="\label{fig:Stoch2} "  />
<p class="caption">
</p>

Density distribution at May 1 of the infected cases considering three
values of $\\alpha(\\text{March 21\\textsuperscript{st}})$. The green
lines represent their median values.

Studying the effects of different percentages of asyntomatic individuals.
-------------------------------------------------------------------------

<img src="./Figures/ViolinPlot-1.png" alt="\label{fig:Boxplot}"  />
<p class="caption">
</p>

The number of detected cases from February, 21$\\textsuperscript{st}$ to
May, 1$\\textsuperscript{st}$. In each time point, three boxes report
the infected cases distribution for the three scenario considered:
one-to-one, one-to-one-half and one-to-ten scenarios colored in yellow,
green and purple, respectively.

References
==========
