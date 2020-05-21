-   [COVID-19 model](#covid-19-model)
-   [Repository structure](#repository-structure)
-   [Model Calibration](#model-calibration)
    -   [Studying the effects of different population responses to
        government
        actions.](#studying-the-effects-of-different-population-responses-to-government-actions.)
    -   [COVID-19 epidemic containment
        strategies.](#covid-19-epidemic-containment-strategies.)
-   [References](#references)

COVID-19 model
==============

<img src="./Figures/Model.png" alt="\label{fig:Pertussis_PN}"  />
<p class="caption">
</p>

SEIRS model and surveillance data on Piedmont region.: (A) The
age-dependent SEIRS model. (B) The total infected cases distributed in
the counties of the Piedmont region. (C) Distribution of quarantine
infected (Iq), hospitalized infected (Ih) and deaths (D) from February
24th to May 2nd. The control strategies are reported below the bar
graph.

Repository structure
====================

Model Calibration
=================

The calibration phase was performed to fit the model outcomes with the
surveillance Piedmont infection and death data (from February 24 to May
2nd) using squared error estimator via trajectory matching. Hence, a
global optimization algorithm, based on ~ Yang Xiang et al. (2012), was
exploited to estimate 13 model parameters characterized by a high
uncertainty due to their difficulty of being empirically measured.

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

Studying the effects of different population responses to government actions.
-----------------------------------------------------------------------------

<img src="./Figures/DiffStrengths-1.png" alt="\label{fig:AlphaVar}"  />
<p class="caption">
</p>

COVID-19 epidemic containment strategies.
-----------------------------------------

The daily evolution of infected individuals is shown varying on the
columns the the efficacy of individual-level measures and on the rows
the efficacy of community surveillance.

<img src="./Figures/Plot3_withLine-1.png" alt="\label{fig:Boxplot}"  />
<p class="caption">
</p>

Specifically, in Figure~ we show the daily forecasts of the number of
infected individuals with the efficacy of individual-level measures
ranging from 0% to 60% on the columns (increasing by steps of 20%) and,
on the rows, increasing capability (from 0% to 30%, by 10% steps) of
identifying otherwise undetected infected individuals. These results are
obtained as median value of 5000 traces for each scenario obtained from
the stochastic simulation.

References
==========

Yang Xiang, Sylvain Gubian, Brian Suomela, and Julia Hoeng. 2012.
“Generalized Simulated Annealing for Efficient Global Optimization: The
GenSA Package for R.” *The R Journal*. <http://journal.r-project.org/>.
