# COst of MAstitis in CANadian Dairies (CoMaCAN)

* Réseau canadien de recherche sur la mammite bovine et la qualité du lait,
  Faculté de médecine vétérinaire, Université de Montréal
* PI: Simon Dufour (simon.dufour@umontreal.ca)

## Objective
* To estimate the total economic loss due to mastitis in Canada
* To quantify the additional revenue that could be generated under various
  putative udder health improvement scenarios
  
## Material & Methods
A cross-sectional study, described in [Aghamohammadi et al.
(2018)](https://www.frontiersin.org/article/10.3389/fvets.2018.00100), was
conducted to collect data on factors having an impact on mastitis costs (current
expenditures for mastitis treatment and control, and mastitis-associated output
losses).
Data were collected with the help of a questionnaire consisting of 35 open-ended
and multiple-choice questions, mailed in January 2016 to the 374 dairy producers
participating in the second phase of the Canadian National Dairy
Study ([Belage et al.,
2017](http://www.sciencedirect.com/science/article/pii/S0022030217302321)).
The 374 farms were selected to reflect the proportion of producers by province
and of DHI-participating herds according to the official records from the
provincial dairy boards (British Columbia, n = 20; Alberta, n = 20;
Saskatchewan, n = 10; Manitoba, n = 10; Ontario, n = 133; Québec, n = 121;
New-Brunswick, n = 17, Nova-Scotia, n = 18; Prince Edward Island, n = 20, and
Newfoundland; n = 5).
More details can be found in [Aghamohammadi et al.
(2018)](https://www.frontiersin.org/article/10.3389/fvets.2018.00100), along
with the mastitis economic model used, based on the framework proposed by
[Halasa et al. (2007)](https://doi.org/10.1080/01652176.2007.9695224).

Based on data collected from these 374 herds, probability distributions of the
various economic model parameters were fit by maximum likelihood
estimation ([Delignette-Muller and Dutang,
2015](https://www.jstatsoft.org/v064/i04)).
Empirical plots, descriptive statistics, as well as skewness and kurtosis
plots (i.e. Cullen and Frey graph, [Cullen and Frey,
1999](https://openlibrary.org/works/OL1978341W/Probabilistic_techniques_in_exposure_assessment))
were used to determine the best distributions for each parameter.
In order to take into account the uncertainty of the estimated values of
kurtosis and skewness from data, a nonparametric bootstrap
procedure ([Efron and Tibshirani,
1994](https://www.crcpress.com/An-Introduction-to-the-Bootstrap/Efron-Tibshirani/p/book/9780412042317))
was performed to obtain boot values of skewness and kurtosis.

The economic model described in [Aghamohammadi et al.
(2018)](https://www.frontiersin.org/article/10.3389/fvets.2018.00100) was
applied to 2014 data of DHI-participating herds (N = 8902), with the parameters
and their fitted distributions.
A total of 1000 data sets were simulated.
For each data set, a stratified random sampling without replacement was used to
fill data for herds not participating into DHI, i.e. for which no data were
available.
Median values, 2.5 and 97.5 percentiles are then provided for the various
costs across the 1000 data sets.

## Instructions
* `01-load_data.R`: load data
* `02-make_data.R`: create simulated data sets. It uses `stratified.R` to add
  stratified random samples to the data sets.
* `fit_distribution.R`: define the fitted distributions of the model parameters.
* `CoMaCAN.Rnw`: run analyses and create stat report (LaTeX and pdf files).
  Costs are computed with `compute_cost.R`.
* `plotting.R`: creates plot for paper.
