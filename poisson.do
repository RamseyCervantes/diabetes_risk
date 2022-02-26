// Install necessary packages
ssc install coefplot, replace
ssc install blindschemes, replace
// Set graph settings
set scheme plotplainblind

use https://www.stata-press.com/data/r17/heartvalve.dta, clear
describe
poisson deaths i.valve i.age
estimates store pdeaths
margins valve, post
estimates store pvalve
coefplot(pvalve, label(deaths by valve)), bylabel(deaths by valve) ///
title("# Of Deaths By Valve") ytitle("Valve Type")