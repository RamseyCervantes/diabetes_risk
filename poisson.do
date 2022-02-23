// Install coeff plot
ssc install coefplot, replace
// Install graph packages
ssc install grstyle, replace
ssc install palettes, replace
ssc install colrspace
ssc install blindschemes
// Set graph settings
grstyle clear
set scheme s2color
grstyle init

use https://www.stata-press.com/data/r17/heartvalve.dta, clear
describe
poisson deaths i.valve i.age
estimates store pdeaths
margins valve, post
estimates store pvalve
coefplot(pvalve, label(deaths by valve)), bylabel(deaths by valve) ///
title("# Of Deaths By Valve") ytitle("Valve Type")