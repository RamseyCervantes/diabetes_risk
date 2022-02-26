// Install necessary packages
ssc install coefplot, replace
ssc install blindschemes, replace
// Set graph settings
set scheme plotplainblind
// Use stata Heart valve replacement data designed for poisson analysis
use https://www.stata-press.com/data/r17/heartvalve.dta, clear
describe
*Remove existing csv files
capture noisily: rm "Table7.csv"
estimates drop _all
//run poisson on number of deaths based on valve type and age
poisson deaths i.valve i.age
estimates store pdeaths
margins valve, post
estimates store pvalve
coefplot(pvalve, label(deaths by valve)), bylabel(deaths by valve) ///
title("# Of Deaths By Valve") ytitle("Valve Type")
*Export result to CSV 
esttab pdeaths ///
	using Table7.csv, cells(b(fmt(2) star) & ci(par("[" ";" "]"))) ///
	mtitle("pdeaths") append label stats(N_sub)