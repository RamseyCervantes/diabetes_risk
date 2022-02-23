// Make, delete, and navigate directories
pwd
//mkdir Stata
cd Stata
//mkdir "Do Files"
mkdir Delete
cd Delete
cd ..
rmdir Delete
cd "Do Files"
ssc install estout, replace
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
/*
grstyle set plain, box
grstyle color background white
grstyle set color Set1
grstyle yesno draw_major_hgrid yes
grstyle yesno draw_major_ygrid yes
grstyle color major_grid gs8
grstyle linepattern major_grid dot
grstyle set legend 4, box
grstyle color ci_area gs12%50
*/

// Use survey data from National Health and Nutrition Examination Survey
use https://www.stata-press.com/data/r17/nhanes2.dta, clear
describe
svyset psu [pweight = finalwgt], strata(strata)
//global macros
global personstats "c.age c.weight c.height"
global demograph "i.race i.sex"
global cov1 "c.age c.weight c.height"
global cov2 "i.race $cov1"

forvalues i =1/2{
	svy: logit diabetes ${cov`i'}
}
foreach out in diabetes heartatk{
	forvalues i = 1/2{
		forvalues k = 1/2{			
		svy, subpop(if sex==`i'): logit `out' ${cov`k'}
		estimates store m`k'`out'_s`i'
		}
		margins i.race, subpop(if sex==`i') post
		estimates store ma2`out'_s`i'
		marginsplot, title("sex=`i'") name(g2`out'_s`i', replace)
	}
}
graph combine g2diabetes_s1 g2diabetes_s2

//learn about data for black people
corr diabetes $personinfo black
foreach stats in $personstats{
	sum `stats' if black==1
}
//chances of diabetes for older black population
svy: tabulate diabetes black, column percent
svy: logit diabetes c.age c.weight c.height i.black
estimates store mldiabetes
margins, at (age=(50(10)80) weight=(30(30)180) black=1) post
estimates store mdiabtes
marginsplot

//Using loop to get regression for diastolic blood pressure difference between gender
forvalues x = 1/2{		
	svy, subpop(if sex==`x'): regress bpdiast c.age##c.bmi
	estimates store mbpdiast_s`x'
	margins, subpop(if sex==`x') post
	estimates store ma3bpdiast_s`x'
	marginsplot, recast(bar) title(sex=`x') name(g2bpdiast_s`x', replace)

}
graph combine g2bpdiast_s1 g2bpdiast_s2, ycommon


//Standard regression to find diastolic blood pressure difference between gender
svy: regress bpdiast c.age##c.bmi i.sex
estimates store mlbpdiast
margins, at (age=(20(10)80) sex=1) at (age=(20(10)80) sex=2) post
estimates store mbpdiast
marginsplot, title("Blood Pressure Between Gender") ytitle("pr(blood pressure diastolic)") legend(order(1 "sex=Male" 2 "sex=Female"))
// coeff
//set scheme plotplainblind
set scheme s2color
coefplot (ma2diabetes_s1, xlabel(100 "male diabetes", add)) (ma2diabetes_s2, label(female diabetes)), bylabel(Risk of Diabetes) || ///
(ma2heartatk_s1, label(male heart)) (ma2heartatk_s2, label(female heart)), bylabel(Risk of Heart Attack) drop(˙cons) legend(order(1 "Male" 3 "Female") pos(6) col(2)) xlabel(0(.02).1) ytitle("Race/Ethnicity") ///
subtitle(, size(vlarge) margin(medium) justification(left) ///
color(white) bcolor(black) bmargin(top_bottom))
// multinomial logit - systolic blood pressure between 120-140 3 bins
/*
quietly sum bpsystol
gen systolbin=.
replace systolbin=1 if  bpsystol<=120
replace systolbin=2 if  bpsystol>120 & bpsystol<=140
replace systolbin=3 if  bpsystol>140 & bpsystol<=`r(max)'
sum systolbin
sum bpsystol

label define systolicbp 1 "up to 120" 2 "]120-140)" 3 "]140+"
label value systolbin systolicbp
*/
egen systolbin = cut(bpsystol), at(65,121,141,301) label
egen systolbin2 = cut(bpsystol), group(3) label
label define systolicbp 0 "Normal: up to 120" 1 "Elevated: ]120-140)" 2 "High: ]140+"
label values systolbin systolicbp
tab systolbin
tabstat bpsystol, by(systolbin) stats (min max)
svy: mlogit systolbin c.age##c.bmi i.sex
estimates store mlbpsystol
margins, at (age=(20(10)80)) post
//set scheme plotplainblind
set scheme s2color
marginsplot, title("Age Blood Pressure, Predective Margins") ytitle("pr(blood pressure systolic)") legend(pos(6) col(3) order(4 "up to 120" 5 "]120-140)" 6 "]140+")) xsize(7) ysize(6)
//mlogit vs ologit on blood pressure between sex
estimates restore mlbpsystol
margins, at (sex=1) post
estimates store mbpmale
estimates restore mlbpsystol
margins, at (sex=2) post
estimates store mbpfemale
svy:ologit systolbin c.age##c.bmi sex
estimates store olbpsystol
margins, at (sex=1) post
estimates store obpmale
estimates restore olbpsystol
margins, at (sex=2) post
estimates store obpfemale
set scheme s2color
coefplot (mbpmale, label(male blood pressure)), bylabel(mlogit Male) || ///
 (mbpfemale, label(female blood pressure)), bylabel(mlogit Female) || ///
(obpmale, label(male blood pressure ordered)), bylabel(ologit Male) || ///
 (obpfemale, label(female blood pressure ordered)), bylabel(ologit Female) ytitle("Blood Pressure") ///
 coeflabels(1._predict = "Normal: up to 120" 2._predict = "Elevated: ]120-140)" 3._predict = "High: ]140+")
//mlogit vs ologit on health status between genders
svy: mlogit hlthstat i.sex c.age##c.bmi
estimates store mlhlth
margins, at (sex=1) post
estimates store mhlthmale
estimates restore mlhlth
margins, at (sex=2) post
estimates store mhlthfemale
svy: ologit hlthstat i.sex c.age##c.bmi
estimates store olhlth
margins, at (sex=1) post
estimates store ohlthmale
estimates restore olhlth
margins, at (sex=2) post
estimates store ohlthfemale
set scheme s2color
coefplot (mhlthmale, label(male health status)), bylabel(mlogit Male) || ///
 (mhlthfemale, label(female health status)), bylabel(mlogit Female) || ///
(ohlthmale, label(male health ordered)), bylabel(ologit Male) || ///
 (ohlthfemale, label(female health ordered)), bylabel(ologit Female) ytitle("Health Status") coeflabels(1._predict = "Excellent" 2._predict = "Very Good" 3._predict = "Good" 4._predict = "Fair" 5._predict = "Poor" 6._predict = "Blank")
//adapting code
*Generating descriptives tables
*Categoricals
foreach var in sex race ///
  {
  estpost svy, subpop(if black==1): tab `var' diabetes, col perc
  estimates store tab_`var'_t
 }


*Continuous
foreach x in ///
age bmi{
  *Run means and SDs
  *Treatment
  svy, subpop(if black==1): mean `x'
  estat sd
  return list 
  mat `x'_mean_sd_1_t = (r(mean)\r(sd))
  *Run means and SD over ethnicity for standardized scores
  svy, subpop(if black==1): mean `x', over(diabetes)
  estat sd
  return list
  mat `x'_mean_sd_2_t = (r(mean)\r(sd))
  *Creates a combined matrix
  mat `x'_mean_sd_t = (`x'_mean_sd_2_t, `x'_mean_sd_1_t)
  mat list `x'_mean_sd_t
  mat rownames `x'_mean_sd_t = "Mean_`x'_t" "SD_`x'_t"
  estimate store out_`x'_t
  svy, subpop(if black==1): reg `x' i.diabetes
  mat `x'_p_value_t = e(p)
  mat `x'_subpop_t = e(N_sub)
  mat rownames `x'_p_value_t = P-Value
  mat colnames `x'_p_value_t = ""
  mat rownames `x'_subpop_t = N_Sub
  mat colnames `x'_subpop_t = ""
}

*Exporting results using esttab
capture noisily: rm "diabetes_t_matched.csv"


*Treatment with MCI
*Categoricals
foreach var in sex race {
  esttab tab_`var'_t using "diabetes_t_matched.csv", nomtitles ///
  cells(b(fmt(2)) & se(fmt(2) par)) label nonum unstack collabels(none) append ///
  stats(p_Pear N_sub, fmt(3 0) label ("P-value")) drop(Total)
 }

*Continuous
foreach x in ///
 age bmi {
  esttab matrix(`x'_mean_sd_t, fmt(4)) using "diabetes_t_matched.csv", nomtitles ///
  label append nonum ///
  collabels("Not diabetic" "Diabetic" "Total")
  esttab matrix(`x'_p_value_t, fmt(3)) using "diabetes_t_matched.csv", nomtitles collabels(none) ///
  label append nonum
  esttab matrix(`x'_subpop_t, fmt(0)) using "diabetes_t_matched.csv", nomtitles collabels(none) ///
  label append nonum
 }