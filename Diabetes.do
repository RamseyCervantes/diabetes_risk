// Install Necessary Packages
ssc install estout, replace
ssc install coefplot, replace
ssc install blindschemes, replace
// Set graph settings
set scheme plotplainblind

// Use survey data from National Health and Nutrition Examination Survey
use https://www.stata-press.com/data/r17/nhanes2.dta, clear
describe
svyset psu [pweight = finalwgt], strata(strata)

//global macros
global demograph "i.race i.sex"
global cov1 "i.race c.age c.weight c.height"
global cov2 "i.race $cov1"
global workdir "C:\Users\ramse\Documents\GitLab\Ramsey Onboarding"

cd "$workdir"

capture noisily: rmdir "csv"
capture noisily: mkdir "csv"
cd csv

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
	*Diabetes
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


*Diabetes in Black population
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

*Remove existing csv files
capture noisily: rm "Table1.csv"
capture noisily: rm "Table1_margins.csv"
estimates drop _all
//chances of diabetes/heart attack for males and females
foreach out in diabetes heartatk{
	forvalues i = 1/2{
			svy, subpop(if sex==`i'): logit `out' ${cov1}
			estimates store m1`out'_s`i'
			estimates restore m1`out'_s`i'
			margins i.race, post
			matrix temp = r(table)
			*esttab matrix(temp) using Table1_margins.xls,  nomtitles nonum append label
			preserve
			drop _all
			svmat2 temp, names("White" "Black" "Other") rnames("estimates")
			save `out'_s`i'.dta, replace
			restore
		/*margins i.race, subpop(if sex==`i') post
		estimates store ma2`out'_s`i'
		if `i'==1 ///
			marginsplot, title("Male") name(g2`out'_s`i', replace)
		if `i'==2 ///
			marginsplot, title("Female") name(g2`out'_s`i', replace)*/
	}
}
graph combine g2diabetes_s1 g2diabetes_s2, ycommon name(g2diabetes_combined, replace)
graph combine g2heartatk_s1 g2heartatk_s2, ycommon name(g2heartatk_combined, replace)

*Export results to CSV 
foreach x in diabetes heartatk{
	esttab m1`x'_s1  m1`x'_s2 ///
		using Table1.csv, cells(b(fmt(2) star) & ci(par("[" ";" "]"))) eform ///
		mtitle("m1_Male" "m1_Female" "m2_Male" "m2_Female") append label stats(N_sub)
}

regress diabetes i.race##i.sex c.age c.weight c.height
margins i.race#i.sex

//Analyze risk for diabetes and heart attack between sex across different races
coefplot (ma2diabetes_s1, xlabel(100 "male diabetes", add)) (ma2diabetes_s2, label(female diabetes)), bylabel(Risk of Diabetes) || ///
	(ma2heartatk_s1, label(male heart)) (ma2heartatk_s2, label(female heart)), bylabel(Risk of Heart Attack) drop(˙cons) legend(order(1 "Male" 3 "Female") pos(6) col(2)) xlabel(0(.02).1) ytitle("Race/Ethnicity") ///
	subtitle(, size(vlarge) margin(medium) justification(left) color(white) bcolor(black) bmargin(top_bottom)) ///
	name(diabetes_heartatk, replace)

//learn about data for Black population
corr diabetes $personinfo black
foreach stats in $personstats{
	sum `stats' if black==1
}
*Remove existing csv files
capture noisily: rm "Table2.csv"
estimates drop _all
//chances of diabetes for older Black population
svy: tabulate diabetes black, column percent
svy: logit diabetes $cov1 i.black
estimates store mldiabetes
margins, at (age=(50(10)80) weight=(30(30)180) black=1) post
estimates store mdiabtes
marginsplot, name(diabetes_black, replace)

*Export result to CSV 
esttab mldiabetes ///
	using Table2.csv, cells(b(fmt(2) star) & ci(par("[" ";" "]"))) eform ///
	mtitle("mldiabetes") append label stats(N_sub)

//Using loop to get regression for diastolic blood pressure difference between gender
*Remove existing csv files
capture noisily: rm "Table3.csv"
estimates drop _all
forvalues x = 1/2{		
	svy, subpop(if sex==`x' & black==1): regress bpdiast c.age##c.bmi
	estimates store mbpdiast_s`x'
	margins, subpop(if sex==`x') post
	estimates store ma3bpdiast_s`x'
	if `x'==1 ///
		marginsplot, recast(bar) title("Male") name(g2bpdiast_s`x', replace)
	if `x'==2 ///
		marginsplot, recast(bar) title("Female") name(g2bpdiast_s`x', replace)
}
graph combine g2bpdiast_s1 g2bpdiast_s2, ycommon name(g2bpdiast_combined, replace)
*Export result to CSV 
esttab mbpdiast_s1 mbpdiast_s2 ///
	using Table3.csv, cells(b(fmt(2) star) & ci(par("[" ";" "]"))) ///
	mtitle("bpdiast_Male" "bpdiast_Female") append label stats(N_sub)

//likelihood of systolic blood pressure increasing as you age
*Remove existing csv files
capture noisily: rm "Table4.csv"
estimates drop _all
egen systolbin = cut(bpsystol), at(65,121,141,301) label
egen systolbin2 = cut(bpsystol), group(3) label
label define systolicbp 0 "Normal: up to 120" 1 "Elevated: ]120-140)" 2 "High: ]140+"
label values systolbin systolicbp
tab systolbin
tabstat bpsystol, by(systolbin) stats (min max)
svy, subpop(if black==1): mlogit systolbin c.age##c.bmi i.sex
estimates store mlbpsystol
margins, at (age=(20(10)80)) post
marginsplot, title("Age Blood Pressure, Predective Margins") ytitle("pr(blood pressure systolic)") legend(pos(6) col(3) order(4 "up to 120" 5 "]120-140)" 6 "]140+")) xsize(7) ysize(6) /// 
	name(bpsystol_sex, replace)

*Export result to CSV
esttab mlbpsystol ///
	using Table4.csv, cells(b(fmt(2) star) & ci(par("[" ";" "]"))) eform ///
	mtitle("mlbpsystol") append label stats(N_sub)

//comparing mlogit vs ologit on systolic blood pressure differences between sex in Black population
*Remove existing csv files
capture noisily: rm "Table5.csv"
estimates drop _all
foreach model in mlogit ologit{
	forvalues i = 1/2{
		svy, subpop(if sex==`i' & black==1): `model' systolbin c.age##c.bmi
		estimates store `model'_systlobin_sex`i'
		margins, subpop(if sex==`i' & black==1) post
		estimates store `model'_sex`i'_bp
	}
}
coefplot (mlogit_sex1_bp, label(male blood pressure)), bylabel(mlogit Male) || ///
	(mlogit_sex2_bp, label(female blood pressure)), bylabel(mlogit Female) || ///
	(ologit_sex1_bp, label(male blood pressure ordered)), bylabel(ologit Male) || ///
	(ologit_sex2_bp, label(female blood pressure ordered)), bylabel(ologit Female) ytitle("Blood Pressure") ///
	coeflabels(1._predict = "Normal: up to 120" 2._predict = "Elevated: ]120-140)" 3._predict = "High: ]140+") ///
	name(bpsystol_sex_log_combined, replace)
*Export results to CSV 
foreach model in mlogit ologit{
	esttab  `model'_systlobin_sex1 `model'_systlobin_sex2 ///
		using Table5.csv, cells(b(fmt(2) star) & ci(par("[" ";" "]"))) eform ///
		mtitle("Male" "Female") append label stats(N_sub)
} 

//comparing mlogit vs ologit on health status differences between genders
*Remove existing csv files
capture noisily: rm "Table6.csv"
estimates drop _all
foreach model in mlogit ologit{
	forvalues i = 1/2{
		svy, subpop(if sex==`i' & black==1): `model' hlthstat i.sex c.age##c.bmi
		estimates store `model'_health_sex`i'
		margins, subpop(if sex==`i' & black==1) post
		estimates store `model'_sex`i'_health
	}
}
coefplot (mlogit_sex1_health, label(male health status)), bylabel(mlogit Male) || ///
	(mlogit_sex2_health, label(female health status)), bylabel(mlogit Female) || ///
	(ologit_sex1_health, label(male health ordered)), bylabel(ologit Male) || ///
	(ologit_sex2_health, label(female health ordered)), bylabel(ologit Female) ytitle("Health Status") coeflabels(1._predict = "Excellent" 2._predict = "Very Good" 3._predict = "Good" 4._predict = "Fair" 5._predict = "Poor" 6._predict = "Blank") ///
	name(hlthstat_sex_log_combined, replace)
*Export results to CSV 
foreach model in mlogit ologit{
	esttab  `model'_health_sex1 `model'_health_sex2 ///
		using Table6.csv, cells(b(fmt(2) star) & ci(par("[" ";" "]"))) eform ///
		mtitle("Male" "Female") append label stats(N_sub)
}

//alternate approach of logistic regression between sex using margins i.sex
/*  
	   //mlogit vs ologit on blood pressure between sex
	   foreach model in mlogit ologit{
	   svy, subpop(if black==1): `model' systolbin i.sex c.age##c.bmi
	   estimates store `model'_systlobin
	   margins i.sex, post
	   estimates store `model'_sex_bp
	   }

	   coefplot (mlogit_sex_bp, label(male blood pressure)), keep(*1.sex) bylabel(mlogit Male) || ///
	   (mlogit_sex_bp, label(female blood pressure)), keep(*2.sex) bylabel(mlogit Female) || ///
	   (ologit_sex_bp, label(male blood pressure ordered)), keep(*1.sex) bylabel(ologit Male) || ///
	   (ologit_sex_bp, label(female blood pressure ordered)), keep(*2.sex) bylabel(ologit Female) ytitle("Blood Pressure") ///
	   coeflabels(1._predict* = "Normal: up to 120" 2._predict* = "Elevated: ]120-140)" 3._predict* = "High: ]140+")

	   //mlogit vs ologit on health status between genders
	   foreach model in mlogit ologit{
	   svy, subpop(if black==1): `model' hlthstat i.sex c.age##c.bmi
	   estimates store `model'_health
	   margins i.sex, subpop(if black==1), post
	   estimates store `model'_sex_health
	   }

	   coefplot (mlogit_sex_health, label(male health status)), keep(*1.sex) bylabel(mlogit Male) || ///
	   (mlogit_sex_health, label(female health status)), keep(*2.sex) bylabel(mlogit Female) || ///
	   (ologit_sex_health, label(male health ordered)), keep(*1.sex) bylabel(ologit Male) || ///
	   (ologit_sex_health, label(female health ordered)), keep(*2.sex) bylabel(ologit Female) ytitle("Health Status") coeflabels(1._predict* = "Excellent" 2._predict* = "Very Good" 3._predict* = "Good" 4._predict* = "Fair" 5._predict* = "Poor" 6._predict* = "Blank")
*/
