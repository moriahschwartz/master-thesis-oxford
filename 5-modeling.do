* Perform negative binomial fixed effects regression modeling in STATA

clear
cd "C:\..."
log using "panel_analysis.log", replace
use "code\panel_data.dta"

* Convert string variables to factors
encode fullname, gen(county)
encode region, gen(regions)

* drop unneeded variables
keep entered drugdeathrate datayear county child_pop state pov_rate unemp_rate perc_white no_HS_rate perc_married 
some_MAT MAT_acc regions

drop if entered==. | drugdeathrate==.| child_pop==.| pov_rate==.| unemp_rate==.| perc_white==.| no_HS_rate==.| 
perc_married==.| some_MAT==.| MAT_acc==.

* Label variables
lab var entered "Number of youth entering foster care"
lab var drugdeathrate "Rate of death from drug poisoning (per 100K)"
lab var child_pop "Child population"
lab var pov_rate "Poverty rate"
lab var unemp_rate "Unemployment rate"
lab var perc_white "Percent white"
lab var no_HS_rate "Percent without a HS degree"
lab var perc_married "Percent married"
lab var some_MAT "Some MAT services offered"
lab var MAT_acc "Comprehensive MAT offered"
lab var regions "Region"

lab define region_names 1 "Midwest" 2 "Northeast" 3 "South" 4 "West"
lab val regions region_names


* Declare panel data
xtset county datayear
* Get panel data summary stats
xtsum


* Summary stats and descriptions
count if MAT_acc == 0
count if MAT_acc == 1
count

codebook regions

tabstat entered, stat(mean sd min max)
tabstat entered, stat(mean sd min max) by(regions)
tabstat entered, stat(mean sd min max) by(MAT_acc)

tabstat drugdeathrate, stat(mean sd min max)
tabstat drugdeathrate, stat(mean sd min max) by(regions)
tabstat drugdeathrate, stat(mean sd min max) by(MAT_acc)

tabstat entered drugdeathrate child_pop pov_rate unemp_rate perc_white no_HS_rate perc_married some_MAT MAT_acc, 
stat(mean sd min max)

tabstat entered drugdeathrate child_pop pov_rate unemp_rate perc_white no_HS_rate perc_married some_MAT, 
stat(mean sd min max) by(MAT_acc)



* Check for correlation among predictors
correlate drugdeathrate pov_rate unemp_rate perc_white no_HS_rate perc_married some_MAT MAT_acc

* High correlations: poverty & unemployment, poverty & no HS, poverty & percent married
* Scaling technique: Z Scores
sum pov_rate unemp_rate no_HS_rate perc_married

gen perc_unmarried = 1 - perc_married

egen z_pov_rate = std(pov_rate)
egen z_unemp_rate = std(unemp_rate)
egen z_no_HS_rate = std(no_HS_rate)
egen z_perc_unmarried = std(perc_unmarried)

* Check mean = 0, SD = 1
sum z_pov_rate z_unemp_rate z_no_HS_rate z_perc_unmarried

gen composite = z_pov_rate + z_unemp_rate + z_no_HS_rate + z_perc_unmarried



* Model 1
qui nbreg entered drugdeathrate i.datayear i.county, exposure(child_pop) cluster(state)

* Retrieve incidence rate ratios
nbreg, irr

* Save output to word
outreg2 using "STATA_tables\reg_results", title("Negative Binomial Fixed Effects Models: Drug-Related Deaths and 
Number of Foster Youth Entries (IRR)") alpha (.01, .05, .10) symbol (***,**,*) label drop(entered i.county i.datayear) 
nocons addnote("Region: reference category = Midwest") addstat(Pseudo R2, e(r2_p)) ctitle("Model 1") 
addtext("Observations = 642; n = 124; T = 7") eform dec(3) noomitted noobs word replace


* Model 2: include ACS control variables
qui nbreg entered drugdeathrate composite perc_white i.regions i.datayear i.county, exposure(child_pop) cluster(state)

nbreg, irr

outreg2 using "STATA_tables\reg_results", alpha (.01, .05, .10) symbol (***,**,*) label drop(entered i.county i.datayear) 
nocons addstat(Pseudo R2, e(r2_p)) ctitle("Model 2") eform dec(3) noomitted noobs word


* Model 3: Include MAT variable 1 (records whether any form is provided within each county-year)
qui nbreg entered drugdeathrate composite perc_white i.regions i.some_MAT i.datayear i.county, exposure(child_pop) 
cluster(state)

nbreg, irr

outreg2 using "STATA_tables\reg_results", alpha (.01, .05, .10) symbol (***,**,*) label drop(entered i.county i.datayear) 
nocons addstat(Pseudo R2, e(r2_p)) ctitle("Model 3") eform dec(3) noomitted noobs word


* Model 4: Include both MAT variables (records whether any form AND all forms provided within each county-year)
qui nbreg entered drugdeathrate composite perc_white i.regions i.some_MAT i. MAT_acc i.datayear i.county, exposure(child_pop) 
cluster(state)

nbreg, irr

outreg2 using "STATA_tables\reg_results", alpha (.01, .05, .10) symbol (***,**,*) label drop(entered i.county i.datayear) 
nocons addstat(Pseudo R2, e(r2_p)) ctitle("Model 4") eform dec(3) noomitted noobs word



* Sensitivity Analysis: Re-run models without 17 CA counties (see if findings overly influenced by a single state)
count if state == "California"
* 17 counties, 109 county-years
drop if state == "California"

* Re-run models

* Model 1.2
qui nbreg entered drugdeathrate i.datayear i.county, exposure(child_pop) cluster(state)

nbreg, irr

outreg2 using "STATA_tables\sensitivity_analysis", title("Sensitivity Analysis of Negative Binomial Fixed Effects Models: 
Drug-Related Deaths and Number of Foster Youth Entries, Excluding California (IRR)") alpha (.01, .05, .10) symbol (***,**,*) 
label drop(entered i.county i.datayear) nocons addnote("Region: reference category = Midwest") addstat(Pseudo R2, e(r2_p)) 
ctitle("Model 1") addtext("Observations = 642; n = 124; T = 7") eform dec(3) noomitted noobs word replace


* Model 2.2
qui nbreg entered drugdeathrate composite perc_white i.regions i.datayear i.county, exposure(child_pop) cluster(state)

nbreg, irr

outreg2 using "STATA_tables\sensitivity_analysis", alpha (.01, .05, .10) symbol (***,**,*) label drop(entered i.county 
i.datayear) nocons addstat(Pseudo R2, e(r2_p)) ctitle("Model 2") eform dec(3) noomitted noobs word


* Model 3.2
qui nbreg entered drugdeathrate composite perc_white i.regions i.some_MAT i.datayear i.county, exposure(child_pop) 
cluster(state)

nbreg, irr

outreg2 using "STATA_tables\sensitivity_analysis", alpha (.01, .05, .10) symbol (***,**,*) label drop(entered i.county 
i.datayear) nocons addstat(Pseudo R2, e(r2_p)) ctitle("Model 3") eform dec(3) noomitted noobs word


* Model 4.2
qui nbreg entered drugdeathrate composite perc_white i.regions i.some_MAT i. MAT_acc i.datayear i.county, 
exposure(child_pop) cluster(state)

nbreg, irr

outreg2 using "STATA_tables\sensitivity_analysis", alpha (.01, .05, .10) symbol (***,**,*) label drop(entered i.county 
i.datayear) nocons addstat(Pseudo R2, e(r2_p)) ctitle("Model 4") eform dec(3) noomitted noobs word


log close
