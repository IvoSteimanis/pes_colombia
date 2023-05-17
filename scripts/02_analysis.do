*--------------------------------------------------------------------
* SCRIPT: 02_analysis.do
* PURPOSE: replicates the tables and figures and saves the results
*--------------------------------------------------------------------

*--------------------------------------------------
* STRUCTURE OF THE DO-FILE
/*

	1) Analysis Main Manuscript
	2) Analysis Supplementary Online Materials

*/
*--------------------------------------------------
* Load cleaned dataset
clear
use "$working_ANALYSIS/processed/pes_ready.dta"


*--------------------------------------------------------------
* 1) ANALYSIS MAIN MANUSCRIPT
*--------------------------------------------------------------
**set globals
global socio_economic female id_age max_primary income_hh_ppp farms_n land_size residence_farm
global trust_involvement trust_people trust_townpeople trust_familiy civicorg_d pruductiveorg_d polspace never_collective
global ngo_experience tropenbos_d gaia_d omacha_d dononce donmany
global groups group_2 group_3


** 3.2. Data
** Powever analysis: minimal detectable effect sizes (MDES)
*control vs MT
power twomeans 8682.27,  sd(7171.09) alpha(0.05) power(0.8) n1(80) n2(56)

*matched
power twomeans 8682.27,  sd(7171.09) alpha(0.05) power(0.8) n1(45) n2(51)
di 4141/7171.09
*control vs MR
power twomeans 8682.27,  sd(7171.09) alpha(0.05) power(0.8) n1(80) n2(67)

*MT vs MR
power twomeans 8682.27,  sd(7171.09) alpha(0.05) power(0.8) n1(56) n2(67)

*MDES = 2900, sd=0.4 (medium sized effects) 
power twomeans 8682.27,  sd(7171.09) alpha(0.05) n1(80) n2(56 67) diff(0(500)4000) table(alpha power beta N delta:"Difference in donations (Pesos)") graph(ytitle(Effect size) xline(0.8) yline(3525, lcolor("100 143 255") lpattern(dash)) yline(3349, lcolor("120 94 240") lpattern(dash)) legend(ring(1) rows(1) pos(6)) title("Two-sided MDES") xdimension(power) xla(0(0.1)1) yla(0(500)4000) ydimension(diff))
gr save  "$working_ANALYSIS\results\intermediate\MDES.gph", replace
gr export "$working_ANALYSIS\results\figures\MDES.tif", replace width(3165)




** 4.1. Does termination crowd out donations or related motivations?
**Figure. 1.	Donations and motivations across groups
*Panel A: Donation Amounts
reg donation_percent $groups $socio_economic $trust_involvement $ngo_experience, vce(hc3)
testparm $groups
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience, vce(hc3)
est store donation_amount
reg donation_n group_1 group_3 $socio_economic $trust_involvement $ngo_experience, vce(hc3)


coefplot (donation_amount),   keep(group_2)   xline(0, lpattern(dash) lcolor(gs3)) title("{bf:A } Amounts donated", span pos(11) size()) xtitle("Regression estimated impact relative to NM in Pesos") xla(-2000(2000)6000, nogrid) grid(none) levels(95 90) ciopts(lwidth(0.8 2)  lcolor(*1 *.3) recast(rcap)) mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", "")))) msize(4pt) msymbol(D) mlabsize(8pt) mlabposition(12)
gr_edit style.editstyle margin(vsmall) editcopy
gr save "$working_ANALYSIS/results/intermediate/figure1_a.gph", replace

*Panel B: Motivations to protect forests
*internal motivation
reg motivation1 $groups $socio_economic $trust_involvement, vce(hc3)
estimates store motivation1
*social motivation
reg motivation2 $groups $socio_economic $trust_involvement, vce(hc3)
estimates store motivation2
*Money motivation
reg motivation3 $groups $socio_economic $trust_involvement, vce(hc3)
estimates store motivation3
*Fines motivation
reg motivation4 $groups $socio_economic $trust_involvement, vce(hc3)
estimates store motivation4
*External LoC
reg motivation5 $groups $socio_economic $trust_involvement, vce(hc3)
estimates store motivation5
*No point
reg motivation6 $groups $socio_economic $trust_involvement, vce(hc3)
estimates store motivation6

coefplot (motivation1, aseq(Internal)) ///
	(motivation2, aseq(Social))  ///
    (motivation3, aseq(Money)) ///
    (motivation4, aseq(Fines))  ///
	(motivation5, aseq(External LoC)) ///
	(motivation6, aseq(Amotivation)) ///
     , level(95 90) ciopts(color("100 143 255" "100 143 255") lwidth(0.8 2)  lcolor(*1 *.3)  recast(rcap)) nooffsets coeflabels(, wrap(26))	keep(group_2) swapnames drop(_cons) xline(0, lcolor(gs5) lwidth(medium)) msize() legend(off) grid(none) xscale(range(-0.5 0.5)) xlabels(-.75(0.25).75, nogrid labsize()) mlabposition(1) xtitle("Regression estimated impact relative to NM", height(7)) msize(4pt)  mcolor("100 143 255") msymbol(D) mlabsize(8pt) mlabel(cond(@pval<.01, "{bf:***}", cond(@pval<.05, "{bf:**}",  cond(@pval<.10, "{bf:*}", "" )))) title("{bf:B } Motivations to protect forests", span pos(11) size())
gr_edit style.editstyle margin(vsmall) editcopy
gr_edit plotregion1.plot18.style.editstyle marker(fillcolor("100 143 255*1.3")) editcopy
gr_edit plotregion1.plot16.style.editstyle area(linestyle(color("100 143 255*1.3"))) editcopy
gr save  "$working_ANALYSIS/results/intermediate/figure1_b.gph", replace

gr combine "$working_ANALYSIS/results/intermediate/figure1_a.gph" "$working_ANALYSIS/results/intermediate/figure1_b.gph", rows(1) xsize(3.465) ysize(2)
gr save "$working_ANALYSIS/results/intermediate/figure1_main_results.gph", replace
gr export "$working_ANALYSIS/results/figures/figure1_main_results.tif", width(4000) replace



*Figure. 2.	Heterogeneous effects across socio-economic groups
*average effects
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience, vce(hc3)
estimates store all_sample
*by SES
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if high_SES == 1, vce(hc3)
estimates store highSES
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if high_SES == 0, vce(hc3)
estimates store lowSES
*By gender
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if female == 1, vce(hc3)
estimates store female
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if female == 0, vce(hc3)
estimates store male
*by age
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if high_age == 0, vce(hc3)
estimates store age_below
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if high_age == 1, vce(hc3)
estimates store age_above
*by residence
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if residence_farm == 0, vce(hc3)
estimates store city
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if residence_farm == 1, vce(hc3)
estimates store farm

coefplot (all_sample, aseq(All Sample)) ///
	(age_below, aseq(Aged Below 58)) ///
    (age_above, aseq(Aged at or Above 58)) ///
       (female, aseq(Female))  ///
	(male, aseq(Male))  ///
	(highSES, aseq(High SES)) ///						
	(lowSES, aseq(Low SES))  ///
	(city, aseq(Residence: Urban))  ///
	(farm, aseq(Residence: Farm))  ///
     , level(95 90) ciopts(color("100 143 255" "100 143 255") lwidth(0.6 1.5)  lcolor(*1 *.3)  recast(rcap))  nooffsets coeflabels(, wrap(26))	keep(group_2) swapnames drop(_cons) xline(0, lcolor(gs5) lwidth(medium)) msize() legend(off) grid(none) xscale(range(-0.5 0.5)) xlabels(-5000(5000)15000, nogrid labsize()) mlabposition(1) xtitle("Regression estimated impact relative to NM in Pesos", height(7))  mlabel(cond(@pval<.01, "{bf:***}", cond(@pval<.05, "{bf:**}",  cond(@pval<.10, "{bf:*}", "" )))) mlabsize(6pt) mcolor("100 143 255") msize(7pt) msymbol(D) ysize(4) xsize(4)
gr save  "$working_ANALYSIS/results/intermediate/figure2_heterogeneous_effects.gph", replace
gr export "$working_ANALYSIS/results/figures/figure2_heterogeneous_effects.tif", replace width(4500)




** 4.2. Additional support for no crowding out
** Disappointment? Differences in project perception between MR and MT
ranksum p_satisfaction if p_satisfaction != 5, by(pes_participation)
ttest p_satisfaction if p_satisfaction != 5, by(pes_participation)

* Perceived satisfaction with the PES program
* Only for MT: Does knowing that others still get paid affect satisfaction?
ranksum p_satisfaction if pes_participation==2 & p_satisfaction != 5, by(participate_o_d)
ttest  p_satisfaction if pes_participation==2 & p_satisfaction != 5, by(participate_o_d)
*MT even slightly more satisfied, especially those who know that there are others who are still getting paid

* Perceived fairness of the selection process
ranksum selection_f if selection_f != 5, by(pes_participation)
ttest selection_f if selection_f != 5, by(pes_participation)

* Only for MT: Does knowing that others still get paid affect fairness perception?
ranksum selection_f if pes_participation==2 & selection_f != 5, by(participate_o_d)
ttest  selection_f if pes_participation==2 & selection_f != 5, by(participate_o_d)
*MT perceive the selection process as fairer independent of whether they know someone else still receives the PES


*Reciprocity
ttest payment_ppp if payment_rem_d==1, by(group_3)
pwcorr donation_n payment_ppp if payment_rem_d==1 & group_3==0, sig
pwcorr donation_n payment_ppp if payment_rem_d==1 & group_3==1, sig





*--------------------------------------------------------------
* 2) Supplementary Online Materials (SOM)
*--------------------------------------------------------------
*Table S1.	Balances Across Groups
global balance female id_age max_primary income_hh_ppp farms_n land_size residence_farm econ_depend agricultor cattlemen merchant employee  othereconactivity  /// socio-economics
civicorg_d pruductiveorg_d polspace collective_action never_collective  /// community involvement
trust_people trust_townpeople trust_familiy responsibility_gov  /// donation experience
tropenbos_d gaia_d omacha_d donnever dononce donmany
iebaltab $balance , grpvar(pes_participation) onerow  rowvarlabels save("$working_ANALYSIS/results/tables/TableS1_balancing") ftest fmissok replace
* joint f-tests no significant differnces in terms of socio-economics and other pre-specified variables potentially influencing intentions and vaccine hesitancy
*non-participants vs. MT
reg pes_participation $balance if pes_participation <3
*non-participants vs MR
reg pes_participation $balance if pes_participation !=2
*MT vs MR
reg pes_participation $balance if pes_participation !=1


*Table S2.	Donations across groups
reg donation_n $groups, vce(hc3)
outreg2 using "$working_ANALYSIS/results/tables/TableS2_donation_amount", addstat("Adjusted R-squared", e(r2_a)) adec(3) drop() word ctitle("Donation Amount") dec(2) replace
reg donation_n $groups $socio_economic, vce(hc3)
testparm $socio_economic
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS2_donation_amount", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n $groups $trust_involvement, vce(hc3)
testparm $trust_involvement
local F2 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS2_donation_amount", addstat("Adjusted R-squared", e(r2_a), "Trust & Involvement", `F2') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n $groups $ngo_experience, vce(hc3)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS2_donation_amount", addstat("Adjusted R-squared", e(r2_a), "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n $groups $socio_economic $trust_involvement $ngo_experience, vce(hc3)
testparm group_2 group_3
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS2_donation_amount", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append



*Figure S1.	ATT: Overlap and bias reductions
* where do pes members differ from non-members?
reg treated $socio_economic $trust_involvement $ngo_experience

* Common support: Dropping treatment observations whose pscore is higher than the maximum or less than the minimum pscore of the controls
*ATT: adjust the untreated group to resemble the treated group by weighting or dropping members of the untreated group and leaving the members of the treated group untouched 
psmatch2 treated  land_size residence_farm donatedbefore, n(1) common 
psgraph, title("{bf:A } Common Support", span pos(11))
gr save "$working_ANALYSIS/results/intermediate/figureS1_A.gph", replace
pstest land_size residence_farm donatedbefore, both graph legend(rows(1) pos(6) ring(1)) 
gr save "$working_ANALYSIS/results/intermediate/figureS1_B.gph", replace
gr combine "$working_ANALYSIS/results/intermediate/figureS1_A.gph" "$working_ANALYSIS/results/intermediate/figureS1_B.gph", rows(1)
gr_edit plotregion1.graph2.title.text = {}
gr_edit plotregion1.graph2.title.text.Arrpush {bf: B} Bias reduction
gr_edit plotregion1.graph2.title.style.editstyle box_alignment(nwest) editcopy
gr_edit style.editstyle margin(tiny) editcopy
gr_edit style.editstyle declared_ysize(2) editcopy
gr_edit style.editstyle declared_xsize(3.165) editcopy
gr_edit plotregion1.graph2.grpaxis.major.num_rule_ticks = 0
gr_edit plotregion1.graph2.grpaxis.edit_tick 1 16 `"Residence on farm (=1)"', tickset(major)
gr_edit plotregion1.graph2.grpaxis.major.num_rule_ticks = 0
gr_edit plotregion1.graph2.grpaxis.edit_tick 1 16 `"Residence on farm (=1)"', tickset(major)
gr_edit plotregion1.graph2.grpaxis.major.num_rule_ticks = 0
gr_edit plotregion1.graph2.grpaxis.edit_tick 2 50 `"Donated before (=1)"', tickset(major)
gr_edit plotregion1.graph2.grpaxis.major.num_rule_ticks = 0
gr_edit plotregion1.graph2.grpaxis.edit_tick 2 50 `"Donated before (=1)"', tickset(major)
gr_edit plotregion1.graph2.grpaxis.major.num_rule_ticks = 0
gr_edit plotregion1.graph2.grpaxis.edit_tick 3 83 `"Size of farmland (in ha) "', tickset(major)
gr_edit plotregion1.graph2.grpaxis.major.num_rule_ticks = 0
gr_edit plotregion1.graph2.grpaxis.edit_tick 3 83 `"Size of farmland (in ha) "', tickset(major)
gr save "$working_ANALYSIS/results/intermediate/figureS1_matching_ATT.gph", replace
gr export "$working_ANALYSIS/results/figures/figureS1_matching_ATT.tif", replace width(3465)
tab  _support _treated, col

* 5 (4%) treated participants are off common support
* gen matched pair identifier
gen pair = _id if _treated==0 
replace pair = _n1 if _treated==1
bysort pair: egen paircount = count(pair)
foreach x of varlist _weight _id _pscore _pdif _n1 _nn _treated _support pair paircount {
		rename `x'  matched`x'
}
replace matched_treated=. if matchedpaircount < 2
tab matched_treated
* 45 out of 80 NM are retained after matching on number of farms and land size. This reduced bias substantially.

* create inverse-probability of treatment weight (IPTW) to use as probability weight in regression to account for differences in treatment probability
gen     iptw = 1 / matched_pscore if treated==1
replace iptw = 1 / (1 - matched_pscore) if treated==0


** Table S3.	Balance after matching
iebaltab $balance , grpvar(matched_treated) onerow  rowvarlabels save("$working_ANALYSIS/results/tables/TableS3_balancing_matched") ftest fmissok replace
reg matched_treated  $balance


*Figure S2.	ATU: Overlap and bias reduction
*ATU: adjusting the treated group to resemble the untreated group
gen untreated = 1-treated
psmatch2 untreated land_size residence_farm donatedbefore, n(1) common 
psgraph, title("{bf:A } Common Support", span pos(11))
gr save "$working_ANALYSIS/results/intermediate/figureS2_A.gph", replace
pstest land_size residence_farm  donatedbefore , both graph legend(rows(1) pos(6) ring(1)) 
gr save "$working_ANALYSIS/results/intermediate/figureS2_B.gph", replace
gr combine "$working_ANALYSIS/results/intermediate/figureS2_A.gph" "$working_ANALYSIS/results/intermediate/figureS2_B.gph", rows(1)
gr_edit plotregion1.graph1.plotregion1.plot2.style.editstyle area(shadestyle(color("100 143 255"))) editcopy
gr_edit plotregion1.graph1.plotregion1.plot1.style.editstyle area(shadestyle(color("120 94 225"))) editcopy
gr_edit plotregion1.graph1.legend.plotregion1.label[1].text = {}
gr_edit plotregion1.graph1.legend.plotregion1.label[1].text.Arrpush Treated
gr_edit plotregion1.graph1.legend.plotregion1.label[2].text = {}
gr_edit plotregion1.graph1.legend.plotregion1.label[2].text.Arrpush Untreated
gr_edit plotregion1.graph1.legend.plotregion1.label[3].text = {}
gr_edit plotregion1.graph1.legend.plotregion1.label[3].text.Arrpush Untreated: Off support
gr_edit plotregion1.graph2.title.text = {}
gr_edit plotregion1.graph2.title.text.Arrpush {bf: B} Bias reduction
gr_edit plotregion1.graph2.title.style.editstyle box_alignment(nwest) editcopy
gr_edit style.editstyle margin(tiny) editcopy
gr_edit style.editstyle declared_ysize(2) editcopy
gr_edit style.editstyle declared_xsize(3.165) editcopy
gr_edit plotregion1.graph2.grpaxis.major.num_rule_ticks = 0
gr_edit plotregion1.graph2.grpaxis.edit_tick 3 83 `"Residence on farm (=1)"', tickset(major)
gr_edit plotregion1.graph2.grpaxis.major.num_rule_ticks = 0
gr_edit plotregion1.graph2.grpaxis.edit_tick 3 83 `"Residence on farm (=1)"', tickset(major)
gr_edit plotregion1.graph2.grpaxis.major.num_rule_ticks = 0
gr_edit plotregion1.graph2.grpaxis.edit_tick 2 50 `"Donated before (=1)"', tickset(major)
gr_edit plotregion1.graph2.grpaxis.major.num_rule_ticks = 0
gr_edit plotregion1.graph2.grpaxis.edit_tick 2 50 `"Donated before (=1)"', tickset(major)
gr_edit plotregion1.graph2.grpaxis.major.num_rule_ticks = 0
gr_edit plotregion1.graph2.grpaxis.edit_tick 1 16 `"Size of farmland (in ha) "', tickset(major)
gr_edit plotregion1.graph2.grpaxis.major.num_rule_ticks = 0
gr_edit plotregion1.graph2.grpaxis.edit_tick 1 16 `"Size of farmland (in ha) "', tickset(major)
gr export "$working_ANALYSIS/results/figures/figureS2_matching_ATU.tif", replace width(3465)
tab  _support _treated, col
* 1  (1%) of untreated participants are off common support


* gen matched pair identifier
gen pair_ATU = _id if _treated==0 
replace pair_ATU = _n1 if _treated==1
bysort pair_ATU: egen paircount_ATU = count(pair_ATU)
foreach x of varlist _weight _id _pscore _pdif _n1 _nn _treated _support pair_ATU paircount_ATU {
		rename `x'  matched_ATU`x'
}
replace matched_ATU_treated=. if matched_ATUpaircount_ATU < 2
tab matched_ATU_treated
* 43 PES participant observations are kept after matching. This reduced bias substantially. (here labeled as "untreated")


** Table S4.	Donations across groups using matching, propensity score and IPTW
* ATT with different matching approaches and controlling for socio-economics
*matched sample
reg donation_n $groups $socio_economic $trust_involvement $ngo_experience [fweight = matched_weight], vce(cluster id)
testparm $socio_economic
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS4_donation_amount_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1') adec(2) word ctitle("Donation Amount") dec(2) replace

* ATU with different matching approaches and controlling for socio-economics
reg donation_n $groups $socio_economic $trust_involvement $ngo_experience  [fweight = matched_ATU_weight], vce(cluster id)
testparm $socio_economic
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS4_donation_amount_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1') adec(2) word ctitle("Donation Amount") dec(2) append

*control for propensity score
reg donation_n $groups $socio_economic $trust_involvement $ngo_experience   matched_pscore, vce(hc3)
testparm $socio_economic
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS4_donation_amount_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1') adec(2) word ctitle("Donation Amount") dec(2) append

* IPTW as probability weight
reg donation_n $groups $socio_economic $trust_involvement $ngo_experience  [pw = iptw],  vce(cluster id)
testparm $socio_economic
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS4_donation_amount_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1') adec(2) word ctitle("Donation Amount") dec(2) append



* Figure S3. ATT: Overlap and bias reductions
tab econ_activity, gen(econ_)
*ATT
psmatch2 treated  land_size residence_farm donatedbefore never_collective , n(1) common
psgraph, title("{bf:A } Common Support", span pos(11))
gr save "$working_ANALYSIS/results/intermediate/figureS3_A.gph", replace
pstest land_size residence_farm donatedbefore never_collective econ_1, both graph legend(rows(1) pos(6) ring(1)) 
gr save "$working_ANALYSIS/results/intermediate/figureS3_B.gph", replace
gr combine "$working_ANALYSIS/results/intermediate/figureS3_A.gph" "$working_ANALYSIS/results/intermediate/figureS3_B.gph", rows(1)
gr_edit plotregion1.graph1.plotregion1.plot2.style.editstyle area(shadestyle(color("100 143 255"))) editcopy
gr_edit plotregion1.graph1.plotregion1.plot1.style.editstyle area(shadestyle(color("120 94 225"))) editcopy
gr_edit plotregion1.graph1.legend.plotregion1.label[1].text = {}
gr_edit plotregion1.graph1.legend.plotregion1.label[1].text.Arrpush Treated
gr_edit plotregion1.graph1.legend.plotregion1.label[2].text = {}
gr_edit plotregion1.graph1.legend.plotregion1.label[2].text.Arrpush Untreated
gr_edit plotregion1.graph1.legend.plotregion1.label[3].text = {}
gr_edit plotregion1.graph1.legend.plotregion1.label[3].text.Arrpush Untreated: Off support
gr_edit plotregion1.graph2.title.text = {}
gr_edit plotregion1.graph2.title.text.Arrpush {bf: B} Bias reduction
gr_edit plotregion1.graph2.title.style.editstyle box_alignment(nwest) editcopy
gr_edit style.editstyle margin(tiny) editcopy
gr_edit style.editstyle declared_ysize(2) editcopy
gr_edit style.editstyle declared_xsize(3.165) editcopy
gr export "$working_ANALYSIS/results/figures/figureS3_matching2.tif", replace width(3465)
tab  _support _treated, col
* 9 (4%) treated participants are off common support
* gen matched pair identifier
gen pair2 = _id if _treated==0 
replace pair2 = _n1 if _treated==1
bysort pair2: egen paircount2 = count(pair2)
foreach x of varlist _weight _id _pscore _pdif _n1 _nn _treated _support pair2 paircount2 {
		rename `x'  matched2`x'
}
replace matched2_treated=. if matched2paircount < 2
tab matched2_treated
* 44 out of 80 NM are retained after matching on number of farms and land size. This reduced bias substantially.

* create inverse-probability of treatment weight (IPTW) to use as probability weight in regression to account for differences in treatment probability
gen     iptw2 = 1 / matched2_pscore if treated==1
replace iptw2 = 1 / (1 - matched2_pscore) if treated==0


*ATU
psmatch2 untreated land_size residence_farm donatedbefore never_collective econ_1, n(1) common 
pstest land_size residence_farm donatedbefore never_collective econ_1, both graph legend(rows(1) pos(6) ring(1)) 
tab  _support _treated, col
* 12  (15%) of untreated participants are off common support


* gen matched pair identifier
gen pair2_ATU = _id if _treated==0 
replace pair2_ATU = _n1 if _treated==1
bysort pair2_ATU: egen paircount2_ATU = count(pair2_ATU)
foreach x of varlist _weight _id _pscore _pdif _n1 _nn _treated _support pair2_ATU paircount2_ATU {
		rename `x'  matched2_ATU`x'
}
replace matched2_ATU_treated=. if matched2_ATUpaircount2_ATU < 2
tab matched2_ATU_treated
* 39 PES participant observations are kept after matching. This reduced bias substantially. (here labeled as "untreated")


* Table S5.	Donations across groups using matching, propensity score and IPTW
*matched sample
reg donation_n $groups $socio_economic $trust_involvement $ngo_experience [fweight = matched2_weight], vce(cluster id)
testparm $socio_economic
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS5_donation_amount_matched2", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1') adec(2) word ctitle("Donation Amount") dec(2) replace

* ATU with different matching approaches and controlling for socio-economics
reg donation_n $groups $socio_economic $trust_involvement $ngo_experience  [fweight = matched2_ATU_weight], vce(cluster id)
testparm $socio_economic
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS5_donation_amount_matched2", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1') adec(2) word ctitle("Donation Amount") dec(2) append

*control for propensity score
reg donation_n $groups $socio_economic $trust_involvement $ngo_experience   matched2_pscore, vce(hc3)
testparm $socio_economic
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS5_donation_amount_matched2", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1') adec(2) word ctitle("Donation Amount") dec(2) append

* IPTW as probability weight
reg donation_n $groups $socio_economic $trust_involvement $ngo_experience  [pw = iptw2],  vce(cluster id)
testparm $socio_economic
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS5_donation_amount_matched2", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1') adec(2) word ctitle("Donation Amount") dec(2) append




**Table S6.	Motivations across groups
*internal motivation
reg motivation1 $groups $socio_economic $trust_involvement, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS6_motivations", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2') adec(3) word ctitle("Internal") dec(2) replace
*social motivation
reg motivation2 $groups $socio_economic $trust_involvement, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS6_motivations", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2') adec(3) word ctitle("Social") dec(2) append
*Money motivation
reg motivation3 $groups $socio_economic $trust_involvement, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS6_motivations", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2') adec(3) word ctitle("Money") dec(2) append
*Fines motivation
reg motivation4 $groups $socio_economic $trust_involvement, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS6_motivations", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2') adec(3) word ctitle("Fines") dec(2) append
*External LoC
reg motivation5 $groups $socio_economic $trust_involvement, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS6_motivations", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2') adec(3) word ctitle("External LoC") dec(2) append
*No point
reg motivation6 $groups $socio_economic $trust_involvement, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS6_motivations", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2') adec(3) word ctitle("Amotivation") dec(2) append


** Table S7.	Motivations across groups: Matched sample
*internal motivation
reg motivation1 $groups $socio_economic $trust_involvement [fw = matched_weight], vce(cluster id)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS7_motivations_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2') adec(3) word ctitle("Internal") dec(2) replace
*social motivation
reg motivation2 $groups $socio_economic $trust_involvement  [fw = matched_weight], vce(cluster id)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS7_motivations_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2') adec(3) word ctitle("Social") dec(2) append
*Money motivation
reg motivation3 $groups $socio_economic $trust_involvement [fw = matched_weight], vce(cluster id)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS7_motivations_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2') adec(3) word ctitle("Money") dec(2) append
*Fines motivation
reg motivation4 $groups $socio_economic $trust_involvement  [fw = matched_weight], vce(cluster id)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS7_motivations_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2') adec(3) word ctitle("Fines") dec(2) append
*External LoC
reg motivation5 $groups $socio_economic $trust_involvement  [fw = matched_weight], vce(cluster id)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS7_motivations_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2') adec(3) word ctitle("External LoC") dec(2) append
*No point
reg motivation6 $groups $socio_economic $trust_involvement [fw = matched_weight], vce(cluster id)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS7_motivations_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2') adec(3) word ctitle("Amotivation") dec(2) append




** Table S8.	Correlation of motivations with donation amounts
global motivations motivation1 motivation2 motivation3 motivation4 motivation5 motivation6
reg donation_n group_2 group_3 $motivations $socio_economic $trust_involvement $ngo_experience, vce(hc3)
est store motivate_donate1
testparm $motivations
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS8_motivations_donation", drop($socio_economic $trust_involvement $ngo_experience) addstat("Adjusted R-squared", e(r2_a), "Motivations", `F1') adec(3) word ctitle("Donation Amount") dec(2) replace
reg donation_n $motivations $socio_economic $trust_involvement $ngo_experience if pes_participation==1, vce(hc3)
est store motivate_donate2
testparm $motivations
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS8_motivations_donation", drop($socio_economic $trust_involvement $ngo_experience) addstat("Adjusted R-squared", e(r2_a), "Motivations", `F1') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n $motivations $socio_economic $trust_involvement $ngo_experience if pes_participation==2, vce(hc3)
est store motivate_donate3
testparm $motivations
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS8_motivations_donation", drop($socio_economic $trust_involvement $ngo_experience) addstat("Adjusted R-squared", e(r2_a), "Motivations", `F1') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n $motivations $socio_economic $trust_involvement $ngo_experience if pes_participation==3, vce(hc3)
est store motivate_donate4
testparm $motivations
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS8_motivations_donation", drop($socio_economic $trust_involvement $ngo_experience) addstat("Adjusted R-squared", e(r2_a), "Motivations", `F1') adec(3) word ctitle("Donation Amount") dec(2) append



** Table S9.	PES participation effect on donations across socioeconomics characteristics
*average effects
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS9_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) replace
*by SES
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if high_SES == 1, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS9_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if high_SES == 0, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS9_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
*By gender
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if female == 1, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS9_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if female == 0, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS9_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
*by age
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if high_age == 0, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS9_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if high_age == 1, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS9_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
*by residence
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if residence_farm == 0, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS9_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if residence_farm == 1, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS9_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append


** Table S10.	Balancing by recall timing
tab primed pes_participation
iebaltab  $balance , grpvar(primed)  rowvarlabels onerow save("$working_ANALYSIS/results/tables/TableS10_balancing_priming") ftest fmissok replace
reg primed $balance


** Table S11.	Effects of recall on donations
reg donation_n primed group_3 primed_group3, vce(hc3)
outreg2 using "$working_ANALYSIS/results/tables/TableS11_donation_recall", addstat("Adjusted R-squared", e(r2_a)) adec(3) drop() word ctitle("Donation Amount") dec(2) replace
reg donation_n primed group_3 primed_group3 $socio_economic, vce(hc3)
testparm $socio_economic
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS11_donation_recall", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n primed group_3 primed_group3 $trust_involvement, vce(hc3)
testparm $trust_involvement
local F2 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS11_donation_recall", addstat("Adjusted R-squared", e(r2_a), "Trust & Involvement", `F2') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n primed group_3 primed_group3 $ngo_experience, vce(hc3)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS11_donation_recall", addstat("Adjusted R-squared", e(r2_a), "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n primed group_3 primed_group3 $socio_economic $trust_involvement $ngo_experience, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS11_donation_recall", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append


** Figure S4.	Predicted donations by received PES payments
bys pes_participation: pwcorr donation_n payment_ppp, sig
reg donation_n c.payment_ppp##i.group_3 $socio_economic $trust_involvement $ngo_experience if payment_rem_d==1, vce(hc3)
margins i.group_3, at(payment_ppp = (100(1000)11000)) vsquish
marginsplot, xla(100 2000 4000 6000 8000 10000, nogrid) title("") xtitle("Total payment (PPP)") yla(0(5000)20000, nogrid) ///
		ytitle("Predicted Donation in Pesos") ///
		recastci(rarea) ciopts(fcolor(%30) lwidth(none)) legend(order(3 "M-TERM" 4 "M") pos(12) ring(0) rows(1)) 
gr save  "$working_ANALYSIS\results\intermediate\figureS4_reciprocity.gph", replace
gr export "$working_ANALYSIS\results\figures\figureS4_reciprocity.tif", replace width(3165)




** EOF