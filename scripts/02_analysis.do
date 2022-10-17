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


*4.1. Does termination crowd out donations or related motivations?
**Figure. 1.	Donations and motivations across groups
*Panel A: Donation Amounts
reg donation_percent $groups $socio_economic $trust_involvement $ngo_experience, vce(hc3)
testparm $groups
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience, vce(hc3)
est store donation_amount
reg donation_n group_1 group_3 $socio_economic $trust_involvement $ngo_experience, vce(hc3)


coefplot (donation_amount),   keep(group_2)   xline(0, lpattern(dash) lcolor(gs3)) title("{bf:A } Amounts donated", span pos(11) size()) xtitle("Regression estimated impact relative to NM in Pesos") xla(-2000(2000)6000, nogrid) grid(none) levels(95 90) ciopts(lwidth(0.3 1)  msize()  lcolor(*.8 *.2) recast(rcap)) mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", ""))))  mlabsize(8pt) mlabposition(12)
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

coefplot (motivation1, aseq(Internal) level(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2) recast(rcap))) ///
	(motivation2, aseq(Social) level(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2)  recast(rcap)))  ///
    (motivation3, aseq(Money) level(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2)  recast(rcap))) ///
    (motivation4, aseq(Fines) level(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2)  recast(rcap)))  ///
	(motivation5, aseq(External LoC) level(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2)  recast(rcap))) ///
	(motivation6, aseq(No Point) level(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2)  recast(rcap))) ///
     , nooffsets coeflabels(, wrap(26))	keep(group_2) swapnames drop(_cons) xline(0, lcolor(gs5) lwidth(medium)) msize() legend(off) grid(none) xscale(range(-0.5 0.5)) xlabels(-.75(0.25).75, nogrid labsize()) mlabposition(1) xtitle("Regression estimated impact relative to NM", height(7))  mlabel(cond(@pval<.01, "{bf:***}", cond(@pval<.05, "{bf:**}",  cond(@pval<.10, "{bf:*}", "" )))) title("{bf:B } Motivations to protect forests", span pos(11) size())
gr_edit style.editstyle margin(vsmall) editcopy
gr_edit plotregion1.plot18.style.editstyle marker(fillcolor("100 143 255*1.3")) editcopy
gr_edit plotregion1.plot16.style.editstyle area(linestyle(color("100 143 255*1.3"))) editcopy
gr save  "$working_ANALYSIS/results/intermediate/figure1_b.gph", replace

gr combine "$working_ANALYSIS/results/intermediate/figure1_a.gph" "$working_ANALYSIS/results/intermediate/figure1_b.gph", rows(1) xsize(4) ysize(2) iscale(1.1)
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

coefplot (all_sample, aseq(All Sample) level(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2) recast(rcap)) msymbol() ) ///
	(age_below, aseq(Aged Below 58) level(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2)  recast(rcap)) msymbol() ) ///
    (age_above, aseq(Aged at or Above 58) level(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2)  recast(rcap)) msymbol()) ///
       (female, aseq(Female) level(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2)  recast(rcap)) msymbol() )  ///
	(male, aseq(Male) level(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2)  recast(rcap)) msymbol() )  ///
	(highSES, aseq(High SES) level(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2)  recast(rcap)) msymbol()) ///						
	(lowSES, aseq(Low SES) level(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2)  recast(rcap)) msymbol() )  ///
	(city, aseq(Residence: Urban) level(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2)  recast(rcap)) msymbol() )  ///
	(farm, aseq(Residence: Farm) level(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2)  recast(rcap)) msymbol() )  ///
     , nooffsets coeflabels(, wrap(26))	keep(group_2) swapnames drop(_cons) xline(0, lcolor(gs5) lwidth(medium)) msize() legend(off) grid(none) xscale(range(-0.5 0.5)) xlabels(-5000(5000)15000, nogrid labsize()) mlabposition(1) xtitle("Regression estimated impact relative to NM in Pesos", height(7))  mlabel(cond(@pval<.01, "{bf:***}", cond(@pval<.05, "{bf:**}",  cond(@pval<.10, "{bf:*}", "" )))) mlabsize(7pt) ysize(4) xsize(4)
gr_edit .plotregion1.plot18.style.editstyle marker(fillcolor("100 143 255*1.3")) editcopy
gr_edit .plotregion1.plot16.style.editstyle area(linestyle(color("100 143 255*1.3"))) editcopy
gr_edit .plotregion1.plot21.style.editstyle marker(fillcolor("120 94 240*1.3")) editcopy
gr_edit .plotregion1.plot19.style.editstyle area(linestyle(color("120 94 240*1.3"))) editcopy
gr save  "$working_ANALYSIS/results/intermediate/figure2_heterogeneous_effects.gph", replace
gr export "$working_ANALYSIS/results/figures/figure2_heterogeneous_effects.tif", replace width(4500)


*4.2. Additional support for no crowding out
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
trust_people trust_townpeople trust_familiy responsibility_gov 
iebaltab $balance , grpvar(pes_participation) onerow  rowvarlabels save("$working_ANALYSIS/results/tables/TableS1_balancing") ftest fmissok replace
* joint f-tests no significant differnces in terms of socio-economics and other pre-specified variables potentially influencing intentions and vaccine hesitancy
*non-participants vs. phase 1
reg pes_participation $balance if pes_participation <3
*non-participants vs phase 2
reg pes_participation $balance if pes_participation !=2
*phase 1 vs phase 2
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


*Figure S1.	Bias reduction through Propensity Score Matching (PSM)
*Stage 1: Match
gen treated = 1 if pes_participation > 1
replace treated = 0 if pes_participation == 1
*determine caliper
logit treated farms_n land_size
predict pscore
sum pscore
local caliper = sqrt(r(sd))/4
display `caliper'

*PSM: nearest neighbour matching (n=1) with replacement a caliper of 0.094
psmatch2 treated farms_n land_size, caliper(0.094) n(1) logit common
psgraph
pstest farms_n land_size, both graph legend(rows(1) pos(6) ring(1))
gr save "$working_ANALYSIS/results/intermediate/figureS1_matching_bias_reduction.gph", replace
gr export "$working_ANALYSIS/results/figures/figureS1_matching_bias_reduction.tif", replace width(3465)
tab _n1
tab _treated
* gen matched pair identifier
gen pair = _id if _treated==0 
replace pair = _n1 if _treated==1
bysort pair: egen paircount = count(pair)
foreach x of varlist _weight _id _pscore _pdif _n1 _nn _treated _support pair paircount {
		rename `x'  matched`x'
}
replace matched_treated=. if matchedpaircount < 2
* 42 NM are retained after matching on number of farms and land size. This reduced bias substantially.
* This gives us the average treatment effect on the treated "ATET", as we matched control group members to treated participants and not the other way around


*Table S3.	Donations across groups matched sample
reg donation_n $groups [fw = matched_weight], vce(cluster id)
outreg2 using "$working_ANALYSIS/results/tables/TableS3_donation_amount_matched", addstat("Adjusted R-squared", e(r2_a)) adec(3) drop() word ctitle("Donation Amount") dec(2) replace
reg donation_n $groups $socio_economic [fweight = matched_weight] , vce(cluster id)
testparm $socio_economic
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS3_donation_amount_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n $groups $trust_involvement [fw = matched_weight] , vce(cluster id)
testparm $trust_involvement
local F2 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS3_donation_amount_matched", addstat("Adjusted R-squared", e(r2_a), "Trust & Involvement", `F2') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n $groups $ngo_experience [fw = matched_weight] , vce(cluster id)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS3_donation_amount_matched", addstat("Adjusted R-squared", e(r2_a), "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n $groups $socio_economic $trust_involvement $ngo_experience [fw = matched_weight] , vce(cluster id)
testparm group_2 group_3
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS3_donation_amount_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append



*Tables S4. Motivations to protect forests
*internal motivation
reg motivation1 $groups $socio_economic $trust_involvement $ngo_experience, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS4_motivations", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) replace
*social motivation
reg motivation2 $groups $socio_economic $trust_involvement $ngo_experience, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS4_motivations", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
*Money motivation
reg motivation3 $groups $socio_economic $trust_involvement $ngo_experience, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS4_motivations", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
*Fines motivation
reg motivation4 $groups $socio_economic $trust_involvement $ngo_experience, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS4_motivations", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
*External LoC
reg motivation5 $groups $socio_economic $trust_involvement $ngo_experience, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS4_motivations", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
*No point
reg motivation6 $groups $socio_economic $trust_involvement $ngo_experience, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS4_motivations", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append


*Table S5.	Motivations across groups matched sample
*internal motivation
reg motivation1 $groups $socio_economic $trust_involvement $ngo_experience [fw = matched_weight], vce(cluster id)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS5_motivations_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) replace
*social motivation
reg motivation2 $groups $socio_economic $trust_involvement $ngo_experience [fw = matched_weight], vce(cluster id)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS5_motivations_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
*Money motivation
reg motivation3 $groups $socio_economic $trust_involvement $ngo_experience [fw = matched_weight], vce(cluster id)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS5_motivations_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
*Fines motivation
reg motivation4 $groups $socio_economic $trust_involvement $ngo_experience [fw = matched_weight], vce(cluster id)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS5_motivations_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
*External LoC
reg motivation5 $groups $socio_economic $trust_involvement $ngo_experience [fw = matched_weight], vce(cluster id)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS5_motivations_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
*No point
reg motivation6 $groups $socio_economic $trust_involvement $ngo_experience [fw = matched_weight], vce(cluster id)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS5_motivations_matched", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append




*Table S6.	Correlation of motivations with donation amounts
global motivations motivation1 motivation2 motivation3 motivation4 motivation5 motivation6
reg donation_n group_2 group_3 $motivations $socio_economic $trust_involvement $ngo_experience, vce(hc3)
est store motivate_donate1
testparm $motivations
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS6_motivations_donation", drop($socio_economic $trust_involvement $ngo_experience) addstat("Adjusted R-squared", e(r2_a), "Motivations", `F1') adec(3) word ctitle("Donation Amount") dec(2) replace
reg donation_n $motivations $socio_economic $trust_involvement $ngo_experience if pes_participation==1, vce(hc3)
est store motivate_donate2
testparm $motivations
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS6_motivations_donation", drop($socio_economic $trust_involvement $ngo_experience) addstat("Adjusted R-squared", e(r2_a), "Motivations", `F1') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n $motivations $socio_economic $trust_involvement $ngo_experience if pes_participation==2, vce(hc3)
est store motivate_donate3
testparm $motivations
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS6_motivations_donation", drop($socio_economic $trust_involvement $ngo_experience) addstat("Adjusted R-squared", e(r2_a), "Motivations", `F1') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n $motivations $socio_economic $trust_involvement $ngo_experience if pes_participation==3, vce(hc3)
est store motivate_donate4
testparm $motivations
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS6_motivations_donation", drop($socio_economic $trust_involvement $ngo_experience) addstat("Adjusted R-squared", e(r2_a), "Motivations", `F1') adec(3) word ctitle("Donation Amount") dec(2) append



*Table S7. PES participation effect on donations across socioeconomics characteristics
**average effects
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS7_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) replace
*by SES
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if high_SES == 1, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS7_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if high_SES == 0, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS7_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
*By gender
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if female == 1, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS7_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if female == 0, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS7_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
*by age
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if high_age == 0, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS7_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if high_age == 1, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS7_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
*by residence
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if residence_farm == 0, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS7_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n group_2 group_3 $socio_economic $trust_involvement $ngo_experience if residence_farm == 1, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS7_donations_heterogeneous", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append


*Table S8.	Balancing by recall timing
tab primed pes_participation
iebaltab  $balance , grpvar(primed)  rowvarlabels onerow save("$working_ANALYSIS/results/tables/TableS8_balancing_priming") ftest fmissok replace
reg primed $balance

*Table S9.	Effects of recall on donations
reg donation_n primed group_3 primed_group3, vce(hc3)
outreg2 using "$working_ANALYSIS/results/tables/TableS9_donation_recall", addstat("Adjusted R-squared", e(r2_a)) adec(3) drop() word ctitle("Donation Amount") dec(2) replace
reg donation_n primed group_3 primed_group3 $socio_economic, vce(hc3)
testparm $socio_economic
local F1 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS9_donation_recall", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n primed group_3 primed_group3 $trust_involvement, vce(hc3)
testparm $trust_involvement
local F2 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS9_donation_recall", addstat("Adjusted R-squared", e(r2_a), "Trust & Involvement", `F2') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n primed group_3 primed_group3 $ngo_experience, vce(hc3)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS9_donation_recall", addstat("Adjusted R-squared", e(r2_a), "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append
reg donation_n primed group_3 primed_group3 $socio_economic $trust_involvement $ngo_experience, vce(hc3)
testparm $socio_economic
local F1 = r(p)
testparm $trust_involvement
local F2 = r(p)
testparm $ngo_experience
local F3 = r(p)
outreg2 using "$working_ANALYSIS/results/tables/TableS9_donation_recall", addstat("Adjusted R-squared", e(r2_a), "Socio-economics", `F1', "Trust & Involvement", `F2', "Experience with NGOs", `F3') adec(3) word ctitle("Donation Amount") dec(2) append


*Figure S2.	Predicted donations by received PES payments
reg donation_n c.payment_ppp##i.group_3 $socio_economic $trust_involvement $ngo_experience if payment_rem_d==1, vce(hc3)
margins i.group_3, at(payment_ppp = (100(1000)11000)) vsquish
marginsplot, xla(100 2000 4000 6000 8000 10000, nogrid) title("") xtitle("Total payment (PPP)") yla(0(5000)20000, nogrid) ///
		ytitle("Predicted Donation in Pesos") ///
		recastci(rarea) ciopts(fcolor(%30) lwidth(none)) legend(order(3 "M-TERM" 4 "M") pos(12) ring(0) rows(1)) 
gr save  "$working_ANALYSIS\results\intermediate\figureS2_reciprocity.gph", replace
gr export "$working_ANALYSIS\results\figures\figureS2_reciprocity.tif", replace width(3165)




** EOF