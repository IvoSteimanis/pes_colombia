*--------------------------------------------------------------------------------------
* SCRIPT: 01_clean_generate.do
* PURPOSE: cleans the raw excel data and generates additional variables for analysis
*--------------------------------------------------------------------------------------

*----------------------------
* 1) Import excel dataset and cleaning
*----------------------------
clear
import excel  "$working_ANALYSIS/data/psa_data.xlsx", firstrow

*Correcting data entry errors with help of colombian team
replace farms_n =. if id==96
replace farm1_size =. if id==10 
replace farm1_size =. if id==235
replace farm2_unit =1 if id==221
replace payment_n =. if id==15
replace donation_n = 0 if donation_n==.
replace income_n = 18000 if id==172
replace income_n = 18000 if id==411 & donation_n==2000
replace household_income=. if id==10
replace household_income=. if id==96
replace household_income=500000 if id==97
replace household_income=. if id==142
replace household_income=. if id==226 & treatment_d==2


*redefine farm size data (adjust unit size)
replace farm1_size = farm1_size/10000 if farm1_unit==2
replace farm1_size = farm1_size*0.64 if farm1_unit==3
replace farm1_size = farm1_size*0.64 if farm1_unit==4
replace farm2_size = farm2_size/10000 if farm2_unit==2
replace farm2_size = farm2_size*0.64 if farm2_unit==3
replace farm2_size = farm2_size*0.64 if farm2_unit==4
replace farm2_size =. if farm2_size==0
replace farm3_size = farm3_size/10000 if farm3_unit==2
replace farm3_size = farm3_size*0.64 if farm3_unit==3
replace farm3_size = farm3_size*0.64 if farm3_unit==4
replace farm3_size =. if farm3_size==0
replace farm4_size = farm4_size/10000 if farm4_unit==2
replace farm4_size = farm4_size*0.64 if farm4_unit==3
replace farm4_size = farm4_size*0.64 if farm4_unit==4

*label variables
lab var id "Unique identifier"
lab var id_name "Participant name"
lab var assistant "Name of the assistant performing the survey"
lab var date "Date the survey was taken"
lab var treatment_d "Treatment participants was assigned to"
lab def treat_lab 1 "Treatment 1" 2 "Treatment 2" 4 "Control", replace
lab val treatment_d treat_lab
lab var phase_d "Which phase of the PES did the participant complete?"
lab def phase_lab 1 "Phase 1" 2 "Phase 2" 8 "None", replace
lab val phase_d phase_lab
lab var panel_d "Panel data on motivations available"
lab var place_survey "Place where the survey was performed"
lab var remember_d "remember_d=1 if the participant remembers participating in the PES programme"
lab var land_mun "Municipality on which the land that participated in the programme is located"
lab var payment_rem_d "payment_rem=1 if the participan recalls the amount of payments he/she recived"
lab var payment_n "The number of payments given to the participant during the programme."
lab var payment_t "Total amount of money payed to the participant during the porgramme"
lab var payment_u "For what did the participant use the money"
lab var know_part "know_part=1 if the person knows other people that have participated in the programme"
lab var know_part_n "Number of people that the participan knows that have participated too in the programme"
lab var participate_d "participate_d=1 if the person participated in the programme design or adjustment"
lab var participate_w "Way of Participation in the programme design/adjustment"
lab var participate_o_d "participate_o=1 if the person knows that there are other people still receiving the programme benefits."
lab var selection_op "Opinion on the selection of participants of the programme. (open ended)"
lab var selection_f "Perceived fairness of participant selection process."
lab def selection_lab 1 "Not fair" 2 "" 3 "" 4 "totally fair" 5 "Don't know", replace
lab val selection_f selection_lab
lab var p_satisfaction "Satisfaction with the programme."
lab def satisfaction_lab 1 "Not satisfied" 2 "" 3 "" 4 "totally satisfied" 5 "Don't know", replace
lab val p_satisfaction satisfaction_lab
lab var p_people_d "Knows people currently in the pregramme"
lab var p_people_n "Number of people known who are currently in the programme."
lab var donation_d "donation_d=1 if the person wants to do a donation"
lab var donation_org "Donation Organisation"
lab def donation_lab 1 "TropenBos" 2 "Gaia" 3 "Omacha" 4 "Random", replace
lab val donation_org donation_lab
lab var donation_org_r "Randomly chosen Donation Organisation."
lab def donation_rdm_lab 1 "TrompenBos" 2 "Gaia" 3 "Omacha", replace
lab val donation_org_r donation_rdm_lab
lab var donation_n "Amount donated by the person to the selected organisation"
lab var donation_tot "Amount the organisation receives as a donation = (donation_n *2)"
lab var income_n "Amount of money the person keeps after donation."
lab var tropenbos_d "tropenbos_d=1 if the person knows about TropenBos"
lab var tropenbos_1 "What does the person knows about TropenBos?"
lab var gaia_d "gaia_d=1 if the person knows about Gaia"
lab var gaia_1 "What does the person knows about Gaia?"
lab var omacha_d "omacha_d=1 if the person knows about Omacha"
lab var omacha_1 "What does the person knows about Omacha?"
lab var donation_ngo "Donation to NGO"
lab def donation_lab 1 "Never" 2 "Once" 3 "Many times", replace
lab val donation_ngo donation_lab
lab var donation_all "donation_all=1 if the person donated all th money given to him/her"
lab var donation_40k_d "donation_40k_d=1 if they would have donated all if given 40000 instead of 20000"
lab var donation_80k_d "donation_80k_d=1 if they would have donated all if given 80000 instead of 20000"
lab var donation_100k_d "donation_100k_d=1 if they would have donated all if given 100000 instead of 20000"
lab var regret_forest "Regret if I damage the forest"
lab def agree_disagree 1 "Totally disagree" 2 "Disagree" 3 "Agree" 4 "Totally agree", replace
lab val regret_forest agree_disagree
lab var deforestation "There is very little we can do to reduce deforestation."
lab val deforestation agree_disagree
lab var farms_n "Total number of farms owned by the person"
lab var farm1_size "Size of farm 1"
lab var farm1_unit "Units of famr1_size"
lab def unit_lab 1 "ha" 2 "m2" 3 "fanegada" 4 "cuadras" 5 "Don't know", replace
lab val farm1_unit unit_lab
lab var farm2_size "Size of farm 2"
lab var farm2_unit "Units of famr2_size"
lab val farm2_unit unit_lab
lab var farm3_size "Size of farm 3"
lab var farm3_unit "Units of famr3_size"
lab val farm3_unit unit_lab
lab var farm4_size "Size of farm 4"
lab var farm4_unit "Units of famr4_size"
lab val farm4_unit unit_lab
lab var cut_trees_d "cut_trees_d=1 if the person  has cut trees or forest after leaving the programme"
lab var cut_trees_n "amount of trees or ha of forest cut"
lab var cut_trees_u "Units of cut_trees_n"
lab var enjoy_care "Joy from taking care of the Forest"
lab val enjoy_care agree_disagree
lab var polspace "Political and community participation"
lab val polspace agree_disagree
lab var residence "Current Residence of the participant"
lab def residence_lab 1 "Farm (payments)" 2 "City" 3 "Town" 4 "Other farm(no payments)", replace
lab val residence residence_lab
lab var neighbors "The people closest to me would be upset with me if I knock down the forest."
lab val neighbors agree_disagree
lab var econ_activity "Main economic activity"
lab def income_lab 1 "Agriculture" 2 "Cattlemen" 3 "Merchant" 4 "Employee" 5 "Laborer" 6 "Other", replace
lab val econ_activity income_lab
lab var econ_activity_6 "Economic activity of the person if econ_activity==6 (Other)"
lab var econ_depend "Number of persons economically dependent of him/her"
lab var control_publicw "Control over public workers"
lab val control_publicw agree_disagree
lab var id_age "Age of the participant"
lab var id_sex "id_sex=1 woman"
lab var proud_forest "I feel proud to take care of the forest."
lab val proud_forest agree_disagree
lab var id_educ_household "The highest level of education achieved by the people in the household"
lab def edu_lab 1 "None" 2 "Incomplete primary" 3 "Complete primary" 4 "Incomplete secondary" 5 "Complete secondary" 6 "Incomplete technical" 7 "Complete technical" 8 "Incomplete professional" 9 "Complete professional" 10 "Incomplete graduate" 11 "Complete graduate", replace
lab val id_educ_household edu_lab
lab var id_educ "The highest level of education achieved by the person"
lab val id_educ edu_lab
lab var care_forest "Take care of the forest only if paid."
lab val care_forest agree_disagree
lab var civicorg_d "Civic Organisation Participation (Dummy)"
lab var civicorg "Description of which civic organisation the person participates in"
lab var civicorg_y "Year since the person has been participating in the civic organisation."
lab var civicorg_part "Civic Organisation Participation"
lab def org_lab 1 "very little" 2 "a little" 3 "actively" 4 "very actively", replace
lab val civicorg_part org_lab
lab var pruductiveorg_d "Productive Organisation Participation (Dummy)"
lab var pruductiveorg "Description of which productive organisation the person participates in"
lab var pruductiveorg_y "Year since the person has been participating in the productive organisation."
lab var pruductiveorg_part "Productive Organisation Participation"
lab val pruductiveorg_part org_lab
lab var guilt_forest "Guilt from damaging the forest"
lab val guilt_forest agree_disagree
lab var neighbors_critic "My neighbors would criticize me if I damage the forest."
lab val neighbors_critic agree_disagree
lab var household_income "Household monthly income in COP"
lab var responsibility_gov "I believe that in the long term, citizens are responsible for bad government at national and local levels"
lab val responsibility_gov agree_disagree
lab var activity_income "Activity that produces the highest income"
lab var fine_forest "I do not damage the forest for fear of fines that the environmental authority can give me."
lab val fine_forest agree_disagree
lab var noprotect_forest "There is no point in protecting the forest."
lab val noprotect_forest agree_disagree
lab var trust_people "You can trust most people."
lab def likert5 1 "Strongly disagree" 2 "Disagree" 3 "Neither" 4 "Agree" 5 "Strongly agree", replace
lab val trust_people likert5
lab var trust_townpeople "Trust in people from town/neighborhood"
lab val trust_townpeople likert5
lab var trust_familiy "Trust people from family only"
lab val trust_familiy likert5
lab var community_activ "Participation in Community activities"
lab def ca_lab 1 "Never" 2 "Everyday" 3 "Once a week" 4 "Once a month" 5 "Couple times a year", replace
lab val community_activ ca_lab
lab var community_activ_1 "community_activ_1=1 if the participation mas VOLUNTARY"
lab var community_activ_2 "community_activ_2=1 if the participation mas VOLUNTARY"
lab var community_activ_3 "community_activ_3=1 if the participation mas VOLUNTARY"
lab var id_cedula "Cedula (National ID)"
lab var know_nonpart "know_nonpart=1 if he/she knows a person with farms/land that have not participated in the program"
lab var know_prog_d "know_prog_d=1 if the person knows the programme"
lab var know_prog "What does the person knows about the programme (open text)"
lab var apply_prog_d "apply_prog_d=1 if the person applied to the programme"
lab var prog_selection "Reason why the person did not get selected into the programme, according to them. (open text)"
lab var noapply_prog "Reason for not applying to the programme"
lab def reason_lab 1 "Did not know" 2 "Did not meet requirements" 3 "Application process very difficult" 4 "found out too late" 5 "no trust" 6 "not interestest" 7 "other"
lab var noapply_prog_1  "Reason for not applying to the programme: the person did not know the programme"
lab var noapply_prog_2  "Reason for not applying to the programme: the person did not meet the requirements"
lab var noapply_prog_3  "Reason for not applying to the programme: 'The application or the process was very difficult'"
lab var noapply_prog_4  "Reason for not applying to the programme: the person found out when it was too late."
lab var noapply_prog_5  "Reason for not applying to the programme: the person did NOT trust the programme"
lab var noapply_prog_6  "Reason for not applying to the programme: the person was NOT interested in the programme"
lab var noapply_prog_7  "Reason for not applying to the programme: Other"




*----------------------------------
* 2) Generate additional variables
*----------------------------------
*Generate unique identifier
drop id // not unique
gen id = _n

*PES participation dummies
gen participant=0
replace participant=1 if phase_d==1 | phase_d==2

gen phase1 =0
replace phase1=1 if phase_d==1

gen phase2 =0
replace phase2 =1 if phase_d==2

gen pes_participation = 1 if phase_d ==8
replace pes_participation = 2 if phase_d == 1
replace pes_participation = 3 if phase_d == 2
lab def pes_part 1 "NM" 2 "MT" 3 "MR", replace
lab val pes_participation pes_part
tab pes_participation, gen(group_)
lab var group_1 "NM"
lab var group_2 "MT"
lab var group_3 "MR"

gen treated = 0
replace treated = 1 if pes_participation > 1

*Treatment priming dummy
gen primed =0
replace primed =1 if treatment_d==1
replace primed = . if pes_participation == 1
lab def prime_lab 0 "Control" 1 "Treatment", replace
lab val primed prime_lab

*interaction terms with group
gen primed_group2=primed*group_2
gen primed_group3=primed*group_3


*donation in percent
gen donation_percent = (donation_n*100) / 20000
replace donation_percent = 0 if donation_n == 0


*total land size if farmer has more farms
gen land_size = farm1_size if farms_n==1
replace land_size = farm1_size + farm2_size  if farms_n==2
replace land_size = farm1_size + farm2_size + farm3_size  if farms_n==3
replace land_size = farm1_size + farm2_size + farm3_size + farm4_size  if farms_n>=4

*female dummy
gen female =0
replace female =1 if id_sex==1

*education dummy
gen max_primary = 0 
replace max_primary = 1 if id_educ < 4
lab var max_primary "Participant completed up to primary school but not more."

*collective action in community
gen collective_action = 1 if community_activ==1
replace collective_action = 2 if community_activ==5
replace collective_action = 3 if community_activ==4
replace collective_action = 4 if community_activ==3
replace collective_action = 5 if community_activ==2
lab def collective_lab 1 "Never" 2 "Couple times a year" 3 "Once a month" 4 "Once a week" 5 "Everyday", replace
lab val collective_action collective_lab
gen never_collective = 0
replace never_collective = 1 if collective_action == 1
lab var never_collective "=1 if participant never participates in community activities."
*interaction phase and treatment
gen phase1primed = phase1*primed

gen phase2primed = phase2*primed

*dummies for all professions and education levels
gen agricultor =0
replace agricultor =1 if econ_activity ==1
gen cattlemen =0
replace cattlemen =1 if econ_activity ==2
gen merchant =0
replace merchant =1 if econ_activity ==3
gen employee =0
replace employee =1 if econ_activity ==4
gen laborer =0
replace laborer =1 if econ_activity ==5
gen othereconactivity =0
replace othereconactivity =1 if econ_activity ==6
gen housholdnoeducation =0
replace housholdnoeducation =1 if id_educ_household==1
gen housholdincompleteprim =0
replace housholdincompleteprim =1 if id_educ_household==2
gen housholdcompleteprim =0
replace housholdcompleteprim =1 if id_educ_household==3
gen housholdincompletesec =0
replace housholdincompletesec =1 if id_educ_household==4
gen housholdcompletesec =0
replace housholdcompletesec =1 if id_educ_household==5
gen housholdincompletetec =0
replace housholdincompletetec =1 if id_educ_household==6
gen housholdcompletetec =0
replace housholdcompletetec =1 if id_educ_household==7
gen housholdincompleteprof =0
replace housholdincompleteprof =1 if id_educ_household==8
gen housholdcompleteprof =0
replace housholdcompleteprof =1 if id_educ_household==9
gen housholdincompletegrad =0
replace housholdincompletegrad =1 if id_educ_household==10
gen housholdcompletegrad =0
replace housholdcompletegrad =1 if id_educ_household==11
gen noeducation =0
replace noeducation =1 if id_educ==1
gen incompleteprim =0
replace incompleteprim =1 if id_educ==2
gen completeprim =0
replace completeprim =1 if id_educ==3
gen incompletesec =0
replace incompletesec =1 if id_educ==4
gen completesec =0
replace completesec =1 if id_educ==5
gen incompletetec =0
replace incompletetec =1 if id_educ==6
gen completetec =0
replace completetec =1 if id_educ==7
gen incompleteprof =0
replace incompleteprof =1 if id_educ==8
gen completeprof =0
replace completeprof =1 if id_educ==9
gen incompletegrad =0
replace incompletegrad =1 if id_educ==10
gen completegrad =0
replace completegrad =1 if id_educ==11

*donation experience
gen donnever=0
replace donnever = 1 if donation_ngo==1
gen dononce=0
replace dononce = 1 if donation_ngo==2
gen donmany=0
replace donmany = 1 if donation_ngo==3
gen knowsNGO =0
replace knowsNGO =1 if tropenbos_d==1 | gaia_d==1 | omacha_d==1
gen donatedbefore =0
replace donatedbefore=1 if donation_ngo==2 | donation_ngo==3


*dummies on selection fairness
gen selectionnotfair =0
replace selectionnotfair = 1 if selection_f==1
gen selectionnotveryfair =0
replace selectionnotveryfair = 1 if selection_f==2
gen selectionfair =0
replace selectionfair = 1 if selection_f==3
gen selectiontotallyfair =0
replace selectiontotallyfair = 1 if selection_f==4
gen selectionna=0
replace selectionna = 1 if selection_f==5
gen satisfactionnotatall=0
replace satisfactionnotatall = 1 if p_satisfaction==1
gen satisfactionnot=0
replace satisfactionnot = 1 if p_satisfaction==2
gen satisfactionyes=0
replace satisfactionyes = 1 if p_satisfaction==3
gen satisfactiontotally=0
replace satisfactiontotally = 1 if p_satisfaction==4
gen satisfactionna=0
replace satisfactionna = 1 if p_satisfaction==5

*pooled
gen satisfied =0
replace satisfied=1 if p_satisfaction ==3 | p_satisfaction ==4
gen fairselection =0
replace fairselection=1 if selection_f ==3 | selection_f ==4


*dummies for the organisations where subjects want to donate
gen TropenBos=0
replace TropenBos=1 if donation_org==1
gen Gaia=0
replace Gaia=1 if donation_org==2
gen Omacha=0
replace Omacha=1 if donation_org==3
gen Any=0
replace Any=1 if donation_org==4

*pooling different education levels to basic and high education
gen basiceducation =0
replace basiceducation =1 if id_educ <=5
gen higheducation =0
replace higheducation =1 if id_educ >=6
gen higheducationprimed = higheducation*primed
gen basiceductionprimed = basiceducation*primed

*check income levels and convert to PPP
sum household_income, detail
*PPP conversion factor from World Bank for Colombia in 2018 = 1322,16
gen income_hh_ppp = household_income / 1322.16
lab var income_hh_ppp "PPP adjusted monthly household income"

*variables for sample splits:socioeconomic status, age
egen aux_income_hh_ppp = median(income_hh_ppp)
gen high_SES = 1 if income_hh_ppp >= aux_income_hh_ppp
replace high_SES = 0 if income_hh_ppp < aux_income_hh_ppp
lab var high_SES "Above median household income." 
drop aux_income_hh_ppp

egen aux_id_age = median(id_age)
gen high_age = 1 if id_age >= aux_id_age
replace high_age = 0 if id_age < aux_id_age
lab var high_age "Aged above median." 
drop aux_id_age

* PES payments per instalment
gen amount_per_payment = payment_t / payment_n
gen amount_per_payment_ppp = amount_per_payment / 1322.16
gen payment_ppp = payment_t / 1322.16

*current residence
gen residence_farm = 0
replace residence_farm = 1 if residence == 1 | residence == 4

*motivations to protect the forest
*intrinsic: enjoy_care guilt_forest regret_forest proud_forest
*social: neighbors_critic neighbors 
*external(payments): care_forest
*external (fines): fine_forest
*a-motivated: noprotect_forest
*LoC: deforestation
alpha guilt_forest enjoy_care regret_forest proud_forest,  gen(motivation1) // alpha=0.72
alpha neighbors_critic neighbors, gen(motivation2)   // alpha=0.57
rename care_forest motivation3
rename fine_forest motivation4
rename deforestation motivation5
rename noprotect_forest motivation6

*standardize donations
foreach x of varlist motivation1 motivation2 motivation3 motivation4 motivation5 motivation6 {
	egen  z_`x' = std(`x')
}

*replace misssing values with median values for respective group_
bys pes_participation: sum land_size farms_n income_hh_ppp, detail
replace land_size = 12 if land_size==. & pes_participation==2
replace land_size = 25 if land_size==. & pes_participation==3
replace farms_n = 1 if farms_n==. & pes_participation==2
replace farms_n = 2 if farms_n==. & pes_participation==3
replace income_hh_ppp =  642.8874 if income_hh_ppp==. & pes_participation==2
replace income_hh_ppp =  907.6057 if income_hh_ppp==. & pes_participation==3



* Save dataset
save "$working_ANALYSIS/processed/pes_ready.dta", replace




 
** EOF