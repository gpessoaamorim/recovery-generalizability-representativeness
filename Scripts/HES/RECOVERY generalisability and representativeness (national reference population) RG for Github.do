*Author: Raph Goldacre (last updated 5 Dec 2022)

use "[local filepath]\COVID_allrecs_2015jan_2022jun.dta", clear

*Identify spells
gen stillin = .
replace stillin = 0 if spelend == "Y"
replace stillin = 1 if spelend == "N"
sort hesid admidate epiorder stillin
order hesid admidate epiorder stillin epistart epiend
gen long idspell = 1 if epiorder == 1
replace idspell = sum(idspell)
egen long id = group(hesid)
replace disdate = epiend if disdate == . | disdate <= 0
bysort idspell: egen disdatespell = max(disdate)
sort id admidate epiorder disdate

*Identify transfers
cap drop transadmit
gen transadmit = 0
replace transadmit = 1 if (admisorc == 51 | admisorc == 52 | admisorc == 87) & epiorder == 1
replace transadmit = 1 if admimeth == "2B" | admimeth == "81" | admimeth == "28" & epiorder == 1

*RECORD ORDER per person
cap drop recorder
bysort id (epistart epiorder transadmit): gen recorder = _n

*Find randomisation episodes and spells
gen randepi = 0
replace randepi = 1 if epistart <= rand_date & epiend >= rand_date
bysort hesid (recorder): gen randepiorder = cond(randepi == 1, sum(randepi), 0)
bysort idspell: egen randspell = max(randepi) 

*COVID spells
gen covid = 0
foreach var of varlist diag_01 {
 replace covid = 1 if `var' >= "U071" & `var' <= "U072"
}
bysort hesid: egen covidmax = max(covid)
gen covid_anydiag = 0
foreach var of varlist diag_01-diag_20 {
 replace covid_anydiag = 1 if `var' >= "U071" & `var' <= "U072"
}
bysort hesid: egen covid_anydiagmax = max(covid_anydiag)
bysort hesid (recorder): gen covidorder = cond(covid == 1, sum(covid), 0)
gen U071 = 0
foreach var of varlist diag_01-diag_20 {
 replace U071 = 1 if `var' == "U071"
}
bysort idspell: egen U071max = max(U071)
gen U072 = 0
foreach var of varlist diag_01-diag_20 {
 replace U072 = 1 if `var' == "U072"
}
bysort idspell: egen U072max = max(U072)

gen U072only = 0
replace U072only = 1 if U072max == 1 & U071max == 0

*Assign index date
gen index = .
replace index = 1 if covidorder == 1
replace index = 2 if randepiorder == 1 & covidmax == 0 & rand_date <= 22614
bysort idspell: egen indexspell = max(index)
gen indexdatetemp = .
replace indexdatetemp = epistart if covidorder == 1
replace indexdatetemp = epistart if randepiorder == 1 & covidmax == 0
bysort hesid: egen indexdate = max(indexdatetemp)
bysort idspell: egen indexdisdatetemp = max(disdatespell) if indexspell == 1
bysort hesid: egen indexdisdate = max(indexdisdatetemp)
foreach var of varlist indexdatetemp indexdate indexdisdatetemp indexdisdate {
 format `var' %td
}

*AGE
gen agegrp = .
replace agegrp = 1 if age >= 7000 & age <= 7009
replace agegrp = 1 if inrange(age,0,4)
replace agegrp = 2 if inrange(age,5,9)
replace agegrp = 3 if inrange(age,10,14)
replace agegrp = 4 if inrange(age,15,19)
replace agegrp = 5 if inrange(age,20,24)
replace agegrp = 6 if inrange(age,25,29)
replace agegrp = 7 if inrange(age,30,34)
replace agegrp = 8 if inrange(age,35,39)
replace agegrp = 9 if inrange(age,40,44)
replace agegrp = 10 if inrange(age,45,49)
replace agegrp = 11 if inrange(age,50,54)
replace agegrp = 12 if inrange(age,55,59)
replace agegrp = 13 if inrange(age,60,64)
replace agegrp = 14 if inrange(age,65,69)
replace agegrp = 15 if inrange(age,70,74)
replace agegrp = 16 if inrange(age,75,79)
replace agegrp = 17 if inrange(age,80,84)
replace agegrp = 18 if inrange(age,85,89)
replace agegrp = 19 if inrange(age,90,94)
replace agegrp = 20 if inrange(age,95,105)
label define agegrp 1 "0" 1 "1-4" 2 "5-9" 3 "10-14" 4 "15-19" 5 "20-24" 6 "25-29" 7 "30-34" 8 "35-39" 9 "40-44" 10 "45-49" 11 "50-54" 12 "55-59" 13 "60-64" 14 "65-69" 15 "70-74" 16 "75-79" 17 "80-84" 18 "85-89" 19 "90-94" 20 "95+"
label values agegrp agegrp

*SEX
replace sex = . if sex <1 | sex >2

*ETHNICITY
***gen ethgrp code based on group mode (excl where group mode is 'unknown'). If 2+ group modes, priotise least common group (i.e. Mixed, Other, Black, Asian, then White) ***
cap drop ethcode
gen ethcode = .
replace ethcode = 1 if ethnos == "A"
replace ethcode = 2 if ethnos == "B"
replace ethcode = 3 if ethnos == "C"
replace ethcode = 4 if ethnos == "D"
replace ethcode = 5 if ethnos == "E"
replace ethcode = 6 if ethnos == "F"
replace ethcode = 7 if ethnos == "G"
replace ethcode = 8 if ethnos == "H"
replace ethcode = 9 if ethnos == "J"
replace ethcode = 10 if ethnos == "K"
replace ethcode = 11 if ethnos == "L"
replace ethcode = 12 if ethnos == "M"
replace ethcode = 13 if ethnos == "N"
replace ethcode = 14 if ethnos == "P"
replace ethcode = 15 if ethnos == "R"
replace ethcode = 16 if ethnos == "S"
replace ethcode = . if ethnos == "Z"
replace ethcode = . if ethnos == "99"
gen ethvalid = 0
replace ethvalid = 1 if ethcode >= 1 & ethcode <= 16
bysort hesid: egen ethvalidmax = max(ethvalid)
gen ethgrp = .
replace ethgrp = 1 if ethnos == "A" | ethnos == "B" | ethnos == "C"
replace ethgrp = 2 if ethnos == "H" | ethnos == "J" | ethnos == "K" | ethnos == "L" | ethnos == "R"
replace ethgrp = 3 if ethnos == "M" | ethnos == "N" | ethnos == "P"
replace ethgrp = 4 if ethnos == "S"
replace ethgrp = 5 if ethnos == "D" | ethnos == "E" | ethnos == "F" | ethnos == "G"
bysort hesid: egen ethgrpmodemax = mode(ethgrp), maxmode
cap drop ethgrpfinal
gen ethgrpfinal = .
replace ethgrpfinal = ethgrpmodemax
replace ethgrpfinal = 99 if ethvalidmax == 0
label define ethgrpfinal 1 "White" 2 "Asian" 3 "Black" 4 "Other" 5 "Mixed" 99 "Unknown"
drop ethgrp
rename ethgrpfinal ethgrp
cap drop label ethgrp
label define ethgrp 1 "White" 2 "Asian" 3 "Black" 4 "Other" 5 "Mixed" 99 "Unknown"
label values ethgrp ethgrp

*CHARLSON
gen Cancer = 0
gen Lymphoma = 0
gen Leukaemia = 0
gen Cerebrovascular = 0
gen Chronic_pulmonary = 0
gen Congestive_HF = 0
gen Dementia = 0
gen Diabetes_with_comp = 0
gen Diabetes_without_comp = 0
gen Severe_Liver = 0
gen HIV = 0
gen Mild_Liver = 0
gen Hemi_paraplegia = 0
gen Metastasis = 0
gen MI = 0
gen Peptic_ulcer = 0
gen Periph_vasc = 0
gen Renal = 0
gen Rheumatic = 0

foreach var of varlist diag_01-diag_20 {
replace Cancer  = 1 if (inrange(`var',"C00","C769") | inrange(`var',"C86","C909") | inrange(`var',"C96","C979")) & disdate > indexdate-1826 & disdate <indexdate
}
foreach var of varlist diag_01-diag_20 {
replace Leukaemia  = 1 if (inrange(`var',"C91","C959")) & disdate > indexdate-1826 & disdate <indexdate
}
foreach var of varlist diag_01-diag_20 {
replace Lymphoma = 1 if (inrange(`var',"C81","C859")) & disdate > indexdate-1826 & disdate <indexdate
}
foreach var of varlist diag_01-diag_20 {
replace Cerebrovascular = 1 if (inrange(`var',"I60","I699") | inrange(`var',"G45","G469") | inrange(`var',"H34","H349")) & disdate > indexdate-1826 & disdate <indexdate 
}
foreach var of varlist diag_01-diag_20 {
replace Chronic_pulmonary = 1 if (inrange(`var',"I278","I279") | inrange(`var',"J400","J479") | inrange(`var',"J600","J679") | inrange(`var',"J684","J684") | inrange(`var',"J701","J701") | inrange(`var',"J703","J703")) & disdate > indexdate-1826 & disdate <indexdate
}
foreach var of varlist diag_01-diag_20 {
replace Congestive_HF = 1 if (inrange(`var',"I500","I509") | inrange(`var',"I099","I099") | inrange(`var',"I110","I110") | inrange(`var',"I130","I130") | inrange(`var',"I132","I132") | inrange(`var',"I255","I255") | inrange(`var',"I420","I420") | inrange(`var',"I425","I429") | inrange(`var',"I430","I439") | inrange(`var',"P290","P299")) & disdate > indexdate-1826 & disdate <indexdate
}
foreach var of varlist diag_01-diag_20 {
replace Dementia = 1 if (inrange(`var',"F000","F039") | inrange(`var',"F051","F051") | inrange(`var',"G300","G309") | inrange(`var',"G311","G311")) & disdate > indexdate-1826 & disdate <indexdate
}
foreach var of varlist diag_01-diag_20 {
replace Diabetes_with_comp = 1 if (inrange(`var',"E102","E105") | inrange(`var',"E107","E107") | inrange(`var',"E112","E115") | inrange(`var',"E117","E117") | inrange(`var',"E122","E125") | inrange(`var',"E127","E127") | inrange(`var',"E132","E135") | inrange(`var',"E137","E137") | inrange(`var',"E142","E145") | inrange(`var',"E147","E147")) & disdate > indexdate-1826 & disdate <indexdate
}
foreach var of varlist diag_01-diag_20 {
replace Diabetes_without_comp = 1 if (inrange(`var',"E100","E101") | inrange(`var',"E106","E109") | inrange(`var',"E110","E111") | inrange(`var',"E116","E119") | inrange(`var',"E120","E121") | inrange(`var',"E126","E129") | inrange(`var',"E130","E131") | inrange(`var',"E136","E139") | inrange(`var',"E140","E141") | inrange(`var',"E146","E149")) & disdate > indexdate-1826 & disdate <indexdate
}
foreach var of varlist diag_01-diag_20 {
replace Hemi_paraplegia = 1 if (inrange(`var',"G810","G829") | inrange(`var',"G830","G834") | inrange(`var',"G839","G839")) & disdate > indexdate-1826 & disdate <indexdate
}
foreach var of varlist diag_01-diag_20 {
replace Metastasis = 1 if (inrange(`var',"C770","C809")) & disdate > indexdate-1826 & disdate <indexdate
}
foreach var of varlist diag_01-diag_20 {
replace MI = 1 if (inrange(`var',"I210","I229") | inrange(`var',"I252","I252")) & disdate > indexdate-1826 & disdate <indexdate
}
foreach var of varlist diag_01-diag_20 {
replace Peptic_ulcer = 1 if (inrange(`var',"K250","K289")) & disdate > indexdate-1826 & disdate <indexdate
}
foreach var of varlist diag_01-diag_20 {
replace Periph_vasc = 1 if (inrange(`var',"I700","I719") | inrange(`var',"I731","I731") | inrange(`var',"I738","I739") | inrange(`var',"I771","I771") | inrange(`var',"I790","I790") | inrange(`var',"I792","I792") | inrange(`var',"K551","K551") | inrange(`var',"K558","K559") | inrange(`var',"Z958","Z958")) & disdate > indexdate-1826 & disdate <indexdate
}
foreach var of varlist diag_01-diag_20 {
replace Renal = 1 if (inrange(`var',"I120","I120") | inrange(`var',"I131","I132") | inrange(`var',"N032","N037") | inrange(`var',"N052","N057") | inrange(`var',"N180","N199") | inrange(`var',"N250","N250") | inrange(`var',"Z490","Z492") | inrange(`var',"Z940","Z940") | inrange(`var',"Z992","Z992")) & disdate > indexdate-1826 & disdate <indexdate
}
foreach var of varlist diag_01-diag_20 {
replace Severe_Liver = 1 if (inrange(`var',"I850","I850") | inrange(`var',"I859","I859") | inrange(`var',"I864","I864") | inrange(`var',"I982","I982") | inrange(`var',"K704","K704") | inrange(`var',"K711","K711") | inrange(`var',"K721","K721") | inrange(`var',"K729","K729") | inrange(`var',"K765","K765")) & disdate > indexdate-1826 & disdate <indexdate
}
/*
foreach var of varlist diag_01-diag_20 {
replace HIV = 1 if (inrange(`var',"B210","B229") | inrange(`var',"B240","B249"))  & disdate > indexdate-1826 & disdate <indexdate
}
*/
foreach var of varlist diag_01-diag_20 {
replace Mild_Liver = 1 if (inrange(`var',"B180","B189") | inrange(`var',"K70","K704") | inrange(`var',"K709","K709") | inrange(`var',"K710","K710") | inrange(`var',"K713","K715") | inrange(`var',"K717","K717") | inrange(`var',"K73","K749") | inrange(`var',"K760","K760") | inrange(`var',"K762","K764") | inrange(`var',"K768","K769") | inrange(`var',"Z944","Z944"))  & disdate > indexdate-1826 & disdate <indexdate
}
foreach var of varlist diag_01-diag_20 {
replace Rheumatic = 1 if (inrange(`var',"M050","M069") | inrange(`var',"M315","M315") | inrange(`var',"M320","M349") | inrange(`var',"M351","M351") | inrange(`var',"M353","M353") | inrange(`var',"M360","M360")) & disdate > indexdate-1826 & disdate <indexdate
}

foreach var of varlist Cancer-Rheumatic {
bysort hesid: egen `var'max = max(`var')
}

foreach var of varlist Cancer-Rheumatic {
replace `var' = `var'max
}
drop Cancermax-Rheumaticmax

replace Cancer = 0 if Metastasis == 1
replace Mild_Liver = 0 if Severe_Liver == 1
replace Diabetes_without_comp = 0 if Diabetes_with_comp == 1

order Metastasis HIV Severe_Liver Hemi_paraplegia Renal Diabetes_with_comp Cancer Leukaemia Lymphoma Congestive_HF Cerebrovascular Chronic_pulmonary Rheumatic Diabetes_without_comp Peptic_ulcer Dementia Periph_vasc Mild_Liver

gen Metastasis_c = 0
replace Metastasis_c = 6 if Metastasis == 1
gen HIV_c = 0
replace HIV_c = 6 if HIV == 1
 
gen Severe_Liver_c = 0
replace Severe_Liver_c = 3 if Severe_Liver == 1

foreach var of varlist Hemi_paraplegia Renal Diabetes_with_comp Cancer Leukaemia Lymphoma {
gen `var'_c = 0
}
foreach var of varlist Hemi_paraplegia Renal Diabetes_with_comp Cancer Leukaemia Lymphoma {
replace `var'_c = 2 if `var' == 1
}

foreach var of varlist Congestive_HF Cerebrovascular Chronic_pulmonary Rheumatic Diabetes_without_comp Peptic_ulcer Dementia Mild_Liver Periph_vasc MI {
gen `var'_c = 0
}
foreach var of varlist Congestive_HF Cerebrovascular Chronic_pulmonary Rheumatic Diabetes_without_comp Peptic_ulcer Dementia Mild_Liver Periph_vasc MI {
replace `var'_c = 1 if `var' == 1
}

*Charlson score
gen ageindex = .
replace ageindex = agegrp if index == 1
gen agescore = 0
replace agescore = 1 if agegrp>= 11 & agegrp<= 12
replace agescore = 2 if agegrp>= 13 & agegrp<= 14
replace agescore = 3 if agegrp>= 15 & agegrp<= 16
replace agescore = 4 if agegrp>= 17 & agegrp<= 20

cap drop charl*
egen charlson_score = rowtotal(Metastasis_c-MI_c)
cap drop charlgrp
gen charlgrp = charl
replace charlgrp = 3 if charlson >= 3 & charlson <=5
replace charlgrp = 4 if charlson >= 6
egen charlson_score_age = rowtotal(Metastasis_c-MI_c agescore)
gen charlgrp_age = charlson_score_age
replace charlgrp_age  = 3 if charlson_score_age >= 3 & charlson_score_age <=5
replace charlgrp_age  = 4 if charlson_score_age >= 6
label values charlgrp charlgrp_age 


*RRT RULE 2
gen acute = 0
foreach var of varlist diag_01-diag_20 {
replace acute = 1 if `var' >= "N17" & `var' <= "N179"
replace acute = 1 if `var' >= "N170" & `var' <= "N179"
}
bysort idspell: egen acutespell = max(acute)

gen rrt2 = 0
foreach var of varlist diag_01-diag_20 {
replace rrt2 = 1 if `var' >= "Z492" & `var' <= "Z492z" & acutespell == 0
}
foreach var of varlist opertn_01-opertn_24 {
replace rrt2 = 1 if `var' >= "X411" & `var' <= "X411z" & acutespell == 0
}
foreach var of varlist opertn_01-opertn_24 {
replace rrt2 = 1 if `var' >= "X402" & `var' <= "X402z" & acutespell == 0
}
foreach var of varlist opertn_01-opertn_24 {
replace rrt2 = 1 if `var' >= "X405" & `var' <= "X405z" & acutespell == 0
}
foreach var of varlist opertn_01-opertn_24 {
replace rrt2 = 1 if `var' >= "X406" & `var' <= "X406z" & acutespell == 0
}
bysort hesid: egen rrt2max = max(rrt2)
gen rrt2date = admidate if rrt2 == 1 & admidate < indexdate & disdate > indexdate-1826
bysort hesid: egen rrt2datemin = min(rrt2date)

*RRT RULE 3
gen rrt3temp = 0
foreach var of varlist diag_01-diag_20 {
replace rrt3temp = 1 if `var' == "E853"
replace rrt3temp = 1 if `var' == "Y602"
replace rrt3temp = 1 if `var' == "Y612"
replace rrt3temp = 1 if `var' == "Y622"
replace rrt3temp = 1 if `var' == "Y841"
replace rrt3temp = 1 if `var' == "Z992"
replace rrt3temp = 1 if `var' == "T824"
replace rrt3temp = 1 if `var' == "Z491"
}
foreach var of varlist opertn_01-opertn_24 {
replace rrt3temp = 1 if `var' == "X401"
replace rrt3temp = 1 if `var' == "X403"
replace rrt3temp = 1 if `var' == "X404"
}
gen rrt3datetemp = admidate if rrt3temp == 1 & admidate < indexdate & disdate > indexdate-1826

gen esrdorfist = 0
foreach var of varlist diag_01-diag_20 {
replace esrd = 1 if `var' == "N180"
replace esrd = 1 if `var' == "N185"
replace esrd = 1 if `var' == "Q601"
}

foreach var of varlist opertn_01-opertn_24 {
replace esrd = 1 if `var' == "L741"
replace esrd = 1 if `var' == "L742"
replace esrd = 1 if `var' == "L746"
replace esrd = 1 if `var' == "L748"
replace esrd = 1 if `var' == "L749"
}
gen esrddate = admidate if esrd == 1 & admidate < indexdate & disdate > indexdate-1826
bysort hesid: egen esrdmin = min(esrddate)
gen rrt3 = 0
replace rrt3 = 1 if rrt3temp == 1 & (esrdmin < rrt3datetemp+365) & esrdmin != . & rrt3datetemp != .
bysort hesid: egen rrt3max = max(rrt3)
gen rrt3date = admidate if rrt3 == 1 & admidate < indexdate & disdate > indexdate-1826
bysort hesid: egen rrt3datemin = min(rrt3date)

*RRT RULE 4
gen rrt4temp = 0
foreach var of varlist diag_01-diag_20 {
replace rrt4temp = 1 if `var' == "E853" & acutespell == 0
replace rrt4temp = 1 if `var' == "Y602" & acutespell == 0
replace rrt4temp = 1 if `var' == "Y612" & acutespell == 0
replace rrt4temp = 1 if `var' == "Y622" & acutespell == 0
replace rrt4temp = 1 if `var' == "Y841" & acutespell == 0
replace rrt4temp = 1 if `var' == "Z992" & acutespell == 0
replace rrt4temp = 1 if `var' == "T824" & acutespell == 0
replace rrt4temp = 1 if `var' == "Z491" & acutespell == 0
}
foreach var of varlist opertn_01-opertn_24 {
replace rrt4temp = 1 if `var' == "X401" & acutespell == 0
replace rrt4temp = 1 if `var' == "X403" & acutespell == 0
replace rrt4temp = 1 if `var' == "X404" & acutespell == 0
}
gen rrt4date = admidate if rrt4temp == 1 & admidate < indexdate & disdate > indexdate-1826
bysort hesid: egen rrt4datemin = min(rrt4date)
bysort hesid: egen rrt4datemax = max(rrt4date) 
gen rrt4interval = rrt4datemax-rrt4datemin
cap drop rrt4final
gen rrt4 = 0
replace rrt4 = 1 if rrt4temp == 1 & rrt4interval >= 90 & rrt4interval != .
bysort hesid: egen rrt4max = max(rrt4)
cap drop rrt4date
cap drop rrt4datemin
cap drop rrt4datemax
gen rrt4date = admidate if rrt4 == 1 & admidate < indexdate & disdate > indexdate-1826
bysort hesid: egen rrt4datemin = min(rrt4date)

gen rrt = 0
replace rrt = 1 if rrt2 == 1 | rrt3 == 1 | rrt4 == 1
gen rrtdate = admidate if rrt == 1
bysort hesid: egen rrtdatemax = max(rrtdate)
bysort hesid: egen rrtmax = max(rrt)

*****IMMUNOSUPPRESSION****
cap drop cancerdiag
gen cancerdiag = 0
foreach var of varlist diag_01-diag_20 {
replace cancerdiag = 1 if inlist(substr(`var',1,4),"B211","B212","B213","C000","C001","C002","C003","C004","C006") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C008","C009","C020","C021","C022","C023","C024","C028","C030") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C031","C039","C040","C041","C048","C049","C050","C051","C058") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C059","C060","C061","C062","C068","C069","C080","C081","C089") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C090","C091","C098","C099","C100","C101","C102","C103","C108") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C109","C110","C111","C112","C113","C118","C119","C130","C132") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C138","C139","C140","C142","C148","C150","C151","C152","C154") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C155","C158","C159","C160","C161","C162","C163","C164","C166") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C168","C169","C170","C171","C172","C173","C178","C179","C181") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C182","C183","C184","C185","C186","C187","C188","C189","C211") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C212","C218","C220","C221","C222","C223","C224","C227","C240") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C241","C248","C249","C250","C251","C252","C253","C254","C258") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C259","C260","C261","C268","C269","C300","C301","C310","C312") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C313","C318","C319","C320","C321","C322","C323","C328","C340") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C341","C342","C343","C348","C349","C380","C381","C382","C384") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C388","C390","C398","C399","C400","C401","C402","C403","C409") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C410","C411","C412","C413","C414","C418","C419","C430","C432") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C433","C434","C435","C436","C437","C438","C439","C450","C452") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C457","C459","C460","C461","C462","C463","C467","C468","C470") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C471","C472","C473","C474","C475","C476","C478","C479","C481") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C482","C488","C490","C491","C492","C493","C494","C495","C498") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C499","C500","C501","C502","C503","C504","C505","C506","C509") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C510","C511","C512","C518","C519","C530","C531","C538","C540") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C541","C542","C543","C548","C549","C570","C571","C572","C574") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C577","C578","C579","C600","C601","C602","C608","C609","C621") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C629","C630","C631","C632","C637","C638","C639","C670","C672") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C673","C674","C675","C676","C677","C678","C679","C680","C688") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C689","C690","C691","C692","C693","C694","C695","C696","C699") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C700","C701","C709","C710","C711","C712","C713","C714","C716") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C717","C718","C719","C720","C721","C722","C723","C724","C728") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C729","C740","C741","C749","C750","C751","C752","C753","C755") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C758","C759","C760","C761","C762","C763","C764","C765","C768") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C770","C771","C772","C773","C774","C775","C778","C779","C781") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C782","C783","C784","C785","C786","C787","C788","C790","C792") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C793","C794","C795","C796","C797","C798","C799","C800","C810") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C811","C812","C813","C814","C817","C819","C820","C821","C823") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C824","C825","C826","C827","C829","C830","C831","C833","C837") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C838","C839","C840","C841","C844","C845","C846","C847","C849") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C851","C852","C857","C859","C860","C861","C862","C863","C865") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C866","C880","C882","C883","C884","C887","C889","C900","C902") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C903","C910","C911","C913","C914","C915","C916","C917","C919") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C920","C921","C922","C923","C924","C925","C926","C927","C929") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C930","C931","C933","C937","C939","C940","C942","C943","C946") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C947","C950","C951","C957","C959","C960","C962","C964","C966") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C967","C968","C969","D371","D372","D373","D374","D375","D377") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"D379","D380","D381","D382","D383","D384","D385","D386","D391") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"D392","D397","D399","D400","D401","D407","D409","D410","D412") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"D413","D414","D417","D419","D420","D421","D429","D430","D432") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"D433","D434","D437","D439","D440","D441","D442","D443","D445") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"D446","D447","D448","D449","D470","D470","D471","D475","D479") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"D480","D481","D482","D483","D484","D485","D486","D487","M361") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"N161","Z080","Z081","Z082","Z087","Z088","Z089") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,3),"C00","C01","C02","C03","C04","C05","C06","C07","C09") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,3),"C10","C11","C12","C13","C14","C15","C16","C17","C19") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,3),"C20","C21","C22","C23","C24","C25","C26","C30","C32") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,3),"C33","C34","C37","C38","C39","C40","C41","C43","C46") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,3),"C47","C48","C49","C50","C51","C52","C53","C54","C56") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,3),"C57","C58","C60","C61","C62","C63","C64","C65","C67") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,3),"C68","C69","C70","C71","C72","C73","C74","C75","C77") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,3),"C78","C79","C80","C81","C82","C83","C84","C85","C88") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,3),"C90","C91","C92","C93","C94","C95","C96","C97","D39") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,3),"D40","D41","D42","D43","D44","D48","Z08") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C005","C029","C052","C088","C104","C131","C153","C165") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C180","C210","C229","C257","C311","C329","C383","C408") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C431","C451","C469","C480","C496","C508","C539","C573") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C620","C671","C681","C698","C715","C725","C754","C767") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C780","C791","C809","C822","C835","C848","C864","C901") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"C918","C928","C944","C965","D376","D390","D411","D431") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,4),"D444","D477","D489") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,3),"C08","C18","C31","C45","C55") & epistart < indexdate & epistart >= indexdate-1827
replace cancerdiag = 1 if inlist(substr(`var',1,3),"C66","C76","C86","D38") & epistart < indexdate & epistart >= indexdate-1827
}

rename opertn_0* opertn_*
rename opdate_0* opdate_*

cap drop chemodiag
cap drop chemoop
gen chemodiag = 0
gen chemoop = 0
foreach var of varlist diag_01-diag_20 {
replace chemodiag = 1 if inlist(substr(`var',1,4),"Y431","Y433","Z511") & epistart < indexdate & epistart > indexdate-183
}
forval var = 1/24 {
replace chemoop = 1 if inlist(substr(opertn_`var',1,3),"X70","X71","X72","X73","X74") & opdate_`var' < indexdate & opdate_`var' >= indexdate-183
replace chemoop = 1 if inlist(substr(opertn_`var',1,4),"X701","X702","X703","X704","X705") & opdate_`var' < indexdate & opdate_`var' >= indexdate-183
replace chemoop = 1 if inlist(substr(opertn_`var',1,4),"X708","X709","X711","X712","X713") & opdate_`var' < indexdate & opdate_`var' >= indexdate-183
replace chemoop = 1 if inlist(substr(opertn_`var',1,4),"X714","X715","X718","X719","X721") & opdate_`var' < indexdate & opdate_`var' >= indexdate-183
replace chemoop = 1 if inlist(substr(opertn_`var',1,4),"X722","X723","X724","X728","X729") & opdate_`var' < indexdate & opdate_`var' >= indexdate-183
replace chemoop = 1 if inlist(substr(opertn_`var',1,4),"X731","X738","X739","X741","X742","X748","X749") & opdate_`var' < indexdate & opdate_`var' >= indexdate-183
}

gen cancer = 0
replace cancer = 1 if cancerdiag == 1 & (chemodiag == 1 | chemoop == 1)

gen haem = 0
foreach var of varlist diag_01-diag_20 {
replace haem = 1 if `var' >= "B211" & `var' <= "B213" & epistart < indexdate & epistart > indexdate-732
replace haem = 1 if `var' >= "C81" & `var' <= "C869" & epistart < indexdate & epistart > indexdate-732
replace haem = 1 if `var' >= "C88" & `var' <= "C889" & epistart < indexdate & epistart > indexdate-732
replace haem = 1 if `var' >= "C90" & `var' <= "C969" & epistart < indexdate & epistart > indexdate-732
replace haem = 1 if `var' == "D470" & epistart < indexdate & epistart > indexdate-732
replace haem = 1 if `var' == "D471"  & epistart < indexdate & epistart > indexdate-732
replace haem = 1 if `var' == "D475"  & epistart < indexdate & epistart > indexdate-732
replace haem = 1 if `var' == "D477"  & epistart < indexdate & epistart > indexdate-732
replace haem = 1 if `var' == "D479"  & epistart < indexdate & epistart > indexdate-732
replace haem = 1 if `var' == "M361"  & epistart < indexdate & epistart > indexdate-732
replace haem = 1 if `var' == "M161"  & epistart < indexdate & epistart > indexdate-732
replace haem = 1 if `var' == "T860"  & epistart < indexdate & epistart > indexdate-732
}  
forval var = 1/24 {  
replace haem = 1 if opertn_`var' == "W34" & opertn_`var' <= "W349"  & opdate_`var' < indexdate & opdate_`var' > indexdate-732
replace haem = 1 if opertn_`var' == "X336"  & opdate_`var' < indexdate & opdate_`var' > indexdate-732
}  

gen transplant = 0
foreach var of varlist diag_01-diag_20 {  
replace transplant = 1 if inlist(substr(`var',1,3),"Z94") & epistart < indexdate & epistart > indexdate-1827
replace transplant = 1 if `var' == "Y830" & epistart < indexdate & epistart > indexdate-1827
replace transplant = 1 if `var' == "T861" & epistart < indexdate & epistart > indexdate-1827
replace transplant = 1 if `var' == "T862" & epistart < indexdate & epistart > indexdate-1827
replace transplant = 1 if `var' == "T863" & epistart < indexdate & epistart > indexdate-1827
replace transplant = 1 if `var' == "T864" & epistart < indexdate & epistart > indexdate-1827
replace transplant = 1 if `var' == "T868" & epistart < indexdate & epistart > indexdate-1827
replace transplant = 1 if `var' == "T869" & epistart < indexdate & epistart > indexdate-1827
replace transplant = 1 if `var' == "N165" & epistart < indexdate & epistart > indexdate-1827
}  

forval var = 1/24 {  
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"E531") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"E532") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"E533") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"E538") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"E539") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"G261") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"G268") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"G269") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"G681") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"G688") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"G689") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"J011") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"J012") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"J013") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"J014") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"J015") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"J018") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"J019") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"J721") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"K011") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"K012") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"K018") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"K019") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"K021") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"K022") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"K023") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"K024") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"K025") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"K026") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"K028") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"K029") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"M012") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"M013") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"M014") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"M015") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"M018") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"M019") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"M084") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"M174") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"M178") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"M179") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"Y012") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"Y013") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"Y014") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"Y015") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"Y016") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"Y018") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
replace transplant = 1 if inlist(substr(opertn_`var',1,4),"Y019") & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
}

gen hypospl = 0
foreach var of varlist diag_01-diag_20 {
replace hypospl = 1 if inlist(substr(`var',1,4),"D730","Q890","D570","D571") & epistart < indexdate & epistart > indexdate-1827
}
forval var = 1/24 {
replace hypospl = 1 if opertn_`var' == "J692" & opdate_`var' < indexdate & opdate_`var' > indexdate-1827
}


gen immunolong = 0
foreach var of varlist diag_01-diag_20 {
replace immunolong = 1 if inrange(`var',"D80","D849") & epistart < indexdate & epistart > indexdate-1827
replace immunolong = 1 if inlist(substr(`var',1,3),"D71") & epistart < indexdate & epistart > indexdate-1827
}
gen immunoshort = 0
foreach var of varlist diag_01-diag_20 {
replace immunoshort = 1 if inlist(substr(`var',1,3),"D70") & epistart < indexdate & epistart > indexdate-183
replace immunoshort = 1 if inlist(substr(`var',1,4),"Y434") & epistart < indexdate & epistart > indexdate-183
}
forval var = 1/24 {
replace immunoshort = 1 if inlist(substr(opertn_`var',1,4),"X921") & opdate_`var' < indexdate & opdate_`var' > indexdate-183
replace immunoshort = 1 if inlist(substr(opertn_`var',1,4),"X951") & opdate_`var' < indexdate & opdate_`var' > indexdate-183
}

gen immuno = 0
foreach var of varlist cancer haem trans hypospl immunolong immunoshort {
bysort hesid: egen `var'max = max(`var')
}

gen immunomax = 0
foreach var of varlist cancer haem trans hypospl immunolong immunoshort {
replace immunomax = 1 if `var'max == 1
}

*HEART

gen heart = 0
foreach var of varlist diag_01-diag_20 {
replace heart = 1 if inlist(substr(`var',1,3),"I08","I09","I11","I13") & epistart >= indexdate-1825 & epistart < indexdate
replace heart = 1 if inlist(substr(`var',1,3),"I20","I21","I22","I23","I24","I25") & epistart >= indexdate-1825 & epistart < indexdate
replace heart = 1 if `var' >= "I30" & `var' <= "I529" & epistart >= indexdate-1825 & epistart < indexdate
replace heart = 1 if inlist(`var',"I271","I278","I279") & epistart >= indexdate-1825 & epistart < indexdate
}
bysort hesid: egen heartmax = max(heart)

*OBESITY
cap drop obesity*
gen obesity = 0
foreach var of varlist diag_01-diag_20 {
replace obesity = 1 if `var' >= "E66" & `var' <= "E669" & epistart >= indexdate-1825 & epistart < indexdate
}
bysort hesid: egen obesitymax = max(obesity)
gen obesityspelltemp = obesity
replace obesityspelltemp = 0 if indexspell == 0
bysort idspell: egen obesityspell = max(obesityspelltemp)

*DIABETES
cap drop diabetes*
gen diabetes = 0
foreach var of varlist diag_01-diag_20 {
replace diabetes = 1 if `var' >= "E10" & `var' <= "E149" & epistart >= indexdate-1825 & epistart < indexdate
}
bysort hesid: egen diabetesmax = max(diabetes)

*MENTAL ILLNESS
cap drop psych*
gen psych = 0
foreach var of varlist diag_01-diag_20 {
replace psych = 1 if `var' >= "F20" & `var' <= "F259" & epistart >= indexdate-1825 & epistart < indexdate
replace psych = 1 if `var' >= "F28" & `var' <= "F319" & epistart >= indexdate-1825 & epistart < indexdate
replace psych = 1 if inlist(`var',"F322","F323","F328","F329","F332","F333","F334","F338","F339") & epistart >= indexdate-1825 & epistart < indexdate
}
bysort hesid: egen psychmax = max(psych)

*ALCOHOL
cap drop alcohol*
gen alcohol = 0
foreach var of varlist diag_01-diag_20 {
replace alcohol = 1 if inlist(`var',"F103","F104","F105","F106","F107","F108","F109","F100","F101") & epistart >= indexdate-1825 & epistart < indexdate
replace alcohol = 1 if inlist(`var',"F102","G621","G312","G721","I426","K292","K700") & epistart >= indexdate-1825 & epistart < indexdate
replace alcohol = 1 if inlist(`var',"K701","K702","K703","K704","K709","K852","K860","Q860","P043") & epistart >= indexdate-1825 & epistart < indexdate
}
bysort hesid: egen alcoholmax = max(alcohol)

*Frailty
gen F00 = 0
gen G81 = 0
gen G30 = 0
gen I69 = 0
gen R29 = 0
gen N39 = 0
gen F05 = 0
gen W19 = 0
gen S00 = 0
gen R31 = 0
gen B96 = 0
gen R41 = 0
gen R26 = 0
gen I67 = 0
gen R56 = 0
gen R40 = 0
gen T83 = 0
gen S06 = 0
gen S42 = 0
gen E87 = 0
gen M25 = 0
gen E86 = 0
gen R54 = 0
gen Z50 = 0
gen F03 = 0
gen W18 = 0
gen Z75 = 0
gen F01 = 0
gen S80 = 0
gen L03 = 0
gen H54 = 0
gen E53 = 0
gen Z60 = 0
gen G20 = 0
gen R55 = 0
gen S22 = 0
gen K59 = 0
gen N17 = 0
gen L89 = 0
gen Z22 = 0
gen B95 = 0
gen L97 = 0
gen R44 = 0
gen K26 = 0
gen I95 = 0
gen N19 = 0
gen A41 = 0
gen Z87 = 0
gen J96 = 0
gen X59 = 0
gen M19 = 0
gen G40 = 0
gen M81 = 0
gen S72 = 0
gen S32 = 0
gen E16 = 0
gen R94 = 0
gen N18 = 0
gen R33 = 0
gen R69 = 0
gen N28 = 0
gen R32 = 0
gen G31 = 0
gen Y95 = 0
gen S09 = 0
gen R45 = 0
gen G45 = 0
gen Z74 = 0
gen M79 = 0
gen W06 = 0
gen S01 = 0
gen A04 = 0
gen A09 = 0
gen J18 = 0
gen J69 = 0
gen R47 = 0
gen E55 = 0
gen Z93 = 0
gen R02 = 0
gen R63 = 0
gen H91 = 0
gen W10 = 0
gen W01 = 0
gen E05 = 0
gen M41 = 0
gen R13 = 0
gen Z99 = 0
gen U80 = 0
gen M80 = 0
gen K92 = 0
gen I63 = 0
gen N20 = 0
gen F10 = 0
gen Y84 = 0
gen R00 = 0
gen J22 = 0
gen Z73 = 0
gen R79 = 0
gen Z91 = 0
gen S51 = 0
gen F32 = 0
gen M48 = 0
gen E83 = 0
gen M15 = 0
gen D64 = 0
gen L08 = 0
gen R11 = 0
gen K52 = 0
gen R50 = 0

foreach var of varlist diag_01-diag_20 {
replace F00 = 1 if substr(`var',1,3) ==  "F00" & admidate < indexdisdate & admidate > indexdate-730 
replace G81 = 1 if substr(`var',1,3) ==  "G81" & admidate < indexdisdate & admidate > indexdate-730 
replace G30 = 1 if substr(`var',1,3) ==  "G30" & admidate < indexdisdate & admidate > indexdate-730 
replace I69 = 1 if substr(`var',1,3) ==  "I69" & admidate < indexdisdate & admidate > indexdate-730 
replace R29 = 1 if substr(`var',1,3) ==  "R29" & admidate < indexdisdate & admidate > indexdate-730 
replace N39 = 1 if substr(`var',1,3) ==  "N39" & admidate < indexdisdate & admidate > indexdate-730 
replace F05 = 1 if substr(`var',1,3) ==  "F05" & admidate < indexdisdate & admidate > indexdate-730 
replace W19 = 1 if substr(`var',1,3) ==  "W19" & admidate < indexdisdate & admidate > indexdate-730 
replace S00 = 1 if substr(`var',1,3) ==  "S00" & admidate < indexdisdate & admidate > indexdate-730 
replace R31 = 1 if substr(`var',1,3) ==  "R31" & admidate < indexdisdate & admidate > indexdate-730 
replace B96 = 1 if substr(`var',1,3) ==  "B96" & admidate < indexdisdate & admidate > indexdate-730 
replace R41 = 1 if substr(`var',1,3) ==  "R41" & admidate < indexdisdate & admidate > indexdate-730 
replace R26 = 1 if substr(`var',1,3) ==  "R26" & admidate < indexdisdate & admidate > indexdate-730 
replace I67 = 1 if substr(`var',1,3) ==  "I67" & admidate < indexdisdate & admidate > indexdate-730 
replace R56 = 1 if substr(`var',1,3) ==  "R56" & admidate < indexdisdate & admidate > indexdate-730 
replace R40 = 1 if substr(`var',1,3) ==  "R40" & admidate < indexdisdate & admidate > indexdate-730 
replace T83 = 1 if substr(`var',1,3) ==  "T83" & admidate < indexdisdate & admidate > indexdate-730 
replace S06 = 1 if substr(`var',1,3) ==  "S06" & admidate < indexdisdate & admidate > indexdate-730 
replace S42 = 1 if substr(`var',1,3) ==  "S42" & admidate < indexdisdate & admidate > indexdate-730 
replace E87 = 1 if substr(`var',1,3) ==  "E87" & admidate < indexdisdate & admidate > indexdate-730 
replace M25 = 1 if substr(`var',1,3) ==  "M25" & admidate < indexdisdate & admidate > indexdate-730 
replace E86 = 1 if substr(`var',1,3) ==  "E86" & admidate < indexdisdate & admidate > indexdate-730 
replace R54 = 1 if substr(`var',1,3) ==  "R54" & admidate < indexdisdate & admidate > indexdate-730 
replace Z50 = 1 if substr(`var',1,3) ==  "Z50" & admidate < indexdisdate & admidate > indexdate-730 
replace F03 = 1 if substr(`var',1,3) ==  "F03" & admidate < indexdisdate & admidate > indexdate-730 
replace W18 = 1 if substr(`var',1,3) ==  "W18" & admidate < indexdisdate & admidate > indexdate-730 
replace Z75 = 1 if substr(`var',1,3) ==  "Z75" & admidate < indexdisdate & admidate > indexdate-730 
replace F01 = 1 if substr(`var',1,3) ==  "F01" & admidate < indexdisdate & admidate > indexdate-730 
replace S80 = 1 if substr(`var',1,3) ==  "S80" & admidate < indexdisdate & admidate > indexdate-730 
replace L03 = 1 if substr(`var',1,3) ==  "L03" & admidate < indexdisdate & admidate > indexdate-730 
replace H54 = 1 if substr(`var',1,3) ==  "H54" & admidate < indexdisdate & admidate > indexdate-730 
replace E53 = 1 if substr(`var',1,3) ==  "E53" & admidate < indexdisdate & admidate > indexdate-730 
replace Z60 = 1 if substr(`var',1,3) ==  "Z60" & admidate < indexdisdate & admidate > indexdate-730 
replace G20 = 1 if substr(`var',1,3) ==  "G20" & admidate < indexdisdate & admidate > indexdate-730 
replace R55 = 1 if substr(`var',1,3) ==  "R55" & admidate < indexdisdate & admidate > indexdate-730 
replace S22 = 1 if substr(`var',1,3) ==  "S22" & admidate < indexdisdate & admidate > indexdate-730 
replace K59 = 1 if substr(`var',1,3) ==  "K59" & admidate < indexdisdate & admidate > indexdate-730 
replace N17 = 1 if substr(`var',1,3) ==  "N17" & admidate < indexdisdate & admidate > indexdate-730 
replace L89 = 1 if substr(`var',1,3) ==  "L89" & admidate < indexdisdate & admidate > indexdate-730 
replace Z22 = 1 if substr(`var',1,3) ==  "Z22" & admidate < indexdisdate & admidate > indexdate-730 
replace B95 = 1 if substr(`var',1,3) ==  "B95" & admidate < indexdisdate & admidate > indexdate-730 
replace L97 = 1 if substr(`var',1,3) ==  "L97" & admidate < indexdisdate & admidate > indexdate-730 
replace R44 = 1 if substr(`var',1,3) ==  "R44" & admidate < indexdisdate & admidate > indexdate-730 
replace K26 = 1 if substr(`var',1,3) ==  "K26" & admidate < indexdisdate & admidate > indexdate-730 
replace I95 = 1 if substr(`var',1,3) ==  "I95" & admidate < indexdisdate & admidate > indexdate-730 
replace N19 = 1 if substr(`var',1,3) ==  "N19" & admidate < indexdisdate & admidate > indexdate-730 
replace A41 = 1 if substr(`var',1,3) ==  "A41" & admidate < indexdisdate & admidate > indexdate-730 
replace Z87 = 1 if substr(`var',1,3) ==  "Z87" & admidate < indexdisdate & admidate > indexdate-730 
replace J96 = 1 if substr(`var',1,3) ==  "J96" & admidate < indexdisdate & admidate > indexdate-730 
replace X59 = 1 if substr(`var',1,3) ==  "X59" & admidate < indexdisdate & admidate > indexdate-730 
replace M19 = 1 if substr(`var',1,3) ==  "M19" & admidate < indexdisdate & admidate > indexdate-730 
replace G40 = 1 if substr(`var',1,3) ==  "G40" & admidate < indexdisdate & admidate > indexdate-730 
replace M81 = 1 if substr(`var',1,3) ==  "M81" & admidate < indexdisdate & admidate > indexdate-730 
replace S72 = 1 if substr(`var',1,3) ==  "S72" & admidate < indexdisdate & admidate > indexdate-730 
replace S32 = 1 if substr(`var',1,3) ==  "S32" & admidate < indexdisdate & admidate > indexdate-730 
replace E16 = 1 if substr(`var',1,3) ==  "E16" & admidate < indexdisdate & admidate > indexdate-730 
replace R94 = 1 if substr(`var',1,3) ==  "R94" & admidate < indexdisdate & admidate > indexdate-730 
replace N18 = 1 if substr(`var',1,3) ==  "N18" & admidate < indexdisdate & admidate > indexdate-730 
replace R33 = 1 if substr(`var',1,3) ==  "R33" & admidate < indexdisdate & admidate > indexdate-730 
replace R69 = 1 if substr(`var',1,3) ==  "R69" & admidate < indexdisdate & admidate > indexdate-730 
replace N28 = 1 if substr(`var',1,3) ==  "N28" & admidate < indexdisdate & admidate > indexdate-730 
replace R32 = 1 if substr(`var',1,3) ==  "R32" & admidate < indexdisdate & admidate > indexdate-730 
replace G31 = 1 if substr(`var',1,3) ==  "G31" & admidate < indexdisdate & admidate > indexdate-730 
replace Y95 = 1 if substr(`var',1,3) ==  "Y95" & admidate < indexdisdate & admidate > indexdate-730 
replace S09 = 1 if substr(`var',1,3) ==  "S09" & admidate < indexdisdate & admidate > indexdate-730 
replace R45 = 1 if substr(`var',1,3) ==  "R45" & admidate < indexdisdate & admidate > indexdate-730 
replace G45 = 1 if substr(`var',1,3) ==  "G45" & admidate < indexdisdate & admidate > indexdate-730 
replace Z74 = 1 if substr(`var',1,3) ==  "Z74" & admidate < indexdisdate & admidate > indexdate-730 
replace M79 = 1 if substr(`var',1,3) ==  "M79" & admidate < indexdisdate & admidate > indexdate-730 
replace W06 = 1 if substr(`var',1,3) ==  "W06" & admidate < indexdisdate & admidate > indexdate-730 
replace S01 = 1 if substr(`var',1,3) ==  "S01" & admidate < indexdisdate & admidate > indexdate-730 
replace A04 = 1 if substr(`var',1,3) ==  "A04" & admidate < indexdisdate & admidate > indexdate-730 
replace A09 = 1 if substr(`var',1,3) ==  "A09" & admidate < indexdisdate & admidate > indexdate-730 
replace J18 = 1 if substr(`var',1,3) ==  "J18" & admidate < indexdisdate & admidate > indexdate-730 
replace J69 = 1 if substr(`var',1,3) ==  "J69" & admidate < indexdisdate & admidate > indexdate-730 
replace R47 = 1 if substr(`var',1,3) ==  "R47" & admidate < indexdisdate & admidate > indexdate-730 
replace E55 = 1 if substr(`var',1,3) ==  "E55" & admidate < indexdisdate & admidate > indexdate-730 
replace Z93 = 1 if substr(`var',1,3) ==  "Z93" & admidate < indexdisdate & admidate > indexdate-730 
replace R02 = 1 if substr(`var',1,3) ==  "R02" & admidate < indexdisdate & admidate > indexdate-730 
replace R63 = 1 if substr(`var',1,3) ==  "R63" & admidate < indexdisdate & admidate > indexdate-730 
replace H91 = 1 if substr(`var',1,3) ==  "H91" & admidate < indexdisdate & admidate > indexdate-730 
replace W10 = 1 if substr(`var',1,3) ==  "W10" & admidate < indexdisdate & admidate > indexdate-730 
replace W01 = 1 if substr(`var',1,3) ==  "W01" & admidate < indexdisdate & admidate > indexdate-730 
replace E05 = 1 if substr(`var',1,3) ==  "E05" & admidate < indexdisdate & admidate > indexdate-730 
replace M41 = 1 if substr(`var',1,3) ==  "M41" & admidate < indexdisdate & admidate > indexdate-730 
replace R13 = 1 if substr(`var',1,3) ==  "R13" & admidate < indexdisdate & admidate > indexdate-730 
replace Z99 = 1 if substr(`var',1,3) ==  "Z99" & admidate < indexdisdate & admidate > indexdate-730 
replace U80 = 1 if substr(`var',1,3) ==  "U80" & admidate < indexdisdate & admidate > indexdate-730 
replace M80 = 1 if substr(`var',1,3) ==  "M80" & admidate < indexdisdate & admidate > indexdate-730 
replace K92 = 1 if substr(`var',1,3) ==  "K92" & admidate < indexdisdate & admidate > indexdate-730 
replace I63 = 1 if substr(`var',1,3) ==  "I63" & admidate < indexdisdate & admidate > indexdate-730 
replace N20 = 1 if substr(`var',1,3) ==  "N20" & admidate < indexdisdate & admidate > indexdate-730 
replace F10 = 1 if substr(`var',1,3) ==  "F10" & admidate < indexdisdate & admidate > indexdate-730 
replace Y84 = 1 if substr(`var',1,3) ==  "Y84" & admidate < indexdisdate & admidate > indexdate-730 
replace R00 = 1 if substr(`var',1,3) ==  "R00" & admidate < indexdisdate & admidate > indexdate-730 
replace J22 = 1 if substr(`var',1,3) ==  "J22" & admidate < indexdisdate & admidate > indexdate-730 
replace Z73 = 1 if substr(`var',1,3) ==  "Z73" & admidate < indexdisdate & admidate > indexdate-730 
replace R79 = 1 if substr(`var',1,3) ==  "R79" & admidate < indexdisdate & admidate > indexdate-730 
replace Z91 = 1 if substr(`var',1,3) ==  "Z91" & admidate < indexdisdate & admidate > indexdate-730 
replace S51 = 1 if substr(`var',1,3) ==  "S51" & admidate < indexdisdate & admidate > indexdate-730 
replace F32 = 1 if substr(`var',1,3) ==  "F32" & admidate < indexdisdate & admidate > indexdate-730 
replace M48 = 1 if substr(`var',1,3) ==  "M48" & admidate < indexdisdate & admidate > indexdate-730 
replace E83 = 1 if substr(`var',1,3) ==  "E83" & admidate < indexdisdate & admidate > indexdate-730 
replace M15 = 1 if substr(`var',1,3) ==  "M15" & admidate < indexdisdate & admidate > indexdate-730 
replace D64 = 1 if substr(`var',1,3) ==  "D64" & admidate < indexdisdate & admidate > indexdate-730 
replace L08 = 1 if substr(`var',1,3) ==  "L08" & admidate < indexdisdate & admidate > indexdate-730 
replace R11 = 1 if substr(`var',1,3) ==  "R11" & admidate < indexdisdate & admidate > indexdate-730 
replace K52 = 1 if substr(`var',1,3) ==  "K52" & admidate < indexdisdate & admidate > indexdate-730 
replace R50 = 1 if substr(`var',1,3) ==  "R50" & admidate < indexdisdate & admidate > indexdate-730 
}

foreach var of varlist F00-R50 {
bysort hesid: egen `var'max = max(`var')
}

gen F00_score = 7.1 if F00max == 1
gen G81_score = 4.4 if G81max == 1
gen G30_score = 4 if G30max == 1
gen I69_score = 3.7 if I69max == 1
gen R29_score = 3.6 if R29max == 1
gen N39_score = 3.2 if N39max == 1
gen F05_score = 3.2 if F05max == 1
gen W19_score = 3.2 if W19max == 1
gen S00_score = 3.2 if S00max == 1
gen R31_score = 3 if R31max == 1
gen B96_score = 2.9 if B96max == 1
gen R41_score = 2.7 if R41max == 1
gen R26_score = 2.6 if R26max == 1
gen I67_score = 2.6 if I67max == 1
gen R56_score = 2.6 if R56max == 1
gen R40_score = 2.5 if R40max == 1
gen T83_score = 2.4 if T83max == 1
gen S06_score = 2.4 if S06max == 1
gen S42_score = 2.3 if S42max == 1
gen E87_score = 2.3 if E87max == 1
gen M25_score = 2.3 if M25max == 1
gen E86_score = 2.3 if E86max == 1
gen R54_score = 2.2 if R54max == 1
gen Z50_score = 2.1 if Z50max == 1
gen F03_score = 2.1 if F03max == 1
gen W18_score = 2.1 if W18max == 1
gen Z75_score = 2 if Z75max == 1
gen F01_score = 2 if F01max == 1
gen S80_score = 2 if S80max == 1
gen L03_score = 2 if L03max == 1
gen H54_score = 1.9 if H54max == 1
gen E53_score = 1.9 if E53max == 1
gen Z60_score = 1.8 if Z60max == 1
gen G20_score = 1.8 if G20max == 1
gen R55_score = 1.8 if R55max == 1
gen S22_score = 1.8 if S22max == 1
gen K59_score = 1.8 if K59max == 1
gen N17_score = 1.8 if N17max == 1
gen L89_score = 1.7 if L89max == 1
gen Z22_score = 1.7 if Z22max == 1
gen B95_score = 1.7 if B95max == 1
gen L97_score = 1.6 if L97max == 1
gen R44_score = 1.6 if R44max == 1
gen K26_score = 1.6 if K26max == 1
gen I95_score = 1.6 if I95max == 1
gen N19_score = 1.6 if N19max == 1
gen A41_score = 1.6 if A41max == 1
gen Z87_score = 1.5 if Z87max == 1
gen J96_score = 1.5 if J96max == 1
gen X59_score = 1.5 if X59max == 1
gen M19_score = 1.5 if M19max == 1
gen G40_score = 1.5 if G40max == 1
gen M81_score = 1.4 if M81max == 1
gen S72_score = 1.4 if S72max == 1
gen S32_score = 1.4 if S32max == 1
gen E16_score = 1.4 if E16max == 1
gen R94_score = 1.4 if R94max == 1
gen N18_score = 1.4 if N18max == 1
gen R33_score = 1.3 if R33max == 1
gen R69_score = 1.3 if R69max == 1
gen N28_score = 1.3 if N28max == 1
gen R32_score = 1.2 if R32max == 1
gen G31_score = 1.2 if G31max == 1
gen Y95_score = 1.2 if Y95max == 1
gen S09_score = 1.2 if S09max == 1
gen R45_score = 1.2 if R45max == 1
gen G45_score = 1.2 if G45max == 1
gen Z74_score = 1.1 if Z74max == 1
gen M79_score = 1.1 if M79max == 1
gen W06_score = 1.1 if W06max == 1
gen S01_score = 1.1 if S01max == 1
gen A04_score = 1.1 if A04max == 1
gen A09_score = 1.1 if A09max == 1
gen J18_score = 1.1 if J18max == 1
gen J69_score = 1 if J69max == 1
gen R47_score = 1 if R47max == 1
gen E55_score = 1 if E55max == 1
gen Z93_score = 1 if Z93max == 1
gen R02_score = 1 if R02max == 1
gen R63_score = 0.9 if R63max == 1
gen H91_score = 0.9 if H91max == 1
gen W10_score = 0.9 if W10max == 1
gen W01_score = 0.9 if W01max == 1
gen E05_score = 0.9 if E05max == 1
gen M41_score = 0.9 if M41max == 1
gen R13_score = 0.8 if R13max == 1
gen Z99_score = 0.8 if Z99max == 1
gen U80_score = 0.8 if U80max == 1
gen M80_score = 0.8 if M80max == 1
gen K92_score = 0.8 if K92max == 1
gen I63_score = 0.8 if I63max == 1
gen N20_score = 0.7 if N20max == 1
gen F10_score = 0.7 if F10max == 1
gen Y84_score = 0.7 if Y84max == 1
gen R00_score = 0.7 if R00max == 1
gen J22_score = 0.7 if J22max == 1
gen Z73_score = 0.6 if Z73max == 1
gen R79_score = 0.6 if R79max == 1
gen Z91_score = 0.5 if Z91max == 1
gen S51_score = 0.5 if S51max == 1
gen F32_score = 0.5 if F32max == 1
gen M48_score = 0.5 if M48max == 1
gen E83_score = 0.4 if E83max == 1
gen M15_score = 0.4 if M15max == 1
gen D64_score = 0.4 if D64max == 1
gen L08_score = 0.4 if L08max == 1
gen R11_score = 0.3 if R11max == 1
gen K52_score = 0.3 if K52max == 1
gen R50_score = 0.1 if R50max == 1

egen frailty_score = rowtotal(F00_score-R50_score)

order hesid admidate epistart epiend disdate index* covid
sort hesid epistart

save "[local filepath]\COVID_allrecs_2015_2022jun_READY.dta", replace

*use "[local filepath]\COVID_allrecs_2015_2022jun_READY.dta", clear

*Age group
cap drop agegrp
gen agegrp = .
replace agegrp = 1 if startage >= 7000 & startage <= 7009
replace agegrp = 1 if inrange(startage,0,4)
replace agegrp = 2 if inrange(startage,5,9)
replace agegrp = 3 if inrange(startage,10,14)
replace agegrp = 4 if inrange(startage,15,19)
replace agegrp = 5 if inrange(startage,20,24)
replace agegrp = 6 if inrange(startage,25,29)
replace agegrp = 7 if inrange(startage,30,34)
replace agegrp = 8 if inrange(startage,35,39)
replace agegrp = 9 if inrange(startage,40,44)
replace agegrp = 10 if inrange(startage,45,49)
replace agegrp = 11 if inrange(startage,50,54)
replace agegrp = 12 if inrange(startage,55,59)
replace agegrp = 13 if inrange(startage,60,64)
replace agegrp = 14 if inrange(startage,65,69)
replace agegrp = 15 if inrange(startage,70,74)
replace agegrp = 16 if inrange(startage,75,79)
replace agegrp = 17 if inrange(startage,80,84)
replace agegrp = 18 if inrange(startage,85,89)
replace agegrp = 19 if inrange(startage,90,94)
replace agegrp = 20 if inrange(startage,95,105)
cap label drop agegrp
label define agegrp 1 "0" 1 "1-4" 2 "5-9" 3 "10-14" 4 "15-19" 5 "20-24" 6 "25-29" 7 "30-34" 8 "35-39" 9 "40-44" 10 "45-49" 11 "50-54" 12 "55-59" 13 "60-64" 14 "65-69" 15 "70-74" 16 "75-79" 17 "80-84" 18 "85-89" 19 "90-94" 20 "95+"
label values agegrp agegrp

****

*28-day mortality (merge with national deaths file)
merge m:1 hesid using "[local filepath]\NIC315419_HES_ONS_sep2022.dta"
drop if _m == 2
drop _m
gen death = 0
replace death = 1 if deathdat != .
gen death28 = 0
replace death28 = 1 if deathdat != . & deathdat >= indexdate & deathdat <= indexdate+27

*Housekeeping
drop if startage >105
drop if startage <16 | (startage >= 7000 & startage <= 7007)
keep if index == 1
keep if indexdate <= 22614
keep if index != .
drop if startage >105
drop age
rename startage age
drop hesid epikey procode admidate epistart epiend deathdat
order indexdate* agegrp* sex* frailty*
drop month year leading
gen month = month(indexdate)
gen year = year(indexdate)
gen leading = .
replace leading = 0 if month <10
egen yearmonth = concat(year leading month)
replace yearmonth = subinstr(yearmonth, ".", "",.) 
rename covid covid_primary
rename u071 u071_anydiag
rename u072 u072_anydiag
rename u072_only u072_only_anydiag
cap drop u072_only_anydiag
gen u072_only_anydiag = 0
replace u072_only_anydiag = 1 if u071_any == 0 & u072_any == 1
format deathdat %td
drop if age == .
bysort epikey: gen unique = _n
drop if unique >1
drop unique
save "[local filepath]\COVID_allrecs_2015_2022jun_READY_INDEXONLY_FINAL.dta", replace

*Create 28-day mortality file
use "[local filepath]\COVID_allrecs_2015_2022jun_READY_INDEXONLY_FINAL.dta", clear
keep yearmonth agegrp sex deathdat death28 charlson_grp frailty_score
order yearmonth agegrp sex deathdat death28 charlson_grp frailty_score 
outsheet * using ///
"K:\QNAP\RECOVERY-deidentified\Generalizability and representativeness project\RECOVERY_generalizability_representativeness\Intermediate outputs\Datasets\national_HES_cf28_jun2022_v1.csv", replace delim(",")

*Create shuffled national HES file
use "[local filepath]\COVID_allrecs_2015_2022jun_READY_INDEXONLY_FINAL.dta", clear
shufflevar *
keep *_shuffled
rename *_shuffled *
format indexdate %td
label values agegrp agegrp
label define ethgrp 1 "White" 2 "Asian" 3 "Black" 4 "Other" 5 "Mixed" 99 "Unknown"
label values ethgrp ethgrp
save "K:\QNAP\RECOVERY\Raph\Generalisability and representativeness\national_HES_for_Gui_shuffled_jun2022.dta", replace
drop lsoa* lad*
outsheet * using ///
"K:\QNAP\RECOVERY-deidentified\Generalizability and representativeness project\RECOVERY_generalizability_representativeness\Intermediate outputs\Datasets\national_HES_shuffled_jun2022_v1.csv", replace delim(",")

*Create geographical fields file, combine with equivalent file for RECOVERY
use "K:\QNAP\RECOVERY\Raph\Generalisability and representativeness\national_HES_for_Gui_shuffled_jun2022.dta", clear
keep ladcd
gen total = 1
collapse (sum) total, by(ladcd) 
rename total freq_national
save "K:\QNAP\RECOVERY\Raph\Generalisability and representativeness\national_LA_freqcounts_jun2022_v1.dta", replace
outsheet * using ///
"K:\QNAP\RECOVERY-deidentified\Generalizability and representativeness project\RECOVERY_generalizability_representativeness\Intermediate outputs\Datasets\national_LA_freqcounts_jun2022_v1.csv", replace delim(",")
use "K:\QNAP\RECOVERY\Raph\Generalisability and representativeness\national_LA_freqcounts_jun2022_v1.dta", clear
merge 1:1 ladcd using "K:\QNAP\RECOVERY\Raph\Generalisability and representativeness\recovery_LA_freqcounts_79_v1.dta"
drop _m
outsheet * using ///
"K:\QNAP\RECOVERY-deidentified\Generalizability and representativeness project\RECOVERY_generalizability_representativeness\Intermediate outputs\Datasets\national_vs_recovery_LA_freqcounts_jun2022_v1.csv", replace delim(",")

*Create Charlson and frailty file
use "[local filepath]\COVID_allrecs_2015_2022jun_READY_INDEXONLY_FINAL.dta", clear
keep agegrp metastasis-mild_liver charlson_score frailty_score yearmonth
outsheet * using ///
"K:\QNAP\RECOVERY-deidentified\Generalizability and representativeness project\RECOVERY_generalizability_representativeness\Intermediate outputs\Datasets\national_unscrambled_charlson_frailty_age_jun2022_v2.csv", replace delim(",")

*Create age-sex-period file
use "[local filepath]\COVID_allrecs_2015_2022jun_READY_INDEXONLY_FINAL.dta", clear
cap drop agebroad
gen agebroad = .
replace agebroad = 1 if age >= 4 & age <= 12
replace agebroad = 2 if age >= 13 & age <= 14
replace agebroad = 3 if age >= 15 & age <= 16
replace agebroad = 4 if age >= 17 & age <= 18
replace agebroad = 5 if age >= 19 & age <= 20
cap label drop agebroad
label define agebroad 1 "<60" 2 "60-69" 3 "70-79" 4 "80-89" 5 "90+"
label values agebroad agebroad 
gen period = .
replace period = 1 if yearmonth >= "202003" & yearmonth <= "202005"
replace period = 2 if yearmonth >= "202006" & yearmonth <= "202008"
replace period = 3 if yearmonth >= "202009" & yearmonth <= "202011"
replace period = 4 if yearmonth >= "202012" & yearmonth <= "202102"
replace period = 5 if yearmonth >= "202103" & yearmonth <= "202105"
replace period = 6 if yearmonth >= "202106" & yearmonth <= "202108"
replace period = 7 if yearmonth >= "202109" & yearmonth <= "202111"
replace period = 8 if yearmonth >= "202112" & yearmonth <= "202202"
replace period = 9 if yearmonth >= "202203" & yearmonth <= "202205"
replace period = 10 if yearmonth >= "202206" & yearmonth <= "202208"
cap label drop period
label define period 1 "202003-202005" 2 "202006-202008" 3 "202009-202011" 4 "202012-202102" 5 "202103-202105" 6 "202106-202108" 7 "202109-202111" 8 "202112-202202" ///
9 "202203-202205" 10 "202206-202208"
label values period period
keep period agebroad sex
sort period sex agebroad
order period sex agebroad
outsheet * using ///
"K:\QNAP\RECOVERY-deidentified\Generalizability and representativeness project\RECOVERY_generalizability_representativeness\Intermediate outputs\Datasets\national_unscrambled_sex_agebroad_month_jun2022.csv", replace delim(",")

*Create region-age-period file
use "[local filepath]\COVID_allrecs_2015_2022jun_READY_INDEXONLY_FINAL.dta", clear
rename ladcd lacode
merge m:1 lacode using "[local filepath]\local authority to resgor region lookup.dta"
drop if _m == 2
drop _m
cap drop agebroad
gen agebroad = .
replace agebroad = 1 if age >= 4 & age <= 12
replace agebroad = 2 if age >= 13 & age <= 16
replace agebroad = 3 if age >= 17 & age <= 20
cap label drop agebroad
label define agebroad 1 "<60" 2 "60-79" 3 "80+"
label values agebroad agebroad 
rename regionname region 
gen period = .
replace period = 1 if yearmonth >= "202003" & yearmonth <= "202005"
replace period = 2 if yearmonth >= "202006" & yearmonth <= "202008"
replace period = 3 if yearmonth >= "202009" & yearmonth <= "202011"
replace period = 4 if yearmonth >= "202012" & yearmonth <= "202102"
replace period = 5 if yearmonth >= "202103" & yearmonth <= "202105"
replace period = 6 if yearmonth >= "202106" & yearmonth <= "202108"
replace period = 7 if yearmonth >= "202109" & yearmonth <= "202111"
replace period = 8 if yearmonth >= "202112" & yearmonth <= "202202"
replace period = 9 if yearmonth >= "202203" & yearmonth <= "202205"
replace period = 10 if yearmonth >= "202206" & yearmonth <= "202208"
cap label drop period
label define period 1 "202003-202005" 2 "202006-202008" 3 "202009-202011" 4 "202012-202102" 5 "202103-202105" 6 "202106-202108" 7 "202109-202111" 8 "202112-202202" ///
9 "202203-202205" 10 "202206-202208"
label values period period
keep period agebroad region
sort period region agebroad
order period region agebroad
outsheet * using ///
"K:\QNAP\RECOVERY-deidentified\Generalizability and representativeness project\RECOVERY_generalizability_representativeness\Intermediate outputs\Datasets\national_unscrambled_region_agebroad_month_jun2022.csv", replace delim(",")

*Create ethnicity-age-period file
use "[local filepath]\COVID_allrecs_2015_2022jun_READY_INDEXONLY_FINAL.dta", clear
cap drop agebroad
gen agebroad = .
replace agebroad = 1 if age >= 4 & age <= 12
replace agebroad = 2 if age >= 13 & age <= 14
replace agebroad = 3 if age >= 15 & age <= 16
replace agebroad = 4 if age >= 17 & age <= 20
cap label drop agebroad
label define agebroad 1 "<60" 2 "60-69" 3 "70-79" 4 "80+"
label values agebroad agebroad 
label define ethgrp 1 "White" 2 "Asian" 3 "Black" 4 "Other" 5 "Mixed" 99 "Unknown"
label values ethgrp ethgrp
gen period = .
replace period = 1 if yearmonth >= "202003" & yearmonth <= "202005"
replace period = 2 if yearmonth >= "202006" & yearmonth <= "202008"
replace period = 3 if yearmonth >= "202009" & yearmonth <= "202011"
replace period = 4 if yearmonth >= "202012" & yearmonth <= "202102"
replace period = 5 if yearmonth >= "202103" & yearmonth <= "202105"
replace period = 6 if yearmonth >= "202106" & yearmonth <= "202108"
replace period = 7 if yearmonth >= "202109" & yearmonth <= "202111"
replace period = 8 if yearmonth >= "202112" & yearmonth <= "202202"
replace period = 9 if yearmonth >= "202203" & yearmonth <= "202205"
replace period = 10 if yearmonth >= "202206" & yearmonth <= "202208"
cap label drop period
label define period 1 "202003-202005" 2 "202006-202008" 3 "202009-202011" 4 "202012-202102" 5 "202103-202105" 6 "202106-202108" 7 "202109-202111" 8 "202112-202202" ///
9 "202203-202205" 10 "202206-202208"
label values period period
keep period agebroad ethgrp
sort period agebroad ethgrp
order period agebroad ethgrp
outsheet * using ///
"K:\QNAP\RECOVERY-deidentified\Generalizability and representativeness project\RECOVERY_generalizability_representativeness\Intermediate outputs\Datasets\national_unscrambled_ethnicity_agebroad_month_jun2022.csv", replace delim(",")

*Create time-to-death file
use "[local filepath]\COVID_allrecs_2015_2022jun_READY_INDEXONLY_FINAL.dta", clear
gen agebroad = .
replace agebroad = 1 if age >= 4 & age <= 12
replace agebroad = 2 if age >= 13 & age <= 14
replace agebroad = 3 if age >= 15 & age <= 16
replace agebroad = 4 if age >= 17 & age <= 18
replace agebroad = 5 if age >= 19 & age <= 20
cap label drop agebroad
label define agebroad 1 "<60" 2 "60-69" 3 "70-79" 4 "80-89" 5 "90+"
label values agebroad agebroad 
format deathdat %td
gen deathinterval = deathdat-indexdate
replace deathint = . if deathint <0
keep if death28 == 1
gen firstwave = 0
replace firstwave = 1 if yearmonth >= "202003" & yearmonth <= "202005"
cap label drop firstwave
label define firstwave 0 "other" 1 "mar-may 2020"
label values firstwave firstwave 
gen total = 1
collapse (sum) total, by(deathint agebroad firstwave)
sort agebroad deathint firstwave 
order agebroad deathint firstwave 
outsheet * using ///
"K:\QNAP\RECOVERY-deidentified\Generalizability and representativeness project\RECOVERY_generalizability_representativeness\Intermediate outputs\Datasets\national_unscrambled_deathinterval_agebroad_wave_jun2022.csv", replace delim(",")

*Create IMD file
use "[local filepath]\COVID_allrecs_2015_2022jun_READY_INDEXONLY_FINAL.dta", clear
drop hesid epikey procode admidate epistart epiend deathdat
order indexdate* agegrp* sex* frailty*
drop month year leading
gen hesid = .
gen epikey = .
gen rand_date = .
gen extract = .
keep hesid epikey rand_date indexdate covid u071 u072 u072_only age agegrp sex ethgrp metastasis-mild_liver mi charlson_score charlson_score_age charlson_grp charlson_grp_age immuno heart diabetes psych alcohol rrt obesity obesity_index frailty_score lsoa* lad* extract
order hesid epikey index indexdate covid u071 u072 u072_only age agegrp sex ethgrp metastasis-mild_liver mi charlson_score charlson_score_age charlson_grp charlson_grp_age immuno heart diabetes psych alcohol rrt obesity obesity_index frailty_score lsoa* lad* extract
rename covid covid_primary
rename u071 u071_anydiag
rename u072 u072_anydiag
rename u072_only u072_only_anydiag
cap drop u072_only_anydiag
gen u072_only_anydiag = 0
replace u072_only_anydiag = 1 if u071_any == 0 & u072_any == 1
rename lsoacd lsoa
merge m:1 lsoa using "K:\uhce\Libraries\Geographical\soal_imd19_lookup.dta"
drop if _m == 2
drop _m
cap drop month
gen month = month(indexdate)
cap drop year
gen year = year(indexdate)
cap drop leading
gen leading = .
replace leading = 0 if month <10
cap drop yearmonth 
egen yearmonth = concat(year leading month)
replace yearmonth = subinstr(yearmonth, ".", "",.) 
sort yearmonth lsoa
keep yearmonth agegrp lsoa imdrank
cap drop imd19_quintile
gen imd19_quintile = .
replace imd19_quintile = 1 if imdrank >= 1 & imdrank <= 6569
replace imd19_quintile = 2 if imdrank >= 6570 & imdrank <= 13138
replace imd19_quintile = 3 if imdrank >= 13139 & imdrank <= 19706
replace imd19_quintile = 4 if imdrank >= 19707 & imdrank <= 26275
replace imd19_quintile = 5 if imdrank >= 26276 & imdrank <= 32844
cap label drop imd19_quintile
label define imd19_quintile 1 "1 (Most deprived)" 2 "2" 3 "3" 4 "4" 5 "5 (Least deprived)"
label values imd19_quintile imd19_quintile
gen count = 1
collapse (sum) count, by(yearmonth imd19 agegrp) 
outsheet * using ///
"K:\QNAP\RECOVERY-deidentified\Generalizability and representativeness project\RECOVERY_generalizability_representativeness\Intermediate outputs\Datasets\national_IMD_jun2022_v1.csv", replace delim(",")

*END