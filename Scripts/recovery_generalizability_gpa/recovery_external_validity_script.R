# RECOVERY generalizability and representativeness paper
# RECOVERY CRF, GPES, NHSBSA data analysis

# Author: Guilherme Amorim
# guilherme.pessoa-amorim@ndph.ox.ac.uk

# 1. Libraries -------

library(tidyverse) # data manipulation (including dplyr, lubridate, haven)
library(magrittr) # pipe operator
library(stringr) # working with strings
library(ggplot2) # plotting
library(readxl) # load excel/csv files
library(fs) # tools for file system operations
library(haven) # import sas/stata files
library(dtplyr) # data.table backend for dplyr functions (helps speed up processing)
library(gtsummary) # quick summary table
library(flextable) # export summary table to word
library(officer)# export summary table to word
library(forcats) # tools for reordering factors
library(lubridate) # handling dates
library(viridis) # color scales for plotting
library(patchwork) # combining plot
library(ggrepel) # repelling labels in plots
library(directlabels) # labelling in plots
library(modelsummary) # contingency tables
library(scales) # adjust scaling in plots
library(ggh4x) # nested ggplot facets
library(forestploter) # forest plots
library(grateful) # citing packages
 
# Directory

setwd("K:/QNAP/RECOVERY-deidentified/Generalizability and representativeness project/RECOVERY_generalizability_representativeness")


# prevent scientific notation in plots
options(scipen=10000) 


# prevent conflicts in select between packages
select<-dplyr::select

# save workspace image

# save.image("K:/QNAP/RECOVERY-deidentified/Generalizability and representativeness project/RECOVERY_generalizability_representativeness/Scripts/recovery_generalizability_gpa/Workspace/recovery_generalizability_workspace_image.RData")


# color scheme 

colors=c("#A13059", "#59A130","#3059A1")


# 2. Load data and codelists  ----------- 

## Case Report Form -------
# version: recovery_2203_1b\version_2022_03_08

baseline_crf <- read_dta(file="K:/QNAP/RECOVERY-deidentified/Datasets/ADaM datasets/recovery_2209_1b/version_2022_10_20/adsl.dta")




### select variables of interest for this analysis

baseline_crf%<>%
  select(study_number=usubjid,
         siteid,
         withdrew=withdrfl,
         crf_rand_ver=crfrver,
         crf_fu_ver = crffver,
         brthdtc,
         age,
         agegr2,
         agegr2n,
         sex,
         race,
         rand_date = randdt,
         admission_day = hospdy,
         diabetes_crf = diabfl,
         heart_crf = hdfl,
         liver_crf = liverfl,
         renal_crf = kidneyfl,
         lung_crf = lungfl,
         renal_replacement_crf = rrtfl,
         immunosuppression = immunofl,
         imv = imvfl,
         niv = nivfl,
         oxy = oxyfl
         )

### total number of participants included
baseline_crf%>%distinct(study_number)%>%nrow() 
# 48019 study numbers in the raw data (up until September 2022)

### restrict to randomisations up until the end of June 

baseline_crf%>%
  filter(rand_date<"2022-01-01")%>%
  distinct(study_number)%>%
  nrow()
  # 46010

baseline_crf%<>%
  filter(rand_date<"2022-01-01")

### identify and remove withdrawn participants

baseline_crf%>%
  filter(withdrew=="Y")%>%
  distinct(study_number)%>%
  .[[1]]->withdrawn_participants 
# 278 withdrawn/duplicates

baseline_crf%<>%
  filter(!study_number %in% withdrawn_participants)%>%
  select(-withdrew)

baseline_crf%>%
  distinct(study_number)%>%
  nrow() # 45732 randomisations (excluding duplicates/withdrawals)






















## Study site data ---------------------

sites <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/RECOVERY sites/RECOVERY Sites 2022-03-29.csv")

sites%<>%
  left_join(read_excel("K:/QNAP/RECOVERY-deidentified/Datasets/RECOVERY sites/RECOVERY_Sites 2022-06-30.xlsx"))


## Flagged RECOVERY HES data (from Raph)-------
# extract 73

RECOVERY_HES_flags <- read_csv("Intermediate outputs/Datasets/Recovery_HES_79_v1.csv")%>%
  mutate(indexdate=as.Date(indexdate, format="%d%b%Y"))


RECOVERY_HES_flags%>%
  distinct(study_id)%>%
  nrow() # 40130 people with HES data


# restrict to people randomised in England
RECOVERY_HES_flags%<>%
  left_join(baseline_crf%>%select(study_number, siteid), by=c("study_id"="study_number"))%>%
  left_join(sites%>%
            mutate(SiteID=as.character(SiteID)), b=c("siteid"="SiteID"))%>%
  filter(Nation=="England")


RECOVERY_HES_flags%>%
  distinct(study_id)%>%
  nrow() # 40078 people with HES data recruited in England (at any time point)



# death intervals

RECOVERY_death_intervals<-read_csv("Intermediate outputs/Datasets/Recovery_HES_79_deathinterval_v1.csv")









## National HES data (shuffled for baseline characteristics) --------------

National_HES_flags <- read_csv("Intermediate outputs/Datasets/national_HES_shuffled_jun2022_v1.csv", 
                                        col_types = cols(indexdate = col_date(format = "%m%b%Y")))%>%
  filter(age>15)






## National HES data (unshuffled) ------------

# Charlson, Frailty Score, age, index month
National_HES_flags_unscrambled <- read_csv("Intermediate outputs/Datasets/national_unscrambled_charlson_frailty_age_jun2022_v2.csv", 
                                                    col_types = cols(yearmonth = col_date(format = "%Y%m")))


# IMD, age, index month 

National_HES_flags_IMD_unscrambled <- read_csv("Intermediate outputs/Datasets/national_IMD_jun2022_v1.csv", 
                                                    col_types = cols(yearmonth = col_date(format = "%Y%m")))




# Ethnicity, index month, age 


National_HES_flags_ethnicity_unscrambled <- read_csv("Intermediate outputs/Datasets/national_unscrambled_ethnicity_agebroad_month_jun2022.csv")%>%
  mutate(Period = case_when(period=="202003-202005" ~ "Mar 20 - May 20",
                            period=="202006-202008" ~ "Jun 20 - Aug 20",
                            period=="202009-202011" ~ "Sep 20 - Nov 20",
                            period=="202012-202102" ~ "Dec 20 - Feb 21",
                            period=="202103-202105" ~ "Mar 21 - May 21",
                            period=="202106-202108" ~ "Jun 21 - Aug 21",
                            period=="202109-202111" ~ "Sep 21 - Nov 21"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21")))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%
  select(-period)

# Region, age, index month


National_HES_flags_region_unscrambled <- read_csv("Intermediate outputs/Datasets/national_unscrambled_region_agebroad_month_jun2022.csv")%>%
  mutate(Period = case_when(period=="202003-202005" ~ "Mar 20 - May 20",
                          period=="202006-202008" ~ "Jun 20 - Aug 20",
                          period=="202009-202011" ~ "Sep 20 - Nov 20",
                          period=="202012-202102" ~ "Dec 20 - Feb 21",
                          period=="202103-202105" ~ "Mar 21 - May 21",
                          period=="202106-202108" ~ "Jun 21 - Aug 21",
                          period=="202109-202111" ~ "Sep 21 - Nov 21"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21"
  )))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%
  select(-period)


# sex, period, age

National_HES_flags_sex_unscrambled<-read_csv("Intermediate outputs/Datasets/national_unscrambled_sex_agebroad_month_jun2022.csv")%>%
  mutate(Period = case_when(period=="202003-202005" ~ "Mar 20 - May 20",
                            period=="202006-202008" ~ "Jun 20 - Aug 20",
                            period=="202009-202011" ~ "Sep 20 - Nov 20",
                            period=="202012-202102" ~ "Dec 20 - Feb 21",
                            period=="202103-202105" ~ "Mar 21 - May 21",
                            period=="202106-202108" ~ "Jun 21 - Aug 21",
                            period=="202109-202111" ~ "Sep 21 - Nov 21"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21"
  )))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%
  select(-period)


# death intervals and age

National_HES_flags_death_intervals_unscrambled <- read_csv("Intermediate outputs/Datasets/national_unscrambled_deathinterval_agebroad_wave_jun2022.csv")




## National HES event data (aggregate, un-shuffled with age and sex) ----------------

National_HES_events <- read_csv("Intermediate outputs/Datasets/national_HES_cf28_jun2022_v1.csv")



rm(withdrawn_participants)

## Geographical data --------

maps_data<-read_csv("K:/QNAP/RECOVERY-deidentified/Generalizability and representativeness project/RECOVERY_generalizability_representativeness/Intermediate outputs/Datasets/national_vs_recovery_LA_freqcounts_jun2022_v1.csv") 
         




## LAD to region mapping from https://geoportal.statistics.gov.uk/maps/6a41affae7e345a7b2b86602408ea8a2

# map boundaries from https://geoportal.statistics.gov.uk/search?q=england%20regions%202021%20boundaries&sort=-created&type=feature%20layer

lad_region_mapping<-read_csv("K:/QNAP/RECOVERY-deidentified/Generalizability and representativeness project/RECOVERY_generalizability_representativeness/Tools/Maps/LAD to region/Local_Authority_District_to_Region_(April_2021)_Lookup_in_England.csv")





## lsoa-lad mapping
# from https://geoportal.statistics.gov.uk/documents/lower-layer-super-output-area-2011-to-ward-2021-to-lad-2021-lookup-in-england-and-wales-v2/about

lsoa_lad_region_mapping<-read_excel("K:/QNAP/RECOVERY-deidentified/Generalizability and representativeness project/RECOVERY_generalizability_representativeness/Tools/Maps/LSOA to LAD/LSOA11_WD21_LAD21_EW_LU_V2.xlsx")%>%
  left_join(lad_region_mapping)


## RECOVERY HES data (crude) ------

# extract 44

HES_data_44 <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT02_HESAPC/HES_2021_03_12/DP_INT02_HES_Deidentified_2021-03-16_12-17-40.csv")

HES_data_44%<>%
  select(study_number, 
         admidate,
         admimeth,
         admisorc,
         epistart,
         epiend,
         epidur,
         disdate,
         disdest,
         dismeth,
         spelbgin,
         speldur,
         spelend,
         starts_with("diag"),
         starts_with("opertn"),
         starts_with("opdate"),
         extract_number)

# extract 73

HES_data73 <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT02_HESAPC/HES_2022_03_14/DP_INT02_HES_Deidentified_2022-05-04_12-08-38.csv")

HES_data73%<>%
  select(study_number, 
         epikey,
         admidate,
         admimeth,
         admisorc,
         epistart,
         epiend,
         epidur,
         disdate,
         disdest,
         dismeth,
         spelbgin,
         speldur,
         spelend,
         starts_with("diag"),
         starts_with("opertn"),
         starts_with("opdate"),
         extract_number)



# extract 79

HES_data79 <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/INT02_HESAPC/HES_2022_09_19/DP_INT02_HES_Deidentified_2022-10-25_09-42-26.csv")

HES_data79%<>%
  select(study_number, 
         epikey,
         admidate,
         admimeth,
         admisorc,
         epistart,
         epiend,
         epidur,
         disdate,
         disdest,
         dismeth,
         spelbgin,
         speldur,
         spelend,
         starts_with("diag"),
         starts_with("opertn"),
         starts_with("opdate"),
         extract_number)






# 3. Apply analyses population restrictions -------


## Restrict to people recruited in England------

baseline_crf_all_nations<-baseline_crf%>%
  left_join(sites%>%
              mutate(SiteID=as.character(SiteID)), b=c("siteid"="SiteID"))

baseline_crf_all_nations%>%
  count(Country)
  # 44766 in the UK, 1277 elsewhere

baseline_crf_all_nations%<>%
  filter(Country=="United Kingdom")%>%
  filter(rand_date<"2022-01-01")

baseline_crf%>%
  left_join(sites%>%
              mutate(SiteID=as.character(SiteID)), b=c("siteid"="SiteID"))%>%
  filter(Nation=="England")->baseline_crf_england

baseline_crf_england%>%
  distinct(study_number)%>%
  nrow() # 39952 in England (4814 elsewhere) at any time point

baseline_crf_england%>%distinct(study_number)%>%nrow()->RECOVERY_England_cohort_total

baseline_crf_england%>%distinct(study_number)%>%.[[1]]->RECOVERY_England_cohort_list

nrow(RECOVERY_HES_flags)
 # 38510 people recruited in England with HES data (any age, any period)



## Calculate and apply index dates ------

# extract index dates 
baseline_crf_england%>%
  left_join(RECOVERY_HES_flags%>% select(study_id, indexdate, age_at_index_date = age), by=c("study_number"="study_id"))%>%  # join HES index date
  mutate(age=if_else(!is.na(age_at_index_date), age_at_index_date, age))

baseline_crf_england%>%
  left_join(RECOVERY_HES_flags%>% select(study_id, indexdate, age_at_index_date = age), by=c("study_number"="study_id"))%>%  # join HES index date
  mutate(age=if_else(!is.na(age_at_index_date), age_at_index_date, age))%>%
  mutate(indexdate=if_else(is.na(indexdate), rand_date, indexdate))%>%
  mutate(indexdate=if_else(indexdate<"2020-03-01", rand_date, indexdate))%>% # if index date is before March then apply randomisation date
  select(study_number, age, indexdate)%>%
  mutate(study_number=as.character(study_number))->index_dates

# assign index date (in HES data) as randomisation date for RECOVERY participants with index date before March 2020 
RECOVERY_HES_flags%<>%
  select(-rand_date)%>%
  left_join(baseline_crf_england%>%select(study_id=study_number, rand_date))%>%
  mutate(indexdate=if_else(indexdate<"2020-03-01", rand_date, indexdate))
  


index_dates%>%
  filter(indexdate<="2021-11-30")->analysis_population
nrow(analysis_population)
# 39412 with index date within study period (524 outside)

## Restrict to people aged 16 or over -------

analysis_population%>%
  filter(age<16)%>%
  nrow() # 270 below 16 years

analysis_population%<>%
    filter(age>=16)%>%
    select(study_number)%>%
    .[[1]] 
  
length(analysis_population)
# 39412 aged 16 or over (analysis population: England, >=16, within analysis period)

baseline_crf_england%<>%
    filter(study_number %in% analysis_population)
  
RECOVERY_HES_flags%<>%
    filter(study_id %in% analysis_population)
  
nrow(RECOVERY_HES_flags) # 38510 people have HES data

RECOVERY_HES_flags%>%
  distinct(study_id)%>%
  .[[1]]->hes_participants

length(setdiff(analysis_population, RECOVERY_HES_flags%>%distinct(study_id)%>%.[[1]]))
  
# 648 people with no HES data
  
## Recalculate age groups --------  
  

baseline_crf_all_nations%<>%
  mutate(agegr2=as.factor(case_when(age<60 ~ "<60",
                           between(age, 60, 69) ~ "60-69",
                           between(age, 70, 79) ~ "70-79",
                           between(age, 80, 89) ~ "80-89",
                           age>=90 ~ "90+")))
  

baseline_crf_england%<>%
  mutate(agegr2=as.factor(case_when(age<60 ~ "<60",
                                    between(age, 60, 69) ~ "60-69",
                                    between(age, 70, 79) ~ "70-79",
                                    between(age, 80, 89) ~ "80-89",
                                    age>=90 ~ "90+")))





# 4. Analyse CRF data  -------------


## Baseline characteristics per UK nation (supplementary table 1) -------

baseline_crf_all_nations%>%
  # filter(rand_date<="2021-11-30")%>%

  select(Nation,
         age,
         agegr2,
         sex,
         race,
         lung_crf,
         diabetes_crf,
         heart_crf,
         liver_crf,
         renal_crf,
         # renal_replacement_crf,
         # immunosuppression,
         niv,
         imv,
         oxy)%>%
  
  mutate(agegr2=as.factor(case_when(age<60 ~ "<60",
                                        between(age, 60, 69) ~ "60-69",
                                        between(age, 70, 79) ~ "70-79",
                                        between(age, 80, 89) ~ "80-89",
                                        age>=90 ~ "90+")))%>%
  
  mutate(niv_oxy = case_when(niv=="Y" | oxy=="Y" ~ "Y", 
                             niv %in% c("N", "U") & oxy %in% c("N", "U") ~ "N",
                             niv=="U" & oxy=="U" ~ "U"))%>%
  
  mutate(resp_status = case_when(imv=="Y" ~ "Invasive mechanical ventilation or ECMO",
                                 imv=="N" & niv_oxy =="Y" ~ "Non-invasive mechanical ventilation or supplementary oxygen",
                                 imv=="N" & niv_oxy=="N" ~ "None",
                                 imv=="U" & niv_oxy=="U" ~ "Unknown"))%>%
  
  mutate(sex = case_when(sex == "F" ~ "Female",
                         sex == "M" ~ "Male",
                         sex == "" ~ "Unknown"),
         race = case_when(race == "ASIAN" ~ "Asian",
                          race == "BLACK" ~ "Black",
                          race == "MIXED" ~ "Mixed",
                          race == "OTHER" ~ "Other",
                          race == "UNKNOWN" | race =="" ~ "Unknown",
                          race == "WHITE" ~ "White"),
         diabetes_crf = case_when(diabetes_crf=="Y" ~ "Yes",
                                  diabetes_crf=="N" ~ "No",
                                  diabetes_crf=="U" ~ "Unknown"),
         heart_crf = case_when(heart_crf=="Y" ~ "Yes",
                               heart_crf=="N" ~ "No",
                               heart_crf=="U" ~ "Unknown"),
         liver_crf = case_when(liver_crf=="Y" ~ "Yes",
                               liver_crf=="N" ~ "No",
                               liver_crf=="U" ~ "Unknown"),
         renal_crf = case_when(renal_crf=="Y" ~ "Yes",
                               renal_crf=="N" ~ "No",
                               renal_crf=="U" ~ "Unknown"),
         lung_crf = case_when(lung_crf=="Y" ~ "Yes",
                              lung_crf=="N" ~ "No",
                              lung_crf=="U" ~ "Unknown"),
         # immunosuppression = case_when(immunosuppression=="Y" ~ "Yes",
         #                               immunosuppression=="N" ~ "No",
         #                               immunosuppression=="U" | is.na(immunosuppression)~ "Unknown"),
         imv = case_when(imv=="Y" ~ "Yes",
                         imv=="N" ~ "No",
                         imv=="U" ~ "Unknown"),
         niv_oxy = case_when(niv_oxy=="Y" ~ "Yes",
                             niv_oxy=="N" ~ "No",
                             niv_oxy=="U" ~ "Unknown"),
         # renal_replacement_crf = case_when(renal_replacement_crf=="Y" ~ "Yes",
         #                                   renal_replacement_crf=="N" ~ "No",
         #                                   renal_replacement_crf=="" ~ "Unknown")
         )%>%

  mutate(across(c(diabetes_crf, 
                  heart_crf, 
                  liver_crf, 
                  renal_crf, 
                  lung_crf, 
                  # renal_replacement_crf,
                  resp_status,
                  # immunosuppression
                  ),
                ~as.factor(.)))%>%
  mutate(across(c(diabetes_crf, 
                  heart_crf, 
                  liver_crf, 
                  renal_crf, 
                  lung_crf,
                  # immunosuppression
                  ),
                ~fct_relevel(., "Yes", "No")))%>%
  # mutate(renal_replacement_crf = fct_relevel(renal_replacement_crf, "Yes"))%>%
  mutate(resp_status = fct_relevel(resp_status, "Invasive mechanical ventilation or ECMO", "Non-invasive mechanical ventilation or supplementary oxygen", "None"))%>%
  mutate(race= na_if(race,"Unknown"))%>%
  mutate(sex=reorder(sex,sex, length))%>%
  mutate(sex=reorder(sex,desc(sex)))%>%
  mutate(race=fct_relevel(race, "White", "Black", "Asian", "Other", "Mixed"),
         sex=fct_relevel(sex, "Male", "Female", "Unknown"))%>%
  select(-niv, -oxy, -imv, -niv_oxy)%>%
  relocate(resp_status, .before = "lung_crf")%>%
  mutate(across(c(race), is.na, .names = "{.col}_missing"))%>%
  relocate(race_missing, .after = "race")%>%
  
  
  tbl_summary(by="Nation",
              value=list(diabetes_crf ~"Yes",
                        heart_crf ~"Yes",
                        liver_crf ~"Yes",
                        renal_crf ~"Yes",
                        lung_crf ~"Yes",
                        # renal_replacement_crf ~"Yes",
                        # immunosuppression ~ "Yes",
                        sex ~ "Female"
                        
                        ),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               race ~ c("{n} ({p}%)",
                                        "{N_miss} ({p_miss})"),
                               all_categorical() ~ "{n} ({p}%)"),
              label=list(
                age ~ "Age",
                agegr2 ~ "Age group",
                sex ~ "Female",
                race ~ "Ethnicity",
                resp_status ~ "Respiratory support status",
                diabetes_crf ~ "Diabetes",
                heart_crf ~ "Chronic heart disease",
                liver_crf ~ "Severe liver disease",
                renal_crf ~ "Severe renal impairment",
                lung_crf ~ "Chronic lung disease",
                # renal_replacement_crf ~ "Renal replacement therapy",
                # immunosuppression ~ "Immunosuppression",
              race_missing ~ "Unknown"),
              missing = "no")%>%
  modify_column_indent(columns=label, rows=endsWith(variable, "_missing"))%>%
  remove_row_type(c("agegr2"), type="header")%>%
  as_flex_table()%>%
  width(j=1, width=8, unit="cm")%>%
  footnote(j=1, i=8, ref_symbols = c("b"), value=as_paragraph("Ethnicity extracted from either primary care data (General Practice Extraction Service Data for Pandemic Planning and Research - GDPPR) or HES data for people in England, and  data sources equivalent to HES Scotland and Wales; no such data was available in Northern Ireland (participants recruited in Northern Ireland with known ethnicity had previous admissions recorded in one of the other nations); proportions for people with known and unknown ethnicity were calculated separately and using the entire cohort as denominator"))%>%
  footnote(j=1, i=15, ref_symbols = c("c"), value=as_paragraph("Respiratory status derived from the case-report form combined with linked data sources such as hospital admissions and intensive care data (except for Northern Ireland where no linkage data was available)"))%>%
  set_caption("Baseline characteristics of the RECOVERY population recruited in the UK, split by nation (according to the case report form)")%>%
  add_footer_lines("Cohorts not restricted on age or index date. ECMO - extracorporeal membrane oxygenation; HES - Hospital Episode Statistics", top = T)->crf_summary_nations_table

crf_summary_nations_table


sect_properties <- prop_section(page_size = page_size(
  orient = "landscape",
  width = 15,
  #height = 11.7
                        ),
  type = "continuous",
  page_margins = page_mar())


save_as_docx(crf_summary_nations_table, path = "K:/QNAP/RECOVERY-deidentified/Generalizability and representativeness project/RECOVERY_generalizability_representativeness/Outputs/Publication/Tables/supplementary_table_S1_crf_summary_nations_table.docx",
             pr_section = sect_properties)


rm(crf_summary_nations_table)





## Baseline characteristics based on HES linkage status (supplementary table 2) -------


baseline_crf_england%>%
  mutate(hes_flag = factor(if_else(study_number %in% hes_participants, "HES data available", "HES data unavailable")))%>%
  select(hes_flag,
         age,
         agegr2,
         sex,
         race,
         lung_crf,
         diabetes_crf,
         heart_crf,
         liver_crf,
         renal_crf,
         # renal_replacement_crf,
         niv,
         imv,
         oxy)%>%
  
  mutate(agegr2=as.factor(case_when(age<60 ~ "<60",
                                    between(age, 60, 69) ~ "60-69",
                                    between(age, 70, 79) ~ "70-79",
                                    between(age, 80, 89) ~ "80-89",
                                    age>=90 ~ "90+")))%>%
  
  mutate(niv_oxy = case_when(niv=="Y" | oxy=="Y" ~ "Y", 
                             niv %in% c("N", "U") & oxy %in% c("N", "U") ~ "N",
                             niv=="U" & oxy=="U" ~ "U"))%>%
  
  mutate(resp_status = case_when(imv=="Y" ~ "Invasive mechanical ventilation or ECMO",
                                 imv=="N" & niv_oxy =="Y" ~ "Non-invasive mechanical ventilation or supplementary oxygen",
                                 imv=="N" & niv_oxy=="N" ~ "None",
                                 imv=="U" & niv_oxy=="U" ~ "Unknown"))%>%
  
  mutate(sex = case_when(sex == "F" ~ "Female",
                         sex == "M" ~ "Male",
                         sex == "" ~ "Unknown"),
         race = case_when(race == "ASIAN" ~ "Asian",
                          race == "BLACK" ~ "Black",
                          race == "MIXED" ~ "Mixed",
                          race == "OTHER" ~ "Other",
                          race == "UNKNOWN" | race =="" ~ "Unknown",
                          race == "WHITE" ~ "White"),
         diabetes_crf = case_when(diabetes_crf=="Y" ~ "Yes",
                                  diabetes_crf=="N" ~ "No",
                                  diabetes_crf=="U" ~ "Unknown"),
         heart_crf = case_when(heart_crf=="Y" ~ "Yes",
                               heart_crf=="N" ~ "No",
                               heart_crf=="U" ~ "Unknown"),
         liver_crf = case_when(liver_crf=="Y" ~ "Yes",
                               liver_crf=="N" ~ "No",
                               liver_crf=="U" ~ "Unknown"),
         renal_crf = case_when(renal_crf=="Y" ~ "Yes",
                               renal_crf=="N" ~ "No",
                               renal_crf=="U" ~ "Unknown"),
         lung_crf = case_when(lung_crf=="Y" ~ "Yes",
                              lung_crf=="N" ~ "No",
                              lung_crf=="U" ~ "Unknown"),
         imv = case_when(imv=="Y" ~ "Yes",
                         imv=="N" ~ "No",
                         imv=="U" ~ "Unknown"),
         niv_oxy = case_when(niv_oxy=="Y" ~ "Yes",
                             niv_oxy=="N" ~ "No",
                             niv_oxy=="U" ~ "Unknown"),
         # renal_replacement_crf = case_when(renal_replacement_crf=="Y" ~ "Yes",
         #                                   renal_replacement_crf=="N" ~ "No",
         #                                   renal_replacement_crf=="" ~ "Unknown")
         )%>%
  
  mutate(across(c(diabetes_crf, 
                  heart_crf, 
                  liver_crf, 
                  renal_crf, 
                  lung_crf, 
                  # renal_replacement_crf,
                  resp_status),
                ~as.factor(.)))%>%
  mutate(across(c(diabetes_crf, 
                  heart_crf, 
                  liver_crf, 
                  renal_crf, 
                  lung_crf),
                ~fct_relevel(., "Yes", "No")))%>%
  # mutate(renal_replacement_crf = fct_relevel(renal_replacement_crf, "Yes"))%>%
  mutate(resp_status = fct_relevel(resp_status, "Invasive mechanical ventilation or ECMO", "Non-invasive mechanical ventilation or supplementary oxygen", "None"))%>%
  mutate(race= na_if(race,"Unknown"))%>%
  mutate(sex=reorder(sex,sex, length))%>%
  mutate(sex=reorder(sex,desc(sex)))%>%
  mutate(race=fct_relevel(race, "White", "Black", "Asian", "Other", "Mixed"),
         sex=fct_relevel(sex, "Male", "Female"))%>%
  
  select(-niv, -oxy, -imv, -niv_oxy)%>%
  relocate(resp_status, .before = "lung_crf")%>%
  mutate(across(c(race), is.na, .names = "{.col}_missing")) %>%
  relocate(race_missing, .after = "race")%>%
  
  
  tbl_summary(by=hes_flag,
              value=list(diabetes_crf ~"Yes",
                         heart_crf ~"Yes",
                         liver_crf ~"Yes",
                         renal_crf ~"Yes",
                         lung_crf ~"Yes",
                         # renal_replacement_crf ~"Yes",
                         sex ~ "Female"
                         
              ),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               race ~ c("{n} ({p}%)",
                                        "{N_miss} ({p_miss})"),
                               all_categorical() ~ "{n} ({p}%)"),
              label=list(
                age ~ "Age",
                agegr2 ~ "Age group",
                sex ~ "Female",
                race ~ "Ethnicity",
                resp_status ~ "Respiratory support status",
                diabetes_crf ~ "Diabetes",
                heart_crf ~ "Chronic heart disease",
                liver_crf ~ "Severe liver disease",
                renal_crf ~ "Severe renal impairment",
                lung_crf ~ "Chronic lung disease",
                # renal_replacement_crf ~ "Renal replacement therapy",
                race_missing ~ "Unknown"),
              missing = "no")%>%
  modify_column_indent(columns=label, rows=endsWith(variable, "_missing"))%>%
  remove_row_type(c("agegr2"), type="header")%>%
  as_flex_table()%>%
  add_footer_lines(top=T, "Restricted to RECOVERY participants aged >=16 years and recruited in England within the analysis period. ECMO - extracorporeal membrane oxygenation; HES - Hospital Episode Statistics; SD - standard deviation")%>%
  footnote(j=1, i=8, ref_symbols = c("b"), value=as_paragraph("Ethnicity extracted from either primary care data (General Practice Extraction Service Data for Pandemic Planning and Research - GDPPR) or HES data. Proportions for people with known and unknown ethnicity were calculated separately, using the entire cohort as denominator"))%>%
  footnote(j=1, i=15, ref_symbols = c("c"), value=as_paragraph("Respiratory status derived from the case-report form combined with linked data sources such as hospital admissions and intensive care data"))%>%
  set_caption("Baseline characteristics of the RECOVERY population recruited in England split by HES linkage status (based on the case-report form)")->linkage_baseline_status
  
linkage_baseline_status

  
sect_properties <- prop_section(page_size = page_size(
  orient = "landscape",
  width = 15,
  #height = 11.7
),
type = "continuous",
page_margins = page_mar())


save_as_docx(linkage_baseline_status, path = "K:/QNAP/RECOVERY-deidentified/Generalizability and representativeness project/RECOVERY_generalizability_representativeness/Outputs/Publication/Tables/supplementary_table_S2_crf_summary_linkage_status_table.docx",
             pr_section = sect_properties)


rm(linkage_baseline_status)




# 7. Baseline characteristics (Table 1) ----------

## RECOVERY -----

# pivot HES data
hes_flags_pivotted<-RECOVERY_HES_flags%>%
  select(study_number=study_id,
         age,
         # covid_primary,
         # covid_anydiag,
         # u071_anydiag,
         # u072_anydiag,
         # u072_only_anydiag,
         sex,
         ethnicity=ethgrp,
         imd19_quintile,
         cancer_mets=metastasis,
         hiv,
         liver_moderate_severe=severe_liver,
         hemi_paraplegia,
         ckd=renal,
         dm_complicated = diabetes_with_comp,
         dm_uncomplicated=diabetes_without_comp,
         mi,
         cancer_solid_non_mets=cancer,
         leukemia=leukaemia,
         lymphoma,
         chf=congestive_hf,
         cerebrovascular,
         chronic_lung_disease=chronic_pulmonary,
         rheumatic_disease=rheumatic,
         peptic_ulcer,
         dementia,
         pad=periph_vasc,
         liver_mild=mild_liver,
         Charlson=charlson_score_age,
         Charlson_no_age = charlson_score,
         rrt=rrt_algorithm,
         immunosuppression=immunosup,
         cardiac_chronic=heart,
         dm_any=diabetes,
         severe_mental_illness = psychiatric,
         alcoholism=alcohol,
         obesity,
         hospital_frailty_score=frailty_score)%>%
  mutate(liver_chronic=if_else(liver_mild=="1" | liver_moderate_severe=="1", "1", "0"))%>% # apply definition of chronic liver disease (includes mild, moderate, or severe groups)
  mutate(across(everything(), ~as.character(.)))%>%
  pivot_longer(-study_number, 
               names_to = "codelist",
               values_to = "hes")



Table1_RECOVERY_HES<-
  hes_flags_pivotted%>%
  rename(Characteristic=codelist)%>%
  filter(study_number%in%hes_participants)%>%
  mutate(flag=if_else(is.na(hes) & Characteristic=="imd19_quintile", "Unknown", hes))%>%
  mutate(flag=replace_na(flag, "0"))%>%
  filter(!Characteristic %in% c("anticoagulants",
                                "antiplatelets",
                                "any_raasi",
                                "aspirin",
                                "cancer_any",
                                "diabetes_drugs_not_insulin",
                                "immunosuppressive_drugs",
                                "insulin",
                                "nebulisers_asthma_copd",
                                "other_antiplatelets",
                                "smoking",
                                "sglt2",
                                "haematological_cancer",
                                "systemic_steroids",
                                "age"))%>%
  # prepare for table format
  pivot_wider(id_cols=study_number, names_from="Characteristic", values_from="flag")%>%
  left_join(RECOVERY_HES_flags%>%
              select(study_id, age)%>%
              mutate(study_number=as.character(study_id))%>%
              select(-study_id))%>%
  mutate(age=as.numeric(age),
         sex=as.factor(sex),
         Charlson=as.numeric(Charlson),
         hospital_frailty_score=as.numeric(hospital_frailty_score))%>%
  
  # recode sex
  mutate(sex=as.factor(case_when(sex==2 ~ "F",
                                 sex==1 ~ "M")))%>%
  
  # create charlson groups
  # mutate(`Charlson score group` = case_when(Charlson == 0 ~ "0",
  #                                           Charlson == 1 ~ "1",
  #                                           Charlson == 2 ~ "2",
  #                                           between(Charlson, 3,5) ~ "3-5",
  #                                           Charlson>=6 ~ "6+"))%>%
  
  
  # create age groups
  
mutate(age=as.numeric(age))%>%
  mutate(`Age groups`=as.factor(case_when(age<60 ~ "<60",
                                          between(age, 60, 69) ~ "60-69",
                                          between(age, 70, 79) ~ "70-79",
                                          age>=80 ~ "80+")))%>%
  
  # create hospital frailty score groups
  mutate(hospital_frailty_score = as.numeric(hospital_frailty_score))%>%
  mutate(`Hospital frailty score group` = case_when(hospital_frailty_score <5 ~"Low risk (<5)",
                                                    between(hospital_frailty_score, 5, 15) ~"Intermediate risk (5-15)",
                                                    hospital_frailty_score >15 ~"High-risk (>15)"),
         `Hospital frailty score group` = fct_relevel(`Hospital frailty score group`,
                                                      "Low risk (<5)",
                                                      "Intermediate risk (5-15)",
                                                      "High-risk (>15)"))%>%
  
  # data format transformations
  
  ungroup()%>%
  mutate(across(-c(age, sex, Charlson, study_number,
                   # `Charlson score group`,
                   `Charlson_no_age`, hospital_frailty_score, `Hospital frailty score group`, ethnicity, `Age groups`, imd19_quintile), ~replace_na(., "No")),
         ethnicity = replace_na(ethnicity, "Unknown"),
         hospital_frailty_score=as.numeric(hospital_frailty_score),
         imd19_quintile = replace_na(imd19_quintile, "Unknown"),
         `Age groups` = as.factor(`Age groups`),
         across(-c(age, sex, Charlson, study_number,
                   # `Charlson score group`,
                   `Charlson_no_age`, `Age groups`, hospital_frailty_score, `Hospital frailty score group`, ethnicity, imd19_quintile), ~if_else(.=="1", "Yes", "No")))%>%
  
  # cohort flag
  mutate(Cohort="RECOVERY")%>%
  
  select(Age=age,
         `Age groups`,
         Sex=sex,
         Ethnicity=ethnicity,
         `Index of multiple deprivation (quintile)` = imd19_quintile,
         `Charlson score, median (IQR)`=Charlson,
         # `Charlson score group`,
         `Myocardial infarction` = mi,
         `Congestive heart failure` = chf,
         `Peripheral vascular disease` = pad,
         `Cerebrovascular disease` = cerebrovascular,
         `Chronic pulmonary disease` = chronic_lung_disease,
         `Rheumatic disease` = rheumatic_disease,
         Dementia = dementia,
         `Peptic ulcer disease` = peptic_ulcer,
         `Liver disease (mild)` = liver_mild,
         `Liver disease (moderate-severe)` = liver_moderate_severe,
         `Diabetes mellitus (without chronic complications)` = dm_uncomplicated,
         `Diabetes mellitus (with chronic complications)` = dm_complicated,
         `Chronic kidney disease` = ckd,
         `Solid tumour` = cancer_solid_non_mets,
         `Metastatic cancer` = cancer_mets,
         Lymphoma = lymphoma,
         Leukaemia = leukemia,
         `AIDS/HIV` = hiv,
         `Hospital frailty score, median (IQR)` = hospital_frailty_score,
         `Hospital frailty score group`,
         `Renal replacement therapy` = rrt,
         Immunosuppression =  immunosuppression,
         Obesity = obesity,
         `Severe mental illness`= severe_mental_illness,
         `Alcohol-attributable diseases` = alcoholism,
         Cohort)%>%
  mutate(Sex=case_when(Sex=="M" ~ "Male",
                       Sex=="F" ~ "Female"),
         Ethnicity= na_if(Ethnicity,"Unknown"),
         Ethnicity=fct_relevel(Ethnicity, "White", "Black", "Asian", "Other", "Mixed"),
         Sex=fct_relevel(Sex, "Female", "Male"),
  )%>%
  mutate(across(c(Ethnicity), is.na, .names = "{.col}_missing")) %>%
  mutate(`Charlson score, median (IQR)` = as.integer(`Charlson score, median (IQR)`))%>%
  relocate(Ethnicity_missing, .after = "Ethnicity")

  


## Reference population  -----------

Table1_National_HES<-
  National_HES_flags%>%
  select(-c(indexdate, study, epikey, obesity_index, extract, rand_date, charlson_grp, agegrp))%>%
  rename(hospital_frailty_score=frailty_score,
         Charlson_score=charlson_score_age,
         ethnicity=ethgrp,
         Charlson_no_age = charlson_score)%>%
  
  # merge IMD data
  bind_cols(
    
    tibble(
      imd19_quintile=c(
        rep("1 (Most deprived)", 
          National_HES_flags_IMD_unscrambled%>%
          select(imd19_quintile, count)%>%
          group_by(imd19_quintile)%>%
          summarise(n=sum(count))%>%
          filter(imd19_quintile=="1 (Most deprived)")%>%
          select(n)%>%
          .[[1]]),
        rep("2", 
            National_HES_flags_IMD_unscrambled%>%
              select(imd19_quintile, count)%>%
              group_by(imd19_quintile)%>%
              summarise(n=sum(count))%>%
              filter(imd19_quintile=="2")%>%
              select(n)%>%
              .[[1]]),
        rep("3", 
            National_HES_flags_IMD_unscrambled%>%
              select(imd19_quintile, count)%>%
              group_by(imd19_quintile)%>%
              summarise(n=sum(count))%>%
              filter(imd19_quintile=="3")%>%
              select(n)%>%
              .[[1]]),
        rep("4", 
            National_HES_flags_IMD_unscrambled%>%
              select(imd19_quintile, count)%>%
              group_by(imd19_quintile)%>%
              summarise(n=sum(count))%>%
              filter(imd19_quintile=="4")%>%
              select(n)%>%
              .[[1]]),
        rep("5 (Least deprived)", 
            National_HES_flags_IMD_unscrambled%>%
              select(imd19_quintile, count)%>%
              group_by(imd19_quintile)%>%
              summarise(n=sum(count))%>%
              filter(imd19_quintile=="5 (Least deprived)")%>%
              select(n)%>%
              .[[1]]),
        rep("Unknown", 
            National_HES_flags_IMD_unscrambled%>%
              select(imd19_quintile, count)%>%
              group_by(imd19_quintile)%>%
              summarise(n=sum(count))%>%
              filter(is.na(imd19_quintile))%>%
              select(n)%>%
              .[[1]])
              )
    )
  )%>%
  
  # prepare for table format
  mutate(age=as.numeric(age),
         sex=as.factor(case_when(sex==2 ~ "F",
                                 sex==1 ~ "M")),
         Charlson_score=as.numeric(Charlson_score),
         hospital_frailty_score=as.numeric(hospital_frailty_score))%>%
  
  # create charlson groups
  mutate(Charlson_score=as.integer(Charlson_score))%>%
  # mutate(`Charlson score group` = case_when(Charlson_score == 0 ~ "0",
  #                                           Charlson_score == 1 ~ "1",
  #                                           Charlson_score == 2 ~ "2",
  #                                           between(Charlson_score, 3,5) ~ "3-5",
  #                                           Charlson_score>=6 ~ "6+"))%>%
  
  
  # create age groups
  mutate(age=as.numeric(age))%>%
  mutate(`Age groups`=as.factor(case_when(age<60 ~ "<60",
                                          between(age, 60, 69) ~ "60-69",
                                          between(age, 70, 79) ~ "70-79",
                                          age>=80 ~ "80+")))%>%
  
  # create hospital frailty score groups
  mutate(hospital_frailty_score = as.numeric(hospital_frailty_score))%>%
  mutate(`Hospital frailty score group` = case_when(hospital_frailty_score <5 ~"Low risk (<5)",
                                                    between(hospital_frailty_score, 5, 15) ~"Intermediate risk (5-15)",
                                                    hospital_frailty_score >15 ~"High-risk (>15)"),
         `Hospital frailty score group` = fct_relevel(`Hospital frailty score group`, 
                                                      "Low risk (<5)", 
                                                      "Intermediate risk (5-15)",
                                                      "High-risk (>15)"))%>%
  
  

  # data format transformations
    mutate(across(everything(), ~as.character(.)))%>%
  mutate(
    across(-c(age, sex, Charlson_score, 
              # `Charlson score group`, 
              Charlson_no_age, hospital_frailty_score, `Hospital frailty score group`, ethnicity), ~replace_na(., "No")),
         ethnicity = replace_na(ethnicity, "Unknown"),
         across(-c(age, sex, Charlson_score, 
                   # `Charlson score group`, 
                   Charlson_no_age, `Age groups`, hospital_frailty_score, `Hospital frailty score group`, ethnicity, imd19_quintile), ~if_else(.=="1", "Yes", "No"))
         )


## Combine table -----


Table1_data_combined<-Table1_National_HES %>%
  select(Age=age,
         `Age groups`,
         Sex=sex,
         Ethnicity=ethnicity,
         `Index of multiple deprivation (quintile)` = imd19_quintile,
         `Charlson score, median (IQR)`=Charlson_score,
         # `Charlson score group`,
         `Myocardial infarction` = mi,
         `Congestive heart failure` = congestive_hf,
         `Peripheral vascular disease` = periph_vasc,
         `Cerebrovascular disease` = cerebrovascular,
         `Chronic pulmonary disease` = chronic_pulmonary,
         `Rheumatic disease` = rheumatic,
         Dementia = dementia,
         `Peptic ulcer disease` = peptic_ulcer,
         `Liver disease (mild)` = mild_liver,
         `Liver disease (moderate-severe)` = severe_liver,
         `Diabetes mellitus (without chronic complications)` = diabetes_without_comp,
         `Diabetes mellitus (with chronic complications)` = diabetes_with_comp,
         `Chronic kidney disease` = renal,
         `Solid tumour` = cancer,
         `Metastatic cancer` = metastasis,
         Lymphoma = lymphoma,
         Leukaemia = leukaemia,
         `AIDS/HIV` = hiv,
         `Hospital frailty score, median (IQR)` = hospital_frailty_score,
         `Hospital frailty score group`,
         `Renal replacement therapy` = rrt_algorithm,
         Immunosuppression =  immunosup,
         Obesity = obesity,
         `Severe mental illness`= psychiatric,
         `Alcohol-attributable diseases` = alcohol)%>%
  
  mutate(Sex=case_when(Sex=="M" ~ "Male",
                       Sex=="F" ~ "Female"),
         Ethnicity= na_if(Ethnicity,"Unknown"),
         Ethnicity=fct_relevel(Ethnicity, "White", "Black", "Asian", "Other", "Mixed"),
         Sex=fct_relevel(Sex, "Female", "Male"),
         `Charlson score, median (IQR)` = as.integer(`Charlson score, median (IQR)`),
         Age = as.numeric(Age),
         `Hospital frailty score, median (IQR)` = as.numeric(`Hospital frailty score, median (IQR)`)
  )%>%
  mutate(across(c(Ethnicity), is.na, .names = "{.col}_missing")) %>%
  relocate(Ethnicity_missing, .after = "Ethnicity")%>%
  mutate(Cohort="Reference population")%>%
  
  rbind(Table1_RECOVERY_HES)%>%
  mutate(Cohort=fct_relevel(Cohort, "RECOVERY", "Reference population"))



#### produce table



table1_final<-
  Table1_data_combined %>%
  # mutate(across(everything(), ~replace_na(., "Unknown")))%>%
  tbl_summary(value=list("AIDS/HIV"~"Yes"),
              type=list("Charlson score, median (IQR)" ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               Ethnicity ~ c("{n} ({p}%)",
                                        "{N_miss} ({p_miss})"),
                               all_categorical() ~ "{n} ({p}%)",
                               "Charlson score, median (IQR)" ~ "{median} ({p25}, {p75})",
                               "Hospital frailty score, median (IQR)" ~ "{median} ({p25}, {p75})"),
              label = c(Ethnicity_missing ~ "Unknown"),
              missing="no",
              by="Cohort",
              digits=list(all_numeric()~c(1,1),
                          all_categorical()~c(0,1)))%>%
  remove_row_type(c("Age groups", "Hospital frailty score group",
                    #"Charlson score group"
                    ), type="header")%>%
  modify_column_indent(columns=label, rows=endsWith(variable, "_missing"))%>%
  modify_table_body(
    
    ~.x %>%
      rbind(tibble(variable="Other comorbidities/demographics",
                   var_type=NA,
                   var_label="Other comorbidities/demographics",
               row_type="label",
               label="Other comorbidities/demographics",
               stat_1 = NA,
               stat_2 = NA)
      )%>%
      arrange(factor(variable, levels=c("Age",
                                        "Age groups",
                                        "Sex",
                                        "Ethnicity",
                                        "Ethnicity_missing",
                                        "Index of multiple deprivation (quintile)",
                                        "Charlson score, median (IQR)",
                                        # "Charlson score group",
                                        "Charlson score components",
                                        "Myocardial infarction",
                                        "Congestive heart failure",
                                        "Peripheral vascular disease",
                                        "Cerebrovascular disease",
                                        "Chronic pulmonary disease",
                                        "Rheumatic disease",
                                        "Dementia",
                                        "Peptic ulcer disease",
                                        "Liver disease (mild)",
                                        "Liver disease (moderate-severe)",
                                        "Diabetes mellitus (without chronic complications)",
                                        "Diabetes mellitus (with chronic complications)",
                                        "Chronic kidney disease",
                                        "Solid tumour",
                                        "Metastatic cancer",
                                        "Lymphoma",
                                        "Leukaemia",
                                        "AIDS/HIV",
                                        "Hospital frailty score, median (IQR)",
                                        "Hospital frailty score group",
                                        "Other comorbidities/demographics",
                                        "Renal replacement therapy",
                                        "Immunosuppression",
                                        "Obesity",
                                        "Severe mental illness",
                                        "Alcohol-attributable diseases"
      ))))%>%
  
  modify_column_indent(columns=label, rows=(!variable%in%c("Age",
                                                           "Sex",
                                                           "Ethnicity",
                                                           "Ethnicity_missing",
                                                           "Index of multiple deprivation (quintile)",
                                                           "Charlson score, median (IQR)",
                                                           "Charlson score components",
                                                           "Hospital frailty score, median (IQR)",
                                                           "Other comorbidities/demographics")))%>%

  as_flex_table()%>%
  footnote(j=1, i=9, ref_symbols = c("b"), value=as_paragraph("Proportions for people with known and unknown ethnicity were calculated separately, using the entire cohort as denominator"))%>%
  footnote(j=1, i=41, ref_symbols = c("c"), value=as_paragraph("ICD-10 codes for AIDS/HIV are censored from HES data"))%>%
  add_footer_lines(top=T, "HES - Hospital Episode Statistics; IQR - interquartile range; SD - standard deviation")%>%
  set_caption("Baseline characteristics of the RECOVERY HES and All-England HES cohorts")%>%
  width(j=c(2,3), width=3, unit="cm")%>%
  width(j=1, width=9, unit="cm")

table1_final


sect_properties <- prop_section(page_size = page_size(
  orient = "portrait",
  # width = 11.7,
  # height = 11.7
),
type = "continuous",
page_margins = page_mar())


save_as_docx(table1_final,
             path="Outputs/Tables/Table_1_cohort_characteristics.docx",
             pr_section = sect_properties)


rm(table1_final)








# 8. Maps ------
  

##  RECOVERY participants with no HES data (not used) -------

# mapping performed using Trust location as no other location data avaialble for these people (could use GPES but the focus here is HES)

baseline_crf_england%>%
  left_join(sites)%>%
  mutate(`Has HES data` = as.character(ifelse(study_number %in% hes_participants, 1, 0)))%>%
  group_by(siteid, SiteName, Postcode, `Has HES data`)%>%
  summarise(Participants = n_distinct(study_number))%>%
  group_by(siteid, SiteName, Postcode)%>%
  mutate(Proportion=round(Participants/sum(Participants)*100,1))->counts_per_trust
# 
# View(counts_per_trust)
# 
# write_csv(counts_per_trust, file="Outputs/Tables/recovery_counts_per_trust.csv")


#### import maps of all UK countries 
### from https://geoportal.statistics.gov.uk/datasets/ons::countries-december-2021-uk-bfc/explore?showTable=true
# 
postcodes<-read_csv("K:/QNAP/RECOVERY-deidentified/Team folders/Guilherme/RECOVERY_gui_analysis/Tools/UK maps/Postcode longitude and latitute/National_Statistics_Postcode_Lookup_UK_Coordinates.csv")

postcodes%<>%
  select(`Postcode 3`,
         Easting,
         Northing)

counts_per_trust%>%
  mutate(Postcode=str_to_upper(Postcode))%>%
  left_join(postcodes, by=c("Postcode"="Postcode 3"))%>%
  select(siteid, SiteName, Participants, `Has HES data`, Proportion, Easting, Northing)%>%
  mutate(SiteID=as.integer(siteid))->participants_sites









##  RECOVERY vs national HES -------

###  plot (Figure 2) --------
  
geographic_representation_ratios<-
  maps_data%>%
  left_join(lad_region_mapping, by=c("ladcd"="LAD21CD"))%>%
  group_by(RGN21CD, RGN21NM)%>%
  summarise(RECOVERY_total = sum(freq_recovery, na.rm=T),
            All_England_total = sum(freq_national, na.rm=T))%>%
  ungroup()%>%
  mutate(RGN21NM = replace_na(RGN21NM, "Unknown"))%>%
  mutate(`Recruitment ratio` = round(RECOVERY_total/All_England_total*100,1))

geographic_representation_ratios%<>%
  filter(!is.na(RGN21NM))%>%
  mutate(RECOVERY_prop = RECOVERY_total/sum(RECOVERY_total, na.rm=T),
       All_England_prop=All_England_total/sum(All_England_total, na.rm=T),
       Ratio=RECOVERY_prop/All_England_prop)%>%
  rbind(
    geographic_representation_ratios%>%
      filter(is.na(RGN21NM))%>%
      mutate(RECOVERY_prop = RECOVERY_total/sum(RECOVERY_total, na.rm=T),
             All_England_prop=All_England_total/sum(All_England_total, na.rm=T),
             Ratio=RECOVERY_prop/All_England_prop))%>%
  arrange(desc(All_England_prop))%>%
  mutate(RECOVERY_prop = round(RECOVERY_prop*100,1),
         All_England_prop = round(All_England_prop*100,1))
  

filename<-"K:/QNAP/RECOVERY-deidentified/Generalizability and representativeness project/RECOVERY_generalizability_representativeness/Tools/Maps/UK regions/RGN_DEC_2021_EN_BGC.shp"
  

library(rgdal)
regions_map<-raster::shapefile(filename)

regions_map$id<-row.names(regions_map)

regions_map_df<-broom::tidy(regions_map)

regions_map_df%<>%left_join(regions_map@data, by="id")

regions_map_df%<>%left_join(geographic_representation_ratios, by="RGN21CD")


representativeness_map<-
  ggplot(
    data=regions_map_df%>%
      mutate(Ratio=round(Ratio,2))%>%
      rename(Region=RGN21NM.x)
    )+
  geom_polygon(aes(x=long, y=lat, 
                   group=group,
                   fill=`Recruitment ratio`),
               color="black", size=0.1)+
  
  geom_point(data=participants_sites%>%
               group_by(siteid, Postcode, SiteName, Easting, Northing)%>%
               summarise(Participants = sum(Participants))%>%
               rename(`RECOVERY participants`=Participants,
                      Site = SiteName)%>%
               ungroup(),
             
               mapping=aes(x=Easting,
                         y=Northing,
                         size=`RECOVERY participants`,
                         group=Site),
             alpha=0.9,
             color="black",
             fill="light blue",
             pch=21)+
  theme_void()+
  scale_fill_viridis(
     limits=c(5,17),
     option = "magma",
     breaks=c(5,8,11,14,17)
    
    
  )+
  # scale_fill_gradient(
  #   high="#34cb99",
  #   low= "#CB3466",
  #   limits=c(0.5,1.5)
  # )+
  scale_size(breaks=rev(c(10,100,500,1000)))+
  
  labs(
    #title="Geographical representativeness of the RECOVERY HES population\nin comparison with the All-England HES population",
       fill="Recruitment\nratio (%)",
     #   caption="Representativeness ratio calculated as the ratio between the proportion of individuals in each region in RECOVERY HES over All-England HES\n(i.e. ratio > 1 indicates overrepresentation in RECOVERY, ratio <1 indicates underrepresentation)",
       size="RECOVERY\nparticipants")+
  theme(text=element_text(size=15,
                          family = "Mulish"),
        plot.caption = element_text(size=10,
                                    hjust=0),
        plot.background = element_rect(fill="white",
                                       color="transparent"),
        # legend.position="bottom",
        # legend.key.width = unit(2, 'cm'),
  )

representativeness_map

ggsave("Outputs/Figures/figure_2_representativeness_map.tiff",
       dpi = "retina",
       scale= 1)

ggsave("Outputs/Figures/figure_2_representativeness_map.png",
       dpi = "retina",
       scale= 1)




###  table (for figure) -------

geographic_representation_ratios%>%
  mutate(RECOVERY = paste0(RECOVERY_total, " (", RECOVERY_prop, "%)"),
         `Reference population` = paste0(All_England_total, " (", All_England_prop, "%)"))%>%
  flextable()->geographical_representation_table

geographical_representation_table

save_as_docx(geographical_representation_table, 
             path="Outputs/Tables/supplementary_table_S4_geographical_representation_table.docx")

    




























# 9. Representation of select characteristics over time -------

##  age and sex (supplementary figure 1) -------

### age ------

age_groups_along_time<-
  RECOVERY_HES_flags%>%
  select(study_id, indexdate, agegrp, sex)%>%
  mutate(month=as.Date(paste0(str_sub(indexdate, 1, 8), "01")))%>%
  select(-study_id, -indexdate)%>%
  group_by(agegrp, month, sex)%>%
  summarise(Individuals=n())%>%
  mutate(Cohort="RECOVERY HES")%>%
  
  rbind(
    National_HES_events%>%
      select(yearmonth,
             agegrp, sex)%>%
      mutate(month=paste0(yearmonth, "01"))%>%
      mutate(month=as.Date(month, format="%Y%m%d", origin = "1984-01-01"))%>%
  group_by(agegrp, month, sex)%>%
  summarise(Individuals=n())%>%
  mutate(Cohort="All-England HES"))
  

age_groups_along_time%>%
  mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                           "20-24",
                                           "25-29",
                                           "30-34",
                                           "35-39",
                                           "40-44",
                                           "45-49",
                                           "50-54",
                                           "55-59") ~ "<60",
                            agegrp %in% c("60-64",
                                           "65-69") ~ "60-69",
                            agegrp %in% c("70-74",
                                           "75-79") ~ "70-79",
                            agegrp %in% c("80-84",
                                          "85-89",
                                          "90-94",
                                          "95+") ~ "80+"))%>%
  mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
                            month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
                            month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
                            month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
                            month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
                            month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
                            month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21")))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%
    filter(!is.na(Period))%>%
  mutate(sex=as.factor(case_when(sex==2 ~ "Female",
                                 sex==1 ~ "Male")))%>%
  filter(!is.na(sex))%>%
  group_by(agegrp1, Period, Cohort, sex)%>%
  summarise(Individuals=sum(Individuals))%>%
  group_by(agegrp1, Period, Cohort, sex)%>%
  pivot_wider(c(Period, agegrp1, sex), names_from = "Cohort", values_from=c("Individuals"))%>%
  group_by(Period)%>%
  mutate(Proportion_RECOVERY=`RECOVERY HES`/(sum(`RECOVERY HES`, na.rm=T)),
         Proportion_National=`All-England HES`/(sum(`All-England HES`, na.rm=T)))%>%
  group_by(agegrp1, Period, sex)%>%
  mutate(Ratio=`Proportion_RECOVERY`/`Proportion_National`,
         # SE = ((abs(Ratio*(1-Ratio)))/sum(`RECOVERY HES`, na.rm=T)),
         EF_95 = exp(1.96*sqrt(1/`RECOVERY HES` + 1/`All-England HES`)))%>%

  filter(!is.na(agegrp1))->age_groups_along_time_ratios
  
p1<-age_groups_along_time_ratios%>%
  bind_rows(
    # add aggregate
    age_groups_along_time_ratios%>%
      ungroup()%>%
      mutate(`All-England HES`=as.numeric(`All-England HES`),
         `RECOVERY HES`=as.numeric(`RECOVERY HES`))%>%
      select(`All-England HES`, `RECOVERY HES`, sex, agegrp1)%>%
      group_by(sex, agegrp1)%>%
      summarise(across(c(`All-England HES`, `RECOVERY HES`), sum))%>%
      group_by(sex)%>%
      mutate(Proportion_RECOVERY=`RECOVERY HES`/(sum(`RECOVERY HES`, na.rm=T)),
             Proportion_National=`All-England HES`/(sum(`All-England HES`, na.rm=T)))%>%
      mutate(Ratio=`Proportion_RECOVERY`/`Proportion_National`,
             EF_95 = exp(1.96*sqrt(1/`RECOVERY HES` + 1/`All-England HES`))))%>%
  
  mutate(Period=as.character(Period))%>%
  mutate(Period=replace_na(Period, "All time periods"))%>%
  
  mutate(Period=factor(Period, levels=c("All time periods",
                                        "Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21" )))%>%
  mutate(Period=fct_relevel(Period, 
                            "All time periods",
                            "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%
  filter(!is.na(Period))%>%
  ggplot(aes(Period, Ratio, color=agegrp1, group=agegrp1, shape=agegrp1))+
  # geom_line()+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=Ratio/EF_95, ymax=Ratio*EF_95, group=agegrp1),
                width=0.05)+
  # scale_color_viridis()
  # facet_wrap(~agegrp, nrow=3)+
  geom_hline(aes(yintercept=1), color="black")+
  facet_grid(rows=vars(sex),
             switch="y")+
  
  scale_y_continuous(
    
                trans = "log2",
                labels = label_number(accuracy=0.1),
                breaks = c(0.2, 0.5, 1.0, 2, 5)
                )+
  coord_cartesian(ylim = c(0.2, 5))+
 
  theme(
    axis.text.x = element_blank(),
      axis.title.x = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        text=element_text(size=20))+
  labs(y="Representativeness ratio",
       x="Period",
       subtitle="Age groups",
       # title="Age representativeness of the RECOVERY HES population in comparison with the All-England HES population, split by age groups",
       # subtitle="Representativeness ratio",
       color="Age groups",
       shape="Age groups")+
  scale_shape_manual(values=seq(15,19,1))+
  geom_text_repel(aes(label=round(Ratio,1)),
                  # vjust=-2,
                  show.legend = F,
                  direction="y",
                  nudge_x = 0.1,
                  # force_pull = 5
                  min.segment.length=0.1	
                  )

  
p1



# "at risk" table 
at_risk_table<-age_groups_along_time_ratios%>%
  group_by(Period)%>%
  summarise(`RECOVERY HES` = sum(`RECOVERY HES`),
            `All-England HES` = sum(`All-England HES`))%>%
  select(Period,
         `RECOVERY` = `RECOVERY HES`,
         `Reference population`=`All-England HES`)%>%
  mutate(`Recruitment rate` = round(`RECOVERY`/`Reference population`*100,1))
  

write_csv(at_risk_table,
          "K:/QNAP/RECOVERY-deidentified/Generalizability and representativeness project/RECOVERY_generalizability_representativeness/Outputs/Tables/age_representativeness_at_risk_table.csv")


### sex -----


sex_over_time<-
  RECOVERY_HES_flags%>%
  select(study_id, indexdate, sex, agegrp)%>%
  mutate(month=paste0(str_sub(indexdate, 1, 8), "01"))%>%
  mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                           "20-24",
                                           "25-29",
                                           "30-34",
                                           "35-39",
                                           "40-44",
                                           "45-49",
                                           "50-54",
                                           "55-59") ~ "<60",
                             agegrp %in% c("60-64",
                                           "65-69") ~ "60-69",
                             agegrp %in% c("70-74",
                                           "75-79") ~ "70-79",
                             agegrp %in% c("80-84",
                                           "85-89",
                                           "90-94",
                                           "95+") ~ "80+"))%>%
  mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
                            month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
                            month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
                            month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
                            month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
                            month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
                            month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21")))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%
  group_by(Period, sex, agegrp1)%>%
  summarise(Individuals=n_distinct(study_id, na.rm=T))%>%
  mutate(Cohort="RECOVERY")%>%
  ungroup()%>%
  
  rbind(
    National_HES_flags_sex_unscrambled%>%
      rename(agegrp1=agebroad)%>%
      mutate(agegrp1 = case_when(agegrp1 =="<60" ~ "<60",
                                 agegrp1 == "60-69" ~ "60-69",
                                 agegrp1 == "70-79" ~ "70-79",
                                 agegrp1 %in% c("80-89", "90+") ~ "80+"))%>%
      group_by(agegrp1, Period, sex)%>%
      summarise(Individuals=n())%>%
      mutate(Cohort="Reference population")%>%
      ungroup())%>%
  ungroup()%>%
  filter(!is.na(sex))%>%
  mutate(sex=as.factor(case_when(sex==2 ~ "Female",
                                 sex==1 ~ "Male")))



sex_over_time_ratios<-
  sex_over_time%>%
  mutate(Individuals=ifelse(is.na(Individuals), 0, Individuals))%>%
  group_by(Period, Cohort, agegrp1)%>%
  mutate(Proportion = round(Individuals/sum(Individuals)*100,1))%>%
  mutate(Proportion=if_else(is.na(Proportion), 0, Proportion))%>%
  ungroup()%>%
  filter(!is.na(sex))%>%
  filter(!is.na(Period))%>%
  group_by(Period, agegrp1, sex)%>%
  summarise(Ratio=Proportion[Cohort=="RECOVERY"]/Proportion[Cohort=="Reference population"],
            EF_95 = exp(1.96*sqrt(1/Individuals[Cohort=="RECOVERY"] + 1/Individuals[Cohort=="Reference population"])))%>%
  
  rbind(sex_over_time%>%
          mutate(Individuals=ifelse(is.na(Individuals), 0, Individuals))%>%
          filter(!is.na(Period))%>%
          group_by(Cohort, agegrp1, sex)%>%
          summarise(Individuals=sum(Individuals))%>%
          mutate(Proportion = round(Individuals/sum(Individuals)*100,1))%>%
          mutate(Proportion=if_else(is.na(Proportion), 0, Proportion))%>%
          ungroup()%>%
          filter(!is.na(sex))%>%
          group_by(agegrp1, sex)%>%
          summarise(Ratio=Proportion[Cohort=="RECOVERY"]/Proportion[Cohort=="Reference population"],
                    EF_95 = exp(1.96*sqrt(1/Individuals[Cohort=="RECOVERY"] + 1/Individuals[Cohort=="Reference population"])))%>%
          mutate(Period="All time periods"))%>%
  mutate(Period=factor(Period, levels=c("All time periods",
                                        "Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21")))


p2<-sex_over_time_ratios%>%
  filter(sex=="Female")%>%
  ggplot(aes(Period, Ratio, color=agegrp1, shape=agegrp1
             # shape=region
  ))+
  # geom_line()+
  geom_point(size=4,
             position=position_dodge(0.1)
             )+
  geom_errorbar(aes(ymin=Ratio/EF_95, ymax=Ratio*EF_95, group=sex),
                position=position_dodge(width=0.1),
                width=0.05)+
  
  geom_hline(aes(yintercept=1), color="black")+
  
  scale_y_continuous(
    trans = "log2",
    labels = label_number(accuracy=0.1),
    breaks = c(0.5, 1, 2),
    # limits=c(0.2,2.5)
  )+
  coord_cartesian(ylim = c(0.5, 2))+
  theme(
    axis.text.x = element_text(angle=90,
                               vjust=0,
                               hjust=1
    ),
    legend.position = "bottom",
    # panel.grid.minor = element_blank(),
    text=element_text(size=20),
    # strip.placement = "outside",
  )+
  labs(y="Representativeness ratio",
       x="Period",
       subtitle="Female sex",
       # title="Age representativeness of the RECOVERY HES population in comparison with the All-England HES population, split by age groups",
       # subtitle="Representativeness ratio",
       color="Age group",
       shape="Age group")+
  # facet_grid(rows=vars(agegrp1),
  #            labeller = label_wrap_gen(width = 15),
  #            switch = "y")+
  geom_text_repel(aes(label=round(Ratio,1)),
                  # vjust=-2,
                  show.legend = F,
                  direction="y",
                  nudge_x = 0.1,
                  # force_pull = 5
                  min.segment.length=0.1	
  )+
  scale_shape_manual(values=seq(15,19,1))

p2


### combined plot ----
p1/p2


ggsave("Outputs/Figures/supplementary_figure_S1_timeseries_sex_age_ratios_combined.png",
       dpi="retina",
       width=30,
       height=40,
       units="cm")




##  ethnicity (supplementary figure 2) -------


ethnicity_along_time<-
  RECOVERY_HES_flags%>%
  select(study_id, indexdate, ethgrp, agegrp)%>%
  mutate(month=paste0(str_sub(indexdate, 1, 8), "01"))%>%
  mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                           "20-24",
                                           "25-29",
                                           "30-34",
                                           "35-39",
                                           "40-44",
                                           "45-49",
                                           "50-54",
                                           "55-59") ~ "<60",
                             agegrp %in% c("60-64",
                                           "65-69") ~ "60-69",
                             agegrp %in% c("70-74",
                                           "75-79") ~ "70-79",
                             agegrp %in% c("80-84",
                                           "85-89",
                                           "90-94",
                                           "95+") ~ "80+"))%>%
  mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
                            month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
                            month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
                            month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
                            month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
                            month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
                            month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21" )))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%
  group_by(Period, ethgrp, agegrp1)%>%
  summarise(Individuals=n_distinct(study_id, na.rm=T))%>%
  mutate(Cohort="RECOVERY")%>%
  ungroup()%>%
  
  rbind(
    National_HES_flags_ethnicity_unscrambled%>%
      rename(agegrp1=agebroad)%>%
      group_by(agegrp1, Period, ethgrp)%>%
      summarise(Individuals=n())%>%
      mutate(Cohort="Reference population")%>%
      ungroup())%>%
  ungroup()

# fill with count 0 for months and bands with no participants in RECOVERY and calculate proportions

ethnicity_along_time<-
  expand.grid(
    Period=ethnicity_along_time%>%distinct(Period)%>%.[[1]],
    agegrp1 = ethnicity_along_time%>%distinct(agegrp1)%>%.[[1]],
    ethgrp = ethnicity_along_time%>%distinct(ethgrp)%>%.[[1]],
    Cohort = ethnicity_along_time%>%distinct(Cohort)%>%.[[1]]
  )%>%
  left_join(ethnicity_along_time)%>%
  mutate(Individuals=ifelse(is.na(Individuals), 0, Individuals))%>%
  group_by(Period, Cohort, agegrp1)%>%
  mutate(Proportion = round(Individuals/sum(Individuals)*100,1))%>%
  mutate(Proportion=if_else(is.na(Proportion), 0, Proportion))%>%
  ungroup()%>%
  mutate(ethgrp = if_else(is.na(ethgrp), "Unknown", ethgrp))%>%
  mutate(ethgrp = fct_relevel(ethgrp, "Other", "Mixed", "Black", "Asian", "White", "Unknown"))






ethnicity_along_time%>%
  rbind(ethnicity_along_time%>%
          group_by(Cohort, agegrp1, ethgrp)%>%
          summarise(Individuals=sum(Individuals))%>%
          mutate(Proportion = round(Individuals/sum(Individuals)*100,1))%>%
          mutate(Proportion=if_else(is.na(Proportion), 0, Proportion))%>%
          mutate(Period="All time periods"))%>%
  mutate(Period=factor(Period, levels=c("All time periods",
                                        "Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21")))%>%
  group_by(Period, Cohort)%>%
  mutate(SE=sqrt(Proportion/100*(1-Proportion/100)/Individuals)*100)%>%
  filter(Proportion>0)%>%
  mutate(ethgrp = fct_relevel(ethgrp, "Unknown", "Other", "Mixed", "Black", "Asian", "White"))%>%
  mutate(ethgrp=fct_rev(ethgrp))%>%
  
  ggplot(aes(Period, 
             Proportion, 
             group=Cohort,
             color=Cohort,
             shape=Cohort
  ))+
  geom_point(size=2)+
  geom_line(data=.%>%filter(Period!="All time periods"),
            size=1)+
  geom_errorbar(aes(ymin=if_else(Proportion-SE <0,0,Proportion-SE),
                    ymax=if_else(Proportion+SE >100,100,Proportion+SE)),
                width=0.2)+
  
  facet_grid(agegrp1~ethgrp,
             scales="free",
             switch="y",
             labeller = label_wrap_gen(width = 15))+
  
  theme(axis.text.x = element_text(angle=90, 
                                   vjust= 0.5, 
                                   hjust=1,
                                   # hjust=-1, 
                                   size=10), 
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        text=element_text(size=20),
        legend.box="vertical",
        strip.placement="outside"
  )+
  labs(y="Proportion of cohort in each period (%)",
       x="Index period",
       # title="Ethnical group distribution over time in the RECOVERY cohort and reference population, split by age bands",
       color="",
       shape="")+
  scale_color_manual(values=colors[c(1,3)])


ggsave("Outputs/Publication/Figures/supplementary_figure_S2_timeseries_ethicity_linecharts.png",
       dpi="retina",
       width=50,
       height=30,
       units="cm")

ggsave("Outputs/Publication/Figures/High-resolution/supplementary_figure_S2_timeseries_ethicity_linecharts.tiff",
       dpi="retina",
       width=50,
       height=30,
       units="cm")













##  deprivation (supplementary figure 3)  -----------

imd_along_time<-
  RECOVERY_HES_flags%>%
  select(study_id, indexdate, imd19_quintile, agegrp)%>%
  mutate(month=paste0(str_sub(indexdate, 1, 8), "01"))%>%
  mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                           "20-24",
                                           "25-29",
                                           "30-34",
                                           "35-39",
                                           "40-44",
                                           "45-49",
                                           "50-54",
                                           "55-59") ~ "<60",
                             agegrp %in% c("60-64",
                                           "65-69") ~ "60-69",
                             agegrp %in% c("70-74",
                                           "75-79") ~ "70-79",
                             agegrp %in% c("80-84",
                                           "85-89",
                                           "90-94",
                                           "95+") ~ "80+"))%>%
  mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
                            month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
                            month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
                            month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
                            month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
                            month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
                            month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21")))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%
  group_by(Period, imd19_quintile, agegrp1)%>%
  summarise(Individuals=n_distinct(study_id, na.rm=T))%>%
  mutate(Cohort="RECOVERY")%>%
  ungroup()%>%
  
  rbind(
    National_HES_flags_IMD_unscrambled%>%
      select(month = yearmonth, imd19_quintile, agegrp, count)%>%
      mutate(month = as.character(month))%>%
      mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                               "20-24",
                                               "25-29",
                                               "30-34",
                                               "35-39",
                                               "40-44",
                                               "45-49",
                                               "50-54",
                                               "55-59") ~ "<60",
                                 agegrp %in% c("60-64",
                                               "65-69") ~ "60-69",
                                 agegrp %in% c("70-74",
                                               "75-79") ~ "70-79",
                                 agegrp %in% c("80-84",
                                               "85-89",
                                               "90-94",
                                               "95+") ~ "80+"))%>%
      mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
                                month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
                                month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
                                month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
                                month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
                                month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
                                month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
      mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                            "Jun 20 - Aug 20",
                                            "Sep 20 - Nov 20",
                                            "Dec 20 - Feb 21",
                                            "Mar 21 - May 21",
                                            "Jun 21 - Aug 21",
                                            "Sep 21 - Nov 21")))%>%
      mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                                "Jun 20 - Aug 20",
                                "Sep 20 - Nov 20",
                                "Dec 20 - Feb 21",
                                "Mar 21 - May 21",
                                "Jun 21 - Aug 21",
                                "Sep 21 - Nov 21"))%>%
      group_by(agegrp1, Period, imd19_quintile)%>%
      summarise(Individuals=sum(count))%>%
      mutate(Cohort="Reference population")%>%
      ungroup())%>%
  ungroup()


  # fill with count 0 for months and bands with no participants in RECOVERY and calculate proportions
  
  imd_along_time<-
    expand.grid(
      Period=imd_along_time%>%distinct(Period)%>%.[[1]],
      agegrp1 = imd_along_time%>%distinct(agegrp1)%>%.[[1]],
      imd19_quintile = imd_along_time%>%distinct(imd19_quintile)%>%.[[1]],
      Cohort = imd_along_time%>%distinct(Cohort)%>%.[[1]]
    )%>%
    left_join(imd_along_time)%>%
    mutate(Individuals=ifelse(is.na(Individuals), 0, Individuals))%>%
    group_by(Period, Cohort, agegrp1)%>%
    mutate(Proportion = round(Individuals/sum(Individuals)*100,1))%>%
    mutate(Proportion=if_else(is.na(Proportion), 0, Proportion))%>%
    ungroup()



imd_along_time%>%
  rbind(imd_along_time%>%
          group_by(Cohort, agegrp1, imd19_quintile)%>%
          summarise(Individuals=sum(Individuals))%>%
          mutate(Proportion = round(Individuals/sum(Individuals)*100,1))%>%
          mutate(Proportion=if_else(is.na(Proportion), 0, Proportion))%>%
          mutate(Period="All time periods"))%>%
  mutate(Period=factor(Period, levels=c("All time periods",
                                        "Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21")))%>%
  filter(!is.na(Period))%>%
  group_by(Period, Cohort)%>%
  mutate(SE=sqrt(Proportion/100*(1-Proportion/100)/Individuals)*100)%>%
  filter(Proportion>0)%>%
  mutate(imd19_quintile=replace_na(imd19_quintile, "Unknown"))%>%
  
  ggplot(aes(Period, 
             Proportion, 
             group=Cohort,
             color=Cohort,
             shape=Cohort
  ))+
  geom_point(size=2)+
  geom_line(data=.%>%filter(Period!="All time periods"),
            size=1)+
  geom_errorbar(aes(ymin=if_else(Proportion-SE <0,0,Proportion-SE),
                    ymax=if_else(Proportion+SE >100,100,Proportion+SE)),
                width=0.2)+
  
  facet_grid(agegrp1~imd19_quintile,
             scales="free",
             switch="y",
             labeller = label_wrap_gen(width = 15))+
  
  theme(axis.text.x = element_text(angle=90, 
                                   vjust= 0.5, 
                                   hjust=1,
                                   # hjust=-1, 
                                   size=10), 
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        text=element_text(size=20),
        legend.box="vertical",
        strip.placement="outside"
  )+
  labs(y="Proportion of cohort in each period",
       x="Index period",
       # title="Deprivation quintiles over time in the RECOVERY cohort and reference population, split by age bands",
       color="",
       shape="")+
  scale_color_manual(values=colors[c(1,3)])


ggsave("Outputs/Publication/Figures/supplementary_figure_S3_timeseries_deprivation_linecharts.png",
       dpi="retina",
       width=50,
       height=30,
       units="cm")

ggsave("Outputs/Publication/Figures/High-resolution/supplementary_figure_S3_timeseries_deprivation_linecharts.tiff",
       dpi="retina",
       width=50,
       height=30,
       units="cm")

##  charlson score (excluding age) (removed) -----------
# 
# 
# charlson_score_along_time<-
#   RECOVERY_HES_flags%>%
#   select(study_id, indexdate, charlson_score, agegrp)%>%
#   mutate(month=paste0(str_sub(indexdate, 1, 8), "01"))%>%
#   mutate(agegrp1 = case_when(agegrp %in% c("15-19",
#                                            "20-24",
#                                            "25-29",
#                                            "30-34",
#                                            "35-39",
#                                            "40-44",
#                                            "45-49",
#                                            "50-54",
#                                            "55-59") ~ "<60",
#                              agegrp %in% c("60-64",
#                                            "65-69") ~ "60-69",
#                              agegrp %in% c("70-74",
#                                            "75-79") ~ "70-79",
#                              agegrp %in% c("80-84",
#                                            "85-89") ~ "80-89",
#                              agegrp %in% c("90-94",
#                                            "95+") ~ "90+"))%>%
#   mutate(Cohort="RECOVERY HES")%>%
#   ungroup()%>%
#   select(-study_id, -indexdate)%>%
#   
#   rbind(
#     National_HES_flags_unscrambled%>%
#       select(month = yearmonth, charlson_score, agegrp)%>%
#       mutate(month = as.character(month))%>%
#       mutate(agegrp1 = case_when(agegrp %in% c("15-19",
#                                                "20-24",
#                                                "25-29",
#                                                "30-34",
#                                                "35-39",
#                                                "40-44",
#                                                "45-49",
#                                                "50-54",
#                                                "55-59") ~ "<60",
#                                  agegrp %in% c("60-64",
#                                                "65-69") ~ "60-69",
#                                  agegrp %in% c("70-74",
#                                                "75-79") ~ "70-79",
#                                  agegrp %in% c("80-84",
#                                                "85-89") ~ "80-89",
#                                  agegrp %in% c("90-94",
#                                                "95+") ~ "90+"))%>%
#       
#       mutate(Cohort="All-England HES"))%>%
#   
#   ungroup()%>%
#   mutate(month=as.Date(month),
#          Cohort=fct_relevel(Cohort, "RECOVERY HES", "All-England HES"))%>%
#   mutate(Cohort=factor(case_when(Cohort == "RECOVERY HES" ~ "RECOVERY",
#                                  Cohort == "All-England HES" ~ "Reference population")))%>%
#   mutate(Cohort=fct_relevel(Cohort, "RECOVERY","Reference population"))%>%
#   mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
#                             month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
#                             month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
#                             month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
#                             month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
#                             month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
#                             month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
#   mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
#                                         "Jun 20 - Aug 20",
#                                         "Sep 20 - Nov 20",
#                                         "Dec 20 - Feb 21",
#                                         "Mar 21 - May 21",
#                                         "Jun 21 - Aug 21",
#                                         "Sep 21 - Nov 21")))%>%
#   mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
#                             "Jun 20 - Aug 20",
#                             "Sep 20 - Nov 20",
#                             "Dec 20 - Feb 21",
#                             "Mar 21 - May 21",
#                             "Jun 21 - Aug 21",
#                             "Sep 21 - Nov 21"))%>%
#   filter(!is.na(Period))%>%
#   group_by(Cohort, Period, agegrp1)%>%
#   summarise(Median=median(charlson_score, na.rm=T),
#             SE = sd(charlson_score)/sqrt(length(charlson_score)))
# 
# 
# # add aggregates
# 
# 
# charlson_score_along_time%<>%
#   rbind(
#     RECOVERY_HES_flags%>%
#       select(study_id, indexdate, charlson_score, agegrp)%>%
#       mutate(month=paste0(str_sub(indexdate, 1, 8), "01"))%>%
#       mutate(agegrp1 = case_when(agegrp %in% c("15-19",
#                                                "20-24",
#                                                "25-29",
#                                                "30-34",
#                                                "35-39",
#                                                "40-44",
#                                                "45-49",
#                                                "50-54",
#                                                "55-59") ~ "<60",
#                                  agegrp %in% c("60-64",
#                                                "65-69") ~ "60-69",
#                                  agegrp %in% c("70-74",
#                                                "75-79") ~ "70-79",
#                                  agegrp %in% c("80-84",
#                                                "85-89") ~ "80-89",
#                                  agegrp %in% c("90-94",
#                                                "95+") ~ "90+"))%>%
#       mutate(Cohort="RECOVERY HES")%>%
#       ungroup()%>%
#       select(-study_id, -indexdate)%>%
#       
#       rbind(
#         National_HES_flags_unscrambled%>%
#           select(month = yearmonth, charlson_score, agegrp)%>%
#           mutate(month = as.character(month))%>%
#           mutate(agegrp1 = case_when(agegrp %in% c("15-19",
#                                                    "20-24",
#                                                    "25-29",
#                                                    "30-34",
#                                                    "35-39",
#                                                    "40-44",
#                                                    "45-49",
#                                                    "50-54",
#                                                    "55-59") ~ "<60",
#                                      agegrp %in% c("60-64",
#                                                    "65-69") ~ "60-69",
#                                      agegrp %in% c("70-74",
#                                                    "75-79") ~ "70-79",
#                                      agegrp %in% c("80-84",
#                                                    "85-89") ~ "80-89",
#                                      agegrp %in% c("90-94",
#                                                    "95+") ~ "90+"))%>%
#           
#           mutate(Cohort="All-England HES"))%>%
#       
#       ungroup()%>%
#       mutate(month=as.Date(month),
#              Cohort=fct_relevel(Cohort, "RECOVERY HES", "All-England HES"))%>%
#       mutate(Cohort=factor(case_when(Cohort == "RECOVERY HES" ~ "RECOVERY",
#                                      Cohort == "All-England HES" ~ "Reference population")))%>%
#       mutate(Cohort=fct_relevel(Cohort, "RECOVERY","Reference population"))%>%
#       mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
#                                 month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
#                                 month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
#                                 month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
#                                 month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
#                                 month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
#                                 month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
#       mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
#                                             "Jun 20 - Aug 20",
#                                             "Sep 20 - Nov 20",
#                                             "Dec 20 - Feb 21",
#                                             "Mar 21 - May 21",
#                                             "Jun 21 - Aug 21",
#                                             "Sep 21 - Nov 21")))%>%
#       mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
#                                 "Jun 20 - Aug 20",
#                                 "Sep 20 - Nov 20",
#                                 "Dec 20 - Feb 21",
#                                 "Mar 21 - May 21",
#                                 "Jun 21 - Aug 21",
#                                 "Sep 21 - Nov 21"))%>%
#       filter(!is.na(Period))%>%
#       group_by(Cohort, agegrp1)%>%
#       summarise(Median=median(charlson_score, na.rm=T),
#                 SE = sd(charlson_score)/sqrt(length(charlson_score)))%>%
#       mutate(Period="Overall"))%>%
#   mutate(Period=factor(Period, levels=c("Overall",
#                                         "Mar 20 - May 20",
#                                         "Jun 20 - Aug 20",
#                                         "Sep 20 - Nov 20",
#                                         "Dec 20 - Feb 21",
#                                         "Mar 21 - May 21",
#                                         "Jun 21 - Aug 21",
#                                         "Sep 21 - Nov 21" )))
# 
# 
# 
# 
# charlson_score_along_time%>%
#   ggplot(aes(Period, Median, color=Cohort, shape=Cohort, group=Cohort))+
#   geom_point(size=4)+
#   geom_errorbar(aes(ymin=if_else(Median-SE>0, Median-SE, 0), ymax=Median+SE, group=Cohort),width=0.1)+ 
#   facet_wrap(~agegrp1,
#              nrow=1,
#   )+
#   
#   scale_y_continuous(limits=c(0, 5))+
#   theme(axis.text.x = element_text(angle=90, 
#                                    vjust=0.1, 
#                                    hjust=1),
#         legend.position = "bottom",
#         panel.grid.minor = element_blank(),
#         tex=element_text(size=20))+
#   labs(y="Charlson score, excluding age (median)",
#        x="Period",
#        # title="Charlson score (excluding age) along time in the RECOVERY HES population in comparison with the All-England HES population",
#        # subtitle="Split by age bands",
#        color="Cohort",
#        shape="Cohort")+
#   scale_color_manual(values=colors[c(1,3)])
# 
# 
# ggsave("Outputs/Publication/Figures/supplementary_figure_S5_timeseries_charlson_score_representativeness.png",
#        dpi="retina",
#        width=60,
#        height=30,
#        units="cm")
# 
# 
# ggsave("Outputs/Publication/Figures/supplementary_figure_S5_timeseries_charlson_score_representativeness.tiff",
#        dpi="retina",
#        width=60,
#        height=30,
#        units="cm")




















##  hospital frailty score (supplementary figure 6) -----------

hospital_frailty_score_along_time<-
  RECOVERY_HES_flags%>%
  select(study_id, indexdate, frailty_score, agegrp)%>%
  # left_join(baseline_crf_england%>%select(study_number, age), by=c("study_id"="study_number"))%>%
  mutate(month=paste0(str_sub(indexdate, 1, 8), "01"))%>%
  mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                           "20-24",
                                           "25-29",
                                           "30-34",
                                           "35-39",
                                           "40-44",
                                           "45-49",
                                           "50-54",
                                           "55-59") ~ "<60",
                             agegrp %in% c("60-64",
                                           "65-69") ~ "60-69",
                             agegrp %in% c("70-74",
                                           "75-79") ~ "70-79",
                             agegrp %in% c("80-84",
                                           "85-89",
                                           "90-94",
                                           "95+") ~ "80+"))%>%
  mutate(Cohort="RECOVERY")%>%
  select(-indexdate, -study_id)%>%
  ungroup()%>%
  
  rbind(
    National_HES_flags_unscrambled%>%
      select(month = yearmonth, frailty_score, agegrp)%>%
      mutate(month = as.character(month))%>%
      mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                               "20-24",
                                               "25-29",
                                               "30-34",
                                               "35-39",
                                               "40-44",
                                               "45-49",
                                               "50-54",
                                               "55-59") ~ "<60",
                                 agegrp %in% c("60-64",
                                               "65-69") ~ "60-69",
                                 agegrp %in% c("70-74",
                                               "75-79") ~ "70-79",
                                 agegrp %in% c("80-84",
                                               "85-89",
                                               "90-94",
                                               "95+") ~ "80+"))%>%
      
      mutate(Cohort="Reference population"))%>%
  ungroup()%>%
  mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
                            month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
                            month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
                            month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
                            month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
                            month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
                            month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21")))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%
  filter(!is.na(Period))%>%
  group_by(Period, agegrp1, Cohort)%>%
  summarise(Median=median(frailty_score, na.rm=T),
            SE = sd(frailty_score)/sqrt(n()))

## add overall
hospital_frailty_score_along_time%<>%
  rbind(
    RECOVERY_HES_flags%>%
      select(study_id, indexdate, frailty_score, agegrp)%>%
      # left_join(baseline_crf_england%>%select(study_number, age), by=c("study_id"="study_number"))%>%
      mutate(month=paste0(str_sub(indexdate, 1, 8), "01"))%>%
      mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                               "20-24",
                                               "25-29",
                                               "30-34",
                                               "35-39",
                                               "40-44",
                                               "45-49",
                                               "50-54",
                                               "55-59") ~ "<60",
                                 agegrp %in% c("60-64",
                                               "65-69") ~ "60-69",
                                 agegrp %in% c("70-74",
                                               "75-79") ~ "70-79",
                                 agegrp %in% c("80-84",
                                               "85-89",
                                               "90-94",
                                               "95+") ~ "80+"))%>%
      mutate(Cohort="RECOVERY")%>%
      select(-indexdate, -study_id)%>%
      ungroup()%>%
      
      rbind(
        National_HES_flags_unscrambled%>%
          select(month = yearmonth, frailty_score, agegrp)%>%
          mutate(month = as.character(month))%>%
          mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                                   "20-24",
                                                   "25-29",
                                                   "30-34",
                                                   "35-39",
                                                   "40-44",
                                                   "45-49",
                                                   "50-54",
                                                   "55-59") ~ "<60",
                                     agegrp %in% c("60-64",
                                                   "65-69") ~ "60-69",
                                     agegrp %in% c("70-74",
                                                   "75-79") ~ "70-79",
                                     agegrp %in% c("80-84",
                                                   "85-89",
                                                   "90-94",
                                                   "95+") ~ "80+"))%>%
          
          mutate(Cohort="Reference population"))%>%
      ungroup()%>%
      mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
                                month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
                                month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
                                month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
                                month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
                                month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
                                month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
      mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                            "Jun 20 - Aug 20",
                                            "Sep 20 - Nov 20",
                                            "Dec 20 - Feb 21",
                                            "Mar 21 - May 21",
                                            "Jun 21 - Aug 21",
                                            "Sep 21 - Nov 21")))%>%
      mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                                "Jun 20 - Aug 20",
                                "Sep 20 - Nov 20",
                                "Dec 20 - Feb 21",
                                "Mar 21 - May 21",
                                "Jun 21 - Aug 21",
                                "Sep 21 - Nov 21"))%>%
      filter(!is.na(Period))%>%
      group_by(agegrp1, Cohort)%>%
      summarise(Median=median(frailty_score, na.rm=T),
                SE = sd(frailty_score)/sqrt(n()))%>%
      mutate(Period="All time periods"))%>%
  mutate(Period=factor(Period, levels=c("All time periods",
                                        "Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21")))



hospital_frailty_score_along_time%>%
  ggplot(aes(Period, Median, color=Cohort, shape=Cohort, group=Cohort))+
  geom_line(data=.%>%filter(Period!="All time periods"))+
  geom_point(size=2.5)+
  geom_errorbar(aes(ymin=Median-SE, ymax=Median+SE, group=Cohort),width=0.1)+
  facet_wrap(~agegrp1, ncol=5)+

  scale_y_continuous(limits=c(0, 30))+
  theme(axis.text.x = element_text(angle=90, 
                                   vjust=0.1, 
                                   hjust=1
                                   ), 
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        text=element_text(size=20),
        plot.margin = margin(
          # t = 20,  # Top margin
          # r = 100  # Right margin
          # b = 40,  # Bottom margin
          # l = 80  # Left margin
        )
        )+
  labs(y="Hospital frailty score (median)",
       x="Period",
       # title="Hospital frailty score along time in the RECOVERY HES population in comparison with the All-England HES population",
       # subtitle="Split by age bands",
       # caption="Error bars depict standard errors",
       color="Cohort",
       shape="Cohort")+
  scale_color_manual(values=colors[c(1,3)])

ggsave("Outputs/Publication/Figures/supplementary_figure_S5_timeseries_hospital_frailty_score_representativeness.png",
       dpi="retina",
       width=50,
       height=20,
       units="cm")

ggsave("Outputs/Publication/Figures/High-resolution/supplementary_figure_S5_timeseries_hospital_frailty_score_representativeness.tiff",
       dpi="retina",
       width=50,
       height=20,
       units="cm")

# 10. Mortality rates and admissions------

##  Mortality rates ------

###  RECOVERY HES --------

RECOVERY_HES_death_rates<-
  RECOVERY_HES_flags%>%
  select(study_number=study_id,
         indexdate,
         sex, 
         death28)%>%
  left_join(baseline_crf_england%>%select(study_number, age), by="study_number")%>%
  mutate(month=paste0(str_sub(indexdate, 1, 8), "01"))%>%
  filter(month>"2020-02-01")%>%
  
  count(month, death28)%>%
  mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 -\nMay 20",
                            month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 -\nAug 20",
                            month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 -\nNov 20",
                            month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 -\nFeb 21",
                            month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 -\nMay 21",
                            month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 -\nAug 21",
                            month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 -\nNov 21"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 -\nMay 20",
                                        "Jun 20 -\nAug 20",
                                        "Sep 20 -\nNov 20",
                                        "Dec 20 -\nFeb 21",
                                        "Mar 21 -\nMay 21",
                                        "Jun 21 -\nAug 21",
                                        "Sep 21 -\nNov 21")))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 -\nMay 20",
                            "Jun 20 -\nAug 20",
                            "Sep 20 -\nNov 20",
                            "Dec 20 -\nFeb 21",
                            "Mar 21 -\nMay 21",
                            "Jun 21 -\nAug 21",
                            "Sep 21 -\nNov 21"))%>%
  group_by(Period)%>%
  summarise(
    deaths=sum(n[death28==1]),
      death_rate=sum(n[death28==1])/sum(n),
         population=sum(n))%>%
  mutate(Cohort = "RECOVERY HES")%>%
  mutate(SE = sqrt(death_rate * (1-death_rate) / population)) # SE using normal approximation of the binomial distribution

  


  

###  National HES --------

National_HES_death_rates<-
  National_HES_events%>%
  select(yearmonth,
         sex, 
         death28)%>%
  mutate(month=paste0(yearmonth, "01"))%>%
  mutate(month=as.Date(month, format="%Y%m%d", origin = "1984-01-01"))%>%
  filter(month>"2020-02-01")%>%
  
  count(month, death28)%>%
  mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 -\nMay 20",
                            month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 -\nAug 20",
                            month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 -\nNov 20",
                            month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 -\nFeb 21",
                            month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 -\nMay 21",
                            month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 -\nAug 21",
                            month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 -\nNov 21"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 -\nMay 20",
                                        "Jun 20 -\nAug 20",
                                        "Sep 20 -\nNov 20",
                                        "Dec 20 -\nFeb 21",
                                        "Mar 21 -\nMay 21",
                                        "Jun 21 -\nAug 21",
                                        "Sep 21 -\nNov 21")))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 -\nMay 20",
                            "Jun 20 -\nAug 20",
                            "Sep 20 -\nNov 20",
                            "Dec 20 -\nFeb 21",
                            "Mar 21 -\nMay 21",
                            "Jun 21 -\nAug 21",
                            "Sep 21 -\nNov 21"))%>%
  group_by(Period)%>%
  summarise(
    deaths=sum(n[death28==1]),
    death_rate=sum(n[death28==1])/sum(n),
    population=sum(n))%>%
  mutate(Cohort = "All-England HES")%>%
  mutate(SE = sqrt(death_rate * (1-death_rate) / population))



###  RECOVERY HES age- and sex- adjusted to national HES --------
  
# investigate number of events per stratum (5 year bands)
# 
# RECOVERY_HES_flags%>%
#   select(study_number=study_id,
#          indexdate,
#          sex,
#          death28)%>%
#   left_join(baseline_crf_england%>%select(study_number, age), by="study_number")%>%
#   mutate(month=as.Date(paste0(str_sub(indexdate, 1, 8), "01"), format="%Y-%m-%d"))%>%
#   filter(month>"2020-02-01")%>%
#   
#   mutate(agegrp = case_when(age<20 ~ "15-19",
#                             between(age, 20, 24) ~ "20-24",
#                             between(age, 25, 29) ~ "25-29",
#                             between(age, 30, 34) ~ "30-34",
#                             between(age, 35, 39) ~ "35-39",
#                             between(age, 40, 44) ~ "40-44",
#                             between(age, 45, 49) ~ "45-49",
#                             between(age, 50, 54) ~ "50-54",
#                             between(age, 55, 59) ~ "55-59",
#                             between(age, 60, 64) ~ "60-64",
#                             between(age, 65, 69) ~ "65-69",
#                             between(age, 70, 74) ~ "70-74",
#                             between(age, 75, 79) ~ "75-79",
#                             between(age, 80, 84) ~ "80-84",
#                             between(age, 85, 89) ~ "85-89",
#                             between(age, 90, 94) ~ "90-94",
#                             age>=95 ~ "95+"))%>%
#   mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
#                             month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
#                             month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
#                             month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
#                             month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
#                             month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
#                             month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
#   mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
#                                         "Jun 20 - Aug 20",
#                                         "Sep 20 - Nov 20",
#                                         "Dec 20 - Feb 21",
#                                         "Mar 21 - May 21",
#                                         "Jun 21 - Aug 21",
#                                         "Sep 21 - Nov 21")))%>%
#   mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
#                             "Jun 20 - Aug 20",
#                             "Sep 20 - Nov 20",
#                             "Dec 20 - Feb 21",
#                             "Mar 21 - May 21",
#                             "Jun 21 - Aug 21",
#                             "Sep 21 - Nov 21"))%>%
#   # calculate number of people and deaths in each stratum
#   group_by(Period, agegrp, sex)%>%
#   summarise(recovery_population=n_distinct(study_number),
#             recovery_deaths = n_distinct(study_number[death28=="1"]))%>%
#   ungroup()%>%
#   filter(!is.na(sex))%>%
#   View() # low number of events in many strata


# investigate number of events per stratum (5 age bands)
# 
# RECOVERY_HES_flags%>%
#   select(study_number=study_id,
#          indexdate,
#          sex,
#          death28)%>%
#   left_join(baseline_crf_england%>%select(study_number, age), by="study_number")%>%
#   mutate(month=as.Date(paste0(str_sub(indexdate, 1, 8), "01"), format="%Y-%m-%d"))%>%
#   filter(month>"2020-02-01")%>%
#   
#   mutate(agegrp = case_when(age<20 ~ "15-19",
#                             between(age, 20, 24) ~ "20-24",
#                             between(age, 25, 29) ~ "25-29",
#                             between(age, 30, 34) ~ "30-34",
#                             between(age, 35, 39) ~ "35-39",
#                             between(age, 40, 44) ~ "40-44",
#                             between(age, 45, 49) ~ "45-49",
#                             between(age, 50, 54) ~ "50-54",
#                             between(age, 55, 59) ~ "55-59",
#                             between(age, 60, 64) ~ "60-64",
#                             between(age, 65, 69) ~ "65-69",
#                             between(age, 70, 74) ~ "70-74",
#                             between(age, 75, 79) ~ "75-79",
#                             between(age, 80, 84) ~ "80-84",
#                             between(age, 85, 89) ~ "85-89",
#                             between(age, 90, 94) ~ "90-94",
#                             age>=95 ~ "95+"))%>%
#   mutate(agegrp1 = case_when(agegrp %in% c("15-19",
#                                            "20-24",
#                                            "25-29",
#                                            "30-34",
#                                            "35-39",
#                                            "40-44",
#                                            "45-49",
#                                            "50-54",
#                                            "55-59") ~ "<60",
#                              agegrp %in% c("60-64",
#                                            "65-69") ~ "60-69",
#                              agegrp %in% c("70-74",
#                                            "75-79") ~ "70-79",
#                              agegrp %in% c("80-84",
#                                            "85-89") ~ "80-89",
#                              agegrp %in% c("90-94",
#                                            "95+") ~ "90+"))%>%
#   mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
#                             month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
#                             month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
#                             month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
#                             month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
#                             month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
#                             month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
#   mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
#                                         "Jun 20 - Aug 20",
#                                         "Sep 20 - Nov 20",
#                                         "Dec 20 - Feb 21",
#                                         "Mar 21 - May 21",
#                                         "Jun 21 - Aug 21",
#                                         "Sep 21 - Nov 21")))%>%
#   mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
#                             "Jun 20 - Aug 20",
#                             "Sep 20 - Nov 20",
#                             "Dec 20 - Feb 21",
#                             "Mar 21 - May 21",
#                             "Jun 21 - Aug 21",
#                             "Sep 21 - Nov 21"))%>%
#   # calculate number of people and deaths in each stratum
#   group_by(Period, agegrp1, sex)%>%
#   summarise(recovery_population=n_distinct(study_number),
#             recovery_deaths = n_distinct(study_number[death28=="1"]))%>%
#   ungroup()%>%
#   filter(!is.na(sex))%>%
#   View() # enough number of events per stratum


# calculations


RECOVERY_HES_standardised_death_rates<-
  
  # take HES data
  RECOVERY_HES_flags%>%
  
  # preliminary data processing
  select(study_number=study_id,
         indexdate,
         sex,
         death28)%>%
  left_join(baseline_crf_england%>%select(study_number, age), by="study_number")%>%
  mutate(month=as.Date(paste0(str_sub(indexdate, 1, 8), "01"), format="%Y-%m-%d"))%>%
  filter(month>"2020-02-01")%>%
  
  mutate(agegrp = case_when(age<20 ~ "15-19",
                            between(age, 20, 24) ~ "20-24",
                            between(age, 25, 29) ~ "25-29",
                            between(age, 30, 34) ~ "30-34",
                            between(age, 35, 39) ~ "35-39",
                            between(age, 40, 44) ~ "40-44",
                            between(age, 45, 49) ~ "45-49",
                            between(age, 50, 54) ~ "50-54",
                            between(age, 55, 59) ~ "55-59",
                            between(age, 60, 64) ~ "60-64",
                            between(age, 65, 69) ~ "65-69",
                            between(age, 70, 74) ~ "70-74",
                            between(age, 75, 79) ~ "75-79",
                            between(age, 80, 84) ~ "80-84",
                            between(age, 85, 89) ~ "85-89",
                            between(age, 90, 94) ~ "90-94",
                            age>=95 ~ "95+"))%>%
  mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                           "20-24",
                                           "25-29",
                                           "30-34",
                                           "35-39",
                                           "40-44",
                                           "45-49",
                                           "50-54",
                                           "55-59") ~ "<60",
                             agegrp %in% c("60-64",
                                           "65-69") ~ "60-69",
                             agegrp %in% c("70-74",
                                           "75-79") ~ "70-79",
                             agegrp %in% c("80-84",
                                           "85-89") ~ "80-89",
                             agegrp %in% c("90-94",
                                           "95+") ~ "90+"))%>%
  mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 -\nMay 20",
                            month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 -\nAug 20",
                            month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 -\nNov 20",
                            month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 -\nFeb 21",
                            month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 -\nMay 21",
                            month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 -\nAug 21",
                            month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 -\nNov 21"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 -\nMay 20",
                                        "Jun 20 -\nAug 20",
                                        "Sep 20 -\nNov 20",
                                        "Dec 20 -\nFeb 21",
                                        "Mar 21 -\nMay 21",
                                        "Jun 21 -\nAug 21",
                                        "Sep 21 -\nNov 21")))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 -\nMay 20",
                            "Jun 20 -\nAug 20",
                            "Sep 20 -\nNov 20",
                            "Dec 20 -\nFeb 21",
                            "Mar 21 -\nMay 21",
                            "Jun 21 -\nAug 21",
                            "Sep 21 -\nNov 21"))%>%
  # calculate number of people and deaths in each stratum
  group_by(Period, agegrp1, sex)%>%
  summarise(recovery_population=n_distinct(study_number),
            recovery_deaths = n_distinct(study_number[death28=="1"]))%>%
  ungroup()%>%
  
  # calculate and join number of people and deaths in each stratum in the national population
  left_join(
    National_HES_events%>%
      mutate(month=paste0(yearmonth, "01"))%>%
      mutate(month=as.Date(month, format="%Y%m%d", origin="1964-10-22"))%>% 
      mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 -\nMay 20",
                                month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 -\nAug 20",
                                month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 -\nNov 20",
                                month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 -\nFeb 21",
                                month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 -\nMay 21",
                                month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 -\nAug 21",
                                month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 -\nNov 21"))%>%
      mutate(Period=factor(Period, levels=c("Mar 20 -\nMay 20",
                                            "Jun 20 -\nAug 20",
                                            "Sep 20 -\nNov 20",
                                            "Dec 20 -\nFeb 21",
                                            "Mar 21 -\nMay 21",
                                            "Jun 21 -\nAug 21",
                                            "Sep 21 -\nNov 21")))%>%
      mutate(Period=fct_relevel(Period, "Mar 20 -\nMay 20",
                                "Jun 20 -\nAug 20",
                                "Sep 20 -\nNov 20",
                                "Dec 20 -\nFeb 21",
                                "Mar 21 -\nMay 21",
                                "Jun 21 -\nAug 21",
                                "Sep 21 -\nNov 21"))%>%
      mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                               "20-24",
                                               "25-29",
                                               "30-34",
                                               "35-39",
                                               "40-44",
                                               "45-49",
                                               "50-54",
                                               "55-59") ~ "<60",
                                 agegrp %in% c("60-64",
                                               "65-69") ~ "60-69",
                                 agegrp %in% c("70-74",
                                               "75-79") ~ "70-79",
                                 agegrp %in% c("80-84",
                                               "85-89") ~ "80-89",
                                 agegrp %in% c("90-94",
                                               "95+") ~ "90+"))%>%
      group_by(Period, agegrp1, sex)%>%
      summarise(National_population = n(),
                National_deaths = sum(death28)),
    
    by=c("Period", "agegrp1", "sex"))%>%
  

  # calculate adjusted number of deaths if the recovery population was structured as the national

  # calculate adjusted 28-day mortality (adjusted deaths in recovery over national population)
  group_by(Period)%>%
  summarise(
    population_recovery=sum(recovery_population),
    deaths_recovery=sum(recovery_deaths),
    population_national=sum(National_population),
    deaths_national = sum(National_deaths),
    
    death_rate_recovery=deaths_recovery/population_recovery,
    SE_recovery = sqrt(death_rate_recovery * (1-death_rate_recovery) / population_recovery),
    
    death_rate_national = deaths_national/population_national,
    SE_national = sqrt(death_rate_national * (1-death_rate_national) / population_national),
    
    recovery_deaths_adjusted = sum(
      recovery_deaths/recovery_population*National_population
      ), 
    # this is the directly-standardised number of deaths (mortality rate, deaths/28 days); we first calculate the number of expected deaths in each stratum if the reference population had the death experience of the recovery population, then multiply by the number of people in that stratum in the reference population, and then calculate the sum of all those deaths across the entirity of our age and sex bands
    
    SE_recovery_deaths_adjusted = sqrt(sum((recovery_deaths/recovery_population^2) * National_population^2)), # Raph's reference 
    
    death_rate_adjusted=sum(recovery_deaths/recovery_population*National_population)/sum(National_population), # this is the age-standardised rate (as recommended by Kirkwood and Sterne)


    SE_adjusted = 1/sum(National_population) * sqrt(sum((National_population^2*(recovery_deaths/recovery_population)*(1-recovery_deaths/recovery_population))/recovery_population)),# SE for the DSR as recommended by Kirkwood and Sterne (using proportion formula)
    SE_adjusted_rate1 = sqrt(sum((recovery_deaths/recovery_population^2) * National_population^2))/sum(National_population), # using Raph's reference
    
    
    SE_adjusted_rate2 = 1/sum(National_population) * sqrt(sum(((recovery_deaths* National_population^2)/recovery_population^2))),    # SE for the DSR as recommended by Kirkwood and Sterne 
    # 
    )
    
  

# generate table (for investigation)
# RECOVERY_HES_flags%>%
#   
#   # preliminary data processing
#   select(study_number=study_id,
#          indexdate,
#          sex,
#          death28)%>%
#   left_join(baseline_crf_england%>%select(study_number, age), by="study_number")%>%
#   mutate(month=as.Date(paste0(str_sub(indexdate, 1, 8), "01"), format="%Y-%m-%d"))%>%
#   # restrict to covid period
#   filter(month>"2020-02-01")%>%
#   
#   mutate(agegrp = case_when(age<20 ~ "15-19",
#                             between(age, 20, 24) ~ "20-24",
#                             between(age, 25, 29) ~ "25-29",
#                             between(age, 30, 34) ~ "30-34",
#                             between(age, 35, 39) ~ "35-39",
#                             between(age, 40, 44) ~ "40-44",
#                             between(age, 45, 49) ~ "45-49",
#                             between(age, 50, 54) ~ "50-54",
#                             between(age, 55, 59) ~ "55-59",
#                             between(age, 60, 64) ~ "60-64",
#                             between(age, 65, 69) ~ "65-69",
#                             between(age, 70, 74) ~ "70-74",
#                             between(age, 75, 79) ~ "75-79",
#                             between(age, 80, 84) ~ "80-84",
#                             between(age, 85, 89) ~ "85-89",
#                             between(age, 90, 94) ~ "90-94",
#                             age>=95 ~ "95+"))%>%
#   mutate(agegrp1 = case_when(agegrp %in% c("15-19",
#                                            "20-24",
#                                            "25-29",
#                                            "30-34",
#                                            "35-39",
#                                            "40-44",
#                                            "45-49",
#                                            "50-54",
#                                            "55-59") ~ "<60",
#                              agegrp %in% c("60-64",
#                                            "65-69") ~ "60-69",
#                              agegrp %in% c("70-74",
#                                            "75-79") ~ "70-79",
#                              agegrp %in% c("80-84",
#                                            "85-89") ~ "80-89",
#                              agegrp %in% c("90-94",
#                                            "95+") ~ "90+"))%>%
#   mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
#                             month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
#                             month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
#                             month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
#                             month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
#                             month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
#                             month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
#   mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
#                                         "Jun 20 - Aug 20",
#                                         "Sep 20 - Nov 20",
#                                         "Dec 20 - Feb 21",
#                                         "Mar 21 - May 21",
#                                         "Jun 21 - Aug 21",
#                                         "Sep 21 - Nov 21")))%>%
#   mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
#                             "Jun 20 - Aug 20",
#                             "Sep 20 - Nov 20",
#                             "Dec 20 - Feb 21",
#                             "Mar 21 - May 21",
#                             "Jun 21 - Aug 21",
#                             "Sep 21 - Nov 21"))%>%
#   
#   # calculate number of people and deaths in each stratum
#   group_by(Period, agegrp1, sex)%>%
#   summarise(recovery_population=n_distinct(study_number),
#             recovery_deaths = n_distinct(study_number[death28=="1"]))%>%
#   ungroup()%>%
#   
#   # calculate and join number of people and deaths in each stratum in the national population
#   left_join(
#     National_HES_events%>%
#       
#       mutate(month=paste0(yearmonth, "01"))%>%
#       mutate(month=as.Date(month, format="%Y%m%d", origin="1964-10-22"))%>%  
#       # restrict to covid period
#       filter(month>"2020-02-01")%>%
#       mutate(agegrp1 = case_when(agegrp %in% c("15-19",
#                                                "20-24",
#                                                "25-29",
#                                                "30-34",
#                                                "35-39",
#                                                "40-44",
#                                                "45-49",
#                                                "50-54",
#                                                "55-59") ~ "<60",
#                                  agegrp %in% c("60-64",
#                                                "65-69") ~ "60-69",
#                                  agegrp %in% c("70-74",
#                                                "75-79") ~ "70-79",
#                                  agegrp %in% c("80-84",
#                                                "85-89") ~ "80-89",
#                                  agegrp %in% c("90-94",
#                                                "95+") ~ "90+"))%>%
#       mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
#                                 month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
#                                 month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
#                                 month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
#                                 month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
#                                 month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
#                                 month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
#       mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
#                                             "Jun 20 - Aug 20",
#                                             "Sep 20 - Nov 20",
#                                             "Dec 20 - Feb 21",
#                                             "Mar 21 - May 21",
#                                             "Jun 21 - Aug 21",
#                                             "Sep 21 - Nov 21")))%>%
#       mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
#                                 "Jun 20 - Aug 20",
#                                 "Sep 20 - Nov 20",
#                                 "Dec 20 - Feb 21",
#                                 "Mar 21 - May 21",
#                                 "Jun 21 - Aug 21",
#                                 "Sep 21 - Nov 21"))%>%
#       group_by(Period, agegrp1, sex)%>%
#       summarise(National_population = n(),
#                 National_deaths = sum(death28)),
#     
#     by=c("Period", "agegrp1", "sex"))%>%
#   
# 
#   
#   # calculate adjusted number of deaths if the recovery population was structured as the national
#   
#   # calculate adjusted 28-day mortality (adjusted deaths in recovery over national population)
#   group_by(Period)%>%
#   mutate(
#     population_recovery=sum(recovery_population),
#     deaths_recovery=sum(recovery_deaths),
#     population_national=sum(National_population),
#     deaths_national = sum(National_deaths),
#     
#     recovery_deaths_adjusted = sum(recovery_deaths/recovery_population*National_population),
#     SE_recovery_deaths_adjusted = sqrt(sum((recovery_deaths/recovery_population^2) * National_population^2)),
#     
#     death_rate_recovery=deaths_recovery/population_recovery,
#     SE_recovery = sqrt(death_rate_recovery * (1-death_rate_recovery) / population_recovery),
#     
#     death_rate_national = deaths_national/population_national,
#     SE_national = sqrt(death_rate_national * (1-death_rate_national) / population_national),
#     
#     death_rate_adjusted=recovery_deaths_adjusted/population_national,
#     SE_adjusted = SE_recovery_deaths_adjusted/population_national)%>%
#   
#   write_csv(path="Intermediate outputs/standardised_death_rates_by_age_bands.csv")
# 
# write_csv(RECOVERY_HES_standardised_death_rates, "Intermediate outputs/standardised_death_rates.csv")

# merge RECOVERY crude, RECOVERY adjusted, and national population rates
combined_death_rates<-
  RECOVERY_HES_death_rates%>%
  select(Period, death_rate, deaths, SE, Cohort)%>%
  rbind(RECOVERY_HES_standardised_death_rates%>%
          select(Period,deaths=recovery_deaths_adjusted,
                 death_rate=death_rate_adjusted,
                 SE=SE_adjusted)%>%
          mutate(Cohort="RECOVERY (age- and sex-adjusted)"))%>%
  rbind(National_HES_death_rates%>%
          select(Period, deaths, death_rate, SE, Cohort))%>%
  mutate(lower_bound_95CI = death_rate-1.96*SE,
         upper_bound_95CI = death_rate+1.96*SE)




### aggregate (all periods) -----

RECOVERY_HES_death_rates%>%
  ungroup()%>%
  summarise(Deaths=sum(deaths),
            Population=sum(population),
            Mortality=Deaths/Population)%>%
  mutate(SE = sqrt(Mortality * (1-Mortality) / Population))%>%
  mutate(lower_bound = Mortality - 1.96*SE, # 95% CI
         upper_bound = Mortality + 1.96*SE)%>% # 95% CI
  mutate(cohort="RECOVERY (crude)")%>%
  
  bind_rows(
    National_HES_death_rates%>%
      ungroup()%>%
      summarise(Deaths=sum(deaths),
                Population=sum(population),
                Mortality=Deaths/Population)%>%
      mutate(SE = sqrt(Mortality * (1-Mortality) / Population))%>%
      mutate(lower_bound = Mortality - 1.96*SE,
             upper_bound = Mortality + 1.96*SE)%>%
      mutate(cohort="Reference population")
    )%>%
  bind_rows(
    RECOVERY_HES_standardised_death_rates%>%
      ungroup()%>%
      summarise(Deaths=sum(recovery_deaths_adjusted),
                Population=sum(population_national),
                Mortality=Deaths/Population,
                SE=1/sum(population_national)*sqrt(sum(((population_national)^2 * (deaths_recovery/population_recovery) *(1-deaths_recovery/population_recovery))/population_recovery)))%>%
      mutate(lower_bound = Mortality - 1.96*SE,
             upper_bound = Mortality + 1.96*SE)%>%
      mutate(cohort="RECOVERY (age- and sex-adjusted)")
  )%>%
  mutate(cohort=factor(cohort, levels=c("RECOVERY (crude)", "Reference population", "RECOVERY (age- and sex-adjusted)")))->aggregate_mortality_rates_estimates


 View(aggregate_mortality_rates_estimates)

  



##  Admissions -----

###  RECOVERY HES ------

RECOVERY_HES_timeseries_counts<-
  RECOVERY_HES_flags%>%
  select(study_number=study_id,
         indexdate)%>%
  mutate(month=as.Date(paste0(str_sub(indexdate, 1, 8), "01"), format="%Y-%m-%d"))%>%
  count(month)%>%
  mutate(Cohort = "RECOVERY HES")%>%
  mutate(month=as.Date(month, format))

###  National HES ------

National_HES_timeseries_counts<-
  National_HES_events%>%
  select(yearmonth)%>%
  mutate(month=paste0(yearmonth, "01"))%>%
  mutate(month=as.Date(month, format="%Y%m%d", origin="1964-10-22"))%>%
  count(month)%>%
  mutate(Cohort = "All-England HES")%>%
  mutate(month=as.Date(month, format))




### Plots-------


##### Admissions ------
  

  
  
  p1<-RECOVERY_HES_timeseries_counts%>%
    rbind(National_HES_timeseries_counts)%>%
    filter(month>"2020-02-01")%>%
    mutate(Cohort=fct_relevel(Cohort, "RECOVERY HES", "All-England HES"))%>%
    mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
                              month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
                              month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
                              month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
                              month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
                              month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
                              month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
    mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                          "Jun 20 - Aug 20",
                                          "Sep 20 - Nov 20",
                                          "Dec 20 - Feb 21",
                                          "Mar 21 - May 21",
                                          "Jun 21 - Aug 21",
                                          "Sep 21 - Nov 21")))%>%
    mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                              "Jun 20 - Aug 20",
                              "Sep 20 - Nov 20",
                              "Dec 20 - Feb 21",
                              "Mar 21 - May 21",
                              "Jun 21 - Aug 21",
                              "Sep 21 - Nov 21"))%>%
    group_by(Cohort, Period)%>%
    summarise(n=sum(n))%>%
    group_by(Period)%>%
    mutate(`Recruitment rate`=round(n[Cohort=="RECOVERY HES"]/n[Cohort=="All-England HES"]*100,1))%>%
    mutate(Cohort=case_when(Cohort == "RECOVERY HES" ~ "RECOVERY (crude)",
                            Cohort == "All-England HES" ~ "Reference population"))%>%
    mutate(Cohort=fct_relevel(Cohort, "RECOVERY (crude)","Reference population"))%>%
    
    ggplot(aes(Period, n, fill=Cohort, group=Cohort))+

    # geom_col_pattern(data=.%>%filter(Cohort=="RECOVERY (crude)"),
    #                  aes(pattern=Cohort,
    #                      pattern_angle=Cohort,
    #                      pattern_spacing=Cohort,
    #                      linetype=Cohort
    #                      # pattern_shape=Cohort
    #                      ),     
    #                  color="black",
    #                  # size=2,
    #                  pattern="stripe",
    #                  pattern_fill="black",
    #                  pattern_color="black",
    #          width=0.25
    # )+
    # 
    # geom_col_pattern(data=.%>%filter(Cohort=="Reference population"),
    #                  aes(pattern=Cohort,
    #                      pattern_angle=Cohort,
    #                      pattern_spacing=Cohort,
    #                      linetype=Cohort
    #                      # pattern_shape=Cohort
    #                  ),
    #                  color="black",
    #                  # size=1,
    #                  pattern_fill="black",
    #                  pattern_color="black",
    #                  pattern="stripe",
    #                  width=0.25,
    # )+
  geom_col(data=.%>%filter(Cohort=="Reference population"),
           aes(linetype=Cohort),
           color="black",
           # size=1.2,
           width=0.25
  )+
    geom_col(data=.%>%filter(Cohort=="RECOVERY (crude)"),
                     aes(linetype=Cohort),
                     color="black",
                     # size=1.2,
                     width=0.25,
             # linetype="dashed"
    )+

    
    
    
    geom_text(data=.%>%filter(Cohort=="RECOVERY (crude)"),
              aes(label=paste0(n, "\n(", `Recruitment rate`, "%)"),
                  group=Cohort, 
                  color=Cohort, 
                  y=0), 
              size=10*0.36,
              # hjust=1,5,
              # position = position_dodge(width=0.9),
              vjust=-0.2,
              nudge_x = -.35,
              show.legend = F
    )+
    geom_text(data=.%>%filter(Cohort=="Reference population"),
              aes(label=n, 
                  group=Cohort, 
                  color=Cohort,
                  y=max(n)), 
              size=10*0.36,
              # hjust=1,5,
              # position = position_dodge(width=0.9),
              # vjust=-5,
              nudge_x = -.35,
              show.legend = F
    )+
    
    
    
    scale_y_continuous(limits=c(0, 450000),
                       # breaks=seq(0,200000,10000)
                       # sec.axis = sec_axis(~./10000, name="Recruitment rate (%)")
    )+
    theme(legend.position = "bottom",
          axis.text.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          text=element_text(size=20),
          # plot.margin = margin(
          #   # t = 20,  # Top margin
          #   r = 10,  # Right margin
          #   # b = 40,  # Bottom margin
          #   l = 10)  # Left margin
          )+
    labs(
      subtitle="Number of individuals included in RECOVERY and the reference population",
      x="Index date (at monthly level)",
      y="Individuals",
      color="",
      fill="",
      linetype="")+
    # scale_x_date(expand = expansion(0.1))+
    scale_fill_manual(values=colors[c(1,3)])+
    scale_color_manual(values=colors[c(1,3)])+
    scale_linetype_manual(values=c(2,1))
    #scale_pattern_manual(values = c("stripe", "stripe"))+
    #scale_pattern_spacing_discrete(values=c(1,0.5))

  
  p1



##### Mortality ------

p2<-RECOVERY_HES_death_rates%>%
  select(Period, death_rate, SE, Cohort)%>%
  rbind(RECOVERY_HES_standardised_death_rates%>%
          select(Period,
                 death_rate=death_rate_adjusted,
                 # population=population_national,
                 SE=SE_adjusted)%>%
          mutate(Cohort="RECOVERY HES (age- and sex-adjusted)"))%>%
  rbind(National_HES_death_rates%>%
          select(Period, death_rate,SE, 
                 # population, 
                 Cohort))%>%
  mutate(death_rate=round(death_rate*100,1),
         SE=round(SE*100,1))%>%
  mutate(lower_bound_95CI = death_rate-1.96*SE,
         upper_bound_95CI = death_rate+1.96*SE)%>%
  mutate(Cohort=factor(Cohort, levels=c("RECOVERY HES", "All-England HES", "RECOVERY HES (age- and sex-adjusted)")))%>%
  mutate(Cohort=fct_relevel(Cohort, "RECOVERY HES", "All-England HES", "RECOVERY HES (age- and sex-adjusted)"))%>%
  ungroup()%>%
  mutate(Cohort=case_when(Cohort == "RECOVERY HES" ~ "RECOVERY (crude)",
                          Cohort == "RECOVERY HES (age- and sex-adjusted)" ~ "RECOVERY (adjusted)",
                          Cohort == "All-England HES" ~ "Reference population"))%>%
  mutate(Cohort=fct_relevel(Cohort, "RECOVERY (crude)","RECOVERY (adjusted)", "Reference population"))%>%

  ggplot(aes(Period, death_rate, group=Cohort, 
             # fill=Cohort, 
             color=Cohort, shape=Cohort))+
  geom_point(size=8,
             # color="black",
             # pch=21,
             position = position_dodge(width=0.5)
             )+
  # geom_line()+
  geom_errorbar(aes(ymin=lower_bound_95CI,
                    ymax=upper_bound_95CI),
                width=0.1,
                size=2,
                position = position_dodge(width=0.5)
                )+
  scale_y_continuous(limits=c(0, 40),
                     breaks = seq(0,40,5))+
  
  theme(legend.position = "bottom",
        # axis.text.x = element_text(angle=30, hjust=1, vjust=1),
        panel.grid.minor = element_blank(),
        text=element_text(size=30, family="Mulish"))+
  labs(
    # subtitle="28-day mortality",
       # caption=str_wrap("28-day mortality presented as the proportion of people with death recorded within 28 days of their index date along with 95% confidence intervals.
       # Standardisation performed by applying RECOVERY HES 28-day mortality to an age- (5-year bands) and sex-standardised population using All-England HES cohort as reference, in a rolling monthly basis (for 28-day mortality and age and sex breakdown)",200),
       x="Index period",
       y="Mortality (%)",
       color=NULL,
       shape=NULL)+
 
  scale_color_manual(values=colors)

p2  

##### join aggregate summaries -----

  
  
p3<-aggregate_mortality_rates_estimates%>%
  mutate(Period="All time periods")%>%
  mutate(cohort=factor(cohort, levels=c("RECOVERY (crude)", "RECOVERY (age- and sex-adjusted)", "Reference population")))%>%
  mutate(cohort=case_when(cohort=="RECOVERY (age- and sex-adjusted)" ~ "RECOVERY (adjusted)",
                          TRUE ~ as.character(cohort)))%>%
  mutate(cohort=fct_relevel(cohort, "RECOVERY (crude)", "RECOVERY (adjusted)", "Reference population"))%>%
  
    mutate(`Recruitment rate`=round(Population[cohort=="RECOVERY (crude)"]/Population[cohort=="Reference population"]*100,1))%>%
    mutate(cohort=fct_relevel(cohort, "RECOVERY (crude)","Reference population"))%>%
    
    ggplot(aes(Period, Population, fill=cohort, group=cohort))+

    geom_text(data=.%>%filter(cohort=="RECOVERY (crude)"),
              aes(label=paste0(Population, " (", `Recruitment rate`, "%)"),
                  group=cohort, 
                  color=cohort, 
                  y=0), 
              size=10*0.36,
              # position = position_dodge(width=0.9),
              vjust=-0.1,
              nudge_x = -.35
    )+
    geom_text(data=.%>%filter(cohort=="Reference population"),
              aes(label=Population, group=cohort, color=cohort), 
              size=10*0.36,
              # position = position_dodge(width=0.9),
              vjust=-0.1,
              nudge_x = -.35
    )+
    
    geom_col(data=.%>%filter(cohort=="Reference population"),
             aes(linetype=cohort),
             color="black",
             # size=1.2,
             width=0.25
    )+
    geom_col(data=.%>%filter(cohort=="RECOVERY (crude)"),
             aes(linetype=cohort),
             color="black",
             # size=1.2,
             width=0.25,
             # linetype="dashed"
    )+
    
    scale_y_continuous(
      limits=c(0, 450000),
                       # breaks=seq(0,200000,10000)
                       # sec.axis = sec_axis(~./10000, name="Recruitment rate (%)")
    )+
    theme(legend.position="none",
          axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          text=element_text(size=20),
          # axis.text.x=element_text(angle=30, 
          #                          vjust=0.5, 
          #                          # hjust=-0.1
  #        )
  )+
    labs(
      x="Index date (at monthly level)",
      y="Individuals",
      color="",
      fill="",
      linetype="")+
    scale_color_manual(values=colors[c(1,3)])+
  scale_fill_manual(values=colors[c(1,3)])+
    scale_linetype_manual(values=c(2,1))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
p4<-aggregate_mortality_rates_estimates%>%
  mutate(Period="All time periods")%>%
  mutate(cohort=factor(cohort, levels=c("RECOVERY (crude)", "RECOVERY (age- and sex-adjusted)", "Reference population")))%>%
  mutate(cohort=case_when(cohort=="RECOVERY (age- and sex-adjusted)" ~ "RECOVERY (adjusted)",
                          TRUE ~ as.character(cohort)))%>%
  mutate(cohort=fct_relevel(cohort, "RECOVERY (crude)", "RECOVERY (adjusted)", "Reference population"))%>%
  
  ggplot(aes(x=Period,
             y=Mortality*100,
             color=cohort,
             shape=cohort))+
  geom_point(size=8,
             position = position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=lower_bound*100, 
                    ymax=upper_bound*100),
                position = position_dodge(width=0.5),
                width=0.1,
                size=2
  )+
  theme(legend.position="none",
        axis.text.y=element_blank(),
        # axis.text.x = element_text(angle=90, hjust=1, vjust=1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        text=element_text(size=30, family="Mulish"),
 
        )+
  scale_y_continuous(limits=c(0, 40),
                     breaks = seq(0,40,5))+  
  scale_color_manual(values=colors)+
  labs(color="",
       shape="")


#### Plot (figure 3) ------

plot<-
  # p1+p3+
  p2+p4+
  plot_layout(widths=c(2,0.4))

plot

ggsave("Outputs/Figures/figure_3_timeseries_admissions_mortality_periods.png",
       dpi="retina",
       width=50,
       height=26,
       units="cm")

# ggsave("Outputs/Figures/figure_3_timeseries_admissions_mortality_periods.tiff",
#        dpi="retina",
#        width=50,
#        height=26,
#        units="cm")
  



#### forest plot (alternative visualisation, not used) -------
# 
# forest_plot_table<-
#   RECOVERY_HES_timeseries_counts%>%
#   rbind(National_HES_timeseries_counts)%>%
#   filter(month>"2020-02-01")%>%
#   mutate(Cohort=fct_relevel(Cohort, "RECOVERY HES", "All-England HES"))%>%
#   mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
#                             month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
#                             month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
#                             month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
#                             month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
#                             month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
#                             month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
#   mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
#                                         "Jun 20 - Aug 20",
#                                         "Sep 20 - Nov 20",
#                                         "Dec 20 - Feb 21",
#                                         "Mar 21 - May 21",
#                                         "Jun 21 - Aug 21",
#                                         "Sep 21 - Nov 21")))%>%
#   mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
#                             "Jun 20 - Aug 20",
#                             "Sep 20 - Nov 20",
#                             "Dec 20 - Feb 21",
#                             "Mar 21 - May 21",
#                             "Jun 21 - Aug 21",
#                             "Sep 21 - Nov 21"))%>%
#   group_by(Cohort, Period)%>%
#   summarise(n=sum(n))%>%
#   group_by(Period)%>%
#   mutate(`Recruitment rate`=round(n[Cohort=="RECOVERY HES"]/n[Cohort=="All-England HES"]*100,1))%>%
#   mutate(Cohort=case_when(Cohort == "RECOVERY HES" ~ "RECOVERY",
#                           Cohort == "All-England HES" ~ "Reference population"))%>%
#   mutate(Cohort=fct_relevel(Cohort, "RECOVERY","Reference population"))%>%
#   pivot_wider(c(Period, `Recruitment rate`), names_from = "Cohort", values_from = "n",
#               names_glue = "Individuals ({Cohort})")%>%
#   
#   left_join(
#     combined_death_rates%>%
#       mutate(Cohort=case_when(Cohort == "RECOVERY HES" ~ "RECOVERY (crude)",
#                               Cohort == "All-England HES" ~ "Reference population",
#                               Cohort=="RECOVERY (age- and sex-adjusted)" ~ "RECOVERY (adjusted)"))%>%
#       pivot_wider(Period, names_from = "Cohort", values_from = c("death_rate",
#                                                                  "deaths",
#                                                                  "SE",
#                                                                  "lower_bound_95CI",
#                                                                  "upper_bound_95CI")))%>%
#   rename(`Mortality (RECOVERY crude)` = `death_rate_RECOVERY (crude)`,
#          `Mortality (RECOVERY adjusted)` = `death_rate_RECOVERY (adjusted)`,
#          `Mortality (Reference population)` = `death_rate_Reference population`,
#          `Deaths (RECOVERY crude)` = `deaths_RECOVERY (crude)`,
#          `Deaths (RECOVERY adjusted)` = `deaths_RECOVERY (adjusted)`,
#          `Deaths (Reference population)` = `deaths_Reference population`)%>%
#   
#   rbind(
#     aggregate_mortality_rates_estimates%>%
#           mutate(cohort=as.character(cohort))%>%
#           mutate(Period="Overall")%>%
#           mutate(cohort=if_else(cohort=="RECOVERY (age- and sex-adjusted)", "RECOVERY (adjusted)", cohort))%>%
#           pivot_wider(Period, names_from = "cohort", values_from = -c(Period, cohort),
#                       names_sep=" ",
#                       # names_glue=" ({.name})"
#                       )%>%
#           mutate(`Recruitment rate` = round(`Population RECOVERY (crude)`/`Population Reference population`*100,1))%>%
#       select(-`Population RECOVERY (adjusted)`)%>%
#           rename(`Individuals (RECOVERY)` = `Population RECOVERY (crude)`,
#                  `Individuals (Reference population)` = `Population Reference population`,
#                  `Deaths (RECOVERY crude)` = `Deaths RECOVERY (crude)`,
#                  `Deaths (RECOVERY adjusted)` = `Deaths RECOVERY (adjusted)`,
#                  `Deaths (Reference population)` = `Deaths Reference population`,
#                  
#                  `Mortality (RECOVERY crude)` = `Mortality RECOVERY (crude)`,
#                  `Mortality (RECOVERY adjusted)` = `Mortality RECOVERY (adjusted)`,
#                  `Mortality (Reference population)` = `Mortality Reference population`,
#                  
#                  `SE_RECOVERY (crude)` = `SE RECOVERY (crude)`,
#                  `SE_RECOVERY (adjusted)` = `SE RECOVERY (adjusted)`,
#                  `SE_Reference population` = `SE Reference population`,
#                  
#                  `lower_bound_95CI_RECOVERY (crude)` = `lower_bound RECOVERY (crude)`,
#                  `lower_bound_95CI_RECOVERY (adjusted)` = `lower_bound RECOVERY (adjusted)`,
#                  `lower_bound_95CI_Reference population` = `lower_bound Reference population`,
#                  
#                  `upper_bound_95CI_RECOVERY (crude)` = `upper_bound RECOVERY (crude)`,
#                  `upper_bound_95CI_RECOVERY (adjusted)` = `upper_bound RECOVERY (adjusted)`,
#                  `upper_bound_95CI_Reference population` = `upper_bound Reference population`))%>%
#   select(-`Deaths (RECOVERY adjusted)`)%>%
#   mutate(
#     across(starts_with("lower"), ~round(.*100,1)),
#     across(starts_with("upper"), ~round(.*100,1)),
#     across(starts_with("SE"), ~round(.*100,1)),
#     across(starts_with("Mortality"), ~round(.*100,1)),
#     
#     `Mortality (RECOVERY crude), 95% CI` = paste0(`Mortality (RECOVERY crude)`," (", `lower_bound_95CI_RECOVERY (crude)`, " - ", `upper_bound_95CI_RECOVERY (crude)`, ")"),
#          `Mortality (RECOVERY adjusted), 95% CI` = paste0(`Mortality (RECOVERY adjusted)`," (", `lower_bound_95CI_RECOVERY (adjusted)`, " - ", `upper_bound_95CI_RECOVERY (adjusted)`, ")"),
#          `Mortality (Reference population), 95% CI` = paste0(`Mortality (Reference population)`," (", `lower_bound_95CI_Reference population`, " - ", `upper_bound_95CI_Reference population`, ")"))%>%
#   select(Period, 
#          `Individuals\n(RECOVERY)` = `Individuals (RECOVERY)`,
#          `Individuals\n(Reference\npopulation)` = `Individuals (Reference population)`,
#          `Recruitment\nrate (%)` = `Recruitment rate`,
#          `Deaths\n(RECOVERY)` = `Deaths (RECOVERY crude)`,
#          `Deaths\n(Reference\npopulation)` = `Deaths (Reference population)`,
# 
#          starts_with("Mortality"),
#          starts_with("lower_bound"),
#          starts_with("upper_bound"),
#          starts_with("SE"))%>%
#   rename(`RECOVERY\n(crude)`=`Mortality (RECOVERY crude), 95% CI`,
#          `RECOVERY\n(adjusted)` = `Mortality (RECOVERY adjusted), 95% CI`,
#         `Reference\npopulation` =`Mortality (Reference population), 95% CI`)%>%
#   add_column(` `=NA, .after = "Deaths\n(Reference\npopulation)")%>%
#   mutate(` `=paste(rep(" ", 60), collapse = " "))%>%
#   mutate(`Variance (RECOVERY crude)` = `Mortality (RECOVERY crude)`/100*(1-`Mortality (RECOVERY crude)`/100),
#          `Variance (RECOVERY adjusted)` = `Mortality (RECOVERY adjusted)`/100*(1-`Mortality (RECOVERY adjusted)`/100),
#          `Variance (Reference population)` = `Mortality (Reference population)`/100*(1-`Mortality (Reference population)`/100))%>%
#   mutate(across(starts_with("Individuals") | starts_with("Deaths"), ~formatC(., format="f", big.mark=",", digits=0)))
#   
#   
# 
# forest_plot_mortality<-forest(
#   forest_plot_table[,c(1:7,11:13)],
# 
#         est=list(
#         forest_plot_table$`Mortality (RECOVERY crude)`,
#          forest_plot_table$`Mortality (RECOVERY adjusted)`,
#          forest_plot_table$`Mortality (Reference population)`),
#   
# lower=list(forest_plot_table$`lower_bound_95CI_RECOVERY (crude)`,
#            forest_plot_table$`lower_bound_95CI_RECOVERY (adjusted)`,
#            forest_plot_table$`lower_bound_95CI_Reference population`
# ),
# 
# upper=list(forest_plot_table$`upper_bound_95CI_RECOVERY (crude)`,
#            forest_plot_table$`upper_bound_95CI_RECOVERY (adjusted)`,
#            forest_plot_table$`upper_bound_95CI_Reference population`
# ),
# 
# sizes=list(1/forest_plot_table$`Variance (RECOVERY crude)`/10,
#            1/forest_plot_table$`Variance (RECOVERY adjusted)`/10,
#            1/forest_plot_table$`Variance (Reference population)`/10),
# 
# ci_column = 7,
# xlim=c(0,35),
# xlab = "Mortality (%)",
# ref_line = 0,
# nudge_y=0.3,
# theme = forest_theme(
#   ci_col="black",
#                     ci_fill =colors,
#                      ci_alpha=0.9,
#                      legend_name="Cohort",
#                      ci_pch=22,
#                      ci_Theight = 0.2,
#                      legend_position="bottom",
#                      base_family = "Mulish",
#                      legend_value=c("RECOVERY (crude)",
#                                     "RECOVERY (adjusted)",
#                                     "Reference population")))%>%
#   insert_text(.,
#               text = "Mortality (95% CI)",
#               col = 8:10,
#               part = "header",
#               gp = gpar(fontface = "bold"))%>%
#   edit_plot(row=8,
#             which="text",
#             gp=gpar(fontface="bold"
#                     ))%>%
#   add_text(text="
#            
#            ",
#            row=1,
#            col=1)%>%
#   add_text(text="
#            
#            ",
#            row=2,
#            col=1)%>%
#   add_text(text="
#            
#            ",
#            row=3,
#            col=1)%>%
#   add_text(text="
#            
#            ",
#            row=4,
#            col=1)%>%
#   add_text(text="
#            
#            ",
#            row=5,
#            col=1)%>%
#   add_text(text="
#            
#            ",
#            row=6,
#            col=1)%>%
#   add_text(text="
#            
#            ",
#            row=7,
#            col=1)%>%
#   add_text(text="
#            
#            ",
#            row=8,
#            col=1)
# 
#   
# forest_plot_mortality
# 
# ggsave(filename="Outputs/Publication/Figures/forestplot mortality rates.png", 
#        plot=forest_plot_mortality,
#        dpi = "retina",
#        units = "cm",
#        width = get_wh(plot=forest_plot_mortality, unit="cm")*1.05,
#        # width=50,
#        height=get_wh(plot=forest_plot_mortality, unit="cm"),
#        limitsize = F,
#        # height = 30
#        # scale=2
# )






##### save table -----

RECOVERY_HES_death_rates%>%
  select(Period, 
         deaths_recovery=deaths, 
         population_recovery = population, 
         death_rate_recovery=death_rate, 
         SE_recovery=SE)%>%
  
  left_join(RECOVERY_HES_standardised_death_rates%>%
              select(Period,
                     death_rate_adjusted,
                     SE_adjusted,
              ))%>%
  left_join(National_HES_death_rates%>%
              select(Period, 
                     deaths_national=deaths, 
                     population_national = population, 
                     death_rate_national = death_rate, 
                     SE_national = SE))%>%
  rbind(data.frame(
    deaths_recovery=aggregate_mortality_rates_estimates$Deaths[aggregate_mortality_rates_estimates$cohort=="RECOVERY (crude)"],
    population_recovery=aggregate_mortality_rates_estimates$Population[aggregate_mortality_rates_estimates$cohort=="RECOVERY (crude)"],
    death_rate_recovery=aggregate_mortality_rates_estimates$Mortality[aggregate_mortality_rates_estimates$cohort=="RECOVERY (crude)"],
    SE_recovery=aggregate_mortality_rates_estimates$SE[aggregate_mortality_rates_estimates$cohort=="RECOVERY (crude)"],
    
    death_rate_adjusted=aggregate_mortality_rates_estimates$Mortality[aggregate_mortality_rates_estimates$cohort=="RECOVERY (age- and sex-adjusted)"],
    SE_adjusted=aggregate_mortality_rates_estimates$SE[aggregate_mortality_rates_estimates$cohort=="RECOVERY (age- and sex-adjusted)"],
    
    deaths_national=aggregate_mortality_rates_estimates$Deaths[aggregate_mortality_rates_estimates$cohort=="Reference population"],
    population_national=aggregate_mortality_rates_estimates$Population[aggregate_mortality_rates_estimates$cohort=="Reference population"],
    death_rate_national=aggregate_mortality_rates_estimates$Mortality[aggregate_mortality_rates_estimates$cohort=="Reference population"],
    SE_national=aggregate_mortality_rates_estimates$SE[aggregate_mortality_rates_estimates$cohort=="Reference population"],
    Period="Overall"))%>%
    
    mutate(death_rate_recovery = round(death_rate_recovery*100,1),
         SE_recovery = round(SE_recovery*100, 1),
         CI_recovery = paste0(round(death_rate_recovery-1.96*SE_recovery,1), " - ", round(death_rate_recovery+1.96*SE_recovery,1)),
         
         death_rate_adjusted = round(death_rate_adjusted*100,1),
         SE_adjusted = round(SE_adjusted*100, 1),
         CI_adjusted = paste0(round(death_rate_adjusted-1.96*SE_adjusted,1), " - ", round(death_rate_adjusted+1.96*SE_adjusted,1)),
         
         death_rate_national = round(death_rate_national*100,1),
         SE_national = round(SE_national*100, 1),
         CI_national = paste0(round(death_rate_national-1.96*SE_national,1), " - ", round(death_rate_national+1.96*SE_national,1)))%>%
  
  mutate(death_rate_recovery = paste0(death_rate_recovery, " (", CI_recovery, ")"),
         death_rate_adjusted = paste0(death_rate_adjusted, " (", CI_adjusted, ")"),
         death_rate_national = paste0(death_rate_national, " (", CI_national, ")"))%>%
  mutate(Period=fct_relevel(Period,"Overall",
                            "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%  
  arrange(Period)%>%
  select(Period,
         `Deaths_RECOVERY` = deaths_recovery,
         `Population_RECOVERY` = population_recovery,
         `28-day mortality (95% CI)_RECOVERY` = death_rate_recovery,
         `28-day mortality, adjusted (95% CI)` = death_rate_adjusted,
         `Deaths_national` = deaths_national,
         Population_national = population_national,
         `28-day mortality (95% CI)_national` = death_rate_national)->timeseries_death_rates_table


my_header <- data.frame(
  col_keys = c(colnames(timeseries_death_rates_table)),
  line1 = c("Period", 
            rep("RECOVERY",4), 
            rep("Reference population", 3)),
  line2 = c(colnames(timeseries_death_rates_table)), 
  stringsAsFactors = FALSE
)%>%
  mutate(line2=map(strsplit(line2, "_"), 1))%>%
  mutate(across(everything(), ~as.character(.)))


aggregated_mortality_rates_table<-
  timeseries_death_rates_table%>%
  flextable(col_keys = my_header$col_keys)%>%
  set_header_df(mapping = my_header, key = "col_keys")%>%
  theme_booktabs() %>% 
  merge_v(part = "header") %>%
  merge_h(part = "header") %>%
  align(align = "center", part = "all") %>%
  colformat_num(na_str = "")%>%
  width(width=4, unit="cm")%>%
  width(j=c(2,3,6,7), width=2.5,  unit="cm")%>%
  set_caption("28-day mortality along time in the RECOVERY cohort and the reference population")%>%
  border(i=2, j=c(1,5), border.right = fp_border(), part="header")%>%
  border(i=1, j=c(1,2), border.right = fp_border(), part="header")%>%
  align(i=NULL, j=1, part="all", align="left")%>%
  border(j=c(1,5), border.right = fp_border(), part="body")%>%
  # footnote(i=2, j=5, ref_symbols = c("1"), value = as_paragraph("Adjusted using RECOVERY 28-day mortality applied to the All-England HES population characteristics by sex and age strata using 5-year bands"), part="header")%>%
  # footnote(i=2, j=5, ref_symbols = c("2"), value = as_paragraph("95% confidence intervals calculated as: 95% CI = rate +/-1.96*√(Σ RECOVERY deaths/RECOVERY population ^2 * National population ^2)/National population"), part="header")%>%
  font(fontname = "Mulish", part = "all")

aggregated_mortality_rates_table

sect_properties <- prop_section(page_size = page_size(
  orient = "landscape",
  width = 29.7/2.54,
  height=20/2.54
),
type = "continuous",
page_margins = page_mar())

save_as_docx(aggregated_mortality_rates_table, 
             path="Outputs/Tables/supplementary_table_S5_aggregated_mortality_rates_table.docx",
             pr_section =sect_properties)









##  Inclusion timeseries  -------

RECOVERY_HES_timeseries_counts%>%
  rbind(National_HES_timeseries_counts)%>%
  # filter(month>="2020-03-01")%>%
  summarise(`RECOVERY HES (individuals)` = sum(n[Cohort=="RECOVERY HES"]),
            `All-England HES (individuals)` = sum(n[Cohort=="All-England HES"]),
            `Proportion included in RECOVERY (%)` = round(`RECOVERY HES (individuals)`/`All-England HES (individuals)`*100,1))%>%
  mutate(`Index month` = "Overall")%>%
  relocate(`Index month`, .before=`RECOVERY HES (individuals)`)%>%
  
  rbind(
RECOVERY_HES_timeseries_counts%>%
  rbind(National_HES_timeseries_counts)%>%
  # filter(month>="2020-03-01")%>%
  group_by(month)%>%
  summarise(`RECOVERY HES (individuals)` = n[Cohort=="RECOVERY HES"],
            `All-England HES (individuals)` = n[Cohort=="All-England HES"],
            `Proportion included in RECOVERY (%)` = round(`RECOVERY HES (individuals)`/`All-England HES (individuals)`*100,1))%>%
  rename(`Index month` = month)%>%
  mutate(`Index month` = as.character(`Index month`))
)%>%
  mutate(`Index month` = as.Date(`Index month`, "%Y-%m-%d"))%>%
  mutate(`Index month` = strftime(`Index month`, "%Y %B"))%>%
  mutate(`Index month` = if_else(is.na(`Index month`), "Aggregate", `Index month`))%>%
  mutate(prop1 = round(`RECOVERY HES (individuals)`/`RECOVERY HES (individuals)`[`Index month`=="Aggregate"]*100,1),
         prop2 = round(`All-England HES (individuals)`/`All-England HES (individuals)`[`Index month`=="Aggregate"]*100, 1))%>%
  mutate(`RECOVERY HES (individuals)` = paste0(`RECOVERY HES (individuals)`, " (", prop1, "%)"),
         `All-England HES (individuals)` = paste0(`All-England HES (individuals)`, " (", prop2, "%)"),
         `Proportion included in RECOVERY (%)` = paste0(`Proportion included in RECOVERY (%)`, "%"))%>%
  rename(`RECOVERY HES` = `RECOVERY HES (individuals)`,
         `All-England HES` = `All-England HES (individuals)`,
         `Proportion recruited` = `Proportion included in RECOVERY (%)`)%>%
  select(-prop1, -prop2)%>%
  flextable()%>%
  # add_header_row(values = c("", "Number of individuals included in each index month", ""), colwidths = c(1,2,1))%>%
  set_caption("Number of individuals included in RECOVERY HES versus All-England HES along time")%>%
  border(i=1, j=NULL, border.bottom = fp_border(), part="body")%>%
  width(j=NULL, width=4, unit = "cm")%>%
  footnote(i=1, 
           j=2:3,
           value = as_paragraph("Individuals (proportion within cohort)"),
           ref_symbols =c("1"),
           part="header"
          
  )%>%
  align(align = "center", j=2:4, part = "all")-> inclusion_timeseries_table

inclusion_timeseries_table

save_as_docx(inclusion_timeseries_table, 
             path="Outputs/Tables/RECOVERY_inclusion_timeseries_table.docx")





##   Admissions and mortality split by age groups -------

### calculations --------

### RECOVERY 28-day mortality
RECOVERY_HES_death_rates_by_age_groups<-
  RECOVERY_HES_flags%>%
  select(study_number=study_id,
         indexdate,
         sex, 
         death28,
         agegrp)%>%
  mutate(month=paste0(str_sub(indexdate, 1, 8), "01"))%>%
  mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                           "20-24",
                                           "25-29",
                                           "30-34",
                                           "35-39",
                                           "40-44",
                                           "45-49",
                                           "50-54",
                                           "55-59") ~ "<60",
                             agegrp %in% c("60-64",
                                           "65-69") ~ "60-69",
                             agegrp %in% c("70-74",
                                           "75-79") ~ "70-79",
                             agegrp %in% c("80-84",
                                           "85-89",
                                           "90-94",
                                           "95+") ~ "80+"))%>%
  mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
                            month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
                            month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
                            month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
                            month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
                            month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
                            month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21")))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%
  count(Period, death28, agegrp1)%>%
  group_by(Period, agegrp1)%>%
  summarise(deaths=sum(n[death28==1]),
            population=sum(n),
            death_rate=sum(n[death28==1])/sum(n),
            SE = sqrt(death_rate * (1-death_rate) / sum(n)))%>%
  mutate(Cohort = "RECOVERY")



# national HES 28-day mortality
National_HES_death_rates_by_age_groups<-
  National_HES_events%>%
  select(yearmonth,
         sex, 
         death28, 
         agegrp)%>%
  mutate(month=paste0(yearmonth, "01"))%>%
  mutate(month=as.Date(month, format="%Y%m%d", origin = "1984-01-01"))%>%
  mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                           "20-24",
                                           "25-29",
                                           "30-34",
                                           "35-39",
                                           "40-44",
                                           "45-49",
                                           "50-54",
                                           "55-59") ~ "<60",
                             agegrp %in% c("60-64",
                                           "65-69") ~ "60-69",
                             agegrp %in% c("70-74",
                                           "75-79") ~ "70-79",
                             agegrp %in% c("80-84",
                                           "85-89",
                                           "90-94",
                                           "95+") ~ "80+"))%>%
  mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
                            month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
                            month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
                            month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
                            month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
                            month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
                            month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21")))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%
  count(Period, death28, agegrp1)%>%
  group_by(Period, agegrp1)%>%
  summarise(deaths=sum(n[death28==1]),
            population=sum(n),
            death_rate=sum(n[death28==1])/sum(n),
            SE = sqrt(death_rate * (1-death_rate) / sum(n)))%>%
  mutate(Cohort = "Reference population")



### mortality rates in RECOVERY HES age- and sex- adjusted to national HES

RECOVERY_HES_standardised_death_rates_by_age_groups<-
  
  # take HES data
  RECOVERY_HES_flags%>%
  
  # preliminary data processing
  select(study_number=study_id,
         indexdate,
         sex,
         death28)%>%
  left_join(baseline_crf_england%>%select(study_number, age), by="study_number")%>%
  mutate(month=as.Date(paste0(str_sub(indexdate, 1, 8), "01"), format="%Y-%m-%d"))%>%
  mutate(agegrp = case_when(age<20 ~ "15-19",
                            between(age, 20, 24) ~ "20-24",
                            between(age, 25, 29) ~ "25-29",
                            between(age, 30, 34) ~ "30-34",
                            between(age, 35, 39) ~ "35-39",
                            between(age, 40, 44) ~ "40-44",
                            between(age, 45, 49) ~ "45-49",
                            between(age, 50, 54) ~ "50-54",
                            between(age, 55, 59) ~ "55-59",
                            between(age, 60, 64) ~ "60-64",
                            between(age, 65, 69) ~ "65-69",
                            between(age, 70, 74) ~ "70-74",
                            between(age, 75, 79) ~ "75-79",
                            between(age, 80, 84) ~ "80-84",
                            between(age, 85, 89) ~ "85-89",
                            between(age, 90, 94) ~ "90-94",
                            age>=95 ~ "95+"))%>%
  mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
                            month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
                            month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
                            month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
                            month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
                            month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
                            month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21")))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%
  
  # calculate number of people and deaths in each stratum
  group_by(Period, agegrp, sex)%>%
  summarise(recovery_population=n_distinct(study_number),
            recovery_deaths = n_distinct(study_number[death28=="1"]))%>%
  ungroup()%>%
  mutate(recovery_death_rate = recovery_deaths/recovery_population)%>%
  
  # calculate and join number of people and deaths in each stratum in the national population
  left_join(
    National_HES_events%>%
      mutate(month=paste0(yearmonth, "01"))%>%
      mutate(month=as.Date(month, format="%Y%m%d", origin="1964-10-22"))%>%    
      mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
                                month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
                                month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
                                month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
                                month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
                                month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
                                month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
      mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                            "Jun 20 - Aug 20",
                                            "Sep 20 - Nov 20",
                                            "Dec 20 - Feb 21",
                                            "Mar 21 - May 21",
                                            "Jun 21 - Aug 21",
                                            "Sep 21 - Nov 21")))%>%
      mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                                "Jun 20 - Aug 20",
                                "Sep 20 - Nov 20",
                                "Dec 20 - Feb 21",
                                "Mar 21 - May 21",
                                "Jun 21 - Aug 21",
                                "Sep 21 - Nov 21"))%>%
      group_by(Period, agegrp, sex)%>%
      summarise(National_population = n(),
                National_deaths=length(death28[death28=="1"]),
                National_death_rate = round(National_deaths/National_population*100,1)),
    
    by=c("Period", "agegrp", "sex"))%>%
  
  # calculate adjusted number of deaths if the recovery population was structured as the national
  mutate(recovery_adjusted_deaths_expected = recovery_death_rate*National_population)%>%
  mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                           "20-24",
                                           "25-29",
                                           "30-34",
                                           "35-39",
                                           "40-44",
                                           "45-49",
                                           "50-54",
                                           "55-59") ~ "<60",
                             agegrp %in% c("60-64",
                                           "65-69") ~ "60-69",
                             agegrp %in% c("70-74",
                                           "75-79") ~ "70-79",
                             agegrp %in% c("80-84",
                                           "85-89",
                                           "90-94",
                                           "95+") ~ "80+"))%>%
  # calculate adjusted 28-day mortality (adjusted deaths in recovery over national population)
filter(!is.na(sex))%>%  
group_by(Period, agegrp1)%>%
  
  summarise(population_recovery=sum(recovery_population),
            deaths_recovery=sum(recovery_deaths),
            population_national=sum(National_population),
            deaths_national = sum(National_deaths),
                
            recovery_deaths_adjusted = sum(recovery_deaths/recovery_population*National_population),
            SE_recovery_deaths_adjusted = sqrt(sum((recovery_deaths/recovery_population^2) * National_population^2)),
                
            death_rate_recovery=deaths_recovery/population_recovery,
            SE_recovery = sqrt(death_rate_recovery * (1-death_rate_recovery) / population_recovery),
                
            death_rate_national = deaths_national/population_national,
            SE_national = sqrt(death_rate_national * (1-death_rate_national) / population_national),
                
            death_rate_adjusted=recovery_deaths_adjusted/population_national,
            SE_adjusted = SE_recovery_deaths_adjusted/population_national)
  
  
  
  
  
combined_death_rates_by_age<-
  RECOVERY_HES_death_rates_by_age_groups%>%
  select(Period, agegrp1, deaths, population, death_rate, SE, Cohort)%>%
  rbind(RECOVERY_HES_standardised_death_rates_by_age_groups%>%
          select(Period,
                 deaths=deaths_recovery,
                 population=population_recovery,
                 death_rate=death_rate_adjusted,
                 SE=SE_adjusted,
                 agegrp1)%>%
          mutate(Cohort="RECOVERY HES (age- and sex-adjusted)"))%>%
  rbind(National_HES_death_rates_by_age_groups%>%
          select(Period, deaths, population, death_rate, SE, Cohort, agegrp1))%>%
  mutate(lower_bound_95CI = death_rate-1.96*SE,
         upper_bound_95CI = death_rate+1.96*SE)%>%
  mutate(lower_bound_95CI=if_else(lower_bound_95CI<0, 0, lower_bound_95CI), # temporary bypass code until exact calculations
         upper_bound_95CI = if_else(upper_bound_95CI>1,1,upper_bound_95CI)) # temporary bypass code until exact calculations





### admissions in RECOVERY HES

RECOVERY_HES_timeseries_counts_by_age_groups<-
  RECOVERY_HES_flags%>%
  select(study_number=study_id,
         indexdate,
         agegrp)%>%
  mutate(month=as.Date(paste0(str_sub(indexdate, 1, 8), "01"), format="%Y-%m-%d"))%>%
  mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                           "20-24",
                                           "25-29",
                                           "30-34",
                                           "35-39",
                                           "40-44",
                                           "45-49",
                                           "50-54",
                                           "55-59") ~ "<60",
                             agegrp %in% c("60-64",
                                           "65-69") ~ "60-69",
                             agegrp %in% c("70-74",
                                           "75-79") ~ "70-79",
                             agegrp %in% c("80-84",
                                           "85-89",
                                           "90-94",
                                           "95+") ~ "80+"))%>%
  mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
                            month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
                            month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
                            month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
                            month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
                            month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
                            month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21")))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%
  count(Period, agegrp1)%>%
  mutate(Cohort = "RECOVERY")






### admissions in National HES

National_HES_timeseries_counts_by_age_groups<-
  National_HES_events%>%
  select(yearmonth, agegrp)%>%
  mutate(month=paste0(yearmonth, "01"))%>%
  mutate(month=as.Date(month, format="%Y%m%d", origin="1964-10-22"))%>%
  mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                           "20-24",
                                           "25-29",
                                           "30-34",
                                           "35-39",
                                           "40-44",
                                           "45-49",
                                           "50-54",
                                           "55-59") ~ "<60",
                             agegrp %in% c("60-64",
                                           "65-69") ~ "60-69",
                             agegrp %in% c("70-74",
                                           "75-79") ~ "70-79",
                             agegrp %in% c("80-84",
                                           "85-89",
                                           "90-94",
                                           "95+") ~ "80+"))%>%
  mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar 20 - May 20",
                            month>="2020-06-01" & month <="2020-08-01" ~ "Jun 20 - Aug 20",
                            month>="2020-09-01" & month <="2020-11-01" ~ "Sep 20 - Nov 20",
                            month>="2020-12-01" & month <="2021-02-01" ~ "Dec 20 - Feb 21",
                            month>="2021-03-01" & month <="2021-05-01" ~ "Mar 21 - May 21",
                            month>="2021-06-01" & month <="2021-08-01" ~ "Jun 21 - Aug 21",
                            month>="2021-09-01" & month <="2021-11-01" ~ "Sep 21 - Nov 21"))%>%
  mutate(Period=factor(Period, levels=c("Mar 20 - May 20",
                                        "Jun 20 - Aug 20",
                                        "Sep 20 - Nov 20",
                                        "Dec 20 - Feb 21",
                                        "Mar 21 - May 21",
                                        "Jun 21 - Aug 21",
                                        "Sep 21 - Nov 21")))%>%
  mutate(Period=fct_relevel(Period, "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%
  count(Period, agegrp1)%>%
  mutate(Cohort = "Reference population")


### Plot (supplementary figure 8)-----

### Admissions
p1<-RECOVERY_HES_timeseries_counts_by_age_groups%>%
  rbind(National_HES_timeseries_counts_by_age_groups)%>%
  # group_by(Period, agegrp1)%>%
  # mutate(`Recruitment rate`=round(n[Cohort=="RECOVERY"]/n[Cohort=="Reference population"]*100,1))%>%
  # mutate(`Recruitment rate`=ifelse(Cohort=="RECOVERY", `Recruitment rate`, NA))%>%
  ggplot(aes(Period, n, fill=Cohort, group=Cohort, shape=Cohort))+

  geom_col(data=.%>%filter(Cohort=="Reference population"),
           aes(linetype=Cohort),
           color="black",
           # size=1.2,
           width=0.25
  )+
  geom_col(data=.%>%filter(Cohort=="RECOVERY"),
           aes(linetype=Cohort),
           color="black",
           # size=1.2,
           width=0.25,
           # linetype="dashed"
  )+
  #geom_line()+
  # geom_text(aes(label=n), 
  #           # data=
  #           #   RECOVERY_HES_timeseries_counts_by_age_groups%>%
  #           #   rbind(National_HES_timeseries_counts_by_age_groups)%>%
  #           #   filter(month>"2020-02-01")%>%
  #           #   mutate(Cohort=fct_relevel(Cohort, "RECOVERY HES", "All-England HES"))%>%
  #           #   filter(Cohort=="RECOVERY HES"),
  #           color="black",
  #           size=3
  #           )+
  
  facet_wrap(~agegrp1, ncol=5,
             # scales="free"
             )+
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        text=element_text(size=20))+
  labs(
    # title="Monthly hospital admissions and 28-day mortality along time in the RECOVERY HES and All-England HES cohort (split by age groups)",
       subtitle="Number of individuals included in RECOVERY and the reference population",
       x="Index date (at monthly level)",
       y="Individuals",
       fill="",
       color="",
       linetype="")+
  # scale_x_date(date_labels="%Y %B", 
  #              limits=as.Date(c("2020-03-01", "2021-11-01"), format="%y-%m-%d"),
  #              breaks = seq(as.Date("2020-03-01"), as.Date("2021-11-01"), by="months")
  #              )+
  scale_color_manual(values=colors[c(1,3)])+
  scale_fill_manual(values=colors[c(1,3)])+
  scale_y_log10()+
  scale_linetype_manual(values=c(2,1))


p1


### Mortality

p2<-combined_death_rates_by_age%>%
  mutate(Cohort=case_when(Cohort=="RECOVERY" ~ "RECOVERY (crude)",
                          Cohort=="RECOVERY HES (age- and sex-adjusted)" ~ "RECOVERY (age- and sex-adjusted)",
                          Cohort=="Reference population" ~ "Reference population"))%>%
                                                                                      mutate(Cohort=factor(Cohort,levels=c("RECOVERY (crude)",
                  "RECOVERY (age- and sex-adjusted)",
                  "Reference population")))%>%
  ggplot(aes(Period, death_rate, group=Cohort, color=Cohort, shape=Cohort))+
  geom_point(size=2,
             position=position_dodge(width=0.5)
    # aes(size=population)
    )+
  # geom_line()+
  geom_errorbar(aes(ymin=lower_bound_95CI, 
                    ymax=upper_bound_95CI,
                    # size=Cohort
                    ),
                width=0.1,
                position=position_dodge(width=0.5)
                )+

  facet_wrap(~agegrp1, ncol=5,
             # scales="free_y"
             )+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=30, 
                                   hjust=1,
                                   vjust=1,
                                   size=10),
        text=element_text(size=20))+
  labs(
    #subtitle="28-day mortality",
      # caption=str_wrap("28-day mortality presented as the proportion of people with death recorded within 28 days of their index date along with 95% confidence intervals. Standardisation performed by applying RECOVERY HES 28-day mortality to an age- (5-year bands) and sex-standardised population using All-England HES cohort as reference, in a rolling monthly basis (for 28-day mortality and age and sex breakdown) and then combined in 4 larger age strata",200),
       x="Period",
       y="Mortality (%)")+
  # scale_x_date(date_labels="%Y %B", 
  #              limits=as.Date(c("2020-03-01", "2021-11-01"), format="%y-%m-%d"),
  #              breaks = seq(as.Date("2020-03-01"), as.Date("2021-11-01"), by="months")
  #              )+
  scale_y_continuous(labels = label_number(scale = 1e2))+
  scale_color_manual(values=colors)

### Combined plot
death_rates_plot_by_age_groups<-#p1/
  p2  

death_rates_plot_by_age_groups

ggsave("Outputs/Publication/Figures/supplementary_figure_S8_timeseries_admissions_mortality_per_age_groups.png",
       dpi="retina",
       width=40,
       height=20,
       units="cm")

ggsave("Outputs/Publication/Figures/High-resolution/supplementary_figure_S8_timeseries_admissions_mortality_per_age_groups.tiff",
       dpi="retina",
       width=40,
       height=20,
       units="cm")










### tables -----

#### inclusion timeseries (supplementary table 3) ------

RECOVERY_HES_timeseries_counts_by_age_groups%>%
  rbind(National_HES_timeseries_counts_by_age_groups)%>%
  # filter(month>="2020-03-01")%>%
  group_by(agegrp1)%>%
  summarise(`RECOVERY (individuals)` = sum(n[Cohort=="RECOVERY"]),
            `Reference population (individuals)` = sum(n[Cohort=="Reference population"]),
            `Proportion included in RECOVERY (%)` = round(`RECOVERY (individuals)`/`Reference population (individuals)`*100,1))%>%
  mutate(`Period` = "Overall")%>%
  relocate(`Period`, .before=`RECOVERY (individuals)`)%>%
  
  rbind(
    RECOVERY_HES_timeseries_counts_by_age_groups%>%
      rbind(National_HES_timeseries_counts_by_age_groups)%>%
      # filter(month>="2020-03-01")%>%
      group_by(Period, agegrp1)%>%
      summarise(`RECOVERY (individuals)` = n[Cohort=="RECOVERY"],
                `Reference population (individuals)` = n[Cohort=="Reference population"],
                `Proportion included in RECOVERY (%)` = round(`RECOVERY (individuals)`/`Reference population (individuals)`*100,1))
      # rename(`Index month` = month)%>%
      # mutate(`Index month` = as.character(`Index month`))
  )%>%
  # mutate(`Index month` = as.Date(`Index month`, "%Y-%m-%d"))%>%
  # mutate(`Index month` = strftime(`Index month`, "%Y %B"))%>%
  # mutate(`Index month` = if_else(is.na(`Index month`), "Aggregate", `Index month`))%>%
  group_by(agegrp1)%>%
  mutate(prop1 = round(`RECOVERY (individuals)`/`RECOVERY (individuals)`[`Period`=="Overall"]*100,1),
         prop2 = round(`Reference population (individuals)`/`Reference population (individuals)`[`Period`=="Overall"]*100, 1))%>%
  mutate(`RECOVERY (individuals)` = paste0(`RECOVERY (individuals)`, "\n(", prop1, ")"),
         `Reference population (individuals)` = paste0(`Reference population (individuals)`, "\n(", prop2, ")"),
         `Proportion included in RECOVERY (%)` = paste0(`Proportion included in RECOVERY (%)`, ""))%>%
  rename(`RECOVERY` = `RECOVERY (individuals)`,
         `Reference population` = `Reference population (individuals)`,
         `Proportion of\nreference population included in\nRECOVERY` = `Proportion included in RECOVERY (%)`)%>%
  select(-prop1, -prop2)->counts_for_inclusion_timeseries_table_by_age_groups
  
my_header <- data.frame(
  col_keys = c("Period", 
               "recovery_group1", "england_group1", "proportion_group1",
               "recovery_group2", "england_group2", "proportion_group2",
               "recovery_group3", "england_group3", "proportion_group3",
               "recovery_group4", "england_group4", "proportion_group4"),
               
               
               line1 = c("Period", rep("Age bands", 12)),
  line2 = c("Period", rep("<60", 3), rep("60-69", 3), rep("70-79", 3), rep("80+", 3)),
  line3 = c("Period", 
            "RECOVERY (% of all ages total)", "Reference population (% of all ages total)", "Proportion included in RECOVERY (%)", 
            "RECOVERY (% of all ages total)", "Reference population (% of all ages total)", "Proportion included in RECOVERY (%)", 
            "RECOVERY (% of all ages total)", "Reference population (% of all ages total)", "Proportion included in RECOVERY (%)", 
            "RECOVERY (% of all ages total)", "Reference population (% of all ages total)", "Proportion included in RECOVERY (%)"
  ), 
  stringsAsFactors = FALSE
)

counts_for_inclusion_timeseries_table_by_age_groups%>%
  pivot_longer(-c(agegrp1, `Period`), names_to = "variable", values_to="number")%>%
  pivot_wider(id_cols=c(`Period`), names_from = c(variable, agegrp1), values_from = "number", 
              )%>%colnames()->my_header$col_keys

counts_for_inclusion_timeseries_table_by_age_groups%>%
  pivot_longer(-c(agegrp1, `Period`), names_to = "variable", values_to="number")%>%
  pivot_wider(id_cols=c(`Period`), names_from = c(variable, agegrp1), values_from = "number", 
  )%>%
  flextable(col_keys = my_header$col_keys)%>%
  set_header_df(mapping = my_header, key = "col_keys") %>%
  theme_booktabs() %>% 
  merge_v(part = "header") %>%
  merge_h(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j=1, align = "left", part = "all") %>%
  align(j=1, i=1, align = "center", part = "header") %>%
  colformat_num(na_str = "")%>%
  set_caption("Number of individuals included in RECOVERY HES versus All-England HES over time, split by age bands")%>%
  width(j=NULL, width=2.2, unit = "cm")%>%
  width(j=1, width=2, unit = "cm")%>%
  width(j=c(4,7,10,13), width=2, unit = "cm")%>%
  border(i=1, j=NULL, border.bottom = fp_border(), part="body")%>%
  border(j=c(1, 4,7,10,13), border.right = fp_border(), part="body")%>%
  border(i=3, j=c(1, 4,7,10,13), border.right = fp_border(), part="header")%>%
  border(i=2, j=c(1, 2, 5, 8, 11), border.right = fp_border(), part="header")%>%
  border(j=1, border.left = fp_border(), part="all")%>%
  border(j=1, border.right=fp_border(), part="header")%>%
  # footnote(i=3, 
  #          j=c(2,3,5,6,8,9,11,12),
  #          value = as_paragraph("Individuals (proportion within cohort)"),
  #          ref_symbols =c("1"),
  #          part="header"
  #          
  # )%>%
  merge_v(j=1,target=1, part="header")%>%
  fontsize(i=NULL, j=NULL, size=9, part="all")-> inclusion_timeseries_table_by_age_groups

inclusion_timeseries_table_by_age_groups

sect_properties <- prop_section(page_size = page_size(
  orient = "landscape",
  height = 21/2.54, # conversion for A4 size from cm to inches
  width = 29.7/2.54 # conversion for A4 size from cm to inches
),
type = "continuous",
page_margins = page_mar())


counts_for_inclusion_timeseries_table_by_age_groups%>%
  ungroup()%>%
  select(-`Proportion of
reference population included in
RECOVERY`)%>%
  mutate(RECOVERY = as.integer(map(str_split(RECOVERY, "\n"), 1)),
         `Reference population` = as.integer(map(str_split(`Reference population`, "\n"), 1)))%>%
  group_by(Period)%>%
  summarise(RECOVERY = sum(`RECOVERY`),
            `Reference population` = sum(`Reference population`),
            `Proportion included in RECOVERY (%)` = round(`RECOVERY`/ `Reference population`*100,1))%>%
  mutate(Period=as.factor(Period))%>%
  mutate(Period=fct_relevel(Period, "Overall", "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%
  arrange(Period)%>%
  flextable()->inclusion_timeseries_table_aggregated_counts

inclusion_timeseries_table_aggregated_counts

save_as_docx(inclusion_timeseries_table_by_age_groups, 
             path="Outputs/Tables/RECOVERY_inclusion_timeseries_per_age_bands_table.docx",
             pr_section = sect_properties)


save_as_docx(inclusion_timeseries_table_aggregated_counts, 
             path="Outputs/Tables/RECOVERY_inclusion_timeseries_table_aggregated.docx",
             pr_section = sect_properties)




#### mortality rates by age group (supplementary table 6) -----------

RECOVERY_HES_standardised_death_rates_by_age_groups%>%
  select(Period, 
         agegrp1,
       deaths_recovery, 
       population_recovery,
       # death_rate_recovery,
       # SE_recovery,
       # death_rate_adjusted,
       # SE_adjusted,
       deaths_national,
       population_national,
       # death_rate_national,
       # SE_national
       )%>%
  
  
  # mutate(death_rate_recovery = round(death_rate_recovery*100,1),
  #        SE_recovery = round(SE_recovery*100, 1),
  #        CI_recovery = paste0(round(death_rate_recovery-1.96*SE_recovery,1), " - ", round(death_rate_recovery+1.96*SE_recovery,1)),
  #        
  #        death_rate_adjusted = round(death_rate_adjusted*100,1),
  #        SE_adjusted = round(SE_adjusted*100, 1),
  #        CI_adjusted = paste0(round(death_rate_adjusted-1.96*SE_adjusted,1), " - ", round(death_rate_adjusted+1.96*SE_adjusted,1)),
  #        
  #        death_rate_national = round(death_rate_national*100,1),
  #        SE_national = round(SE_national*100, 1),
  #        CI_national = paste0(round(death_rate_national-1.96*SE_national,1), " - ", round(death_rate_national+1.96*SE_national,1)))%>%
  # 
  # mutate(death_rate_recovery = paste0(death_rate_recovery, "\n(", CI_recovery, ")"),
  #        death_rate_adjusted = paste0(death_rate_adjusted, "\n(", CI_adjusted, ")"),
  #        death_rate_national = paste0(death_rate_national, "\n(", CI_national, ")"))%>%
  
  select(`Age band` = agegrp1,
         Period,
         `Deaths_RECOVERY` = deaths_recovery,
         `Population_RECOVERY` = population_recovery,
         # `28-day mortality (95% CI)_RECOVERY` = death_rate_recovery,
         # `28-day mortality, adjusted (95% CI)` = death_rate_adjusted,
         `Deaths_national` = deaths_national,
         Population_national = population_national,
         # `28-day mortality (95% CI)_national` = death_rate_national
         )->timeseries_death_rates_by_age_table



my_header <- data.frame(
  col_keys = 
    timeseries_death_rates_by_age_table%>%
    mutate(across(everything(), ~as.character(.)))%>%
    pivot_longer(-c(`Age band`, Period), names_to = "variable", values_to="number")%>%
    pivot_wider(id_cols=c(Period), names_from = c(variable, `Age band`), values_from = "number")%>%
    colnames(),
  
  
  line1 = c("Period", rep("Age bands",16)),
  line2 = c("Period", rep("<60", 4), rep("60-69", 4), rep("70-79", 4), rep("80+", 4)),
  line3 = c("Period", rep(c(rep("RECOVERY",2), rep("Reference population", 2)), 4)),
  line4 =   
    
    timeseries_death_rates_by_age_table%>%
    mutate(across(everything(), ~as.character(.)))%>%
    pivot_longer(-c(`Age band`, Period), names_to = "variable", values_to="number")%>%
    pivot_wider(id_cols=c(Period), names_from = c(variable, `Age band`), values_from = "number")%>%
    colnames(), 
  stringsAsFactors = FALSE
)%>%
  mutate(line4 = map(str_split(line4, "_"), 1))%>%
  mutate(across(everything(), ~as.character(.)))


aggregated_mortality_rates_by_age_table<-
  timeseries_death_rates_by_age_table%>%
  mutate(across(everything(), ~as.character(.)))%>%
  pivot_longer(-c(`Age band`, Period), names_to = "variable", values_to="number")%>%
  pivot_wider(id_cols=c(Period), names_from = c(variable, `Age band`), values_from = "number")%>%
  ungroup()%>%
  mutate(across(-Period, ~as.numeric(.)))%>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum)))%>%
  mutate(Period=as.character(Period))%>%
  mutate(Period=replace_na(Period, "Overall"))%>%
  mutate(Period=as.factor(Period))%>%
  mutate(Period=fct_relevel(Period, 
                            "Overall", 
                            "Mar 20 - May 20",
                            "Jun 20 - Aug 20",
                            "Sep 20 - Nov 20",
                            "Dec 20 - Feb 21",
                            "Mar 21 - May 21",
                            "Jun 21 - Aug 21",
                            "Sep 21 - Nov 21"))%>%
  arrange(Period)%>%
  # mutate(`Index month` = as.Date(`Index month`, "%Y-%m-%d"))%>%
  # mutate(`Index month` = strftime(`Index month`, "%Y %B"))%>%
  # mutate(`Index month` = if_else(is.na(`Index month`), "Aggregate", `Index month`))%>%
  flextable(col_keys = my_header$col_keys)%>%
  set_header_df(mapping = my_header, key = "col_keys")%>%
  theme_booktabs() %>% 
  merge_v(part = "header") %>%
  merge_h(part = "header") %>%
  align(align = "center", part = "all") %>%
  set_caption("28-day mortality along time in the RECOVERY HES and All-England HES cohorts (split by age groups)")%>%
  border(i=1, j=1, border.right = fp_border(), part="header")%>%
  border(i=2, j=c(2,4,6,10,14),border.right = fp_border(), part="header")%>%
  border(i=3, j=c(4,8,12,16), border.right = fp_border(), part="header")%>%
  border(i=4,j=c(1,5,9, 13,17), border.right = fp_border(), part="header")%>%
  border(j=c(1,5,9, 13,17), border.right = fp_border(), part="body")%>%
  fontsize(size=6, part="all")%>%
  align(i=NULL, j=1, align="left", part="all")%>%
  width(width = 1, unit="cm")%>%
  width(j=1, width = 2, unit="cm")
  # width(j=NULL, width=2, unit="cm")%>%
  # width(j=c(2,3,6,7,9,10,13,14,16,17,20,21,23,24,27,28), width=2, unit = "cm")%>%
  # width(j=c(4,7,10,13), width=2, unit = "cm")

aggregated_mortality_rates_by_age_table

sect_properties <- prop_section(
  page_size = page_size(
  orient = "landscape",
  height= 21/2.54*2, 
  width = 29.7/2.54*2
  ),
type = "continuous",
page_margins = page_mar())  
  
save_as_docx(aggregated_mortality_rates_by_age_table, 
             path="Outputs/Publication/Tables/supplementary_table_S4_aggregated_mortality_rates_by_age_table.docx",
             pr_section = sect_properties
             )














# 11 Other supplementary analyses ------

## ICD coding ------

HES_data_44%>%
  left_join(baseline_crf_england)%>%
  filter(epistart<=rand_date & epiend>rand_date)%>%
  select(study_number, starts_with("diag"))%>%
  pivot_longer(starts_with("diag"), names_to = "position", values_to = "code")%>%
  filter(str_detect(code, "U07"))%>%
  group_by(position)%>%
  summarise(Participants=n_distinct(study_number),
            Episodes=n())%>%
  # summarise(`Primary position (participants)` = participants[position=="diag_01"],
  #         `Any position (participants)` = sum(participants[position!="diag_01"]))%>%
  mutate(`Diagnostic position` = str_sub(position, 6,7))%>%
  mutate(Proportion=paste0(round(Participants / sum(Participants)*100,1), "%"))%>%
  select(`Diagnostic position`, Participants, Proportion)%>%
  flextable()%>%
  set_caption("Diagnostic position of ICD-10 COVID-19 codes in the RECOVERY HES population (for Hospital Episode Statistics episodes straddling randomisation date)")%>%
  width(j=NULL, width=2)%>%
  align(j=NULL, part="all", align="center")%>%
  add_footer_lines(values="Episode selected if epistart is before or on the date of randomisation and epiend is after randomisation. Individuals may be counted more than once if they had multiple simultaneous HES episodes")%>%
  save_as_docx(path="Outputs/Tables/diagnostic_position_coding_recovery.docx")


## Admission source (admisorc) --------

HES_data_44%>%
  left_join(baseline_crf_england)%>%
  filter(epistart<=rand_date & epiend>rand_date)%>%
  select(study_number, admisorc)%>%
  group_by(admisorc)%>%
  summarise(Participants=n_distinct(study_number))%>%
  mutate(Description = case_when(admisorc==19 ~ "Usual place of residence",
                                 admisorc==29 ~ "Temporary place of residence",
                                 admisorc==39 ~ "Penal establishment",
                                 admisorc==49 ~ "NHS other hospital provider: high security psychiatric accommodation",
                                 admisorc==51 ~ "NHS other hospital provider: ward or A&E department",
                                 admisorc==52 ~ "NHS other hospital provider: ward for maternity patients or neonates",
                                 admisorc==53 ~ "NHS other hospital provider: ward for patients who are mentally ill or have learning disabilities",
                                 admisorc==54 ~ "NHS run care home",
                                 admisorc==65 ~ "Local authority residential accommodation (where care is provided)",
                                 admisorc==66 ~ "Local authority foster care (not in residential accommodation)",
                                 admisorc==79 ~ "Babies born in or on the way to hospital",
                                 admisorc==85 ~ "Non-NHS run care home (other than local authority)",
                                 admisorc==87 ~ "Non-NHS run hospital",
                                 admisorc==98 ~ "Not applicable",
                                 admisorc==99 ~ "Not known"))%>%
  rename(Admisorc=admisorc)%>%
  select(Admisorc, Description, Participants)%>%
  arrange(desc(Participants))%>%
  flextable()%>%
  set_caption("Source of admission for the RECOVERY HES population (for Hospital Episode Statistics episodes straddling randomisation date)")%>%
  width(j=1, width=2, unit = "cm")%>%
  width(j=2, width=10, unit = "cm")%>%
  width(j=3, width=4, unit = "cm")%>%
  align(j=NULL, align="left", part="all")%>%
  add_footer_lines(values="Episode selected if epistart is before or on the date of randomisation and epiend is after randomisation. Individuals may be counted more than once if they had multiple simultaneous HES episodes straddling randomisation")%>%
  save_as_docx(path="Outputs/Tables/admisorc_coding_recovery.docx")

## Discharge destination (disdest) --------

HES_data_44%>%
  left_join(baseline_crf_england)%>%
  filter(epistart<=rand_date & epiend>rand_date)%>%
  select(study_number, disdest)%>%
  group_by(disdest)%>%
  summarise(Participants=n_distinct(study_number))%>%
  mutate(Description = case_when(disdest==19 ~ "Usual place of residence",
                                 disdest==29 ~ "Temporary place of residence",
                                 disdest==30 ~ "Repatriation from high security psychiatric hospital",
                                 
                                 disdest==38 ~ "Penal establishment - police station",
                                 disdest==49 ~ "NHS other hospital provider: high security psychiatric accommodation",
                                 disdest==50 ~ "NHS other hospital provider: medium secure unit",
                                 disdest==51 ~ "NHS other hospital provider: ward or A&E department",
                                 disdest==52 ~ "NHS other hospital provider: ward for maternity patients or neonates",
                                 disdest==53 ~ "NHS other hospital provider: ward for patients who are mentally ill or have learning disabilities",
                                 disdest==54 ~ "NHS run care home",
                                 disdest==65 ~ "Local authority residential accommodation (where care is provided)",
                                 disdest==66 ~ "Local authority foster care (not in residential accommodation)",
                                 disdest==79 ~ "Babies born in or on the way to hospital",
                                 disdest==84 ~ "Non-NHS run hospital: medium secure unit",
                                 disdest==85 ~ "Non-NHS run care home (other than local authority)",
                                 disdest==87 ~ "Non-NHS run hospital",
                                 disdest==88 ~ "Non-NHS run hospice (other than local authority)",
                                 disdest==98 ~ "Not applicable",
                                 disdest==99 ~ "Not known"))%>%
    rename(Disdest=disdest)%>%
    select(Disdest, Description, Participants)%>%
    arrange(desc(Participants))%>%
  flextable()%>%
  set_caption("Discharge destination for the RECOVERY HES population (for Hospital Episode Statistics episodes straddling randomisation date)")%>%
  width(j=1, width=2, unit = "cm")%>%
  width(j=2, width=10, unit = "cm")%>%
  width(j=3, width=4, unit = "cm")%>%
  align(j=NULL, align="left", part="all")%>%
  add_footer_lines(values="Episode selected if epistart is before or on the date of randomisation and epiend is after randomisation. Individuals may be counted more than once if they had multiple simultaneous HES episodes straddling randomisation")%>%
  save_as_docx(path="Outputs/Tables/disdest_coding_recovery.docx")


## IMV/NIV cross coding in HES and CRF (annex IV) ------



#### IMV/NIV OPCS-4 codes for HES ----------

imv_codes <-c("E851", # Invasive ventilation
              "X581") # ECMO

niv_codes <-c("E856", # CPAP
              "E852") # NIV NEC

### CRF data ----
# List of RECOVERY participants on IMV, NIV, oxygen, and NIV/oxygen at baseline

baseline_crf_england%>%
  mutate(niv_oxy = case_when(niv=="Y" | oxy=="Y" ~ "Y", 
                             niv %in% c("N", "U") & oxy %in% c("N", "U") ~ "N",
                             niv=="U" & oxy=="U" ~ "U"))%>%
  
  mutate(resp_status = case_when(imv=="Y" ~ "IMV/ECMO",
                                 imv=="N" & niv_oxy =="Y" ~ "NIV/oxygen",
                                 imv=="N" & niv_oxy=="N" ~ "None",
                                 imv=="U" & niv_oxy=="U" ~ "U"))%>%
  select(study_number, rand_date, crf_resp_status = resp_status, crf_imv = imv, crf_niv = niv, crf_oxy = oxy, crf_niv_oxy = niv_oxy)->crf_resp_status_list





### HES data --------


# participants included in the extract
HES_data73%>%select(study_number, epistart)%>%
  group_by(study_number)%>%
  slice(which.max(epistart))%>%
  filter(study_number%in%baseline_crf_england$study_number)->hes_participants # list of participants included in the hes extract and the latest epistart date available for each of them

hes_participants%>%distinct(study_number)%>%nrow() # 38873 in the HES data

hes_participants%<>%
  filter(study_number %in% baseline_crf_england$study_number) # all of which recruited in England

# restrict CRF data to people with HES data
crf_resp_status_list%<>%
  filter(study_number%in%hes_participants$study_number) 

# restrict CRF data to people for whom HES data is available straddling randomisation

crf_resp_status_list%<>%
  filter(rand_date<=max(hes_participants$epistart)) # 38873


# restrict HES data to people randomised in England within the study period and not withdrawn

HES_data73%<>%
  filter(study_number%in%baseline_crf_england$study_number) 




##### process OPCS data -------
HES_data73%>%
  left_join(baseline_crf_england%>%select(study_number, rand_date))%>%
  select(study_number, epikey, epistart, epiend, starts_with("opertn"))%>%
  pivot_longer(starts_with("opertn"), names_to = "opcs_field_number", values_to = "opcs_field_code")%>%
  mutate(opcs_field_number=str_sub(opcs_field_number, 8,9))%>%
  filter(!is.na(opcs_field_code))->hes_data_opcs_codes

hes_data_opcs_codes%<>%
  left_join(
    HES_data73%>%
      select(epikey, starts_with("opdate"))%>%
      pivot_longer(starts_with("opdate"), names_to = "opcs_field_number", values_to = "opcs_field_date")%>%
      mutate(opcs_field_number=str_sub(opcs_field_number, 8,9))%>%
      filter(!is.na(opcs_field_date)),
    by=c("epikey", "opcs_field_number"))%>%
  filter(!is.na(opcs_field_date))%>%
  left_join(baseline_crf_england%>%select(study_number, rand_date), by="study_number")%>%
  filter(opcs_field_date<=rand_date)%>%
  mutate(days_before_rand = as.integer(difftime(rand_date, opcs_field_date, units = "days")))%>%
  filter(days_before_rand<=30)->HES_data_opcs_codes_only

rm(hes_data_opcs_codes)

###### participant list (IMV) ------

HES_data_opcs_codes_only%>% 
  mutate(imv = if_else(opcs_field_code %in% imv_codes, "IMV", "None"))%>%
  filter(imv=="IMV")%>%
  select(study_number, imv, days_before_rand_imv=days_before_rand)%>%
  distinct(study_number, .keep_all=T)->hes_imv_list

###### participant list (NIV) -----
HES_data_opcs_codes_only%>%
  mutate(niv = if_else(opcs_field_code %in% niv_codes, "NIV", "None"))%>%
  filter(niv=="NIV")%>%
  select(study_number, niv, days_before_rand_niv=days_before_rand)%>%
  distinct(study_number, .keep_all=T)->hes_niv_list

hes_imv_list%>%
  right_join(hes_niv_list)%>%
  mutate(imv = replace_na(imv, "None"))%>%
  select(study_number, hes_niv = niv, hes_imv = imv, days_before_rand_imv, days_before_rand_niv)->hes_resp_status

rm(hes_imv_list, hes_niv_list)


### Compare sources -------
##### 15 days before rand -----

crf_resp_status_list%>%
  left_join(hes_resp_status)%>%
  mutate(hes_imv = if_else(days_before_rand_imv<=15, hes_imv, "None"),
         hes_niv = if_else(days_before_rand_niv<=15, hes_niv, "None"))%>%
  mutate(across(starts_with("hes"), ~replace_na(., "None")))->joint_table

# table printing properties
options(modelsummary_factory_word = 'flextable')

sect_properties <- prop_section(
  page_size = page_size(orient = "portrait",
                        width = 15, 
                        #height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar())


###### IMV -----
datasummary_crosstab(hes_imv ~ crf_resp_status, data=joint_table,
                     output="dataframe",
                     statistic = 1~ 1 +N + Percent("col"),
                     sparse_header = T)%>%
  flextable()%>%
  add_header_row(values = c("HES data","", "Case report form", "Total"), colwidths = c(1,1,3, 1))%>%
  set_header_labels(values=list(hes_imv=""))%>%
  set_caption("IMV coding cross tabulation (HES vs CRF) - 15 days before randomisation")->imv15

save_as_docx(imv15,
             path = "Outputs/Tables/imv_cross_coding_15d.docx")




###### NIV ------
datasummary_crosstab(hes_niv  ~ crf_resp_status, data=joint_table,
                     output="dataframe",
                     statistic = 1~ 1 +N + Percent("col"),
                     sparse_header = T)%>%
  flextable()%>%
  add_header_row(values = c("HES data","", "Case report form", "Total"), colwidths = c(1,1,3, 1))%>%
  set_header_labels(values=list(hes_niv=""))%>%
  set_caption("NIV coding cross tabulation (HES vs CRF) - 15 days before randomisation")->niv15

save_as_docx(niv15,
             path = "Outputs/Tables/niv_cross_coding_15d.docx")


##### 30 days before rand -----

crf_resp_status_list%>%
  left_join(hes_resp_status)%>%
  mutate(across(starts_with("hes"), ~replace_na(., "None")))->joint_table

###### IMV ----
datasummary_crosstab(hes_imv ~ crf_resp_status, data=joint_table,
                     output="dataframe",
                     statistic = 1~ 1 +N + Percent("col"),
                     sparse_header = T)%>%
  flextable()%>%
  add_header_row(values = c("HES data","", "Case report form", "Total"), colwidths = c(1,1,3, 1))%>%
  set_header_labels(values=list(hes_imv=""))%>%
  set_caption("IMV coding cross tabulation (HES vs CRF) - 30 days before randomisation")->imv30

save_as_docx(imv30,
             path = "Outputs/Tables/imv_cross_coding_30d.docx")

###### NIV ------
datasummary_crosstab(hes_niv  ~ crf_resp_status, data=joint_table,
                     output="dataframe",
                     statistic = 1~ 1 +N + Percent("col"),
                     sparse_header = T)%>%
  flextable()%>%
  add_header_row(values = c("HES data","", "Case report form", "Total"), colwidths = c(1,1,3, 1))%>%
  set_header_labels(values=list(hes_niv=""))%>%
  set_caption("NIV coding cross tabulation (HES vs CRF) - 30 days before randomisation")->niv30

save_as_docx(niv30,
             path = "Outputs/Tables/niv_cross_coding_30d.docx")


# methodology summary: RECOVERY participants randomised in England on or before the 30-11-2021 for whom the most recent episode available in the HES data starts on or after randomisation (i.e. HES data covers the period of randomisation). HES records restricted to OPCS codes recorded on or before the day of randomisation (inclusive), and using a lookback period of 15 or 30 days



## charlson and hospital frailty score by age  -----

age_stratified_charlson_frailty<-
  RECOVERY_HES_flags %>%
  select(agebroad,
         charlson_score,
         frailty_score)%>%
  
  mutate(charlson_no_age_60 = ifelse(agebroad == "<60", charlson_score, NA),
         charlson_no_age_60_69 = ifelse(agebroad == "60-69", charlson_score, NA),
         charlson_no_age_70_79 = ifelse(agebroad == "70-79", charlson_score, NA),
         charlson_no_age_80 = ifelse(agebroad == "80-89" | agebroad=="90+", charlson_score, NA))%>%
  
  mutate(frailty_score_60 = ifelse(agebroad == "<60", frailty_score, NA),
         frailty_score_60_69 = ifelse(agebroad == "60-69", frailty_score, NA),
         frailty_score_70_79 = ifelse(agebroad == "70-79", frailty_score, NA),
         frailty_score_80 = ifelse(agebroad == "80-89" | agebroad=="90+", frailty_score, NA))%>%
  mutate(agebroad=if_else(agebroad %in% c("80-89", "90+"), "80+", agebroad))%>%
  
  mutate(cohort= "RECOVERY")%>%
  rename(`Age groups`=agebroad)%>%
  
  rbind(
    National_HES_flags_unscrambled%>%
      select(agegrp,
             charlson_score,
             frailty_score)%>%
      
      mutate(charlson_score=as.numeric(charlson_score),
             frailty_score=as.numeric(frailty_score))%>%
      
      mutate(`Age groups` = case_when(agegrp %in% c("15-19",
                                                    "20-24",
                                                    "25-29",
                                                    "30-34",
                                                    "35-39",
                                                    "40-44",
                                                    "45-49",
                                                    "50-54",
                                                    "55-59") ~ "<60",
                                      agegrp %in% c("60-64",
                                                    "65-69") ~ "60-69",
                                      agegrp %in% c("70-74",
                                                    "75-79") ~ "70-79",
                                      agegrp %in% c("80-84",
                                                    "85-89",
                                                    "90-94",
                                                    "95+") ~ "80+"))%>%
      
      mutate(charlson_no_age_60 = ifelse(`Age groups` == "<60", charlson_score, NA),
             charlson_no_age_60_69 = ifelse(`Age groups` == "60-69", charlson_score, NA),
             charlson_no_age_70_79 = ifelse(`Age groups` == "70-79", charlson_score, NA),
             charlson_no_age_80 = ifelse(`Age groups` == "80+" , charlson_score, NA))%>%
      mutate(frailty_score_60 = ifelse(`Age groups` == "<60", frailty_score, NA),
             frailty_score_60_69 = ifelse(`Age groups` == "60-69", frailty_score, NA),
             frailty_score_70_79 = ifelse(`Age groups` == "70-79", frailty_score, NA),
             frailty_score_80 = ifelse(`Age groups` == "80+" , frailty_score, NA))%>%
      mutate(cohort= "Reference population")%>%
      select(-agegrp)
  )%>%
  mutate(across(-c(cohort, `Age groups`), ~as.numeric(.)),
         across(starts_with("charlson"), ~as.integer(.)))


age_stratified_charlson_frailty%>%
  select(charlson_score,
         charlson_no_age_60,
         charlson_no_age_60_69,
         charlson_no_age_70_79,
         charlson_no_age_80,
         frailty_score,
         frailty_score_60,
         frailty_score_60_69,
         frailty_score_70_79,
         frailty_score_80,
         cohort)%>%
  mutate(cohort=fct_relevel(cohort, 
                            "RECOVERY",
                            "Reference population"))%>%
  tbl_summary(by="cohort",
              missing = "no",
              label = list(charlson_score ~ "All ages",
                           charlson_no_age_60 ~ "<60",
                           charlson_no_age_60_69 ~ "60-69",
                           charlson_no_age_70_79 ~ "70-79",
                           charlson_no_age_80 ~ "80+",
                           frailty_score ~ "All ages",
                           frailty_score_60 ~ "<60",
                           frailty_score_60_69 ~ "60-69",
                           frailty_score_70_79 ~ "70-79",
                           frailty_score_80 ~ "80+"),
              statistic = all_continuous() ~ "{median} ({p25}, {p75})")%>%
  modify_table_body(
    ~.x %>%
      rbind(tibble(variable="Charlson score (excluding age)",
                   var_label="Charlson score (excluding age)",
                   var_type=NA,
                   row_type="label",
                   label="Charlson score (excluding age)",
                   stat_1 = NA,
                   stat_2 = NA),
            tibble(variable="Hospital frailty score",
                   var_label="Hospital frailty score",
                   row_type="label",
                   label="Hospital frailty score",
                   var_type=NA,
                   stat_1 = NA,
                   stat_2 = NA))%>%
      arrange(factor(variable, levels=c("Charlson score (excluding age)",
                                        "charlson_score",
                                        "charlson_no_age_60",
                                        "charlson_no_age_60_69",
                                        "charlson_no_age_70_79",
                                        "charlson_no_age_80",
                                        "Hospital frailty score",
                                        "frailty_score",
                                        "frailty_score_60",
                                        "frailty_score_60_69",
                                        "frailty_score_70_79",
                                        "frailty_score_80")
      )))%>%
  
  modify_column_indent(columns=label, rows=(!variable%in%c("Charlson score (excluding age)",
                                                           "Hospital frailty score")))%>%
  as_flex_table()%>%
  set_caption("Charlson score (excluding age) and hospital frailty score, overall and age-stratified")->age_stratified_charlson_frailty_table

age_stratified_charlson_frailty_table

save_as_docx(age_stratified_charlson_frailty_table,
             path="Outputs/Tables/age_stratified_charlson_frailty_table.docx",
             pr_section = sect_properties)



### plot (supplementary figure 4) ------

# Charlson score
p1<-age_stratified_charlson_frailty%>%
  select(`Age groups`,
         cohort,
         `Charlson score (excluding age)` = charlson_score)%>%
  mutate(cohort=fct_relevel(cohort, 
                            "RECOVERY",
                            "Reference population"))%>%
  group_by(`Age groups`, cohort, `Charlson score (excluding age)`)%>%
  summarise(n=length(cohort))%>%
  mutate(`Proportion of cohort (%)`=round(n/sum(n)*100,1))%>%

  
  ggplot(aes(`Charlson score (excluding age)`, 
             `Proportion of cohort (%)`, 
             fill=cohort))+
  geom_bar(stat="identity",
           position="dodge",
           color="black")+
  # geom_text(data=age_stratified_charlson_frailty%>%
  #             filter(cohort=="RECOVERY HES")%>%
  #             select(cohort,Charlson_no_age, `Age groups`)%>%
  #             group_by(cohort,`Age groups`)%>%
  #             summarise(median=median(Charlson_no_age, na.rm=T),
  #                       Q1 = quantile(Charlson_no_age, 0.25, na.rm=T),
  #                       Q3 = quantile(Charlson_no_age, 0.75, na.rm=T))%>%
  #             mutate(IQR = paste0("(", Q1, " - ", Q3, ")"))%>%
  #             ungroup(),
  #           aes(label=paste0(cohort,
  #                            ":\n",
#                            median,
#                            " ",
#                            IQR),
#               y=55,
#               x=14,
#               group=`Age groups`),
#           #color="#F8766D",
#           color="black",
#           inherit.aes = F,
#           size=7)+
# geom_text(data=age_stratified_charlson_frailty%>%
#             filter(cohort=="All-England HES")%>%
#             select(cohort,Charlson_no_age, `Age groups`)%>%
#             group_by(cohort,`Age groups`)%>%
#             summarise(median=median(Charlson_no_age, na.rm=T),
#                       Q1 = quantile(Charlson_no_age, 0.25, na.rm=T),
#                       Q3 = quantile(Charlson_no_age, 0.75, na.rm=T))%>%
#             mutate(IQR = paste0("(", Q1, " - ", Q3, ")"))%>%
#             ungroup(),
#           aes(label=paste0(cohort,
#                            ":\n",
#                            median,
#                            " ",
#                            IQR),
#               y=40,
#               x=14,
#               group=`Age groups`),
#           # color="#00BFC4",
#           color="black",
#           inherit.aes = F,
#           size=7)+
geom_vline(data=age_stratified_charlson_frailty%>%
             select(cohort,charlson_score, `Age groups`)%>%
             group_by(cohort,`Age groups`)%>%
             summarise(median=median(charlson_score, na.rm=T)),
           aes(xintercept=median,
               color=cohort),
           linetype="dashed",
           size=1.5
)+
  # annotate(geom="text",
  #          x=14,
  #          y=67,
  #          label="Median (IQR):",
  #          size=7)+
  facet_wrap(~`Age groups`,
             ncol=5,
             # scales = "free",
             # strip.position = "left"
  )+
  theme(legend.position="none",
        text=element_text(size=25),
        axis.title.x=element_blank())+
  labs(
    # title="Distribution of Charlson score (excluding age) and Hospital Frailty score, stratified by age groups",
    subtitle="Charlson score (excluding age)")+
  scale_color_manual(values=colors[c(1,3)])+
  scale_fill_manual(values=colors[c(1,3)])


# Frailty score
p2<-age_stratified_charlson_frailty%>%
  select(`Age groups`,
         cohort,
         `Hospital frailty score` = frailty_score)%>%
  mutate(cohort=fct_relevel(cohort, 
                            "RECOVERY",
                            "Reference population"))%>%
  ggplot(aes(`Hospital frailty score`, 
             fill=cohort))+
  geom_density(
    alpha=0.5,
    
  )+
  geom_vline(data=age_stratified_charlson_frailty%>%
               select(cohort,`Hospital frailty score` = frailty_score, `Age groups`)%>%
               group_by(cohort,`Age groups`)%>%
               summarise(median=median(`Hospital frailty score`, na.rm=T))%>%
               ungroup(),
             aes(xintercept=median,
                 color=cohort),
             linetype="dashed",
             size=1.5
  )+
  
  facet_wrap(~`Age groups`,
             ncol=5,
             
  )+
  
  theme(legend.position="bottom",
        text=element_text(size=25))+
  labs(fill=NULL,
       color=NULL,
       subtitle="Hospital frailty score",
       y="Density",
       x="Score")+
  scale_x_continuous(limits=c(0, 50))+
  scale_color_manual(values=colors[c(1,3)])+
  scale_fill_manual(values=colors[c(1,3)])




p1/p2

ggsave("Outputs/Figures/charlson_frailty_distributions_age_stratified.png",
       dpi="retina",
       height=40,
       width=60,
       units = "cm")


### accompanying table -----

age_stratified_charlson_frailty%>%
  select(cohort,
         `Hospital frailty score` = frailty_score,
         `Age groups`)%>%
  group_by(cohort,`Age groups`)%>%
  summarise(median=round(median(`Hospital frailty score`, na.rm=T),0),
            Q1 = round(quantile(`Hospital frailty score`, 0.25, na.rm=T),0),
            Q3 = round(quantile(`Hospital frailty score`, 0.75, na.rm=T),0),
            Individuals=n())%>%
  ungroup()%>%
  mutate(IQR = paste0("(", Q1, " - ", Q3, ")"),)%>%
  relocate(Individuals, .after=IQR)%>%
  group_by(cohort)%>%
  mutate(Proportion=round(Individuals/sum(Individuals)*100,1))%>%
  mutate(`Hospital frailty score` = paste0(median, " ", IQR))%>%
  select(Cohort=cohort, `Age groups`, Individuals, Proportion, `Hospital frailty score`)%>%
  ungroup()%>%
  left_join(
    
    age_stratified_charlson_frailty%>%
      select(cohort,
             Charlson = charlson_score,
             `Age groups`)%>%
      group_by(cohort,`Age groups`)%>%
      summarise(median=median(Charlson, na.rm=T),
                Q1 = quantile(Charlson, 0.25, na.rm=T),
                Q3 = quantile(Charlson, 0.75, na.rm=T))%>%
      ungroup()%>%
      mutate(IQR = paste0("(", Q1, " - ", Q3, ")"))%>%
      mutate(`Charlson score (excluding age)` = paste0(median, " ", IQR))%>%
      select(Cohort=cohort, `Age groups`, `Charlson score (excluding age)`)
  )%>%
  rename(`Proportion of cohort (%)`= Proportion)%>%
  mutate(Cohort=fct_relevel(Cohort, "RECOVERY", "Reference population"))%>%
  arrange(Cohort)%>%
  flextable()%>%
  merge_v(j=1)%>%
  width(j=c(5,6), width=3, unit="cm")%>%
  border(i=4, border.bottom = fp_border(), part="body")%>%
  border(j=1, border.bottom = fp_border(), part="body")->charlson_frailty_age_bands_table

charlson_frailty_age_bands_table

sect_properties <- prop_section(page_size = page_size(
  orient = "landscape",
  width = 29.7/2.54,
  height=20/2.54
),
type = "continuous",
page_margins = page_mar())

save_as_docx(charlson_frailty_age_bands_table,
             path="K:/QNAP/RECOVERY-deidentified/Generalizability and representativeness project/RECOVERY_generalizability_representativeness/Outputs/Tables/charlson_frailty_age_bands_table.docx",
             pr_section = sect_properties)












## comorbidities split by age ------

Table1_data_combined%>%
  select(Cohort,
         `Age groups`,
         `Metastatic cancer`,
         `Congestive heart failure`,
         `Cerebrovascular disease`,
         `Chronic pulmonary disease`,
         Dementia,
         `Chronic kidney disease`
  )%>%
  filter(Cohort=="RECOVERY")%>%
  
  rbind(
    National_HES_flags_unscrambled%>%
      select(agegrp, 
             `Metastatic cancer` = metastasis, 
             `Congestive heart failure` = congestive_hf, 
             `Cerebrovascular disease`= cerebrovascular, 
             `Chronic pulmonary disease`=chronic_pulmonary, 
             Dementia=dementia, 
             `Chronic kidney disease`=renal)%>%
      mutate(`Age groups` = case_when(agegrp %in% c("15-19",
                                                    "20-24",
                                                    "25-29",
                                                    "30-34",
                                                    "35-39",
                                                    "40-44",
                                                    "45-49",
                                                    "50-54",
                                                    "55-59") ~ "<60",
                                      agegrp %in% c("60-64",
                                                    "65-69") ~ "60-69",
                                      agegrp %in% c("70-74",
                                                    "75-79") ~ "70-79",
                                      agegrp %in% c("80-84",
                                                    "85-89") ~ "80-89",
                                      agegrp %in% c("90-94",
                                                    "95+") ~ "90+"))%>%
      select(-agegrp)%>%
      mutate(Cohort = "Reference population")%>%
      mutate(across(-c(`Age groups`, Cohort), ~if_else(.==1, "Yes", "No")))
  )%>%
  mutate(Cohort=fct_relevel(Cohort, "RECOVERY","Reference population"))%>%
  ungroup()%>%
  rowid_to_column("ID")%>%
  pivot_longer(cols=-c(ID, `Age groups`, Cohort), names_to = "Condition", values_to = "flag")%>%
  group_by(`Age groups`, Cohort, Condition)%>%
  summarise(n=n_distinct(ID[flag=="Yes"]),
            Total=n_distinct(ID),
            Proportion=n/Total*100)->transformed_data_comorbidities_by_age


##### plot (supplementary figure 6) ------

transformed_data_comorbidities_by_age%>%
  ggplot(aes(`Age groups`, Proportion, fill=Cohort))+
  geom_bar(stat='identity', position = position_dodge())+
  facet_wrap(~Condition, 
             # scales = "free_y"
             labeller = label_wrap_gen(width = 30))+
  geom_text(aes(label=round(Proportion, 1),
                color=Cohort),
            position = position_dodge(width=0.9),
            # color="black",
            vjust=-1,
            size=5)+
  theme_gray(base_size=20)+
  theme(
    #text=element_text(family="Mulish"),
        legend.position="bottom")+
  scale_color_manual(values=colors[c(1,3)])+
  scale_fill_manual(values=colors[c(1,3)])+
  scale_y_continuous(expand = expansion(c(0,0.3)))+
  labs(y="Proportion within cohort and age group (%)")

ggsave("Outputs/Publication/Figures/supplementary_figure_S6_comorbidities_split_age.png",
       dpi="retina",
       width=50,
       height=30,
       units = "cm")

ggsave("Outputs/Publication/Figures/High-resolution/supplementary_figure_S6_comorbidities_split_age.tiff",
       dpi="retina",
       width=50,
       height=30,
       units = "cm")

## death date by age and sex (aggregate) -------



death_rates_stratified_age_sex<-
  RECOVERY_HES_flags%>%
  select(study_id,indexdate, age, sex, death28)%>%
  mutate(agegr2=as.factor(case_when(age<60 ~ "<60",
                                    between(age, 60, 69) ~ "60-69",
                                    between(age, 70, 79) ~ "70-79",
                                    age>=80 ~ "80+")))%>%
  mutate(index_month = as.Date(paste0(str_sub(indexdate, 1, 8), "01")), format="%Y-%m-%d")%>%
  group_by(agegr2, sex)%>%
  summarise(Deaths=n_distinct(study_id[death28==1]),
            People = n_distinct(study_id))%>%
  mutate(Cohort="RECOVERY HES")%>%
  ungroup()%>%

  rbind(
    National_HES_events%>%
      select(agegrp,
             sex,
             death28)%>%
      mutate(agegr2 = case_when(agegrp %in% c("15-19",
                                               "20-24",
                                               "25-29",
                                               "30-34",
                                               "35-39",
                                               "40-44",
                                               "45-49",
                                               "50-54",
                                               "55-59") ~ "<60",
                                 agegrp %in% c("60-64",
                                               "65-69") ~ "60-69",
                                 agegrp %in% c("70-74",
                                               "75-79") ~ "70-79",
                                 agegrp %in% c("80-84",
                                               "85-89",
                                               "90-94",
                                               "95+") ~ "80+"))%>%
      group_by(sex, agegr2)%>%
      summarise(Deaths=length(death28[death28==1]),
             People=n())%>%
      mutate(Cohort = "All-England HES")%>%
      ungroup()

  )%>%
  group_by(Cohort,agegr2, sex)%>%
  mutate(Mortality=Deaths/People,
         SE=sqrt(Mortality * (1-Mortality) / People))%>%
  ungroup()%>%

  rbind(
      # take HES data
      RECOVERY_HES_flags%>%

      # preliminary data processing
      select(study_number=study_id,
             sex,
             death28)%>%
      left_join(baseline_crf_england%>%select(study_number, age), by="study_number")%>%
      mutate(agegrp = case_when(age<20 ~ "15-19",
                                between(age, 20, 24) ~ "20-24",
                                between(age, 25, 29) ~ "25-29",
                                between(age, 30, 34) ~ "30-34",
                                between(age, 35, 39) ~ "35-39",
                                between(age, 40, 44) ~ "40-44",
                                between(age, 45, 49) ~ "45-49",
                                between(age, 50, 54) ~ "50-54",
                                between(age, 55, 59) ~ "55-59",
                                between(age, 60, 64) ~ "60-64",
                                between(age, 65, 69) ~ "65-69",
                                between(age, 70, 74) ~ "70-74",
                                between(age, 75, 79) ~ "75-79",
                                between(age, 80, 84) ~ "80-84",
                                between(age, 85, 89) ~ "85-89",
                                between(age, 90, 94) ~ "90-94",
                                age>=95 ~ "95+"))%>%

      # calculate number of people and deaths in each stratum
      group_by(agegrp, sex)%>%
      summarise(recovery_population=n_distinct(study_number),
                recovery_deaths = n_distinct(study_number[death28=="1"]))%>%
      ungroup()%>%
      mutate(recovery_death_rate = recovery_deaths/recovery_population)%>%

      # calculate and join number of people and deaths in each stratum in the national population
      left_join(
        National_HES_events%>%
          group_by(agegrp, sex)%>%
          summarise(National_population = n(),
                    National_deaths=length(death28[death28=="1"]),
                    National_death_rate = round(National_deaths/National_population*100,1)),

        by=c("agegrp", "sex"))%>%

      # calculate adjusted number of deaths if the recovery population was structured as the national
      mutate(recovery_adjusted_deaths_expected = recovery_death_rate*National_population)%>%
      mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                               "20-24",
                                               "25-29",
                                               "30-34",
                                               "35-39",
                                               "40-44",
                                               "45-49",
                                               "50-54",
                                               "55-59") ~ "<60",
                                 agegrp %in% c("60-64",
                                               "65-69") ~ "60-69",
                                 agegrp %in% c("70-74",
                                               "75-79") ~ "70-79",
                                 agegrp %in% c("80-84",
                                               "85-89",
                                               "90-94",
                                               "95+") ~ "80+"))%>%
      # calculate adjusted 28-day mortality (adjusted deaths in recovery over national population)
      group_by(agegrp1, sex)%>%

      summarise(population_recovery=sum(recovery_population),
                deaths_recovery=sum(recovery_deaths),
                population_national=sum(National_population),
                deaths_national = sum(National_deaths),

                recovery_deaths_adjusted = sum(recovery_deaths/recovery_population*National_population),
                SE_recovery_deaths_adjusted = sqrt(sum((recovery_deaths/recovery_population^2) * National_population^2)),

                death_rate_recovery=deaths_recovery/population_recovery,
                SE_recovery = sqrt(death_rate_recovery * (1-death_rate_recovery) / population_recovery),

                death_rate_national = deaths_national/population_national,
                SE_national = sqrt(death_rate_national * (1-death_rate_national) / population_national),

                death_rate_adjusted=recovery_deaths_adjusted/population_national,
                SE_adjusted = SE_recovery_deaths_adjusted/population_national)%>%
  select(sex,
         agegr2=agegrp1,
         Deaths=recovery_deaths_adjusted,
         People=population_national,
         Mortality=death_rate_adjusted,
         SE = SE_adjusted)%>%
  mutate(Cohort= "RECOVERY HES (age- and sex-adjusted)")
  )%>%

  mutate(sex=as.factor(case_when(sex==2 ~ "Female",
                                 sex==1 ~ "Male")))%>%
  filter(!is.na(sex))%>%

  mutate(Cohort=case_when(Cohort == "RECOVERY HES" ~ "RECOVERY (crude)",
                          Cohort == "RECOVERY HES (age- and sex-adjusted)" ~ "RECOVERY (adjusted)",
                          Cohort == "All-England HES" ~ "Reference population"))%>%
  mutate(Cohort=fct_relevel(Cohort, "RECOVERY (crude)", "RECOVERY (adjusted)", "Reference population"))

#### plot (supplementary figure 7)  -----

death_rates_stratified_age_sex%>%
  filter(Cohort!="RECOVERY (adjusted)")%>%
  mutate(Cohort = if_else(Cohort=="RECOVERY (crude)", "RECOVERY", "Reference population"))%>%

  ggplot(aes(Cohort, Mortality, color=Cohort))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=Mortality-1.96*SE,
                    ymax=Mortality+1.96*SE),
                width=0.1)+
  geom_text(data=death_rates_stratified_age_sex%>%
              filter(Cohort!="RECOVERY (adjusted)")%>%
              mutate(Cohort = if_else(Cohort=="RECOVERY (crude)", "RECOVERY", "Reference population"))%>%
              group_by(agegr2, sex, Cohort)%>%
              mutate(label=paste0(round(Mortality*100,1),
                                  "%\n(",
                                  round((Mortality-1.96*SE)*100,1),
                                  " - ",
                                  round((Mortality+1.96*SE)*100,1),
                                  ")"))%>%
              select(agegr2, sex, Cohort, Mortality, label),
            aes(
              # Cohort,
              # Mortality,
                label=label),
                color="black",
            inherit.aes = T,
            vjust=-1,
            size=10/2.54
            )+
  facet_grid(cols=vars(agegr2),
             rows=vars(sex),
             # nrow=2, ncol=5,
             switch = "y"
             )+
  scale_y_continuous(
    labels = label_number(scale = 1e2),
    limits=c(0, 0.7))+
  theme(panel.grid.minor=element_blank(),
        text=element_text(size=20),
        # strip.text=element_text(size=15),
        legend.position="bottom",
        # strip.background = element_blank(),
        strip.placement = "outside",
        # axis.text.x=element_text(angle=30,
        #                          vjust=1,
        #                          hjust=1),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        plot.margin = margin(
          # t = 20,  # Top margin
          r = 10,  # Right margin
          # b = 40,  # Bottom margin
          l = 40)  # Left margin
  )+
  labs(
    # title="28-day mortality in the RECOVERY HES and All-England HES populations",
      #  subtitle="Stratified by age and sex",
       y="Mortality (%)",
       color="Cohort",
       x="Cohort",
       # caption="Error bars show 95% CIs"
       )+
  scale_color_manual(values=colors[c(1,3)])


ggsave("Outputs/Publication/Figures/supplementary_figure_S7_death_stratified_age_sex.png",
       dpi="retina",
       width=45,
       height=30,
       units="cm")

ggsave("Outputs/Publication/Figures/High-resolution/supplementary_figure_S7_death_stratified_age_sex.tiff",
       dpi="retina",
       width=45,
       height=30,
       units="cm")


## investigate individuals with very early index dates -----

index_dates%>%
  filter(index_date<"2020-03-01")%>%
  View() # 27 before March 2020

National_HES_flags%>%
  select(indexdate)%>%
  arrange(indexdate)%>%
  View() # earliest are 2020-03-01


RECOVERY_HES_flags%>%
  select(study_id, indexdate)%>%
  filter(indexdate<"2020-03-01")->early_indexdate_ids # take index_dates before march 2020 and respective ids

HES_data_44%>% # this is crude HES data, not flagged data from Raph
  filter(study_number %in% early_indexdate_ids$study_id)%>%
  left_join(early_indexdate_ids, by=c("study_number"="study_id"))%>%
  left_join(baseline_crf_england%>%select(study_number, rand_date))%>%
  filter(
    # epistart==indexdate, # restrict records to stuff that happended on or before index date
         # epiend>=indexdate
         # if_any(starts_with("diag"), ~str_detect(., pattern="U071|U072"))
         )%>%
  select(study_number, rand_date, indexdate, epistart, epiend, admidate, disdate, starts_with("diag"))%>%
  ungroup()%>%
  arrange(study_number, indexdate)%>%
  mutate(study_number=as.character(study_number))%>%
  View()
  # n = 27, from Feb 2019 to Feb 2020


## investigate missing HES data ------------


baseline_crf_england%>%
  filter(!study_number %in% RECOVERY_HES_flags$study_id)%>%
  select(study_number)%>%
  .[[1]]->missing_hes_participants_list

# Crude HES data
HES_data79%>%
  filter(study_number%in%missing_hes_participants_list)%>%
  left_join(baseline_crf_england%>%select(study_number, rand_date))%>%
  # filter(epistart>="2020-01-01")%>%
  relocate(rand_date, .after=study_number)%>%
  distinct(study_number)%>%
  nrow() # 363 of them do have HES data (but no ICD-10 codes for COVID so no index date and therefore not in HES flags)

HES_data79%>%
  filter(study_number%in%missing_hes_participants_list)%>%
  left_join(baseline_crf_england%>%select(study_number, rand_date))%>%
  filter(epistart>="2020-01-01")%>%
  relocate(rand_date, .after=study_number)%>%
  # filter(between(rand_date, "2021-07-01", "2021-08-01"))%>%
  # distinct(study_number)%>%
  View() 






HES_data79%>%
  filter(study_number%in%missing_hes_participants_list)%>%
  left_join(baseline_crf_england%>%select(study_number, rand_date))%>%
  filter(epistart>="2020-01-01")%>%
  relocate(rand_date, .after=study_number)%>%
  filter(diag_01=="U071")%>%
  View()# 2 people with records on HES crude data that should have been included (U071)

# plot randomisation dates for people not in the crude HES data

HES_data79%>%
  distinct(study_number)%>%
  .[[1]]->HES79_participants

baseline_crf_england%>%
  mutate(hes_flag = factor(if_else(study_number %in% HES79_participants, "HES data available", "HES data unavailable")))%>%
  mutate(month=as.Date(paste0(str_sub(rand_date, 1, 8), "01")))%>%
  group_by(month, hes_flag)%>%
  summarise(Participants=n_distinct(study_number))%>%
  ggplot(aes(month, Participants, color=hes_flag))+
  geom_point()+
  geom_line()+
  labs(title="Date of randomisation for RECOVERY participants randomised in England, split by HES linkage status",
       subtitle="HES extract 79",
       color="HES linkage status",
       x="Month of randomisation")+
  theme(legend.position="bottom",
        text=element_text(size=20),
        axis.text.x=element_text(angle=30,
                                 hjust=1,
                                 vjust=1))+
  scale_x_date(breaks="1 month",
               date_labels = "%Y %b")


##### cross check with missing NHS numbers list -------
  
  # HES extract being used is 73; we have lists of mismatched NHS numbers for extracts 71 (n=318) and 75 (n=76)
  
  mismatched_nhs_numbers_extract_71 <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/NHS number status/DQ_PID_Issues_Reasons_By_Rever_2022-02-23_23-42-26.csv")%>%
    filter(field_name=="nhs_number",
           issue %in% c("invalid nhs number"))%>%
    select(study_number)%>%
    .[[1]]
  
  
  mismatched_nhs_numbers_extract_75 <- read_csv("K:/QNAP/RECOVERY-deidentified/Datasets/NHS number status/DQ_PID_Issues_Reasons_By_Rever_2022-05-30_08-45-33.csv")%>%
    filter(field_name=="nhs_number",
           issue %in% c("invalid nhs number"))%>%
    select(study_number)%>%
    .[[1]]
  
  intersect(mismatched_nhs_numbers_extract_71, hes_participants)%>%length() 
  # 163 people had NHS numbers missing in extract 71 but were included in HES extract 73, so we cannot take extract 71 as a definitive list of people with missing NHS numbers
  
  RECOVERY_HES_flags%>%
    filter(study_id %in% intersect(mismatched_nhs_numbers_extract_75, hes_participants))%>%
    View() 
  # 17 people with supposedly missing NHS numbers in extract 75 were included in HES linkage for extract 73
  
  
  # unclear what the reason is for this, but regardless it shows that, of those people with no HES data, only a minority had issues with NHS numbers 
  
  
##### check randomisation dates ------ 
  
  
  randomisation_non_linked_plot<-baseline_crf_england%>%
    filter(study_number%in%analysis_population)%>%
    mutate(hes_flag = factor(if_else(study_number %in% hes_participants, "HES data available", "HES data unavailable")))%>%
    # filter(hes_flag=="HES data unavailable")%>%
    select(study_number,rand_date,hes_flag)%>%
    mutate(rand_month = paste0(str_sub(rand_date, 1, 8), "01"))%>%
    group_by(hes_flag,rand_month)%>%
    summarise(Participants = n_distinct(study_number))%>%
    
    ggplot(aes(rand_month, Participants, group=hes_flag, color=hes_flag))+
    geom_point()+
    geom_line()+
    theme(axis.text.x=element_text(angle=90))
  
  
  randomisation_non_linked_plot


  
  
  # check date of randomisation for people with missing NHS#
  baseline_crf%>%
    filter(study_number%in%mismatched_nhs_numbers_extract_75)%>%
    select(study_number,rand_date)%>%
    mutate(month=as.Date(paste0(str_sub(rand_date, 1, 8), "01")))%>%
    group_by(month)%>%
    summarise(n=n_distinct(study_number))%>%
    
    ggplot(aes(month, n))+
    geom_point()+
    geom_line()+
    labs(title="Date of randomisation for people with missing NHS# in extract 75, at monthly level")+
    scale_x_date(date_breaks = "1 month")+
    theme(axis.text.x=element_text(angle=90),
          panel.grid.minor=element_blank(),
          text=element_text(size=20))
  
  
  
##### check GPES/BSA data for people randomised in summer 2021-------
  
baseline_crf_england%>%
    distinct(study_number, rand_date)%>%
    mutate(GPES=if_else(study_number %in% gpes_participants, 1, 0),
           NHSBSA = if_else(study_number %in% dispensing_participants, 1, 0),
           HES = if_else(!study_number %in% missing_hes_participants_list, 1, 0))%>%
    pivot_longer(c(GPES, NHSBSA, HES), names_to="dataset", values_to="flag")%>%
    mutate(month=as.Date(paste0(str_sub(rand_date, 1, 8), "01")))%>%
    group_by(month, dataset)%>%
    summarise(`Missing participants`=n_distinct(study_number[flag==0]),
              total=n_distinct(study_number),
              `Proportion of monthly total`=`Missing participants`/total*100)%>%
    select(-total)%>%
    pivot_longer(c(`Missing participants`, `Proportion of monthly total`), names_to="variable", values_to="value")%>%
    
    ggplot(aes(month, value))+
    geom_point()+
    geom_line()+
    facet_grid(variable~dataset,
               switch = "y",
               scales = "free")+
    labs(y=NULL,
         x="Randomisation month",
         title="Number of RECOVERY participants with missing linkage data in England, based on the month of randomisation",
         subtitle="Missing data derived based on extract 73 for each dataset (May 2022)")+
    scale_x_date(date_breaks = "1 month")+
    theme(axis.text.x=element_text(angle=90),
          panel.grid.minor=element_blank(),
          text=element_text(size=20))
           


##### check people with missing HES data by site ------  

baseline_crf_england%>%
  left_join(sites)%>%
  filter(study_number%in%analysis_population)%>%
  select(study_number, SiteName, rand_date)%>%
  mutate(hes_data_available = factor(if_else(study_number %in% hes_participants, "Y", "N")))%>%
  mutate(rand_month = paste0(str_sub(rand_date, 1, 8), "01"))%>%
  group_by(rand_month, SiteName)%>%
  summarise(Participants = n_distinct(study_number),
            Missing_HES_participants=n_distinct(study_number[hes_data_available=="N"]))%>%
  mutate(Proportion_missing=round(Missing_HES_participants/Participants*100,1))%>%
  
    ggplot(aes(rand_month, Missing_HES_participants, group=SiteName, color=SiteName))+
    geom_point()+
    geom_line()+
    theme(axis.text.x=element_text(angle=90),
          legend.position="bottom")
  
  
##### extract list of people with missing HES in July/August 2021 ------
 
  
setdiff(analysis_population, hes_participants)->missing_hes_participants
  
baseline_crf%>%
  filter(study_number %in% missing_hes_participants)%>%
  select(study_number, birth_date=brthdtc,rand_date)%>%
  mutate(rand_month = paste0(str_sub(rand_date, 1, 8), "01"))%>%
  filter(rand_month%in%c("2021-07-01", "2021-08-01"))%>%
  select(-rand_month)->missing_hes_participants_list
  
write_csv(missing_hes_participants_list, file="Intermediate outputs/missing_HES_participants.csv")
  
  
  



## covid coding over time -----

# primary diagnostic position

RECOVERY_HES_flags%>%
  select(`U071/U072 (primary position)` = covid_primary, 
         `U071/U072 (any position)`=covid_anydiag, 
         `U071 (any position)`=u071_anydiag, 
         `U072 (any position)`=u072_anydiag, 
         `U072 only (any position)`=u072_only_anydiag, 
         indexdate,
         study_id)%>%
  mutate(month=as.Date(paste0(str_sub(indexdate, 1, 8), "01")))%>%
  select(-indexdate)%>%
  pivot_longer(-c(month, study_id), names_to = "Variable", values_to = "Value")%>%
  group_by(month, Variable, Value)%>%
  summarise(Participants=n_distinct(study_id))%>%
  group_by(month, Variable)%>%
  mutate(Proportion=Participants/sum(Participants))%>%
  mutate(SE=sqrt(Proportion * (1-Proportion) / Participants))%>%
  mutate(Variable=factor(Variable, levels=c("U071/U072 (primary position)",
                                            "U071/U072 (any position)",
                                            "U071 (any position)",
                                            "U072 (any position)",
                                            "U072 only (any position)")))%>%
  
  mutate(Value=factor(if_else(Value==1, "Yes", "No")))%>%
  
  ggplot(aes(month, Proportion, color=Value, group=Value))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=Proportion-SE,
                    ymax=Proportion+SE),
                width=4)+
  scale_x_date(date_breaks = "2 months",
               date_labels = "%b %Y")+
  facet_wrap(~Variable)+
  labs(x="Index month", 
       y="Proportion within each month (%)",
       color="ICD-10 coding",
       title="COVID-19 ICD-10 coding among RECOVERY participants over time")+
  theme(axis.text.x=element_text(angle=60,
                                 vjust=1,
                                 hjust=1),
        legend.position="bottom",
        text=element_text(size=20))+
  geom_text(aes(label=round(Proportion*100,0)),
            color="black",
            vjust=-1.5,
            size=5)+
  scale_y_continuous(labels = label_number(scale = 1e2),
                     expand=expansion(c(0,0.1)))

ggsave("Outputs/Figures/covid_coding_along_time.png",
       dpi="retina",
       width=40,
       height=30,
       units="cm")


## Death rates by frailty and comorbidity scores ------


mortality_rates_frailty_comorbidity_over_time<-
  National_HES_events%>%
  mutate(frailty_score_group=case_when(frailty_score>15 ~ "High risk (>15)",
                                       frailty_score>=5 & frailty_score<=15 ~ "Intermediate risk (5-15)",
                                       frailty_score<5 ~ "Low-risk (<5)"),
         charlson_grp=case_when(charlson_grp==0 ~ "0",
                                charlson_grp==1 ~ "1",
                                charlson_grp==2 ~ "2",
                                charlson_grp==3 ~ "3-5",
                                charlson_grp==4 ~ "6+"))%>%
  mutate(month=paste0(yearmonth, "01"))%>%
  mutate(month=as.Date(month, format="%Y%m%d", origin="1964-10-22"))%>%    
  mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar-May 2020",
                            month>="2020-06-01" ~ "Following periods"))%>%
  mutate(Period=factor(Period, levels=c("Mar-May 2020",
                                        "Following periods")))%>%
  mutate(Period=fct_relevel(Period, "Mar-May 2020",
                            "Following periods"))%>%
  mutate(agegrp1 = case_when(agegrp %in% c("15-19",
                                           "20-24",
                                           "25-29",
                                           "30-34",
                                           "35-39",
                                           "40-44",
                                           "45-49",
                                           "50-54",
                                           "55-59") ~ "<60",
                             agegrp %in% c("60-64",
                                           "65-69") ~ "60-69",
                             agegrp %in% c("70-74",
                                           "75-79") ~ "70-79",
                             agegrp %in% c("80-84",
                                           "85-89") ~ "80-89",
                             agegrp %in% c("90-94",
                                           "95+") ~ "90+"))%>%
  select(-c(yearmonth, month, agegrp, hesdeath28, frailty_score, hesdeathdat, deathdat, sex))%>%
  pivot_longer(c(frailty_score_group, charlson_grp), names_to = "Score", values_to = "Group")%>%
  count(Score, Group, agegrp1, Period, death28)%>%
  group_by(Score, Group, agegrp1, Period,)%>%
  summarise(death_rate=sum(n[death28==1])/sum(n),
            SE=sqrt(death_rate*(1-death_rate)/sum(n)))%>%
  mutate(Cohort="Reference population")%>%
  
  rbind(RECOVERY_HES_flags%>%
          mutate(frailty_score_group=case_when(frailty_score>15 ~ "High risk (>15)",
                                               frailty_score>=5 & frailty_score<=15 ~ "Intermediate risk (5-15)",
                                               frailty_score<5 ~ "Low-risk (<5)"),
                 charlson_grp=case_when(charlson_grp==0 ~ "0",
                                        charlson_grp==1 ~ "1",
                                        charlson_grp==2 ~ "2",
                                        charlson_grp==3 ~ "3-5",
                                        charlson_grp==4 ~ "6+"))%>%
          mutate(agegrp1=as.factor(case_when(age<60 ~ "<60",
                                             between(age, 60, 69) ~ "60-69",
                                             between(age, 70, 79) ~ "70-79",
                                             between(age, 80, 89) ~ "80-89",
                                             age>=90 ~ "90+")))%>%
          mutate(month=paste0(str_sub(indexdate, 1, 8), "01"))%>%
          mutate(Period = case_when(month>="2020-03-01" & month <="2020-05-01" ~ "Mar-May 2020",
                                    month>="2020-06-01" ~ "Following periods"))%>%
          mutate(Period=factor(Period, levels=c("Mar-May 2020",
                                                "Following periods")))%>%
          mutate(Period=fct_relevel(Period, "Mar-May 2020",
                                    "Following periods"))%>%
          pivot_longer(c(frailty_score_group, charlson_grp), names_to = "Score", values_to = "Group")%>%
          count(Score, Group, agegrp1, Period, death28)%>%
          group_by(Score, Group, Period, agegrp1)%>%
          summarise(death_rate=sum(n[death28==1])/sum(n),
                    SE=sqrt(death_rate*(1-death_rate)/sum(n)))%>%
          mutate(Cohort="RECOVERY")
  )

##### plot (supplementary figure 9) ------

mortality_rates_frailty_comorbidity_over_time%>%
  # filter(Score=="frailty_score_group")%>%
  mutate(Score=case_when(Score=="charlson_grp" ~ "Charlson Comorbidity Score",
                         Score=="frailty_score_group" ~ "Hospital Frailty Score"))%>%
  
  mutate(Group=factor(Group, levels=c("0","1","2","3-5", "6+", "Low-risk (<5)","Intermediate risk (5-15)", "High risk (>15)")))%>%
  mutate(Group=fct_relevel(Group, "0","1","2","3-5", "6+", "Low-risk (<5)","Intermediate risk (5-15)", "High risk (>15)"))%>%
  
  ggplot(aes(Period, death_rate, group=Cohort, color=Cohort))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=death_rate-SE,
                    ymax=death_rate+SE),
                width=0.1)+
  facet_nested(agegrp1~Score+Group,
               switch="y")+
  theme(
    axis.text.x = element_text(angle=30,
                               vjust=1,
                               hjust=1),
    text=element_text(size=15),
    legend.position="bottom")+
  scale_y_continuous(labels = label_number(scale = 1e2))+
  scale_color_manual(values=colors[c(1,3)])+
  labs(x="Index period",
       y="Mortality rate (%)")


ggsave("Outputs/Publication/Figures/supplementary_figure_S9_death_by_frailty_comorbidity_over_time_first_wave_vs_rest.png",
       dpi="retina",
       width=50,
       height=30,
       units="cm")

ggsave("Outputs/Publication/Figures/High-resolution/supplementary_figure_S9_death_by_frailty_comorbidity_over_time_first_wave_vs_rest.tiff",
       dpi="retina",
       width=50,
       height=30,
       units="cm")

## Time interval until death ------

death_rates_first_wave_vs_others<-
  National_HES_flags_death_intervals_unscrambled%>%
  mutate(agebroad=if_else(agebroad%in%c("80-89", "90+"), "80+", agebroad))%>%
  group_by(agebroad, deathinterval, firstwave)%>%
  summarise(total=sum(total))%>%
  mutate(Cohort="Reference population")%>%
  rbind(RECOVERY_death_intervals%>%
          mutate(agebroad=if_else(agebroad%in%c("80-89", "90+"), "80+", agebroad))%>%
          group_by(agebroad, deathinterval, firstwave)%>%
          summarise(total=sum(total))%>%
          mutate(Cohort="RECOVERY"))%>%
  group_by(Cohort, agebroad, firstwave)%>%
  summarise(death_total=sum(total))%>%
  
  left_join(
counts_for_inclusion_timeseries_table_by_age_groups%>%
  mutate(across(c(RECOVERY, `Reference population`), ~as.numeric(map(str_split(., "\n"), 1))))%>%
  mutate(firstwave=if_else(Period=="Mar 20 - May 20", "mar-may 2020", "other"))%>%
  pivot_longer(c(RECOVERY, `Reference population`), names_to="Cohort", values_to="Total")%>%
  group_by(Cohort, firstwave, agegrp1)%>%
  summarise(population_total=sum(Total)),
by=c("agebroad"="agegrp1", "Cohort", "firstwave")
)%>%
  mutate(death_rate=round(death_total/population_total*100,1))%>%
  mutate(firstwave=case_when(firstwave=="mar-may 2020" ~ "Mar-May 2020",
                   firstwave=="other" ~ "Following periods" ))
  

##### plot (supplementary figure 10) ------

National_HES_flags_death_intervals_unscrambled%>%
  mutate(agebroad=if_else(agebroad%in%c("80-89", "90+"), "80+", agebroad))%>%
  group_by(agebroad, deathinterval, firstwave)%>%
    summarise(total=sum(total))%>%
    mutate(Cohort="Reference population")%>%
  rbind(RECOVERY_death_intervals%>%
          mutate(agebroad=if_else(agebroad%in%c("80-89", "90+"), "80+", agebroad))%>%
          group_by(agebroad, deathinterval, firstwave)%>%
          summarise(total=sum(total))%>%mutate(Cohort="RECOVERY"))%>%
  group_by(Cohort, firstwave, agebroad)%>%
  arrange(deathinterval)%>%
  group_by(Cohort, firstwave, agebroad)%>%
  mutate(cumulative=cumsum(total))%>%
  mutate(Prop=cumulative/sum(total)*100)%>%
  mutate(firstwave=factor(if_else(firstwave=="mar-may 2020", "Mar-May 2020", "Following periods")))%>%
  mutate(firstwave=fct_relevel(firstwave, "Mar-May 2020", "Following periods"))%>%
  arrange(desc(firstwave))%>%
  
  ggplot(aes(deathinterval, Prop, color=Cohort))+
  geom_point()+
  geom_line()+
  facet_grid(agebroad~as.factor(firstwave),
             switch="y")+
  theme(legend.position="bottom",
        text=element_text(size=20))+
  geom_text(data=death_rates_first_wave_vs_others%>%filter(Cohort=="RECOVERY"),
              aes(label=paste0("Number of deaths (%), ", Cohort, ": ", death_total, " (", death_rate, "%)"),
                  x=28,
                  y=30),
            hjust=1,
              show.legend = F)+
  geom_text(data=death_rates_first_wave_vs_others%>%filter(Cohort=="Reference population"),
                  aes(label=paste0("Number of deaths (%) ", Cohort, ": ", death_total, " (", death_rate, "%)"),
                      x=28,
                      y=20),
            hjust=1,
                  show.legend = F)+
  labs(y="Proportion of cumulative mortality (%)",
       x="Time interval from index date until death (in days)",
       # title="Time until death in the RECOVERY cohort vs the reference population, split by time period and age groups"
       )+
  scale_color_manual(values=colors[c(1,3)])+
  scale_x_continuous(limits=c(0, 28), breaks=seq(0, 28, 2))

ggsave("Outputs/Publication/Figures/supplementary_figure_S10_death_intervals.png",
       dpi="retina",
       width=50,
       height=30,
       units="cm")

ggsave("Outputs/Publication/Figures/High-resolution/supplementary_figure_S10_death_intervals.tiff",
       dpi="retina",
       width=50,
       height=30,
       units="cm")






## Site activation timeseries -------

sites%>%
  select(SiteID, StartDate, Nation)%>%
  filter(Nation=="England")%>%
  mutate(StartDate=as.Date(StartDate))%>%
  arrange(StartDate)%>%
  mutate(n=1)%>%
  mutate(Cumulative=cumsum(n))%>%
  
  ggplot(aes(StartDate, Cumulative))+
  geom_point(color="black")+
  geom_line(color="black")+
  labs(x="Start date",
       y="Cumulative sum",
       title="RECOVERY study site activation timeseries (in England)")+
  scale_x_date(date_breaks="1 month",
               date_labels = "%B %Y",
               limits=c(as.Date("01-03-2020", format="%d-%m-%Y"), NA))+
  theme(axis.text.x = element_text(angle=30,
                                   vjust=1,
                                   hjust=1),
        panel.grid.minor = element_blank(),
        text=element_text(size=20))+
  scale_y_continuous(limits=c(0,150))

ggsave("Outputs/Figures/site_activation_timeseries.png",
       dpi="retina",
       width=60,
       height=30,
       units="cm")



baseline_crf_england%>%
  mutate(niv_oxy = case_when(niv=="Y" | oxy=="Y" ~ "Y", 
                             niv %in% c("N", "U") & oxy %in% c("N", "U") ~ "N",
                             niv=="U" & oxy=="U" ~ "U"))%>%
  mutate(resp_status = case_when(imv=="Y" ~ "Invasive mechanical ventilation or ECMO",
                                 imv=="N" & niv_oxy =="Y" ~ "Non-invasive mechanical ventilation or supplementary oxygen",
                                 imv=="N" & niv_oxy=="N" ~ "None",
                                 imv=="U" & niv_oxy=="U" ~ "Unknown"))%>%
  mutate(month=as.Date(paste0(str_sub(rand_date, 1, 8), "01")))%>%
  group_by(month, resp_status)%>%
  summarise(n=n_distinct(study_number))%>%
  ungroup()%>%
  group_by(month)%>%
  mutate(prop=n/sum(n))%>%
  pivot_longer(c(n, prop), names_to = "variable", values_to = "value")%>%
  
  ggplot(aes(month, value, color=resp_status))+
  geom_point()+
  geom_line()+
  facet_wrap(~variable, nrow=2, scales = "free_y")+
  scale_x_date(date_breaks="1 month")+
  theme(axis.text.x=element_text(angle=30,
                                 # hjust=0,
                                 # vjust=-0.1
                                 ),
        legend.position="bottom"
                                 )
  

## mortality based on covid coding -----
RECOVERY_HES_flags%>%
  select(`U071/U072 (primary position)` = covid_primary, 
       `U071/U072 (any position)`=covid_anydiag, 
       `U071 (any position)`=u071_anydiag, 
       `U072 (any position)`=u072_anydiag, 
       `U072 only (any position)`=u072_only_anydiag, 
       indexdate,
       study_id, 
       death28)%>%
  mutate(month=as.Date(paste0(str_sub(indexdate, 1, 8), "01")))%>%
  select(-indexdate)%>%
  pivot_longer(-c(month, study_id, death28), names_to = "Variable", values_to = "Value")%>%
  group_by(month, Variable, Value)%>%
  summarise(Participants=n_distinct(study_id),
            Deaths = sum(death28),
            Mortality=Deaths/Participants)%>%
  group_by(month, Variable)%>%
  mutate(SE=sqrt(Mortality * (1-Mortality) / Participants))%>%
  mutate(Variable=factor(Variable, levels=c("U071/U072 (primary position)",
                                            "U071/U072 (any position)",
                                            "U071 (any position)",
                                            "U072 (any position)",
                                            "U072 only (any position)")))%>%
  filter(Value==1)%>%
  select(-Value)->mortality_over_time_icd_coding
  
mortality_over_time_icd_coding%>%
  ggplot(aes(month, Mortality, color=Variable, group=Variable))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=Mortality-SE,
                    ymax=Mortality+SE),
                width=4)+
  scale_x_date(date_breaks = "2 months",
               date_labels = "%b %Y")+
  labs(x="Index month", 
       y="Mortality within each month (%)",
       color="ICD-10 coding",
       title="Mortality over time based on COVID-19 ICD-10 coding among RECOVERY participants")+
  theme(axis.text.x=element_text(angle=60,
                                 vjust=1,
                                 hjust=1),
        legend.position="bottom",
        text=element_text(size=20))+
  # geom_text(aes(label=round(Mortality*100,0)),
  #           color="black",
  #           vjust=-1.5,
  #           size=5)+
  scale_y_continuous(labels = label_number(scale = 1e2),
                     expand=expansion(c(0,0.1)))


