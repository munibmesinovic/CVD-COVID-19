setwd("~/Desktop/COVID/data/")

invisible(lapply(c("tidyr","dplyr","plyr","ggplot2","reshape2",
                   "naniar","stringr","scales","lubridate",
                   "Hmisc","corrplot", "RColorBrewer", "data.table"),
                 library, character.only = TRUE))

# load the dataset
data = read.csv("import.tbl_partner2.csv") # 800459 samples

# Remove rows with NA or missing age, sex, region, country, income
data <- data[!(is.na(data$age) | data$age==""), ] # 768027 samples
data <- data[!(is.na(data$sex) | data$sex==""), ] # 766824 samples
data <- data[!(is.na(data$region) | data$region==""), ] # 766823 samples
data <- data[!(is.na(data$country) | data$country==""), ] # 766823 samples
data <- data[!(is.na(data$income) | data$income==""), ] # 766823 samples

# Group countries with less than 60 samples under new group Few
data$country[data$country == "Singapore"] <- "Few"
data$country[data$country == "Taiwan"] <- "Few"
data$country[data$country == "Vietnam"] <- "Few"
data$country[data$country == "Ghana"] <- "Few"
data$country[data$country == ""] <- "Few"
data$country[data$country == "Bangladesh"] <- "Few"
data$country[data$country == "Kenya"] <- "Few"
data$country[data$country == "Croatia"] <- "Few"
data$country[data$country == "Thailand"] <- "Few"
data$country[data$country == "Philippines"] <- "Few"
data$country[data$country == "Turkey"] <- "Few"
data$country[data$country == "China"] <- "Few"
data$country[data$country == "Dominican Republic"] <- "Few"
data$country[data$country == "Hong Kong"] <- "Few"
data$country[data$country == "Czechia"] <- "Few"
data$country[data$country == "Honduras"] <- "Few"
data$country[data$country == "Ecuador"] <- "Few"
data$country[data$country == "Mexico"] <- "Few"
data$country[data$country == "United Arab Emirates"] <- "Few"
data$country[data$country == "Greece"] <- "Few"
data$country[data$country == "Korea, Republic of"] <- "Few"
data$country[data$country == "Austria"] <- "Few"

# Replace NA with FALSE for comorbidites, symptoms, treatment, PE
data[c("comorbid_aids_hiv", "comorbid_asthma",
       "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
       "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
       "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
       "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
       "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
       "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
       "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
       "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
       "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
       "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
       "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
       "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
       "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
       "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
       "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
       "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
       "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
       "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
       "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
       'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
       'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')][is.na(data[c("comorbid_aids_hiv", "comorbid_asthma",
                                                        "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                                                        "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                                                        "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                                                        "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                                                        "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                                                        "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                                                        "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                                                        "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                                                        "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                                                        "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                                                        "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                                                        "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                                                        "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                                                        "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
                                                        "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
                                                        "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
                                                        "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
                                                        "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
                                                        "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
                                                        'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
                                                        'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')])] <- FALSE

# Combine into PE variable
data$PE = data$pulmonary_embolism | data$pulmonary_embolism_or_dvt | data$thromboembolsim

# Drop the old PE columns
data <-dplyr::select(data, -c('pulmonary_embolism', 'pulmonary_embolism_or_dvt', 'thromboembolsim'))

# Create death variable
data <- data %>%
  mutate(
    PE = case_when(PE == 'TRUE' ~ 1,
                      TRUE ~ 0))

# Merge two oxygen therapy treatment variables into one by OR
data$treat_oxy = data$treat_oxy_therapy | data$treat_oxygen_therapy | data$d1_oxygen_therapy

# Drop the old columns
data <-dplyr::select(data, -c('treat_oxy_therapy', 'treat_oxygen_therapy', 'd1_oxygen_therapy'))

# Combine ICU treatment with relevant general treatment
data$treat_extracorporeal = data$treat_extracorporeal | data$icu_treat_extracorporeal
data$treat_high_flow_nasal_cannula = data$treat_high_flow_nasal_cannula | data$icu_treat_high_flow_nasal_cannula
data$treat_inhaled_nitric_oxide = data$treat_inhaled_nitric_oxide | data$icu_treat_inhaled_nitric_oxide
data$treat_non_invasive_ventilation = data$treat_non_invasive_ventilation | data$icu_treat_non_invasive_ventilation
data$treat_oxy = data$treat_oxy | data$icu_treat_oxy_therapy | data$icu_treat_oxygen_therapy
data$treat_tracheostomy = data$treat_tracheostomy | data$icu_treat_tracheostomy

data <-dplyr::select(data, -c('icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
                              'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
                              'icu_treat_extracorporeal', 'icu_treat_oxy_therapy'))

# Remove ethnicity, lab measurements, unknown variables, date to outcome, outcome
data <- data[, c("age","sex","country", "income", "comorbid_aids_hiv", "comorbid_asthma",
                 "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                 "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                 "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                 "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                 "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                 "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                 "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                 "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                 "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                 "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                 "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                 "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                 "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                 "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
                 "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
                 "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
                 "treat_tracheostomy", "treat_oxy", "PE")] # 766823 samples, 55 total variables

# Save data file as the first dataset without lab measurements
write.csv(data, "data_PE_no_lab_measurements.csv")

##################################################################################################
# Extracting dataset for PE with lab measurements
##################################################################################################

# load the dataset
data = read.csv("import.tbl_partner.csv") # 800459 samples

# Remove rows with NA or missing age, sex, region, country, income
data <- data[!(is.na(data$age) | data$age==""), ] # 768027 samples
data <- data[!(is.na(data$sex) | data$sex==""), ] # 766824 samples
data <- data[!(is.na(data$region) | data$region==""), ] # 766823 samples
data <- data[!(is.na(data$country) | data$country==""), ] # 766823 samples
data <- data[!(is.na(data$income) | data$income==""), ] # 766823 samples

# Group countries with less than 60 samples under new group Few
data$country[data$country == "Singapore"] <- "Few"
data$country[data$country == "Taiwan"] <- "Few"
data$country[data$country == "Vietnam"] <- "Few"
data$country[data$country == "Ghana"] <- "Few"
data$country[data$country == ""] <- "Few"
data$country[data$country == "Bangladesh"] <- "Few"
data$country[data$country == "Kenya"] <- "Few"
data$country[data$country == "Croatia"] <- "Few"
data$country[data$country == "Thailand"] <- "Few"
data$country[data$country == "Philippines"] <- "Few"
data$country[data$country == "Turkey"] <- "Few"
data$country[data$country == "China"] <- "Few"
data$country[data$country == "Dominican Republic"] <- "Few"
data$country[data$country == "Hong Kong"] <- "Few"
data$country[data$country == "Czechia"] <- "Few"
data$country[data$country == "Honduras"] <- "Few"
data$country[data$country == "Ecuador"] <- "Few"
data$country[data$country == "Mexico"] <- "Few"
data$country[data$country == "United Arab Emirates"] <- "Few"
data$country[data$country == "Greece"] <- "Few"
data$country[data$country == "Korea, Republic of"] <- "Few"
data$country[data$country == "Austria"] <- "Few"

# Standardise D-dimer lab measurements to microg/mL
setDT(data)[lborresu_lab_ddimer == "ng/mL", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ug/L", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ng/ml", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ng/ml FEU", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ug/L FEU", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "<b5>g/L", lborres_lab_ddimer := lborres_lab_ddimer*1000]
setDT(data)[lborresu_lab_ddimer == "ug/l", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ng/L", lborres_lab_ddimer := lborres_lab_ddimer/(1000*1000)]
setDT(data)[lborresu_lab_ddimer == "ug/L DDU", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ug/L FEU", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "<b5>g/mlFEU", lborres_lab_ddimer := lborres_lab_ddimer/1000]
data$lborres_lab_ddimer[data$lborres_lab_ddimer > 20] <- NA

# Replace NA with FALSE for comorbidites, symptoms, treatment, PE
data[c("comorbid_aids_hiv", "comorbid_asthma",
       "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
       "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
       "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
       "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
       "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
       "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
       "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
       "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
       "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
       "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
       "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
       "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
       "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
       "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
       "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
       "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
       "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
       "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
       "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
       'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
       'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')][is.na(data[c("comorbid_aids_hiv", "comorbid_asthma",
                                                                          "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                                                                          "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                                                                          "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                                                                          "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                                                                          "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                                                                          "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                                                                          "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                                                                          "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                                                                          "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                                                                          "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                                                                          "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                                                                          "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                                                                          "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                                                                          "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
                                                                          "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
                                                                          "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
                                                                          "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
                                                                          "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
                                                                          "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
                                                                          'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
                                                                          'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')])] <- FALSE

# Combine into PE variable
data$PE = data$pulmonary_embolism | data$pulmonary_embolism_or_dvt | data$thromboembolsim

# Drop the old PE columns
data <-dplyr::select(data, -c('pulmonary_embolism', 'pulmonary_embolism_or_dvt', 'thromboembolsim'))

# Merge two oxygen therapy treatment variables into one by OR
data$treat_oxy = data$treat_oxy_therapy | data$treat_oxygen_therapy | data$d1_oxygen_therapy

# Drop the old columns
data <-dplyr::select(data, -c('treat_oxy_therapy', 'treat_oxygen_therapy', 'd1_oxygen_therapy'))

# Combine ICU treatment with relevant general treatment
data$treat_extracorporeal = data$treat_extracorporeal | data$icu_treat_extracorporeal
data$treat_high_flow_nasal_cannula = data$treat_high_flow_nasal_cannula | data$icu_treat_high_flow_nasal_cannula
data$treat_inhaled_nitric_oxide = data$treat_inhaled_nitric_oxide | data$icu_treat_inhaled_nitric_oxide
data$treat_non_invasive_ventilation = data$treat_non_invasive_ventilation | data$icu_treat_non_invasive_ventilation
data$treat_oxy = data$treat_oxy | data$icu_treat_oxy_therapy | data$icu_treat_oxygen_therapy
data$treat_tracheostomy = data$treat_tracheostomy | data$icu_treat_tracheostomy

data <-dplyr::select(data, -c('icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
                              'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
                              'icu_treat_extracorporeal', 'icu_treat_oxy_therapy'))

# Remove ethnicity, unknown variables, date to outcome, outcome
data <- data[, c("age","sex","country", "income", "comorbid_aids_hiv", "comorbid_asthma",
                 "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                 "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                 "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                 "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                 "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                 "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                 "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                 "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                 "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                 "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                 "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                 "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                 "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                 "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
                 "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
                 "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
                 "treat_tracheostomy", "treat_oxy", "lborres_lab_ddimer", "lab_alt", "lab_bili", "lab_crp",
                 "lab_lym", "lab_neut", "lab_pt", "lab_urean", "lab_wbc", "vs_diabp", "vs_hr",
                 "vs_resp", "vs_sysbp", "vs_temp", "vs_oxysat", "PE")] # 766823 samples, 70 total variables

# Save data file as the first dataset with lab measurements
write.csv(data, "data_PE.csv")

##################################################################################################
# Exracting dataset of UK and Spain for PE without lab measurements
##################################################################################################

# load the dataset
data = read.csv("import.tbl_partner2.csv") # 800459 samples

# Get only Spain and UK
data <- data[(data$country=="United Kingdom" | data$country=="Spain"), ] # 288493 samples

# Remove rows with NA or missing age, sex, region, country, income
data <- data[!(is.na(data$age) | data$age==""), ] # 269784 samples
data <- data[!(is.na(data$sex) | data$sex==""), ] # 269373 samples
data <- data[!(is.na(data$region) | data$region==""), ] # 269373 samples
data <- data[!(is.na(data$income) | data$income==""), ] # 269373 samples

# Replace NA with FALSE for comorbidites, symptoms, treatment, PE
data[c("comorbid_aids_hiv", "comorbid_asthma",
       "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
       "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
       "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
       "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
       "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
       "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
       "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
       "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
       "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
       "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
       "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
       "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
       "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
       "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
       "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
       "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
       "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
       "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
       "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
       'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
       'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')][is.na(data[c("comorbid_aids_hiv", "comorbid_asthma",
                                                                          "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                                                                          "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                                                                          "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                                                                          "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                                                                          "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                                                                          "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                                                                          "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                                                                          "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                                                                          "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                                                                          "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                                                                          "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                                                                          "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                                                                          "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                                                                          "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
                                                                          "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
                                                                          "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
                                                                          "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
                                                                          "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
                                                                          "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
                                                                          'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
                                                                          'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')])] <- FALSE

# Combine into PE variable
data$PE = data$pulmonary_embolism | data$pulmonary_embolism_or_dvt | data$thromboembolsim

# Drop the old PE columns
data <-dplyr::select(data, -c('pulmonary_embolism', 'pulmonary_embolism_or_dvt', 'thromboembolsim'))

# Remove ethnicity, treatment, lab measurements, unknown variables, date to outcome, outcome
data <- data[, c("age","sex","country", "comorbid_aids_hiv", "comorbid_asthma",
                 "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                 "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                 "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                 "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                 "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                 "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                 "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                 "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                 "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                 "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                 "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                 "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                 "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                 "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing", "PE")] # 269373 samples, 46 total variables

# Save data file as the first dataset without lab measurements
write.csv(data, "data_PE_no_lab_measurements_SpainUK.csv")

##################################################################################################
# Extracting dataset for PE with lab measurements only for Spain and UK
##################################################################################################

# load the dataset
data = read.csv("import.tbl_partner2.csv") # 800459 samples

# Get only Spain and UK
data <- data[(data$country=="United Kingdom" | data$country=="Spain"), ] # 288493 samples

# Remove rows with NA or missing age, sex, region, country, income
data <- data[!(is.na(data$age) | data$age==""), ] # 269784 samples
data <- data[!(is.na(data$sex) | data$sex==""), ] # 269373 samples
data <- data[!(is.na(data$region) | data$region==""), ] # 269373 samples
data <- data[!(is.na(data$income) | data$income==""), ] # 269373 samples

# Standardise D-dimer lab measurements to microg/mL
setDT(data)[lborresu_lab_ddimer == "ng/mL", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ug/L", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ng/ml", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ng/ml FEU", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ug/L FEU", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "<b5>g/L", lborres_lab_ddimer := lborres_lab_ddimer*1000]
setDT(data)[lborresu_lab_ddimer == "ug/l", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ng/L", lborres_lab_ddimer := lborres_lab_ddimer/(1000*1000)]
setDT(data)[lborresu_lab_ddimer == "ug/L DDU", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "ug/L FEU", lborres_lab_ddimer := lborres_lab_ddimer/1000]
setDT(data)[lborresu_lab_ddimer == "<b5>g/mlFEU", lborres_lab_ddimer := lborres_lab_ddimer/1000]
data$lborres_lab_ddimer[data$lborres_lab_ddimer > 20] <- NA

# Convert back to dataframe for processing
class(data) <- class(as.data.frame(data))

summary(data)

# Replace NA with FALSE for comorbidites, symptoms, treatment, PE
data[c("comorbid_aids_hiv", "comorbid_asthma",
       "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
       "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
       "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
       "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
       "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
       "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
       "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
       "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
       "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
       "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
       "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
       "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
       "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
       "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
       "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
       "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
       "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
       "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
       "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
       'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
       'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')][is.na(data[c("comorbid_aids_hiv", "comorbid_asthma",
                                                                          "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                                                                          "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                                                                          "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                                                                          "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                                                                          "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                                                                          "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                                                                          "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                                                                          "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                                                                          "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                                                                          "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                                                                          "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                                                                          "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                                                                          "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                                                                          "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
                                                                          "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
                                                                          "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
                                                                          "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
                                                                          "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
                                                                          "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
                                                                          'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
                                                                          'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')])] <- FALSE

# Combine into PE variable
data$PE = data$pulmonary_embolism | data$pulmonary_embolism_or_dvt | data$thromboembolsim

# Drop the old PE columns
data <-dplyr::select(data, -c('pulmonary_embolism', 'pulmonary_embolism_or_dvt', 'thromboembolsim'))

# Get the subset of data matching each patient group (target and control)
data_true_PE <- subset(data, PE == TRUE)
data_false_PE <- subset(data, is.na(PE))

summary(data_false_PE)

# Remove ethnicity, unknown variables, date to outcome, outcome, treatment, income
data <- data[, c("age","sex","country", "comorbid_aids_hiv", "comorbid_asthma",
                 "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                 "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                 "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                 "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                 "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                 "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                 "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                 "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                 "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                 "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                 "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                 "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                 "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                 "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
                 "lborres_lab_ddimer", "lab_alt", "lab_bili", "lab_crp",
                 "lab_lym", "lab_neut", "lab_pt", "lab_urean", "lab_wbc", "vs_diabp", "vs_hr",
                 "vs_resp", "vs_sysbp", "vs_temp", "vs_oxysat", "PE")] # 269373 samples, 62 total variables

# Convert age decile into 5 groups instead
data <- data[!(is.na(data$agegp10) | data$agegp10==""), ] # 254552 samples

data$agegp10[data$agegp10 == "[0,10)" | data$agegp10=="[10,20)"] <- "[0,20)"
data$agegp10[data$agegp10 == "[20,30)" | data$agegp10=="[30,40)"] <- "[20,40)"
data$agegp10[data$agegp10 == "[40,50)" | data$agegp10=="[50,60)"] <- "[40,60)"
data$agegp10[data$agegp10 == "[60,70)" | data$agegp10=="[70,80)"] <- "[60,80)"
data$agegp10[data$agegp10 == "[80,90)" | data$agegp10=="[90,120)"] <- "[80,120)"

ggplot(data, aes(x=agegp10)) + geom_bar(fill = "steelblue4") + 
  scale_y_continuous(labels = function(x) x/1000) + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Age (Years)",
       y = "Frequency (in thousands)") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)

# Save data file as the first dataset with lab measurements
write.csv(data, "data_PE_SpainUK.csv")

##################################################################################################
# Extracting dataset of UK and Spain for PE without lab measurements separately
##################################################################################################

# load the dataset
data = read.csv("import.tbl_partner2.csv") # 800459 samples

# Get only Spain and UK
data_UK <- data[(data$country=="United Kingdom"), ] # 273152 samples
data_Spain <- data[(data$country=="Spain"), ] # 15344 samples

# Remove rows with NA or missing age, sex, region, country, income
data_UK <- data_UK[!(is.na(data_UK$age) | data_UK$age==""), ] # 254552 samples
data_UK <- data_UK[!(is.na(data_UK$sex) | data_UK$sex==""), ] # 254165 samples
data_UK <- data_UK[!(is.na(data_UK$region) | data_UK$region==""), ] # 254165 samples
data_UK <- data_UK[!(is.na(data_UK$income) | data_UK$income==""), ] # 254165 samples

# Remove rows with NA or missing age, sex, region, country, income
data_Spain <- data_Spain[!(is.na(data_Spain$age) | data_Spain$age==""), ] # 15232 samples
data_Spain <- data_Spain[!(is.na(data_Spain$sex) | data_Spain$sex==""), ] # 15208 samples
data_Spain <- data_Spain[!(is.na(data_Spain$region) | data_Spain$region==""), ] # 15208 samples
data_Spain <- data_Spain[!(is.na(data_Spain$income) | data_Spain$income==""), ] # 15208 samples

# Replace NA with FALSE for comorbidites, symptoms, treatment, PE
data_UK[c("comorbid_aids_hiv", "comorbid_asthma",
       "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
       "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
       "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
       "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
       "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
       "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
       "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
       "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
       "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
       "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
       "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
       "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
       "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
       "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
       "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
       "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
       "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
       "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
       "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
       'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
       'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')][is.na(data_UK[c("comorbid_aids_hiv", "comorbid_asthma",
                                                                          "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                                                                          "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                                                                          "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                                                                          "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                                                                          "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                                                                          "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                                                                          "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                                                                          "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                                                                          "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                                                                          "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                                                                          "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                                                                          "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                                                                          "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                                                                          "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
                                                                          "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
                                                                          "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
                                                                          "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
                                                                          "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
                                                                          "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
                                                                          'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
                                                                          'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')])] <- FALSE

# Replace NA with FALSE for comorbidites, symptoms, treatment, PE
data_Spain[c("comorbid_aids_hiv", "comorbid_asthma",
          "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
          "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
          "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
          "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
          "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
          "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
          "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
          "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
          "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
          "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
          "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
          "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
          "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
          "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
          "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
          "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
          "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
          "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
          "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
          'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
          'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')][is.na(data_Spain[c("comorbid_aids_hiv", "comorbid_asthma",
                                                                                "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                                                                                "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                                                                                "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                                                                                "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                                                                                "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                                                                                "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                                                                                "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                                                                                "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                                                                                "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                                                                                "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                                                                                "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                                                                                "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                                                                                "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                                                                                "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
                                                                                "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
                                                                                "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
                                                                                "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
                                                                                "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
                                                                                "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
                                                                                'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
                                                                                'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')])] <- FALSE

# Combine into PE variable
data_UK$PE = data_UK$pulmonary_embolism | data_UK$pulmonary_embolism_or_dvt | data_UK$thromboembolsim

# Combine into PE variable
data_Spain$PE = data_Spain$pulmonary_embolism | data_Spain$pulmonary_embolism_or_dvt | data_Spain$thromboembolsim

# Drop the old PE columns
data_UK <-dplyr::select(data_UK, -c('pulmonary_embolism', 'pulmonary_embolism_or_dvt', 'thromboembolsim'))

# Drop the old PE columns
data_Spain <-dplyr::select(data_Spain, -c('pulmonary_embolism', 'pulmonary_embolism_or_dvt', 'thromboembolsim'))

# Remove ethnicity, treatment, lab measurements, unknown variables, date to outcome, outcome, income
data_UK <- data_UK[, c("age","sex","country", "comorbid_aids_hiv", "comorbid_asthma",
                 "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                 "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                 "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                 "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                 "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                 "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                 "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                 "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                 "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                 "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                 "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                 "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                 "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                 "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing", "PE")] # 254165 samples, 46 total variables

# Remove ethnicity, treatment, lab measurements, unknown variables, date to outcome, outcome, income
data_Spain <- data_Spain[, c("age","sex","country", "comorbid_aids_hiv", "comorbid_asthma",
                       "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                       "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                       "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                       "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                       "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                       "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                       "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                       "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                       "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                       "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                       "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                       "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                       "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                       "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing", "PE")] # 15208 samples, 46 total variables

# Save data file as the first dataset without lab measurements
write.csv(data_UK, "data_PE_no_lab_measurements_UK.csv")
# Save data file as the first dataset without lab measurements
write.csv(data_Spain, "data_PE_no_lab_measurements_Spain.csv")

##################################################################################################
# Extracting dataset of UK and Spain for PE without lab measurements separately
##################################################################################################

# load the dataset
data = read.csv("import.tbl_partner2.csv") # 800459 samples

# Get only Spain and UK
data_UK <- data[(data$country=="United Kingdom"), ] # 273152 samples
data_Spain <- data[(data$country=="Spain"), ] # 15344 samples

# Convert age decile into 5 groups instead
data_UK <- data_UK[!(is.na(data_UK$agegp10) | data_UK$agegp10==""), ] # 254552 samples
data_Spain <- data_Spain[!(is.na(data_Spain$agegp10) | data_Spain$agegp10==""), ] # 15232 samples

data_UK$agegp10[data_UK$agegp10 == "[0,10)" | data_UK$agegp10=="[10,20)"] <- "[0,20)"
data_UK$agegp10[data_UK$agegp10 == "[20,30)" | data_UK$agegp10=="[30,40)"] <- "[20,40)"
data_UK$agegp10[data_UK$agegp10 == "[40,50)" | data_UK$agegp10=="[50,60)"] <- "[40,60)"
data_UK$agegp10[data_UK$agegp10 == "[60,70)" | data_UK$agegp10=="[70,80)"] <- "[60,80)"
data_UK$agegp10[data_UK$agegp10 == "[80,90)" | data_UK$agegp10=="[90,120)"] <- "[80,120)"

data_Spain$agegp10[data_Spain$agegp10 == "[0,10)" | data_Spain$agegp10=="[10,20)"] <- "[0,20)"
data_Spain$agegp10[data_Spain$agegp10 == "[20,30)" | data_Spain$agegp10=="[30,40)"] <- "[20,40)"
data_Spain$agegp10[data_Spain$agegp10 == "[40,50)" | data_Spain$agegp10=="[50,60)"] <- "[40,60)"
data_Spain$agegp10[data_Spain$agegp10 == "[60,70)" | data_Spain$agegp10=="[70,80)"] <- "[60,80)"
data_Spain$agegp10[data_Spain$agegp10 == "[80,90)" | data_Spain$agegp10=="[90,120)"] <- "[80,120)"

ggplot(data_UK, aes(x=agegp10)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Age (Years)",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)

ggplot(data_Spain, aes(x=agegp10)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Age (Years)",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)

# Remove rows with NA or missing sex, region, country, income
data_UK <- data_UK[!(is.na(data_UK$sex) | data_UK$sex==""), ] # 254165 samples
data_UK <- data_UK[!(is.na(data_UK$region) | data_UK$region==""), ] # 254165 samples
data_UK <- data_UK[!(is.na(data_UK$income) | data_UK$income==""), ] # 254165 samples

# Remove rows with NA or missing sex, region, country, income
data_Spain <- data_Spain[!(is.na(data_Spain$sex) | data_Spain$sex==""), ] # 15208 samples
data_Spain <- data_Spain[!(is.na(data_Spain$region) | data_Spain$region==""), ] # 15208 samples
data_Spain <- data_Spain[!(is.na(data_Spain$income) | data_Spain$income==""), ] # 15208 samples

# Replace NA with FALSE for comorbidites, symptoms, treatment, PE
data_UK[c("comorbid_aids_hiv", "comorbid_asthma",
          "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
          "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
          "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
          "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
          "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
          "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
          "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
          "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
          "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
          "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
          "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
          "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
          "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
          "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
          "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
          "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
          "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
          "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
          "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
          'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
          'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')][is.na(data_UK[c("comorbid_aids_hiv", "comorbid_asthma",
                                                                                "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                                                                                "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                                                                                "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                                                                                "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                                                                                "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                                                                                "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                                                                                "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                                                                                "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                                                                                "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                                                                                "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                                                                                "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                                                                                "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                                                                                "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                                                                                "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
                                                                                "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
                                                                                "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
                                                                                "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
                                                                                "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
                                                                                "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
                                                                                'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
                                                                                'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')])] <- FALSE

# Replace NA with FALSE for comorbidites, symptoms, treatment, PE
data_Spain[c("comorbid_aids_hiv", "comorbid_asthma",
             "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
             "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
             "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
             "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
             "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
             "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
             "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
             "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
             "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
             "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
             "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
             "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
             "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
             "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
             "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
             "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
             "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
             "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
             "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
             'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
             'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')][is.na(data_Spain[c("comorbid_aids_hiv", "comorbid_asthma",
                                                                                      "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                                                                                      "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                                                                                      "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                                                                                      "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                                                                                      "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                                                                                      "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                                                                                      "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                                                                                      "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                                                                                      "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                                                                                      "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                                                                                      "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                                                                                      "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                                                                                      "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                                                                                      "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
                                                                                      "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
                                                                                      "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
                                                                                      "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
                                                                                      "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy",
                                                                                      "d1_oxygen_therapy", 'icu_treat_tracheostomy', 'icu_treat_oxygen_therapy', 'icu_treat_non_invasive_ventilation',
                                                                                      'icu_treat_inhaled_nitric_oxide', 'icu_treat_high_flow_nasal_cannula',
                                                                                      'icu_treat_extracorporeal', 'icu_treat_oxy_therapy')])] <- FALSE

# Combine into PE variable
data_UK$PE = data_UK$pulmonary_embolism | data_UK$pulmonary_embolism_or_dvt | data_UK$thromboembolsim

# Combine into PE variable
data_Spain$PE = data_Spain$pulmonary_embolism | data_Spain$pulmonary_embolism_or_dvt | data_Spain$thromboembolsim

# Drop the old PE columns
data_UK <-dplyr::select(data_UK, -c('pulmonary_embolism', 'pulmonary_embolism_or_dvt', 'thromboembolsim'))

# Drop the old PE columns
data_Spain <-dplyr::select(data_Spain, -c('pulmonary_embolism', 'pulmonary_embolism_or_dvt', 'thromboembolsim'))

# Remove ethnicity, treatment, lab measurements, unknown variables, date to outcome, outcome, income
data_UK <- data_UK[, c("agegp10","sex","country", "comorbid_aids_hiv", "comorbid_asthma",
                       "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                       "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                       "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                       "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                       "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                       "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                       "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                       "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                       "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                       "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                       "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                       "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                       "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                       "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing", "PE")] # 254165 samples, 46 total variables

# Remove ethnicity, treatment, lab measurements, unknown variables, date to outcome, outcome, income
data_Spain <- data_Spain[, c("agegp10","sex","country", "comorbid_aids_hiv", "comorbid_asthma",
                             "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                             "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                             "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                             "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                             "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                             "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                             "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                             "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                             "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                             "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                             "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                             "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                             "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                             "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing", "PE")] # 15208 samples, 46 total variables

# Save data file as the first dataset without lab measurements
write.csv(data_UK, "data_PE_no_lab_measurements_UK.csv")
# Save data file as the first dataset without lab measurements
write.csv(data_Spain, "data_PE_no_lab_measurements_Spain.csv")

##################################################################################################
# Exracting dataset for death without lab measurements
##################################################################################################

# load the dataset
data = read.csv("import.tbl_partner2.csv") # 800459 samples

# Remove rows with NA or missing age, sex, region, country, income, outcome
data <- data[!(is.na(data$age) | data$age==""), ] # 768027 samples
data <- data[!(is.na(data$sex) | data$sex==""), ] # 766824 samples
data <- data[!(is.na(data$region) | data$region==""), ] # 766823 samples
data <- data[!(is.na(data$income) | data$income==""), ] # 766823 samples
data <- data[!(is.na(data$outcome) | data$outcome==""), ] # 734282 samples

# Create death variable
data <- data %>%
  mutate(
    death = case_when(outcome == 'death' ~ 1,
                      TRUE ~ 0))

# Replace NA with FALSE for comorbidites, symptoms, treatment, PE
data[c("comorbid_aids_hiv", "comorbid_asthma",
       "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
       "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
       "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
       "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
       "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
       "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
       "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
       "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
       "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
       "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
       "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
       "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
       "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
       "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
       "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
       "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
       "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
       "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy", "treat_mask_oxygen_therapy", "treat_nasal_oxygen_therapy",
       "ever_icu")][is.na(data[c("comorbid_aids_hiv", "comorbid_asthma",
                                 "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                                 "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                                 "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                                 "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                                 "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                                 "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                                 "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                                 "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                                 "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                                 "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                                 "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                                 "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                                 "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                                 "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
                                 "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
                                 "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
                                 "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
                                 "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy", "treat_mask_oxygen_therapy",
                                 "treat_nasal_oxygen_therapy", "ever_icu")])] <- FALSE

# Create PE variable
# Combine into PE variable
data$PE = data$pulmonary_embolism | data$pulmonary_embolism_or_dvt | data$thromboembolsim

# Drop the old PE columns
data <-dplyr::select(data, -c('pulmonary_embolism', 'pulmonary_embolism_or_dvt', 'thromboembolsim'))

# Convert age into custom categories
data$agegp10[data$agegp10 == "[0,10)" | data$agegp10=="[10,20)"] <- "[0,20)"
data$agegp10[data$agegp10 == "[20,30)" | data$agegp10=="[30,40)"] <- "[20,40)"
data$agegp10[data$agegp10 == "[40,50)" | data$agegp10=="[50,60)"] <- "[40,60)"
data$agegp10[data$agegp10 == "[60,70)" | data$agegp10=="[70,80)"] <- "[60,80)"
data$agegp10[data$agegp10 == "[80,90)" | data$agegp10=="[90,120)"] <- "[80,120)"

# Regroup region variable
data <- data %>%
  mutate(
    region = case_when(region == 'East Asia & Pacific' ~ 'EA',
                       region == 'Europe & Central Asia' ~ 'Europe & CA',
                       region == 'Latin America & Caribbean' ~ 'LA',
                       region == 'Middle East & North Africa' ~ 'MENA',
                       region == 'North America' ~ 'NAM',
                       region == 'South Asia' ~ 'SA',
                       region == 'Sub-Saharan Africa' ~ 'SSA'))

# Remove ethnicity, treatment, lab measurements, treatment, unknown variables, date to outcome, outcome
data <- data[, c("agegp10","sex","country", "region", "comorbid_aids_hiv", "comorbid_asthma",
                 "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                 "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                 "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                 "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                 "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                 "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                 "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                 "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                 "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                 "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                 "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                 "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                 "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                 "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing", "PE",
                 "death")] # 734282 samples, 48 total variables

# Save data file as the first dataset without lab measurements
write.csv(data, "data_death_no_lab_measurements.csv")

##################################################################################################
# Extracting dataset for death with lab measurements
##################################################################################################

# load the dataset
data = read.csv("import.tbl_partner2.csv") # 800459 samples

# Remove rows with NA or missing age, sex, region, country, income, outcome
data <- data[!(is.na(data$age) | data$age==""), ] # 768027 samples
data <- data[!(is.na(data$sex) | data$sex==""), ] # 766824 samples
data <- data[!(is.na(data$region) | data$region==""), ] # 766823 samples
data <- data[!(is.na(data$income) | data$income==""), ] # 766823 samples
data <- data[!(is.na(data$outcome) | data$outcome==""), ] # 734282 samples

# Create death variable
data <- data %>%
  mutate(
    death = case_when(outcome == 'death' ~ 1,
                      TRUE ~ 0))

# Replace NA with FALSE for comorbidites, symptoms, treatment, PE
data[c("comorbid_aids_hiv", "comorbid_asthma",
       "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
       "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
       "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
       "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
       "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
       "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
       "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
       "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
       "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
       "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
       "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
       "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
       "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
       "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
       "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
       "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
       "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
       "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy", "treat_mask_oxygen_therapy", "treat_nasal_oxygen_therapy",
       "ever_icu")][is.na(data[c("comorbid_aids_hiv", "comorbid_asthma",
                                 "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                                 "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                                 "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                                 "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                                 "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                                 "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                                 "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                                 "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                                 "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                                 "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                                 "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                                 "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                                 "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                                 "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing",
                                 "treat_extracorporeal", "treat_high_flow_nasal_cannula", "treat_inhaled_nitric_oxide",
                                 "treat_invasive_ventilation", "treat_non_invasive_ventilation", "treat_respiratory_support",
                                 "treat_tracheostomy", "treat_oxy_therapy", "pulmonary_embolism_or_dvt",
                                 "thromboembolsim", "pulmonary_embolism", "treat_oxygen_therapy", "treat_mask_oxygen_therapy",
                                 "treat_nasal_oxygen_therapy", "ever_icu")])] <- FALSE

# Create PE variable
# Combine into PE variable
data$PE = data$pulmonary_embolism | data$pulmonary_embolism_or_dvt | data$thromboembolsim

# Drop the old PE columns
data <-dplyr::select(data, -c('pulmonary_embolism', 'pulmonary_embolism_or_dvt', 'thromboembolsim'))

# Convert age into custom categories
data$agegp10[data$agegp10 == "[0,10)" | data$agegp10=="[10,20)"] <- "[0,20)"
data$agegp10[data$agegp10 == "[20,30)" | data$agegp10=="[30,40)"] <- "[20,40)"
data$agegp10[data$agegp10 == "[40,50)" | data$agegp10=="[50,60)"] <- "[40,60)"
data$agegp10[data$agegp10 == "[60,70)" | data$agegp10=="[70,80)"] <- "[60,80)"
data$agegp10[data$agegp10 == "[80,90)" | data$agegp10=="[90,120)"] <- "[80,120)"

# Regroup region variable
data <- data %>%
  mutate(
    region = case_when(region == 'East Asia & Pacific' ~ 'EA',
                       region == 'Europe & Central Asia' ~ 'Europe & CA',
                       region == 'Latin America & Caribbean' ~ 'LA',
                       region == 'Middle East & North Africa' ~ 'MENA',
                       region == 'North America' ~ 'NAM',
                       region == 'South Asia' ~ 'SA',
                       region == 'Sub-Saharan Africa' ~ 'SSA'))

# Remove ethnicity, treatment, unknown variables, date to outcome, outcome
data <- data[, c("agegp10","sex","country", "region", "comorbid_aids_hiv", "comorbid_asthma",
                 "comorbid_chronic_cardiac_disease", "comorbid_chronic_haematological_disease",
                 "comorbid_chronic_kidney_disease", "comorbid_chronic_neurological_disorder", 
                 "comorbid_chronic_pulmonary_disease", "comorbid_dementia", "comorbid_diabetes",
                 "comorbid_hypertension", "comorbid_immunosuppression", "comorbid_liver_disease", 
                 "comorbid_malignant_neoplasm", "comorbid_malnutrition", "comorbid_obesity",
                 "comorbid_rheumatologic_disorder", "comorbid_smoking", "comorbid_tuberculosis",
                 "symptomatic", "symptoms_abdominal_pain", "symptoms_altered_consciousness_confusion",
                 "symptoms_bleeding", "symptoms_chest_pain", "symptoms_conjunctivitis", 
                 "symptoms_cough", "symptoms_diarrhoea", "symptoms_ear_pain", "symptoms_fatigue_malaise",
                 "symptoms_headache", "symptoms_history_of_fever", "symptoms_lost_altered_sense_of_smell",
                 "symptoms_lost_altered_sense_of_taste", "symptoms_lymphadenopathy",
                 "symptoms_muscle_aches_joint_pain", "symptoms_runny_nose", "symptoms_seizures",
                 "symptoms_severe_dehydration", "symptoms_shortness_of_breath", "symptoms_skin_rash",
                 "symptoms_sore_throat", "symptoms_vomiting_nausea", "symptoms_wheezing", "PE",
                 "lborres_lab_ddimer", "lab_alt", "lab_bili", "lab_crp",
                 "lab_lym", "lab_neut", "lab_pt", "lab_urean", "lab_wbc", "vs_diabp", "vs_hr",
                 "vs_resp", "vs_sysbp", "vs_temp", "vs_oxysat", "death")] # 734282 samples, 63 total variables

# Save data file as the first dataset without lab measurements
write.csv(data, "data_death.csv")

