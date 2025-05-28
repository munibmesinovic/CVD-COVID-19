setwd("~/Desktop/COVID/data/")

invisible(lapply(c("tidyr","dplyr","plyr","ggplot2","reshape2",
                   "naniar","stringr","scales","lubridate",
                   "Hmisc","corrplot", "RColorBrewer"),
                 library, character.only = TRUE))

# load the dataset
data = read.csv("import.tbl_partner2.csv")

# The names of the variables
colnames(data)

# Summary of the data
summary(data)

# Pie chart of outcomes of interest
# Simple Pie Chart
slices <- c(10, 12,4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie(slices, labels = lbls, main="Pie Chart of Outcomes")

# Investigate missingness in lab measurements
missing_alt <- data[is.na(data$lab_alt),] %>%
  group_by(country, lab_alt) %>% 
  tally()

# Plots of each of the variables with the first bin being missing (if categorical)
# Outcome
ggplot(data, aes(x=outcome)) + geom_bar(fill = "steelblue4") + 
  labs(x='Outcome') + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Outcome",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(outcome) %>% 
  tally()
# PE
ggplot(data, aes(x=pulmonary_embolism)) + geom_bar(fill = "steelblue4") + 
  labs(x='PE') + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "PE",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(pulmonary_embolism) %>% 
  tally()
# PE with DVT
ggplot(data, aes(x=pulmonary_embolism_or_dvt)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "PE with DVT",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(pulmonary_embolism_or_dvt) %>% 
  tally()

data %>%
  group_by(pulmonary_embolism_or_dvt, pulmonary_embolism, thromboembolsim) %>% 
  tally()
# Clinical COVID Diagnosis
ggplot(data, aes(x=clin_diag_covid_19)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Clinical COVID Diagnosis",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(clin_diag_covid_19) %>% 
  tally()
# Sex
ggplot(data, aes(x=sex)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Sex",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(sex) %>% 
  tally()
# Ethnicity
summary <- ddply(data,.(ethnic),summarise,freq=length(ethnic))
ethnic <- c("White", "South Asian", "Malay", "Latin American", "East Asian", "NA")
freq <- c(12339, 9256, 3812, 2743, 1395, 762827)

data_ethnic <- data.frame(ethnic, freq)
data_ethnic$ethnic <- factor(data_ethnic$ethnic)

ggplot(data_ethnic, aes(x=ethnic, y=freq)) + geom_bar(stat='identity', fill = "steelblue4") +
  theme_bw() + theme(axis.text.x = element_text(colour = "black"),
                     axis.text.y = element_text(colour = "black")) +
  labs(x = "Ethnicity",
       y = "Frequency")

data %>%
  group_by(ethnic) %>% 
  tally()
# Country
countries_less_than_60 <- data %>%
  group_by(country) %>% 
  tally() %>%
  arrange((n))

ggplot(data, aes(x=country)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Country",
       y = "Frequency") + ylim(0, 100)

summary <- ddply(data,.(country),summarise,freq=length(country))
country <- c("South Africa", "United Kingdom", "Spain", "Norway", "Pakistan", "NA")
freq <- c(432596, 273149, 15341, 7463, 7422, 3)

data_country <- data.frame(country, freq)
data_country$country <- factor(data_country$country)

ggplot(data_country, aes(x=country, y=freq)) + geom_bar(stat='identity', fill = "steelblue4") +
  theme_bw() + theme(axis.text.x = element_text(colour = "black"),
                     axis.text.y = element_text(colour = "black")) +
  labs(x = "Country",
       y = "Frequency")
# Age
ggplot(data, aes(x=age)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Age") +
  theme(legend.position="bottom")
# Income
ggplot(data, aes(x=income)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Income",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(income) %>% 
  tally()
# Region
data <- data %>%
  mutate(
    region = case_when(region == 'East Asia & Pacific' ~ 'EA',
    region == 'Europe & Central Asia' ~ 'Europe & CA',
    region == 'Latin America & Caribbean' ~ 'LA',
    region == 'Middle East & North Africa' ~ 'MENA',
    region == 'North America' ~ 'NAM',
    region == 'South Asia' ~ 'SA',
    region == 'Sub-Saharan Africa' ~ 'SSA'))

ggplot(data, aes(x=region)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Region",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(region) %>% 
  tally()
# COVIDETCRONAVR
ggplot(data, aes(x=cov_det_cronavir)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "COVDETCRONAVIR",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(cov_det_cronavir) %>% 
  tally()
# COVIDETSARSCOV2
ggplot(data, aes(x=cov_det_sarscov2)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "COVDETSARSCOV2",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(cov_det_sarscov2) %>% 
  tally()
# COVIDCRONAVIR
ggplot(data, aes(x=cov_id_cronavir)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "COVIDCRONAVIR",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(cov_id_cronavir) %>% 
  tally()
# COVIDSARSCOV2
ggplot(data, aes(x=cov_id_sarscov2)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "COVIDSARSCOV2",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(cov_id_sarscov2) %>% 
  tally()
# COVDETID
ggplot(data, aes(x=cov_det_id)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "COVDETID",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(cov_det_id) %>% 
  tally()
# AIDS COMORBIDITY
ggplot(data, aes(x=comorbid_aids_hiv)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "AIDS",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_aids_hiv) %>% 
  tally()
# Comorbidity Asplenia
ggplot(data, aes(x=comorbid_asplenia)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Asplenia",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_asplenia) %>% 
  tally()
# Comorbidity Asthma
ggplot(data, aes(x=comorbid_asthma)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Asthma",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_asthma) %>% 
  tally()
# Comorbidity Autoimmune
ggplot(data, aes(x=comorbid_autoimmune_disease)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Autoimmune Disease",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_autoimmune_disease) %>% 
  tally()
# Comorbidity Chronic Cardiac Disease
ggplot(data, aes(x=comorbid_chronic_cardiac_disease)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Chronic Cardiac Disease",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_chronic_cardiac_disease) %>% 
  tally()
# Comorbidity Chronic Haematological Disease
ggplot(data, aes(x=comorbid_chronic_haematological_disease)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Chronic Haematological Disease",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_chronic_haematological_disease) %>% 
  tally()
# Comorbidity Chronic Inflammatory Condition
ggplot(data, aes(x=comorbid_chronic_inflammatory_condition)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Chronic Inflammatory Condition",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_chronic_inflammatory_condition) %>% 
  tally()
# Comorbidity Chronic Kidney Disease
ggplot(data, aes(x=comorbid_chronic_kidney_disease)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Chronic Kidney Disease",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_chronic_kidney_disease) %>% 
  tally()
# Comorbidity Chronic Neurological Disorder
ggplot(data, aes(x=comorbid_chronic_neurological_disorder)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Chronic Neurological Disorder",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_chronic_neurological_disorder) %>% 
  tally()
# Comorbidity Chronic Pulmonary Disease
ggplot(data, aes(x=comorbid_chronic_pulmonary_disease)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Chronic Pulmonary Disease",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_chronic_pulmonary_disease) %>% 
  tally()
# Comorbidity Dementia
ggplot(data, aes(x=comorbid_dementia)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Dementia",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_dementia) %>% 
  tally()
# Comorbidity Diabetes
ggplot(data, aes(x=comorbid_diabetes)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Diabetes",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_diabetes) %>% 
  tally()
# Comorbidity Gastrointestinal Bleeding
ggplot(data, aes(x=comorbid_gastrointestinal_bleeding)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Gastrointestinal Bleeding",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_gastrointestinal_bleeding) %>% 
  tally()
# Comorbidity Hypertension
ggplot(data, aes(x=comorbid_hypertension)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Hypertension",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_hypertension) %>% 
  tally()
# Comorbidity Immunosuppression
ggplot(data, aes(x=comorbid_immunosuppression)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Immunosuppression",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_immunosuppression) %>% 
  tally()
# Comorbidity Lipid Disorder
ggplot(data, aes(x=comorbid_lipid_disorder)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Lipid Disorder",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_lipid_disorder) %>% 
  tally()
# Comorbidity Liver Disease
ggplot(data, aes(x=comorbid_liver_disease)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Liver Disease",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_liver_disease) %>% 
  tally()
# Comorbidity Malignant Neoplasm
ggplot(data, aes(x=comorbid_malignant_neoplasm)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Malignant Neoplasm",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_malignant_neoplasm) %>% 
  tally()
# Comorbidity Malnutrition
ggplot(data, aes(x=comorbid_malnutrition)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Malnutrition",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_malnutrition) %>% 
  tally()
# Comorbidity Mental Disorder
ggplot(data, aes(x=comorbid_mental_disorder)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Mental Disorder",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_mental_disorder) %>% 
  tally()
# Comorbidity Myocardial Infarction
ggplot(data, aes(x=comorbid_myocardial_infarction)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Myocardial Infarction",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_myocardial_infarction) %>% 
  tally()
# Comorbidity Obesity
ggplot(data, aes(x=comorbid_obesity)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Obesity",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_obesity) %>% 
  tally()
# Comorbidity Other Chronic Disease
ggplot(data, aes(x=comorbid_other_chronic_disease)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Other Chronic Disease",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_other_chronic_disease) %>% 
  tally()
# Comorbidity Other Comorbidities
ggplot(data, aes(x=comorbid_other_comorbidities)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Other Comorbidities",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_other_comorbidities) %>% 
  tally()
# Comorbidity Peripheral Vascular Disease
ggplot(data, aes(x=comorbid_peripheral_vascular_disease)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Peripheral Vascular Disease",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_peripheral_vascular_disease) %>% 
  tally()
# Comorbidity Previous COVID-19
ggplot(data, aes(x=comorbid_previous_covid.19_infection)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Previous COVID-19",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_previous_covid.19_infection) %>% 
  tally()
# Comorbidity Rare Diseases and Inborn Errors
ggplot(data, aes(x=comorbid_rare_diseases_and_inborn_errors_of_metabolism)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Rare Diseases and Inborn Errors",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_rare_diseases_and_inborn_errors_of_metabolism) %>% 
  tally()
# Comorbidity Rheumatologic Disorder
ggplot(data, aes(x=comorbid_rheumatologic_disorder)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Rheumatologic Disorder",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_rheumatologic_disorder) %>% 
  tally()
# Comorbidity Smoking
ggplot(data, aes(x=comorbid_smoking)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Smoking",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_smoking) %>% 
  tally()
# Comorbidity Transplantation
ggplot(data, aes(x=comorbid_transplantation)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Transplantation",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_transplantation) %>% 
  tally()
# Comorbidity Tuberculosis
ggplot(data, aes(x=comorbid_tuberculosis)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Tuberculosis",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(comorbid_tuberculosis) %>% 
  tally()
# Date of onset
ggplot(data, aes(x=date_onset)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Date of onset") +
  theme(legend.position="bottom")
# Clinical Diagnosis of COVID-19
ggplot(data, aes(x=clin_diag_covid_19)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Clinical Diagnosis of COVID-19",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(clin_diag_covid_19) %>% 
  tally()
# Symptomatic
ggplot(data, aes(x=symptomatic)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptomatic) %>% 
  tally()
# Symptoms Ambdominal Pain
ggplot(data, aes(x=symptoms_abdominal_pain)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Ambdominal Pain",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_abdominal_pain) %>% 
  tally()
# Symptoms Acute Kidney Injury
ggplot(data, aes(x=symptoms_acute_kidney_injury)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Acute Kidney Injury",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_acute_kidney_injury) %>% 
  tally()
# Symptoms Altered Consciousness Confusion
ggplot(data, aes(x=symptoms_altered_consciousness_confusion)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Altered Consciousness Confusion",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_altered_consciousness_confusion) %>% 
  tally()
# Symptoms Anorexia
ggplot(data, aes(x=symptoms_anorexia)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Anorexia",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_anorexia) %>% 
  tally()
# Symptoms Apnoea
ggplot(data, aes(x=symptoms_apnoea)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Apnoea",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_apnoea) %>% 
  tally()
# Symptoms acute respiratory distress syndrom
ggplot(data, aes(x=symptoms_ards)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms acute respiratory distress syndrom",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_ards) %>% 
  tally()
# Symptoms Asymptomatic
ggplot(data, aes(x=symptoms_asymptomatic)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Asymptomatic",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_asymptomatic) %>% 
  tally()
# Symptoms Atrial Fibrillation
ggplot(data, aes(x=symptoms_atrial_fibrillation)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Atrial Fibrillation",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_atrial_fibrillation) %>% 
  tally()
# Symptoms Bactaraemia
ggplot(data, aes(x=symptoms_bacteraemia)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Bactaraemia",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_bacteraemia) %>% 
  tally()
# Symptoms Bacterial Pneumonia
ggplot(data, aes(x=symptoms_bacterial_pneumonia)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Bacterial Pneumonia",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_bacterial_pneumonia) %>% 
  tally()
# Symptoms Bleeding
ggplot(data, aes(x=symptoms_bleeding)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Bleeding",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_bleeding) %>% 
  tally()
# Symptoms Cardiac Arrest
ggplot(data, aes(x=symptoms_cardiac_arrest)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Cardiac Arrest",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_cardiac_arrest) %>% 
  tally()
# Symptoms Chest Pain
ggplot(data, aes(x=symptoms_chest_pain)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Chest Pain",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_chest_pain) %>% 
  tally()
# Symptoms Chronic Cardiac Disease
ggplot(data, aes(x=symptoms_chronic_cardiac_disease)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Chronic Cardiac Disease",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_chronic_cardiac_disease) %>% 
  tally()
# Symptoms Comorbidities
ggplot(data, aes(x=symptoms_comorbidities)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Comorbidities",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_comorbidities) %>% 
  tally()
# Symptoms Conjunctivitis
ggplot(data, aes(x=symptoms_conjunctivitis)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Conjunctivitis",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_conjunctivitis) %>% 
  tally()
# Symptoms Convulsions
ggplot(data, aes(x=symptoms_convulsions)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Convulsions",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_convulsions) %>% 
  tally()
# Symptoms Cough
ggplot(data, aes(x=symptoms_cough)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Cough",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_cough) %>% 
  tally()
# Symptoms Crepitant
ggplot(data, aes(x=symptoms_crepitant)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Crepitant",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_crepitant) %>% 
  tally()
# Symptoms Diabetic Ketoacidosis
ggplot(data, aes(x=symptoms_diabetic_ketoacidosis)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Diabetic Ketoacidosis",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_diabetic_ketoacidosis) %>% 
  tally()
# Symptoms Diarrhoea
ggplot(data, aes(x=symptoms_diarrhoea)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Diarrhoea",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_diarrhoea) %>% 
  tally()
# Symptoms Disseminated Intravascular Coagulation
ggplot(data, aes(x=symptoms_disseminated_intravascular_coagulation)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Disseminated Intravascular Coagulation",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_disseminated_intravascular_coagulation) %>% 
  tally()
# Symptoms Dizziness
ggplot(data, aes(x=symptoms_dizziness)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Dizziness",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_dizziness) %>% 
  tally()
# Symptoms Ear Pain
ggplot(data, aes(x=symptoms_ear_pain)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Ear Pain",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_ear_pain) %>% 
  tally()
# Symptoms Encephalopathy
ggplot(data, aes(x=symptoms_encephalopathy)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Encephalopathyn",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_encephalopathy) %>% 
  tally()
# Symptoms Fatigue Malaise
ggplot(data, aes(x=symptoms_fatigue_malaise)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Fatigue Malaise",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_fatigue_malaise) %>% 
  tally()
# Symptoms Gastrointestinal Bleeding
ggplot(data, aes(x=symptoms_gastrointestinal_bleeding)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Gastrointestinal Bleeding",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_gastrointestinal_bleeding) %>% 
  tally()
# Symptoms Haemoglobinuria
ggplot(data, aes(x=symptoms_haemoglobinuria)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Haemoglobinuria",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_haemoglobinuria) %>% 
  tally()
# Symptoms Headache
ggplot(data, aes(x=symptoms_headache)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Headache",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_headache) %>% 
  tally()
# Symptoms Hematuria
ggplot(data, aes(x=symptoms_hematuria)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Hematuria",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_hematuria) %>% 
  tally()
# Symptoms History of Fever
ggplot(data, aes(x=symptoms_history_of_fever)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms History of Fever",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_history_of_fever) %>% 
  tally()
# Symptoms Hyperglycaemia
ggplot(data, aes(x=symptoms_hyperglycaemia)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Hyperglycaemia",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_hyperglycaemia) %>% 
  tally()
# Symptoms Hypoglycaemia
ggplot(data, aes(x=symptoms_hypoglycaemia)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Hypoglycaemia",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_hypoglycaemia) %>% 
  tally()
# Symptoms Inability to Walk
ggplot(data, aes(x=symptoms_inability_to_walk)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Inability to Walk",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_inability_to_walk) %>% 
  tally()
# Symptoms Irritability
ggplot(data, aes(x=symptoms_irritability)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Irritability",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_irritability) %>% 
  tally()
# Symptoms Leukocyturia
ggplot(data, aes(x=symptoms_leukocyturia)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Leukocyturia",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_leukocyturia) %>% 
  tally()
# Symptoms Liver Disease
ggplot(data, aes(x=symptoms_liver_disease)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Liver Disease",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_liver_disease) %>% 
  tally()
# Symptoms Lost or Altered Sense of Smell
ggplot(data, aes(x=symptoms_lost_altered_sense_of_smell)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Lost or Altered Sense of Smell",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_lost_altered_sense_of_smell) %>% 
  tally()
# Symptoms Lost or Altered Sense of Taste
ggplot(data, aes(x=symptoms_lost_altered_sense_of_taste)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Lost or Altered Sense of Taste",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_lost_altered_sense_of_taste) %>% 
  tally()
# Symptoms Lymphadenopathy
ggplot(data, aes(x=symptoms_lymphadenopathy)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Lymphadenopathy",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_lymphadenopathy) %>% 
  tally()
# Symptoms Meningitis Encephalitis
ggplot(data, aes(x=symptoms_meningitis_encephalitis)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Meningitis Encephalitisy",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_meningitis_encephalitis) %>% 
  tally()
# Symptoms Mouth Ulcers
ggplot(data, aes(x=symptoms_mouth_ulcers)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Mouth Ulcers",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_mouth_ulcers) %>% 
  tally()
# Symptoms Muscle Aches and Joint Pains
ggplot(data, aes(x=symptoms_muscle_aches_joint_pain)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Muscle Aches and Joint Pains",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_muscle_aches_joint_pain) %>% 
  tally()
# Symptoms Myocardial Infarction
ggplot(data, aes(x=symptoms_myocardial_infarction)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Myocardial Infarction",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_myocardial_infarction) %>% 
  tally()
# Symptoms Myocarditis
ggplot(data, aes(x=symptoms_myocarditis)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Myocarditis",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_myocarditis) %>% 
  tally()
# Symptoms Nausea
ggplot(data, aes(x=symptoms_nausea)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Nausea",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_nausea) %>% 
  tally()
# Symptoms Other Signs
ggplot(data, aes(x=symptoms_other_signs)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Other Signs",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_other_signs) %>% 
  tally()
# Symptoms Other Signs and Symptoms
ggplot(data, aes(x=symptoms_other_signs_and_symptoms)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Other Signs and Symptoms",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_other_signs_and_symptoms) %>% 
  tally()
# Symptoms Pharyngeal Exudate
ggplot(data, aes(x=symptoms_pharyngeal_exudate)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Pharyngeal Exudate",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_pharyngeal_exudate) %>% 
  tally()
# Symptoms Pleural Effusion
ggplot(data, aes(x=symptoms_pleural_effusion)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Pleural Effusion",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_pleural_effusion) %>% 
  tally()
# Symptoms Pretibial Edema or Limb Peeling
ggplot(data, aes(x=symptoms_pretibial_edema_or_limb_peeling)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Pretibial Edema or Limb Peeling",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_pretibial_edema_or_limb_peeling) %>% 
  tally()
# Symptoms Proteinuria
ggplot(data, aes(x=symptoms_proteinuria)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Proteinuria",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_proteinuria) %>% 
  tally()
# Symptoms Rhabdomyolysis
ggplot(data, aes(x=symptoms_rhabdomyolysis)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Rhabdomyolysis",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_rhabdomyolysis) %>% 
  tally()
# Symptoms Ronchus
ggplot(data, aes(x=symptoms_ronchus)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Ronchus",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_ronchus) %>% 
  tally()
# Symptoms Runny Nose
ggplot(data, aes(x=symptoms_runny_nose)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Runny Nose",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_runny_nose) %>% 
  tally()
# Symptoms Seizures
ggplot(data, aes(x=symptoms_seizures)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Seizures",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_seizures) %>% 
  tally()
# Symptoms Sepsis
ggplot(data, aes(x=symptoms_sepsis)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Sepsis",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_sepsis) %>% 
  tally()
# Symptoms Severe Dehydration
ggplot(data, aes(x=symptoms_severe_dehydration)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Severe Dehydration",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_severe_dehydration) %>% 
  tally()
# Symptoms Shock
ggplot(data, aes(x=symptoms_shock)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Shock",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_shock) %>% 
  tally()
# Symptoms Shortness of Breath
ggplot(data, aes(x=symptoms_shortness_of_breath)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Shortness of Breath",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_shortness_of_breath) %>% 
  tally()
# Symptoms Skin Rash
ggplot(data, aes(x=symptoms_skin_rash)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Skin Rash",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_skin_rash) %>% 
  tally()
# Symptoms Sore Throat
ggplot(data, aes(x=symptoms_sore_throat)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Sore Throat",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_sore_throat) %>% 
  tally()
# Symptoms Strawberry Tongue
ggplot(data, aes(x=symptoms_strawberry_tongue)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Strawberry Tongue",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_strawberry_tongue) %>% 
  tally()
# Symptoms Stroke
ggplot(data, aes(x=symptoms_stroke)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Stroke",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_stroke) %>% 
  tally()
# Symptoms Sub Crepitant
ggplot(data, aes(x=symptoms_sub_crepitant)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Sub Crepitant",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_sub_crepitant) %>% 
  tally()
# Symptoms Transplantation
ggplot(data, aes(x=symptoms_transplantation)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Transplantation",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_transplantation) %>% 
  tally()
# Symptoms Upper Respiratory Tract
ggplot(data, aes(x=symptoms_upper_respiratory_tract_symptoms)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Upper Respiratory Tract",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_upper_respiratory_tract_symptoms) %>% 
  tally()
# Symptoms Vomiting Nausea
ggplot(data, aes(x=symptoms_vomiting_nausea)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Vomiting Nausea",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_vomiting_nausea) %>% 
  tally()
# Symptoms Wheezing
ggplot(data, aes(x=symptoms_wheezing)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Wheezing",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_wheezing) %>% 
  tally()
# Symptoms Wheezing
ggplot(data, aes(x=symptoms_wheezing)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Wheezing",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptoms_wheezing) %>% 
  tally()
# Treatment Extracorporeal
ggplot(data, aes(x=treat_extracorporeal)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Extracorporeal",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(treat_extracorporeal) %>% 
  tally()
# Treatment High Flow Nasal Cannula
ggplot(data, aes(x=treat_high_flow_nasal_cannula)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment High Flow Nasal Cannula",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(treat_high_flow_nasal_cannula) %>% 
  tally()
# Treatment Inhaled Nitric Oxide
ggplot(data, aes(x=treat_inhaled_nitric_oxide)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Inhaled Nitric Oxide",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(treat_inhaled_nitric_oxide) %>% 
  tally()
# Treatment Invasive Ventilation
ggplot(data, aes(x=treat_invasive_ventilation)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Invasive Ventilation",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(treat_invasive_ventilation) %>% 
  tally()
# Treatment Mask Oxygen Therapy
ggplot(data, aes(x=treat_mask_oxygen_therapy)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Mask Oxygen Therapy",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(treat_mask_oxygen_therapy) %>% 
  tally()
# Treatment Nasal Oxygen Therapy
ggplot(data, aes(x=treat_nasal_oxygen_therapy)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Nasal Oxygen Therapy",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(treat_nasal_oxygen_therapy) %>% 
  tally()
# Treatment Noninvasive Ventilation
ggplot(data, aes(x=treat_non_invasive_ventilation)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Noninvasive Ventilation",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(treat_non_invasive_ventilation) %>% 
  tally()
# Treatment Oxygen Therapy
ggplot(data, aes(x=treat_oxygen_therapy)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Oxygen Therapy",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(treat_oxygen_therapy) %>% 
  tally()
# Treatment Respiratory Support
ggplot(data, aes(x=treat_respiratory_support)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Respiratory Support",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(treat_respiratory_support) %>% 
  tally()
# Treatment Tracheostomy
ggplot(data, aes(x=treat_tracheostomy)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Tracheostomy",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(treat_tracheostomy) %>% 
  tally()
# Treatment Oxygen Therapy 2 (same column values)
ggplot(data, aes(x=treat_oxygen_therapy)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Oxygen Therapy 2",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(treat_oxygen_therapy) %>% 
  tally()
# Treatment d1 Oxygen Therapy 3
ggplot(data, aes(x=d1_oxygen_therapy)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Oxygen Therapy 3",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(d1_oxygen_therapy) %>% 
  tally()
# Duration of Invasive Ventilation
ggplot(data, aes(x=dur_imv)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Duration of Invasive Ventilation") +
  theme(legend.position="bottom")
sum(!is.na(data$dur_imv))
# Duration of Noninvasive Ventilation
ggplot(data, aes(x=dur_niv)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Duration of Noninvasive Ventilation") +
  theme(legend.position="bottom")
sum(!is.na(data$dur_niv))
# ICU stay
ggplot(data, aes(x=ever_icu)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "ICU Stay",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(ever_icu) %>% 
  tally()
# Hody?
ggplot(data, aes(x=hody)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Hody") +
  theme(legend.position="bottom")
sum(!is.na(data$hody))
# Hostdy?
ggplot(data, aes(x=hostdy)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Hostdy") +
  theme(legend.position="bottom")
sum(!is.na(data$hostdy))
# Hoendy?
ggplot(data, aes(x=hoendy)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Hoendy") +
  theme(legend.position="bottom")
sum(!is.na(data$hoendy))
# Hodur
summary <- ddply(data,.(hodur),summarise,freq=length(hodur))
hodur <- c("P2D", "P1D", "P3D", "P4D", "P5D", "NA")
freq <- c(1518, 1345, 1312, 1162, 943, 786418)

data_ethnic <- data.frame(hodur, freq)
data_ethnic$hodur <- factor(data_ethnic$hodur)

ggplot(data_ethnic, aes(x=hodur, y=freq)) + geom_bar(stat='identity', fill = "steelblue4") +
  theme_bw() + theme(axis.text.x = element_text(colour = "black"),
                     axis.text.y = element_text(colour = "black")) +
  labs(x = "Hodur",
       y = "Frequency")

data %>%
  group_by(hodur) %>% 
  tally()
# Hocdstdy?
ggplot(data, aes(x=hocdstdy)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Hocdstdy") +
  theme(legend.position="bottom")
sum(!is.na(data$hocdstdy))
# ICU Treatment Extracorporeal
ggplot(data, aes(x=icu_treat_extracorporeal)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "ICU Treatment Extracorporeal",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(icu_treat_extracorporeal) %>% 
  tally()
# ICU Treatment High Flow Nasal Cannula
ggplot(data, aes(x=icu_treat_high_flow_nasal_cannula)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "ICU Treatment High Flow Nasal Cannula",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(icu_treat_high_flow_nasal_cannula) %>% 
  tally()
# ICU Treatment Inhaled Nitric Oxide
ggplot(data, aes(x=icu_treat_inhaled_nitric_oxide)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "ICU Treatment Inhaled Nitric Oxide",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(icu_treat_inhaled_nitric_oxide) %>% 
  tally()
# ICU Treatment Noninvasive Ventilation
ggplot(data, aes(x=icu_treat_non_invasive_ventilation)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "ICU Treatment Noninvasive Ventilatione",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(icu_treat_non_invasive_ventilation) %>% 
  tally()
# ICU Treatment Oxygen Therapy
ggplot(data, aes(x=icu_treat_oxy_therapy)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "ICU Treatment Oxygen Therapy",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(icu_treat_oxy_therapy) %>% 
  tally()
# ICU Treatment Tracheostomy
ggplot(data, aes(x=icu_treat_tracheostomy)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "ICU Treatment Tracheostomy",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(icu_treat_tracheostomy) %>% 
  tally()
# ICU Treatment Oxygen Therapy 2
ggplot(data, aes(x=icu_treat_oxygen_therapy)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "ICU Treatment Oxygen Therapy 2",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(icu_treat_oxygen_therapy) %>% 
  tally()
# Laboratory Alt?
ggplot(data, aes(x=lab_alt)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Laboratory Alt") +
  theme(legend.position="bottom")
sum(!is.na(data$lab_alt))
# Laboratory Aptt?
ggplot(data, aes(x=lab_aptt)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Laboratory Aptt") +
  theme(legend.position="bottom")
sum(!is.na(data$lab_aptt))
# Laboratory Ast?
ggplot(data, aes(x=lab_ast)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Laboratory Ast") +
  theme(legend.position="bottom")
sum(!is.na(data$lab_ast))
# Laboratory Bilirubin
ggplot(data, aes(x=lab_bili)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Laboratory Bilirubin") +
  theme(legend.position="bottom")
sum(!is.na(data$lab_bili))
# Laboratory CRP
ggplot(data, aes(x=lab_crp)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Laboratory CRP") +
  theme(legend.position="bottom")
sum(!is.na(data$lab_crp))
# Laboratory lymphocites
ggplot(data, aes(x=lab_lym)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Laboratory lymphocites") +
  theme(legend.position="bottom")
sum(!is.na(data$lab_lym))
# Laboratory neutrophils
ggplot(data, aes(x=lab_neut)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Laboratory neutrophils") +
  theme(legend.position="bottom")
sum(!is.na(data$lab_neut))
# Laboratory PT
ggplot(data, aes(x=lab_pt)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Laboratory PT") +
  theme(legend.position="bottom")
sum(!is.na(data$lab_pt))
# Laboratory Urean
ggplot(data, aes(x=lab_urean)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Laboratory Urean") +
  theme(legend.position="bottom")
sum(!is.na(data$lab_urean))
# Laboratory White Blood Cells
ggplot(data, aes(x=lab_wbc)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Laboratory White Blood Cells") +
  theme(legend.position="bottom")
sum(!is.na(data$lab_wbc))
# BMI
ggplot(data, aes(x=vs_bmi)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "BMI") +
  theme(legend.position="bottom")
sum(!is.na(data$vs_wbc))
# Diastolic Blood Pressure
ggplot(data, aes(x=vs_diabp)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Diastolic Blood Pressure") +
  theme(legend.position="bottom")
sum(!is.na(data$vs_diabp))
# Height
ggplot(data, aes(x=vs_height)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Height") +
  theme(legend.position="bottom")
sum(!is.na(data$vs_height))
# Heart Rate
ggplot(data, aes(x=vs_hr)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Heart Rate") +
  theme(legend.position="bottom")
sum(!is.na(data$vs_hr))
# Mean Arterial Pressure
ggplot(data, aes(x=vs_map)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Mean Arterial Pressure") +
  theme(legend.position="bottom")
sum(!is.na(data$vs_map))
# Muarmcir?
ggplot(data, aes(x=vs_muarmcir)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Muarmcir") +
  theme(legend.position="bottom")
sum(!is.na(data$vs_muarmcir))
# Oxygen Saturation during Oxygen Therapy
ggplot(data, aes(x=vs_oxysat_oxygen_therapy)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Oxygen Saturation during Oxygen Therapy") +
  theme(legend.position="bottom")
sum(!is.na(data$vs_oxysat_oxygen_therapy))
# Oxygen Saturation at Room Air
ggplot(data, aes(x=vs_oxysat_room_air)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Oxygen Saturation at Room Air") +
  theme(legend.position="bottom")
sum(!is.na(data$vs_oxysat_room_air))
# Oxygen Saturation Unknown
ggplot(data, aes(x=vs_oxysat_unknown)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Oxygen Saturation Unknown") +
  theme(legend.position="bottom")
sum(!is.na(data$vs_oxysat_unknown))
# Respiratory Rate
ggplot(data, aes(x=vs_resp)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Respiratory Rate") +
  theme(legend.position="bottom")
sum(!is.na(data$vs_resp))
# Temperature
ggplot(data, aes(x=vs_temp)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Temperature") +
  theme(legend.position="bottom")
sum(!is.na(data$vs_temp))
# Weight
ggplot(data, aes(x=vs_weight)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Weight") +
  theme(legend.position="bottom")
sum(!is.na(data$vs_weight))
# Oxygen Saturation
ggplot(data, aes(x=vs_oxysat)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Oxygen Saturation") +
  theme(legend.position="bottom")
sum(!is.na(data$vs_oxysat))
# DSCDSTDY
ggplot(data, aes(x=dscdstdy)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "DSCDSTDY",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(dscdstdy) %>% 
  tally()
# DSDECOD
data <- data %>%
  mutate(
    dsdecod = case_when(dsdecod == 'LOST TO FOLLOW-UP' ~ 'Lost',
                        dsdecod == 'ONGOING FOLLOW-UP' ~ 'Ongoing',
                        dsdecod == 'STILL IN HOSPITAL' ~ 'Treatment',
                        dsdecod == 'DEATH' ~ 'Dead',
                        dsdecod == 'DISCHARGED' ~ 'Disch',
                        dsdecod == 'TRANSFERRED' ~ 'Trans'))
ggplot(data, aes(x=dsdecod)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "DSDECOD",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(dsdecod) %>% 
  tally()
# DSDY
ggplot(data, aes(x=dsdy)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "DSDY") +
  theme(legend.position="bottom")
sum(!is.na(data$dsdy))
# DSSTDY
ggplot(data, aes(x=dsstdy)) + geom_density(alpha = 0.4) + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "DSSTDY") +
  theme(legend.position="bottom")
sum(!is.na(data$dsstdy))
# Thromboembolism
ggplot(data, aes(x=thromboembolsim)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Thromboembolism",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(thromboembolsim) %>% 
  tally()
# Symptoms Upper Respiratory Tract
ggplot(data, aes(x=symptrcd_upper_respiratory_tract_symptoms)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptoms Upper Respiratory Tract",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(symptrcd_upper_respiratory_tract_symptoms) %>% 
  tally()
# Age in Deciles
ggplot(data, aes(x=agegp10)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Age in Deciles",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)

data %>%
  group_by(agegp10) %>% 
  tally()

# Age in Quintiles
ggplot(data, aes(x=agegp5)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Age in Quintiles",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(agegp5) %>% 
  tally()

# Merge thromboembolsim and pulmonary_embolism and pulmonary_embolism_or_dvt into PE

data$PE = data$pulmonary_embolism | data$pulmonary_embolism_or_dvt | data$thromboembolsim

ggplot(data, aes(x=PE)) + geom_bar(fill = "steelblue4") + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "PE",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)
data %>%
  group_by(PE) %>% 
  tally()

# Drop the old PE columns
data <- data %>%
  select(-one_of(pulmonary_embolism, pulmonary_embolism_or_dvt, thromboembolsim))

# PE
ggplot(data, aes(x=PE)) + geom_bar(fill = "steelblue4") + 
  labs(x='PE') + theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "PE",
       y = "Frequency") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3)

# Get the subset of data matching each patient group (target and control)
data_true_PE <- subset(data, PE == TRUE)
data_false_PE <- subset(data, is.na(PE))
# data_false_PE <- subset(data, PE == FALSE)

### Age
data %>%
  ggplot(aes(x = age, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "Age Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = age, fill = factor(PE))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'PE',
       x = "Age at Admission",
       y = "Density",
       title = "Density of Age by PE") +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7),
        axis.title.x = element_text(colour = "black", size = 11),
        axis.title.y = element_text(colour = "black", size = 11),
        legend.text = element_text(colour = "black", size = 11),
        title = element_text(colour = "black", size = 11)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data$age))
sum(is.na(data_true_PE$age))
sum(is.na(data_false_PE$age))

sum(data$age == "")
sum(data_true_PE$age == "")
sum(data_false_PE$age == "")

summary(data_true_PE)
summary(data_false_PE)

### Sex
data %>%
  ggplot(aes(x = sex)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Sex Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = factor(sex), fill = factor(PE))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 10),
        axis.text.y = element_text(colour = "black", size = 10)) +
  labs(x = "Sex",
       y = "Proportion",
       fill = "PE") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(PE, sex) %>% 
  tally()

sum(is.na(data$sex))
sum(is.na(data_true_PE$sex))
sum(is.na(data_false_PE$sex))

sum(data$sex == "")
sum(data_true_PE$sex == "")
sum(data_false_PE$sex == "")

### Ethnicity
summary <- ddply(data_true_PE,.(ethnic),summarise,freq=length(ethnic))
ethnic <- c("White", "South Asian", "Malay", "Latin American", "East Asian", "NA")
freq <- c(309, 32, 0, 24, 9, 5171)

data_ethnic <- data.frame(ethnic, freq)
data_ethnic$ethnic <- factor(data_ethnic$ethnic)

ggplot(data_ethnic, aes(x=ethnic, y=freq)) + geom_bar(stat='identity', fill = "steelblue4") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Ethnicity Given PE",
       y = "Frequency") + coord_cartesian(ylim=c(0,10000))

summary <- ddply(data_false_PE,.(ethnic),summarise,freq=length(ethnic))
ethnic <- c("White", "South Asian", "Malay", "Latin American", "East Asian", "NA")
freq <- c(12030, 9224, 3812, 2719, 1386, 757656)

data_ethnic <- data.frame(ethnic, freq)
data_ethnic$ethnic <- factor(data_ethnic$ethnic)

ggplot(data_ethnic, aes(x=ethnic, y=freq)) + geom_bar(stat='identity', fill = "steelblue4") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Ethnicity Given no PE",
       y = "Frequency") + coord_cartesian(ylim=c(0,100000))

sum(data$ethnic == "")
sum(data_true_PE$ethnic == "")
sum(data_false_PE$ethnic == "")

### Country
summary <- ddply(data_true_PE,.(country),summarise,freq=length(country))
country <- c("South Africa", "United Kingdom", "Spain", "Norway", "Pakistan", "NA")
freq <- c(0, 4076, 577, 15, 22, 0)

data_ethnic <- data.frame(country, freq)
data_ethnic$country <- factor(data_ethnic$country)

ggplot(data_ethnic, aes(x=country, y=freq)) + geom_bar(stat='identity', fill = "steelblue4") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Country Given PE",
       y = "Frequency") + coord_cartesian(ylim=c(0,4500))

summary <- ddply(data_false_PE,.(country),summarise,freq=length(country))
country <- c("South Africa", "United Kingdom", "Spain", "Norway", "Pakistan", "NA")
freq <- c(432596, 269073, 14764, 7448, 7400, 3)

data_ethnic <- data.frame(country, freq)
data_ethnic$country <- factor(data_ethnic$country)

ggplot(data_ethnic, aes(x=country, y=freq)) + geom_bar(stat='identity', fill = "steelblue4") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Country Given no PE",
       y = "Frequency") + coord_cartesian(ylim=c(0,450000))

sum(data$country == "")
sum(data_true_PE$country == "")
sum(data_false_PE$country == "")

sum(is.na(data$country))
sum(is.na(data_true_PE$country))
sum(is.na(data_false_PE$country))

### Income
data_true_PE <- data_true_PE %>%
  mutate(
    income = case_when(income == 'High income' ~ 'HI',
                       income == 'Low income' ~ 'LI',
                       income == 'Lower middle income' ~ 'LMI',
                       income == 'Upper middle income' ~ 'UMI'))

data_false_PE <- data_false_PE %>%
  mutate(
    income = case_when(income == 'High income' ~ 'HI',
                       income == 'Low income' ~ 'LI',
                       income == 'Lower middle income' ~ 'LMI',
                       income == 'Upper middle income' ~ 'UMI'))

data_true_PE %>%
  ggplot(aes(x = income)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Income Given PE",
       y = "Frequency")

data_false_PE %>%
  ggplot(aes(x = income)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Income Given no PE",
       y = "Frequency")

data_true_PE %>%
  group_by(PE, income) %>% 
  tally()

data_false_PE %>%
  group_by(PE, income) %>% 
  tally()

sum(data$income == "")
sum(data_true_PE$income == "")
sum(data_false_PE$income == "")

sum(is.na(data$income))
sum(is.na(data_true_PE$income))
sum(is.na(data_false_PE$income))

### Region
data_true_PE <- data_true_PE %>%
  mutate(
    region = case_when(region == 'East Asia & Pacific' ~ 'EA',
                       region == 'Europe & Central Asia' ~ 'Europe & CA',
                       region == 'Latin America & Caribbean' ~ 'LA',
                       region == 'Middle East & North Africa' ~ 'MENA',
                       region == 'North America' ~ 'NAM',
                       region == 'South Asia' ~ 'SA',
                       region == 'Sub-Saharan Africa' ~ 'SSA'))

data_false_PE <- data_false_PE %>%
  mutate(
    region = case_when(region == 'East Asia & Pacific' ~ 'EA',
                       region == 'Europe & Central Asia' ~ 'Europe & CA',
                       region == 'Latin America & Caribbean' ~ 'LA',
                       region == 'Middle East & North Africa' ~ 'MENA',
                       region == 'North America' ~ 'NAM',
                       region == 'South Asia' ~ 'SA',
                       region == 'Sub-Saharan Africa' ~ 'SSA'))

data_true_PE %>%
  ggplot(aes(x = region)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Region Given PE",
       y = "Frequency")

data_false_PE %>%
  ggplot(aes(x = region)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Region Given no PE",
       y = "Frequency")

data_true_PE %>%
  group_by(PE, region) %>% 
  tally()

data_false_PE %>%
  group_by(PE, region) %>% 
  tally()

sum(data$region == "")
sum(data_true_PE$region == "")
sum(data_false_PE$region == "")

sum(is.na(data$region))
sum(is.na(data_true_PE$region))
sum(is.na(data_false_PE$region))

### AIDS COMORBIDITY
data %>%
  ggplot(aes(x = comorbid_aids_hiv)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "AIDS/HIV Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_aids_hiv) %>% 
  tally()

sum(is.na(data$comorbid_aids_hiv))
sum(is.na(data_true_PE$comorbid_aids_hiv))
sum(is.na(data_false_PE$comorbid_aids_hiv))

sum(data$comorbid_aids_hiv == "")
sum(data_true_PE$comorbid_aids_hiv == "")
sum(data_false_PE$comorbid_aids_hiv == "")

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_aids_hiv))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "AIDS") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Asthma
data %>%
  ggplot(aes(x = comorbid_asthma)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Asthma Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_asthma) %>% 
  tally()

sum(is.na(data$comorbid_asthma))
sum(is.na(data_true_PE$comorbid_asthma))
sum(is.na(data_false_PE$comorbid_asthma))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_asthma))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Asthma") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Chronic Cardiac
data %>%
  ggplot(aes(x = comorbid_chronic_cardiac_disease)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Chronic Cardiac Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_chronic_cardiac_disease) %>% 
  tally()

sum(is.na(data$comorbid_chronic_cardiac_disease))
sum(is.na(data_true_PE$comorbid_chronic_cardiac_disease))
sum(is.na(data_false_PE$comorbid_chronic_cardiac_disease))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_chronic_cardiac_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Chronic Cardiac Disease") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Haematological
data %>%
  ggplot(aes(x = comorbid_chronic_haematological_disease)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Chronic Haematologic Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_chronic_haematological_disease) %>% 
  tally()

sum(is.na(data$comorbid_chronic_haematological_disease))
sum(is.na(data_true_PE$comorbid_chronic_haematological_disease))
sum(is.na(data_false_PE$comorbid_chronic_haematological_disease))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_chronic_haematological_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Chronic Haematological Disease") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Chronic Kidney
data %>%
  ggplot(aes(x = comorbid_chronic_kidney_disease)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Chronic Kidney Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_chronic_kidney_disease) %>% 
  tally()

sum(is.na(data$comorbid_chronic_kidney_disease))
sum(is.na(data_true_PE$comorbid_chronic_kidney_disease))
sum(is.na(data_false_PE$comorbid_chronic_kidney_disease))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_chronic_kidney_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Chronic Kidney Disease") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Chronic Neurological
data %>%
  ggplot(aes(x = comorbid_chronic_neurological_disorder)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Chronic Neurological Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_chronic_neurological_disorder) %>% 
  tally()

sum(is.na(data$comorbid_chronic_neurological_disorder))
sum(is.na(data_true_PE$comorbid_chronic_neurological_disorder))
sum(is.na(data_false_PE$comorbid_chronic_neurological_disorder))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_chronic_neurological_disorder))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Chronic Neurological Disorder") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Chronic Pulmonary
data %>%
  ggplot(aes(x = comorbid_chronic_pulmonary_disease)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Chronic Neurological Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_chronic_pulmonary_disease) %>% 
  tally()

sum(is.na(data$comorbid_chronic_pulmonary_disease))
sum(is.na(data_true_PE$comorbid_chronic_pulmonary_disease))
sum(is.na(data_false_PE$comorbid_chronic_pulmonary_disease))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_chronic_pulmonary_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Chronic Pulmonary Disease") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Dementia
data %>%
  ggplot(aes(x = comorbid_dementia)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Dementia Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_dementia) %>% 
  tally()

sum(is.na(data$comorbid_dementia))
sum(is.na(data_true_PE$comorbid_dementia))
sum(is.na(data_false_PE$comorbid_dementia))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_dementia))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Dementia") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Diabetes
data %>%
  ggplot(aes(x = comorbid_diabetes)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Diabetes Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_diabetes) %>% 
  tally()

sum(is.na(data$comorbid_diabetes))
sum(is.na(data_true_PE$comorbid_diabetes))
sum(is.na(data_false_PE$comorbid_diabetes))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_diabetes))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Diabetes") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Hypertension
data %>%
  ggplot(aes(x = comorbid_hypertension)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Hypertension Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_hypertension) %>% 
  tally()

sum(is.na(data$comorbid_hypertension))
sum(is.na(data_true_PE$comorbid_hypertension))
sum(is.na(data_false_PE$comorbid_hypertension))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_hypertension))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Hypertension") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Immunosupression
data %>%
  ggplot(aes(x = comorbid_immunosuppression)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Immunosuppression Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_immunosuppression) %>% 
  tally()

sum(is.na(data$comorbid_immunosuppression))
sum(is.na(data_true_PE$comorbid_immunosuppression))
sum(is.na(data_false_PE$comorbid_immunosuppression))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_immunosuppression))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Immunosupppression") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Liver Disease
data %>%
  ggplot(aes(x = comorbid_liver_disease)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Liver Disease Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_liver_disease) %>% 
  tally()

sum(is.na(data$comorbid_liver_disease))
sum(is.na(data_true_PE$comorbid_liver_disease))
sum(is.na(data_false_PE$comorbid_liver_disease))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_liver_disease))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Liver Disease") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Malignant Neoplasm
data %>%
  ggplot(aes(x = comorbid_malignant_neoplasm)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Malignant Neoplasm Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_malignant_neoplasm) %>% 
  tally()

sum(is.na(data$comorbid_malignant_neoplasm))
sum(is.na(data_true_PE$comorbid_malignant_neoplasm))
sum(is.na(data_false_PE$comorbid_malignant_neoplasm))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_malignant_neoplasm))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Malignant Neoplasm") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Malnutrition
data %>%
  ggplot(aes(x = comorbid_malnutrition)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Malnutrition Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_malnutrition) %>% 
  tally()

sum(is.na(data$comorbid_malnutrition))
sum(is.na(data_true_PE$comorbid_malnutrition))
sum(is.na(data_false_PE$comorbid_malnutrition))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_malnutrition))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Malnutrition") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Obesity
data %>%
  ggplot(aes(x = comorbid_obesity)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Obesity Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_obesity) %>% 
  tally()

sum(is.na(data$comorbid_obesity))
sum(is.na(data_true_PE$comorbid_obesity))
sum(is.na(data_false_PE$comorbid_obesity))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_obesity))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Obesity") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Rheumatologic
data %>%
  ggplot(aes(x = comorbid_rheumatologic_disorder)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Rheumatologic Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_rheumatologic_disorder) %>% 
  tally()

sum(is.na(data$comorbid_rheumatologic_disorder))
sum(is.na(data_true_PE$comorbid_rheumatologic_disorder))
sum(is.na(data_false_PE$comorbid_rheumatologic_disorder))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_rheumatologic_disorder))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Rheumatologic Disorder") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Smoking
data %>%
  ggplot(aes(x = comorbid_smoking)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Smoking Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_smoking) %>% 
  tally()

sum(is.na(data$comorbid_smoking))
sum(is.na(data_true_PE$comorbid_smoking))
sum(is.na(data_false_PE$comorbid_smoking))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_smoking))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Smoking") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Comorbidity Tuberculosis
data %>%
  ggplot(aes(x = comorbid_tuberculosis)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Tuberculosis Comorbidity Given PE",
       y = "Frequency")

data %>%
  group_by(PE, comorbid_tuberculosis) %>% 
  tally()

sum(is.na(data$comorbid_tuberculosis))
sum(is.na(data_true_PE$comorbid_tuberculosis))
sum(is.na(data_false_PE$comorbid_tuberculosis))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(comorbid_tuberculosis))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Tuberculosis") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Symptomatic
data %>%
  ggplot(aes(x = symptomatic)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptomatic) %>% 
  tally()

sum(is.na(data$symptomatic))
sum(is.na(data_true_PE$symptomatic))
sum(is.na(data_false_PE$symptomatic))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptomatic))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Symptomatic") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Abdominal Pain
data %>%
  ggplot(aes(x = symptoms_abdominal_pain)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Abdominal Pain Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_abdominal_pain) %>% 
  tally()

sum(is.na(data$symptoms_abdominal_pain))
sum(is.na(data_true_PE$symptoms_abdominal_pain))
sum(is.na(data_false_PE$symptoms_abdominal_pain))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_abdominal_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Abdominal Pain") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Confusion
data %>%
  ggplot(aes(x = symptoms_altered_consciousness_confusion)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Confusion Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_altered_consciousness_confusion) %>% 
  tally()

sum(is.na(data$symptoms_altered_consciousness_confusion))
sum(is.na(data_true_PE$symptoms_altered_consciousness_confusion))
sum(is.na(data_false_PE$symptoms_altered_consciousness_confusion))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_altered_consciousness_confusion))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Confusion") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Asymptomatic
data %>%
  ggplot(aes(x = symptoms_asymptomatic)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Asymptomatic Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_asymptomatic) %>% 
  tally()

sum(is.na(data$symptoms_asymptomatic))
sum(is.na(data_true_PE$symptoms_asymptomatic))
sum(is.na(data_false_PE$symptoms_asymptomatic))

### Symptoms Bleeding
data %>%
  ggplot(aes(x = symptoms_bleeding)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Bleeding Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_bleeding) %>% 
  tally()

sum(is.na(data$symptoms_bleeding))
sum(is.na(data_true_PE$symptoms_bleeding))
sum(is.na(data_false_PE$symptoms_bleeding))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_bleeding))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Bleeding") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Chest Pain
data %>%
  ggplot(aes(x = symptoms_chest_pain)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Chest Pain Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_chest_pain) %>% 
  tally()

sum(is.na(data$symptoms_chest_pain))
sum(is.na(data_true_PE$symptoms_chest_pain))
sum(is.na(data_false_PE$symptoms_chest_pain))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_chest_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Chest Pain") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Conjunctivitis
data %>%
  ggplot(aes(x = symptoms_conjunctivitis)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Conjunctivitis Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_conjunctivitis) %>% 
  tally()

sum(is.na(data$symptoms_conjunctivitis))
sum(is.na(data_true_PE$symptoms_conjunctivitis))
sum(is.na(data_false_PE$symptoms_conjunctivitis))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_conjunctivitis))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Conjunctivitis") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Cough
data %>%
  ggplot(aes(x = symptoms_cough)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Cough Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_cough) %>% 
  tally()

sum(is.na(data$symptoms_cough))
sum(is.na(data_true_PE$symptoms_cough))
sum(is.na(data_false_PE$symptoms_cough))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_cough))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Cough") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Diarrhoea
data %>%
  ggplot(aes(x = symptoms_diarrhoea)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Diarrhoea Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_diarrhoea) %>% 
  tally()

sum(is.na(data$symptoms_diarrhoea))
sum(is.na(data_true_PE$symptoms_diarrhoea))
sum(is.na(data_false_PE$symptoms_diarrhoea))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_diarrhoea))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Diarrhoea") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Ear Pain
data %>%
  ggplot(aes(x = symptoms_ear_pain)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Ear Pain Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_ear_pain) %>% 
  tally()

sum(is.na(data$symptoms_ear_pain))
sum(is.na(data_true_PE$symptoms_ear_pain))
sum(is.na(data_false_PE$symptoms_ear_pain))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_ear_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Ear Pain") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Fatigue
data %>%
  ggplot(aes(x = symptoms_fatigue_malaise)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Fatiguen Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_fatigue_malaise) %>% 
  tally()

sum(is.na(data$symptoms_fatigue_malaise))
sum(is.na(data_true_PE$symptoms_fatigue_malaise))
sum(is.na(data_false_PE$symptoms_fatigue_malaise))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_fatigue_malaise))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Fatigue") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Headache
data %>%
  ggplot(aes(x = symptoms_headache)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Headache Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_headache) %>% 
  tally()

sum(is.na(data$symptoms_headache))
sum(is.na(data_true_PE$symptoms_headache))
sum(is.na(data_false_PE$symptoms_headache))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_headache))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Headache") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Fever
data %>%
  ggplot(aes(x = symptoms_history_of_fever)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Fever Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_history_of_fever) %>% 
  tally()

sum(is.na(data$symptoms_history_of_fever))
sum(is.na(data_true_PE$symptoms_history_of_fever))
sum(is.na(data_false_PE$symptoms_history_of_fever))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_history_of_fever))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Fever") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Lost or Altered Sense of Smell
data %>%
  ggplot(aes(x = symptoms_lost_altered_sense_of_smell)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Lost or Altered Sense of Smell Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_lost_altered_sense_of_smell) %>% 
  tally()

sum(is.na(data$symptoms_lost_altered_sense_of_smell))
sum(is.na(data_true_PE$symptoms_lost_altered_sense_of_smell))
sum(is.na(data_false_PE$symptoms_lost_altered_sense_of_smell))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_lost_altered_sense_of_smell))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Loss of Smell") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Lost or Altered Sense of Taste
data %>%
  ggplot(aes(x = symptoms_lost_altered_sense_of_taste)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Lost or Altered Sense of Taste Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_lost_altered_sense_of_taste) %>% 
  tally()

sum(is.na(data$symptoms_lost_altered_sense_of_taste))
sum(is.na(data_true_PE$symptoms_lost_altered_sense_of_taste))
sum(is.na(data_false_PE$symptoms_lost_altered_sense_of_taste))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_lost_altered_sense_of_taste))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Loss of Taste") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Lymphadenopathy
data %>%
  ggplot(aes(x = symptoms_lymphadenopathy)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Lymphadenopathy Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_lymphadenopathy) %>% 
  tally()

sum(is.na(data$symptoms_lymphadenopathy))
sum(is.na(data_true_PE$symptoms_lymphadenopathy))
sum(is.na(data_false_PE$symptoms_lymphadenopathy))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_lymphadenopathy))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Lymphadenopathy") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Muscle Aches and Joint Pain
data %>%
  ggplot(aes(x = symptoms_muscle_aches_joint_pain)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Muscle Aches and Joint Pain Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_muscle_aches_joint_pain) %>% 
  tally()

sum(is.na(data$symptoms_muscle_aches_joint_pain))
sum(is.na(data_true_PE$symptoms_muscle_aches_joint_pain))
sum(is.na(data_false_PE$symptoms_muscle_aches_joint_pain))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_muscle_aches_joint_pain))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Muscle and Joint Pain") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Runny Nose
data %>%
  ggplot(aes(x = symptoms_runny_nose)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Runny Nose Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_runny_nose) %>% 
  tally()

sum(is.na(data$symptoms_runny_nose))
sum(is.na(data_true_PE$symptoms_runny_nose))
sum(is.na(data_false_PE$symptoms_runny_nose))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_runny_nose))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Runny Nose") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Seizures
data %>%
  ggplot(aes(x = symptoms_seizures)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Seizures Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_seizures) %>% 
  tally()

sum(is.na(data$symptoms_seizures))
sum(is.na(data_true_PE$symptoms_seizures))
sum(is.na(data_false_PE$symptoms_seizures))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_seizures))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Seizures") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Severe Dehydration
data %>%
  ggplot(aes(x = symptoms_severe_dehydration)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Severe Dehydration Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_severe_dehydration) %>% 
  tally()

sum(is.na(data$symptoms_severe_dehydration))
sum(is.na(data_true_PE$symptoms_severe_dehydration))
sum(is.na(data_false_PE$symptoms_severe_dehydration))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_severe_dehydration))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Severe Dehydration") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Shortness of Breath
data %>%
  ggplot(aes(x = symptoms_shortness_of_breath)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Shortness of Breath Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_shortness_of_breath) %>% 
  tally()

sum(is.na(data$symptoms_shortness_of_breath))
sum(is.na(data_true_PE$symptoms_shortness_of_breath))
sum(is.na(data_false_PE$symptoms_shortness_of_breath))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_shortness_of_breath))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Shortness of Breath") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Skin Rash
data %>%
  ggplot(aes(x = symptoms_skin_rash)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Skin Rash Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_skin_rash) %>% 
  tally()

sum(is.na(data$symptoms_skin_rash))
sum(is.na(data_true_PE$symptoms_skin_rash))
sum(is.na(data_false_PE$symptoms_skin_rash))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_skin_rash))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Skin Rash") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Sore Throat
data %>%
  ggplot(aes(x = symptoms_sore_throat)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Sore Throat Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_sore_throat) %>% 
  tally()

sum(is.na(data$symptoms_sore_throat))
sum(is.na(data_true_PE$symptoms_sore_throat))
sum(is.na(data_false_PE$symptoms_sore_throat))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_sore_throat))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Sore Throat") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Vomiting
data %>%
  ggplot(aes(x = symptoms_vomiting_nausea)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Vomiting Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_vomiting_nausea) %>% 
  tally()

sum(is.na(data$symptoms_vomiting_nausea))
sum(is.na(data_true_PE$symptoms_vomiting_nausea))
sum(is.na(data_false_PE$symptoms_vomiting_nausea))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_vomiting_nausea))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Vomiting") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Symptoms Wheezing
data %>%
  ggplot(aes(x = symptoms_wheezing)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Symptomatic Wheezing Given PE",
       y = "Frequency")

data %>%
  group_by(PE, symptoms_wheezing) %>% 
  tally()

sum(is.na(data$symptoms_wheezing))
sum(is.na(data_true_PE$symptoms_wheezing))
sum(is.na(data_false_PE$symptoms_wheezing))

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(symptoms_wheezing))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "Wheezing") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

### Treatment Extracorporeal
data %>%
  ggplot(aes(x = treat_extracorporeal)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Extracorporeal Given PE",
       y = "Frequency")

data %>%
  group_by(PE, treat_extracorporeal) %>% 
  tally()

sum(is.na(data$treat_extracorporeal))
sum(is.na(data_true_PE$treat_extracorporeal))
sum(is.na(data_false_PE$treat_extracorporeal))

### Treatment High Flow Nasal Cannula
data %>%
  ggplot(aes(x = treat_high_flow_nasal_cannula)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment High Flow Nasal Cannula Given PE",
       y = "Frequency")

data %>%
  group_by(PE, treat_high_flow_nasal_cannula) %>% 
  tally()

sum(is.na(data$treat_high_flow_nasal_cannula))
sum(is.na(data_true_PE$treat_high_flow_nasal_cannula))
sum(is.na(data_false_PE$treat_high_flow_nasal_cannula))

### Treatment Inhaled Nitric Oxide
data %>%
  ggplot(aes(x = treat_inhaled_nitric_oxide)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Inhaled Nitric Oxide Given PE",
       y = "Frequency")

data %>%
  group_by(PE, treat_inhaled_nitric_oxide) %>% 
  tally()

sum(is.na(data$treat_inhaled_nitric_oxide))
sum(is.na(data_true_PE$treat_inhaled_nitric_oxide))
sum(is.na(data_false_PE$treat_inhaled_nitric_oxide))

### Treatment Invasive Ventilation
data %>%
  ggplot(aes(x = treat_invasive_ventilation)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Invasive Ventilation Given PE",
       y = "Frequency")

data %>%
  group_by(PE, treat_invasive_ventilation) %>% 
  tally()

sum(is.na(data$treat_invasive_ventilation))
sum(is.na(data_true_PE$treat_invasive_ventilation))
sum(is.na(data_false_PE$treat_invasive_ventilation))

### Treatment Non-Invasive Ventilation
data %>%
  ggplot(aes(x = treat_non_invasive_ventilation)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Non-Invasive Ventilation Given PE",
       y = "Frequency")

data %>%
  group_by(PE, treat_non_invasive_ventilation) %>% 
  tally()

sum(is.na(data$treat_non_invasive_ventilation))
sum(is.na(data_true_PE$treat_non_invasive_ventilation))
sum(is.na(data_false_PE$treat_non_invasive_ventilation))

### Treatment Oxygen Therapy
data %>%
  ggplot(aes(x = treat_oxy_therapy)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Oxygen Therapy Given PE",
       y = "Frequency")

data %>%
  group_by(PE, treat_oxy_therapy) %>% 
  tally()

sum(is.na(data$treat_oxy_therapy))
sum(is.na(data_true_PE$treat_oxy_therapy))
sum(is.na(data_false_PE$treat_oxy_therapy))

### Treatment Respiratory Support
data %>%
  ggplot(aes(x = treat_respiratory_support)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Respiratory Support Given PE",
       y = "Frequency")

data %>%
  group_by(PE, treat_respiratory_support) %>% 
  tally()

sum(is.na(data$treat_respiratory_support))
sum(is.na(data_true_PE$treat_respiratory_support))
sum(is.na(data_false_PE$treat_respiratory_support))

### Treatment Tracheostomy
data %>%
  ggplot(aes(x = treat_tracheostomy)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Tracheostomy Given PE",
       y = "Frequency")

data %>%
  group_by(PE, treat_tracheostomy) %>% 
  tally()

sum(is.na(data$treat_tracheostomy))
sum(is.na(data_true_PE$treat_tracheostomy))
sum(is.na(data_false_PE$treat_tracheostomy))

### Treatment Oxygen Therapy 2
data %>%
  ggplot(aes(x = treat_oxygen_therapy)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Oxygen Therapy 2 Given PE",
       y = "Frequency")

data %>%
  group_by(PE, treat_oxygen_therapy) %>% 
  tally()

sum(is.na(data$treat_oxygen_therapy))
sum(is.na(data_true_PE$treat_oxygen_therapy))
sum(is.na(data_false_PE$treat_oxygen_therapy))

### Treatment Oxygen Therapy 2
data %>%
  ggplot(aes(x = treat_oxygen_therapy)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Treatment Oxygen Therapy 2 Given PE",
       y = "Frequency")

data %>%
  group_by(PE, treat_oxygen_therapy) %>% 
  tally()

sum(is.na(data$treat_oxygen_therapy))
sum(is.na(data_true_PE$treat_oxygen_therapy))
sum(is.na(data_false_PE$treat_oxygen_therapy))

### ICU Admission
data %>%
  ggplot(aes(x = ever_icu)) +
  geom_bar(fill = "steelblue4")  + 
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1), group = 1),
            stat = 'count', nudge_y = 0.125, vjust = -.5, size = 3) +
  theme_bw() +
  facet_wrap(~ PE) + 
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "ICU Admission Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = factor(ever_icu), fill = factor(PE))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 10),
        axis.text.y = element_text(colour = "black", size = 10)) +
  labs(x = "ICU Admission",
       y = "Proportion",
       fill = "PE") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

data %>%
  group_by(PE, ever_icu) %>% 
  tally()

sum(is.na(data$ever_icu))
sum(is.na(data_true_PE$ever_icu))
sum(is.na(data_false_PE$ever_icu))

### Lab D-dimer
data %>%
  ggplot(aes(x = lborres_lab_ddimer, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "Lab D-dimer Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = lborres_lab_ddimer, fill = factor(PE))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'PE',
       x = "D-dimer",
       y = "Density",
       title = "Density of D-dimer by PE") +
  theme(axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 13),
        title = element_text(colour = "black", size = 15)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data$lborres_lab_ddimer))
sum(is.na(data_true_PE$lborres_lab_ddimer))
sum(is.na(data_false_PE$lborres_lab_ddimer))

# The units:

units <- data %>%
  group_by(lborresu_lab_ddimer) %>% 
  tally()

# Drop the units column
data <-dplyr::select(data, -c('lborresu_lab_ddimer'))
data_true_PE <-dplyr::select(data_true_PE, -c('lborresu_lab_ddimer'))
data_false_PE <-dplyr::select(data_false_PE, -c('lborresu_lab_ddimer'))

### Lab ALT
data %>%
  ggplot(aes(x = lab_alt, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "Lab ALT Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = lab_alt, fill = factor(PE))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'PE',
       x = "ALT",
       y = "Density",
       title = "Density of ALT by PE") +
  theme(axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 13),
        title = element_text(colour = "black", size = 15)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data$lab_alt))
sum(is.na(data_true_PE$lab_alt))
sum(is.na(data_false_PE$lab_alt))

### Lab Bilirubin
data %>%
  ggplot(aes(x = lab_bili, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "Lab Bilirubin Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = lab_bili, fill = factor(PE))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'PE',
       x = "Bilirubin",
       y = "Density",
       title = "Density of Bilirubin by PE") +
  theme(axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 13),
        title = element_text(colour = "black", size = 15)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)]) +
  xlim(0, 100)

sum(is.na(data$lab_bili))
sum(is.na(data_true_PE$lab_bili))
sum(is.na(data_false_PE$lab_bili))

### Lab CRP
data %>%
  ggplot(aes(x = lab_crp, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "Lab CRP Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = lab_crp, fill = factor(PE))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'PE',
       x = "CRP",
       y = "Density",
       title = "Density of CRP by PE") +
  theme(axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 13),
        title = element_text(colour = "black", size = 15)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data$lab_crp))
sum(is.na(data_true_PE$lab_crp))
sum(is.na(data_false_PE$lab_crp))

### Lab Lymphocites
data %>%
  ggplot(aes(x = lab_lym, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "Lab Lymphocites Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = lab_lym, fill = factor(PE))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'PE',
       x = "Lymphocites",
       y = "Density",
       title = "Density of Lymphocites by PE") +
  theme(axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 13),
        title = element_text(colour = "black", size = 15)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)]) + xlim(0, 10)

sum(is.na(data$lab_lym))
sum(is.na(data_true_PE$lab_lym))
sum(is.na(data_false_PE$lab_lym))

### Lab Neutrophils
data %>%
  ggplot(aes(x = lab_neut, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "Lab Neutrophils Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = lab_neut, fill = factor(PE))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'PE',
       x = "Neutrophils",
       y = "Density",
       title = "Density of Neutrophils by PE") +
  theme(axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 13),
        title = element_text(colour = "black", size = 15)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data$lab_neut))
sum(is.na(data_true_PE$lab_neut))
sum(is.na(data_false_PE$lab_neut))

### Lab Platelets
data %>%
  ggplot(aes(x = lab_pt, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "Lab Platelets Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = lab_pt, fill = factor(PE))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'PE',
       x = "Platelets",
       y = "Density",
       title = "Density of Platelets by PE") +
  theme(axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 13),
        title = element_text(colour = "black", size = 15)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data$lab_pt))
sum(is.na(data_true_PE$lab_pt))
sum(is.na(data_false_PE$lab_pt))

### Lab Urean
data %>%
  ggplot(aes(x = lab_urean, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "Lab Urean Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = lab_urean, fill = factor(PE))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'PE',
       x = "Urean",
       y = "Density",
       title = "Density of Urean by PE") +
  theme(axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 13),
        title = element_text(colour = "black", size = 15)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data$lab_urean))
sum(is.na(data_true_PE$lab_urean))
sum(is.na(data_false_PE$lab_urean))

### Lab White Blood Cells
data %>%
  ggplot(aes(x = lab_wbc, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "Lab White Blood Cells Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = lab_wbc, fill = factor(PE))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'PE',
       x = "WBC",
       y = "Density",
       title = "Density of WBC by PE") +
  theme(axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 13),
        title = element_text(colour = "black", size = 15)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data$lab_wbc))
sum(is.na(data_true_PE$lab_wbc))
sum(is.na(data_false_PE$lab_wbc))

### Lab DBP
data %>%
  ggplot(aes(x = vs_diabp, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "DBP Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = vs_diabp, fill = factor(PE))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'PE',
       x = "DBP",
       y = "Density",
       title = "Density of DBP by PE") +
  theme(axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 13),
        title = element_text(colour = "black", size = 15)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data$vs_diabp))
sum(is.na(data_true_PE$vs_diabp))
sum(is.na(data_false_PE$vs_diabp))

### Lab HR
data %>%
  ggplot(aes(x = vs_hr, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "HR Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = vs_hr, fill = factor(PE))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'PE',
       x = "HR",
       y = "Density",
       title = "Density of HR by PE") +
  theme(axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 13),
        title = element_text(colour = "black", size = 15)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data$vs_diabp))
sum(is.na(data_true_PE$vs_diabp))
sum(is.na(data_false_PE$vs_diabp))

sum(is.na(data$vs_hr))
sum(is.na(data_true_PE$vs_hr))
sum(is.na(data_false_PE$vs_hr))

### Lab Oxygen Saturation During Therapy
data %>%
  ggplot(aes(x = vs_oxysat_oxygen_therapy, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "Oxygen Saturation During Therapy Given PE",
       y = "Frequency")

sum(is.na(data$vs_oxysat_oxygen_therapy))
sum(is.na(data_true_PE$vs_oxysat_oxygen_therapy))
sum(is.na(data_false_PE$vs_oxysat_oxygen_therapy))

### Lab Oxygen Saturation During Room Air
data %>%
  ggplot(aes(x = vs_oxysat_room_air, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "Oxygen Saturation During Room Air Given PE",
       y = "Frequency")

sum(is.na(data$vs_oxysat_room_air))
sum(is.na(data_true_PE$vs_oxysat_room_air))
sum(is.na(data_false_PE$vs_oxysat_room_air))

### Lab RR
data %>%
  ggplot(aes(x = vs_resp, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "RR Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = vs_resp, fill = factor(PE))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'PE',
       x = "RR",
       y = "Density",
       title = "Density of RR by PE") +
  theme(axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 13),
        title = element_text(colour = "black", size = 15)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data$vs_resp))
sum(is.na(data_true_PE$vs_resp))
sum(is.na(data_false_PE$vs_resp))

### Lab SBP
data %>%
  ggplot(aes(x = vs_sysbp, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "SBP Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = vs_sysbp, fill = factor(PE))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'PE',
       x = "SBP",
       y = "Density",
       title = "Density of SBP by PE") +
  theme(axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 13),
        title = element_text(colour = "black", size = 15)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data$vs_sysbp))
sum(is.na(data_true_PE$vs_sysbp))
sum(is.na(data_false_PE$vs_sysbp))

### Lab Temperature
data %>%
  ggplot(aes(x = vs_temp, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "Temperature Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = vs_temp, fill = factor(PE))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'PE',
       x = "Temperature",
       y = "Density",
       title = "Density of Temperature by PE") +
  theme(axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 13),
        title = element_text(colour = "black", size = 15)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data$vs_temp))
sum(is.na(data_true_PE$vs_temp))
sum(is.na(data_false_PE$vs_temp))

### Oxygen Saturation
data %>%
  ggplot(aes(x = vs_oxysat, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "Oxygen Saturation Given PE",
       y = "Frequency")

data %>%
  ggplot(aes(x = vs_oxysat, fill = factor(PE))) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  labs(fill = 'Oxygen Saturation',
       x = "Temperature",
       y = "Density",
       title = "Density of Oxygen Saturation by PE") +
  theme(axis.text.x = element_text(colour = "black", size = 13),
        axis.text.y = element_text(colour = "black", size = 13),
        axis.title.x = element_text(colour = "black", size = 14),
        axis.title.y = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 13),
        title = element_text(colour = "black", size = 15)) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

sum(is.na(data$vs_oxysat))
sum(is.na(data_true_PE$vs_oxysat))
sum(is.na(data_false_PE$vs_oxysat))

### DSSTDY
data %>%
  ggplot(aes(x = dsstdy, fill = factor(PE))) +
  geom_density(alpha = 0.4, fill = "steelblue4") +
  theme_bw() + facet_wrap(~ PE) +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"))  +
  labs(x = "DSSTDY Given PE",
       y = "Frequency")

sum(is.na(data$dsstdy))
sum(is.na(data_true_PE$dsstdy))
sum(is.na(data_false_PE$dsstdy))

### Outcome
summary <- ddply(data_true_PE,.(outcome),summarise,freq=length(outcome))
outcome <- c("Disch", "Death", "Trans", "Ongoing", "Other")
freq <- c(3492, 1297, 308, 223, 336)

data_ethnic <- data.frame(outcome, freq)
data_ethnic$outcome <- factor(data_ethnic$outcome)

ggplot(data_ethnic, aes(x=outcome, y=freq)) + geom_bar(stat='identity', fill = "steelblue4") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Outcome Given PE",
       y = "Frequency")

summary <- ddply(data_false_PE,.(outcome),summarise,freq=length(outcome))
outcome <- c("Disch", "Death", "Trans", "Ongoing", "Other")
freq <- c(519423, 162091, 22224, 32706, 47449+49+10861)

data_ethnic <- data.frame(outcome, freq)
data_ethnic$outcome <- factor(data_ethnic$outcome)

ggplot(data_ethnic, aes(x=outcome, y=freq)) + geom_bar(stat='identity', fill = "steelblue4") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(x = "Outcome Given no PE",
       y = "Frequency")

data_true_PE %>%
  group_by(PE, outcome) %>% 
  tally()

data_false_PE %>%
  group_by(PE, outcome) %>% 
  tally()

sum(is.na(data$outcome))
sum(is.na(data_true_PE$outcome))
sum(is.na(data_false_PE$outcome))

sum(data$outcome == "")
sum(data_true_PE$outcome == "")
sum(data_false_PE$outcome == "")

data %>%
  ggplot(aes(x = factor(agegp10), fill = factor(PE))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Age",
       y = "Proportion",
       fill = "PE") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])


###################################################################
# United Kingdom and Spain only
###################################################################

# Get only Spain and UK
data <- data[(data$country=="United Kingdom" | data$country=="Spain"), ] # 288493 samples

data %>%
  ggplot(aes(x = factor(country), fill = factor(PE))) +
  geom_bar(position = "fill", alpha = 0.8)  + 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7),
        axis.text.y = element_text(colour = "black", size = 7)) +
  labs(x = "Country",
       y = "Proportion",
       fill = "PE") +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = brewer.pal(9, 'Blues')[c(4,7)])

interest <- data %>%
  group_by(PE, country) %>% 
  tally()

