library(tidyverse)
library(here)

BACk_data_wide_clean <- read.csv(here("BACk_data_wide.csv"))
Transitions_Aim2 <- read.csv(here("Transitions_Aim2.csv"))



# Convert all known conflict variables to character in both datasets
#vars_to_char <- c("ethnicity", "home_ownership", "race_f", "education_f", 
#                  "insurance_type_f", "startback_risk_label")

#BACk_data_wide_clean <- BACk_data_wide_clean %>%
#  mutate(across(all_of(vars_to_char), as.character))

#Transitions_Aim2 <- Transitions_Aim2 %>%
#  mutate(across(any_of(vars_to_char), as.character))

# Now stack
combined_data <- bind_rows(BACk_data_wide_clean, Transitions_Aim2)




analytic_data <- combined_data |> 
  select(
    record_id, sex, marriage, employ_status, physical_activity_days, 
    physical_activity_minutes, physical_activity_score, substance_use1, 
    substance_use2, days_since_lbp_start, lbp_vas_current, cdc_pain_freq, 
    cdc_pain_interfere, nih_lbpfrequency, peg_enjoyment, peg_activity, 
    treatment_nsaid, treatment_nsaid_current, tretment_mr, treatment_mr_current, 
    treatment_opioids, treatment_opioids_current, treatment_injections, 
    treatment_exercise_therapy, treatment_smt, treatment_cbt, prior_lbp, 
    prior_lbp_num, prior_lbp_days_since, prior_lbp_vas, startback_total_score, 
    startback_subscore, startback_risk, startback_risk_label, psqi_global_score, 
    general_health, qol, mental_health, social_relationships, social_activities, 
    physical_health, perform_pa, fatigue, stress, isolation_score, 
    isolation_cat, isolation_cat2, bmi_measured, age, lbp_vas_current_3mo, 
    cdc_pain_freq_3mo, cdc_pain_interfere_3mo, nih_lbpfrequency_3mo, 
    peg_enjoyment_3mo, peg_activity_3mo, race_f, ethnicity_f, education_f, 
    insurance_type_f, home_ownership, home_ownership_f, 
    social_relationships_ordinal, social_relationships_nominal, 
    social_relationships_d, social_relationships2_d, social_activities_ordinal, 
    social_activities_nominal, social_activities_d, social_activities2_d, 
    isolation_cat_ordinal, isolation_cat_nominal, isolation_cat_d, 
    isolation_cat2_d, race_dichotomized, sex_f, marriage_f, cdc_pain_freq_f, 
    cdc_pain_interfere_f, startback_risk_label_f, general_health_f, qol_f, 
    mental_health_f, physical_health_f, social_relationships_num, 
    social_activities_num, social_function_num, social_health_num, 
    social_health2_d, social_health_d, study
  )



# Drop anyone with missing outcome / 3 mo data
analytic_data <- analytic_data %>%
  filter(!is.na(cdc_pain_freq_3mo))



# Create High Impact Chronic Pain Variabl
analytic_data <- analytic_data %>%
  mutate(hicp = case_when(
    cdc_pain_freq_3mo %in% c(0, 1)                                                                 ~ 0,  # No chronic pain
    cdc_pain_freq_3mo %in% c(2, 3) & cdc_pain_interfere_3mo %in% c(2, 3)                          ~ 3,  # High-impact
    cdc_pain_freq_3mo %in% c(2, 3) & cdc_pain_interfere_3mo %in% c(0, 1) & peg_activity_3mo >= 4  ~ 2,  # Bothersome
    cdc_pain_freq_3mo %in% c(2, 3) & cdc_pain_interfere_3mo %in% c(0, 1) & peg_activity_3mo < 4   ~ 1,  # Mild
    TRUE ~ NA_real_
  ),
  hicp_f = factor(hicp,
                  levels = c(0, 1, 2, 3),
                  labels = c("No Chronic Pain", "Mild Chronic Pain", "Bothersome Chronic Pain", "High-Impact Chronic Pain")
  ))


write.csv(analytic_data, here("Aim2_data.csv"))
