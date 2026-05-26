library(tidyverse)
library(here)

BACk_data_raw <- read.csv(here("BACkData.csv"))


# Split into baseline and follow-up
baseline <- BACk_data_raw |> 
  filter(redcap_event_name == "baseline_arm_1")  |> 
  select(-redcap_event_name, -redcap_repeat_instrument, -redcap_repeat_instance)

followup <- BACk_data_raw |>
  filter(redcap_event_name == "3_month_followup_arm_1") |>
  select(record_id, where(~!all(is.na(.)))) |>
  select(
    -any_of(
      c(
        "redcap_event_name", 
        "redcap_repeat_instrument", 
        "redcap_repeat_instance"
        )))

# Identify columns that appear in BOTH splits (besides record_id)
# Followup rows duplicate some baseline cols with all NAs — those were dropped above.
# Any remaining overlap gets a suffix to avoid collision.
BACk_data_wide <- left_join(
  baseline, followup, by = "record_id", suffix = c("_bl", "_fu"))

# Preview
glimpse(BACk_data_wide)


# Define variables to keep
vars_keep <- c(
  "record_id", "race___4", "race___6", "race___3", "race___1", "race___0", 
  "race___7", "race___2", "race___555", "race___777", "race___999", "sex", 
  "educational_attainment_individual_highest_grade", 
  "health_insurance_coverage_from_employer", 
  "health_insurance_coverage_purchased", "health_insurance_coverage_medicare", 
  "health_insurance_coverage_medicaid", "health_insurance_coverage_military", 
  "health_insurance_coverage_indian", "health_insurance_coverage_other", 
  "health_insurance_coverage_nocoverage", "marriage", "housing_ownership", 
  "employ_status", "physical_activity_days", "physical_activity_minutes", 
  "physical_activity_score", "physical_activity_category", 
  "physical_activity_cat2_bl", "family_history_chr_pain", "substance_use1", 
  "substance_use2", "days_since_lbp_start", "lbp_vas_current", "cdc_pain_freq", 
  "cdc_pain_interfere", "nih_lbpfrequency", "peg_enjoyment", "peg_activity", 
  "current_med_lbp", "treatment_nsaid", "treatment_nsaid_current", 
  "tretment_mr", "treatment_mr_current", "treatment_opioids", 
  "treatment_opioids_current", "treatment_injections", 
  "treatment_exercise_therapy", "treatment_smt", "treatment_cbt", 
  "prior_lbp", "prior_lbp_num", "prior_lbp_days_since_bl", "prior_lbp_vas", 
  "char_total_score_bl", "startback_total_score_bl", "startback_subscore_bl", 
  "startback_risk_bl", "startback_risk_label_bl", "psqi_global_score_bl", 
  "promis_pfa11_bl", "promis_pfa21_bl", "promis_pfa23_bl", "promis_pfa53_bl", 
  "promis_sf_v10_depression_4a_tscore_bl", 
  "promis_sf_v10_depression_4a_std_error_bl", "promis_eddep04_bl", 
  "promis_eddep06_bl", "promis_eddep29_bl", "promis_eddep41_bl", 
  "promis_global01_9f6d75_bl", "promis_global02_b59611_bl", 
  "promis_global04_fd7005_bl", "promis_global05_902706_bl", 
  "promis_global09r_fbd679_bl", "promis_global03_a64ef7_bl", 
  "promis_global06_711d3a_bl", "promis_global08r_bl", "stress", 
  "isolation_score", "isolation_cat", "isolation_cat2_bl", "bmi_measured_bl", 
  "charlson_age_bl", "lbp_vas_current_3mo_fu", "cdc_pain_freq_3mo_fu", 
  "cdc_pain_interfere_3mo_fu", "nih_lbpfrequency_3mo_fu", 
  "peg_enjoyment_3mo_fu", "peg_activity_3mo_fu"
)

# Select only those variables
BACk_data_wide_clean <- BACk_data_wide %>%
  select(all_of(vars_keep))



# Race and Ethnicity Cleaning
# Create race_f and ethnicity variables
BACk_data_wide_clean <- BACk_data_wide_clean %>%
  mutate(
    # Sum of race checkboxes (excluding race___6 which is ethnicity)
    race_count = race___4 + race___3 + race___1 + race___0 + 
      race___7 + race___2 + race___555 + race___777 + race___999,
    
    # Create race_f numeric value
    race_f_num = case_when(
      race_count > 1                ~ 888,  # Multiracial (more than one selected)
      race___4   == 1               ~ 4,    # White
      race___3   == 1               ~ 3,    # Black or African American
      race___1   == 1               ~ 1,    # Asian
      race___0   == 1               ~ 0,    # American Indian or Alaskan Native
      race___7   == 1               ~ 7,    # Middle Eastern or North African
      race___2   == 1               ~ 2,    # Native Hawaiian or Other Pacific Islander
      race___555 == 1               ~ 555,  # Other
      race___777 == 1               ~ 777,  # Unknown
      race___999 == 1               ~ 999,  # Choose Not to Respond
      TRUE                          ~ NA_real_
    ),
    
    # Convert to factor with labels
    race_f = factor(race_f_num,
                    levels = c(0, 1, 2, 3, 4, 7, 555, 777, 888, 999),
                    labels = c("American Indian or Alaskan Native",
                               "Asian",
                               "Native Hawaiian or Other Pacific Islander",
                               "Black or African American",
                               "White",
                               "Middle Eastern or North African",
                               "Other",
                               "Unknown",
                               "Multiracial",
                               "Choose Not to Respond")),
    
    # Create ethnicity factor variable
    ethnicity_f = factor(race___6,
                       levels = c(0, 1),
                       labels = c("Non-Hispanic", "Hispanic or Latinx"))
  ) %>%
  # Drop the helper columns
  select(-race_count, -race_f_num)

# Check results
table(BACk_data_wide_clean$race_f, useNA = "always")
table(BACk_data_wide_clean$ethnicity_f, useNA = "always")



# Education Cleaning

BACk_data_wide_clean <- BACk_data_wide_clean %>%
  mutate(
    education_f = case_when(
      educational_attainment_individual_highest_grade %in% 0:12  ~ 0,
      educational_attainment_individual_highest_grade %in% 13:14 ~ 1,
      educational_attainment_individual_highest_grade == 15      ~ 3,
      educational_attainment_individual_highest_grade == 16      ~ 2,
      educational_attainment_individual_highest_grade == 17      ~ 4,
      educational_attainment_individual_highest_grade == 18      ~ 5,
      educational_attainment_individual_highest_grade %in% 19:21 ~ 7,
      educational_attainment_individual_highest_grade %in% c(777, 999) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    education_f = factor(
      education_f,
      levels = c(7, 6, 5, 4, 3, 2, 1, 0),
      labels = c(
        "Graduate degree", "Some graduate school", "Bachelors Degree",
        "Associates Degree", "Some College", "Trade School",
        "High School Graduate", "Less than High School"
      ),
      ordered = TRUE
    )
  )

# Check results
table(BACk_data_wide_clean$education_f, useNA = "always")



# Insurance Cleaning
BACk_data_wide_clean <- BACk_data_wide_clean %>%
  mutate(
    insurance_type_f = case_when(
      health_insurance_coverage_from_employer == 1 | 
        health_insurance_coverage_purchased == 1          ~ 4,  # Private Insurance (highest priority)
      health_insurance_coverage_medicare == 1             ~ 2,  # Medicare
      health_insurance_coverage_medicaid == 1             ~ 1,  # Medicaid
      health_insurance_coverage_military == 1 | 
        health_insurance_coverage_indian == 1             ~ 3,  # Other Public Insurance
      health_insurance_coverage_other == 1                ~ 9,  # Unsure
      health_insurance_coverage_nocoverage == 1           ~ 0,  # None/Uninsured
      TRUE                                                ~ NA_real_
    ),
    insurance_type_f = factor(
      na_if(insurance_type_f, 999),
      levels = c(0, 1, 2, 3, 4, 9),
      labels = c(
        "None/Uninsured", "Medicaid", "Medicare",
        "Other Public Insurance", "Private Insurance", "Unsure"
      )
    )
  )

# Check results
table(BACk_data_wide_clean$insurance_type_f, useNA = "always")



## Housing Cleaning
BACk_data_wide_clean <- BACk_data_wide_clean %>%
  mutate(
    home_ownership = case_when(
      housing_ownership == 2   ~ 0,    # Rent
      housing_ownership == 1   ~ 1,    # Own
      housing_ownership == 3   ~ 3,    # Other
      housing_ownership == 777 ~ 999,  # Choose Not to Respond
      TRUE                     ~ NA_real_
    ),
    home_ownership_f = factor(
      home_ownership,
      levels = c(0, 1, 3, 999),
      labels = c("Rent", "Own", "Other", "Choose Not to Respond")
    )
  )

# Check results
table(BACk_data_wide_clean$home_ownership, useNA = "always")


BACk_data_wide_clean <- BACk_data_wide_clean %>%
  rename(physical_activity_cat2 = physical_activity_cat2_bl)


# Rename BACk variables to match Transitions before stacking
BACk_data_wide_clean <- BACk_data_wide_clean %>%
  rename(
    prior_lbp_days_since    = prior_lbp_days_since_bl,
    isolation_cat2          = isolation_cat2_bl,
    bmi_measured            = bmi_measured_bl,
    char_total_score        = char_total_score_bl,
    startback_total_score   = startback_total_score_bl,
    startback_subscore      = startback_subscore_bl,
    startback_risk          = startback_risk_bl,
    startback_risk_label    = startback_risk_label_bl,
    psqi_global_score       = psqi_global_score_bl,
    lbp_vas_current_3mo     = lbp_vas_current_3mo_fu, 
    cdc_pain_freq_3mo       = cdc_pain_freq_3mo_fu, 
    cdc_pain_interfere_3mo  = cdc_pain_interfere_3mo_fu, 
    nih_lbpfrequency_3mo    = nih_lbpfrequency_3mo_fu, 
    peg_enjoyment_3mo       = peg_enjoyment_3mo_fu, 
    peg_activity_3mo        = peg_activity_3mo_fu,
    age                     = charlson_age_bl,
    general_health          = promis_global01_9f6d75_bl, 
    qol                     = promis_global02_b59611_bl, 
    mental_health           = promis_global04_fd7005_bl, 
    social_relationships    = promis_global05_902706_bl, 
    social_activities       = promis_global09r_fbd679_bl, 
    physical_health         = promis_global03_a64ef7_bl, 
    perform_pa              = promis_global06_711d3a_bl, 
    fatigue                 = promis_global08r_bl
  ) |> 
  mutate(
    # --- Social variables (with Excellent as reference) ---
    social_relationships_ordinal = factor(
      social_relationships,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very good", "Good", "Fair", "Poor"),
      ordered = TRUE
    ),
    social_relationships_nominal = factor(
      social_relationships,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very good", "Good", "Fair", "Poor"),
      ordered = FALSE
    ),
    social_relationships_d = factor(
      case_when(
        social_relationships >= 3 ~ "High",  # Excellent, Very good, Good (5,4,3)
        social_relationships <= 2 ~ "Low",   # Fair, Poor (2,1)
        TRUE ~ NA_character_),
      levels = c("High", "Low")
    ),
    social_relationships2_d = factor(
      case_when(
        social_relationships >= 4 ~ "High",  # Excellent, Very good (5,4)
        social_relationships <= 3 ~ "Low",   # Good, Fair, Poor (3,2,1)
        TRUE ~ NA_character_),
      levels = c("High", "Low")
    ),
    
    social_activities_ordinal = factor(
      social_activities,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very good", "Good", "Fair", "Poor"),
      ordered = TRUE
    ),
    social_activities_nominal = factor(
      social_activities,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very good", "Good", "Fair", "Poor"),
      ordered = FALSE
    ),
    social_activities_d = factor(
      case_when(
        social_activities >= 3 ~ "High",
        social_activities <= 2 ~ "Low",
        TRUE ~ NA_character_),
      levels = c("High", "Low")
    ),
    social_activities2_d = factor(
      case_when(
        social_activities >= 4 ~ "High",   # Excellent, Very good (5,4)
        social_activities <= 3 ~ "Low",    # Good, Fair, Poor (3,2,1)
        TRUE ~ NA_character_),
      levels = c("High", "Low")
    ),
    
    
    # Isolation (with Not Isolated as reference)
    isolation_cat_ordinal = factor(
      isolation_cat,
      levels = c(3, 2, 1, 0),
      labels = c("Not Isolated", "Somewhat Isolated", "Very Isolated", "Most Isolated"),
      ordered = TRUE
    ),
    isolation_cat_nominal = factor(
      isolation_cat,
      levels = c(3, 2, 1, 0),
      labels = c("Not Isolated", "Somewhat Isolated", "Very Isolated", "Most Isolated"),
      ordered = FALSE
    ),
    isolation_cat_d = case_when(
      isolation_cat == 3 ~ "Low",   # Not Isolated
      isolation_cat %in% c(0, 1, 2) ~ "High",  # All other categories
      TRUE ~ NA_character_
    ),
    isolation_cat2_d = case_when(
      isolation_cat %in% c(3, 2) ~ "Low",   # Not Isolated
      isolation_cat %in% c(0, 1) ~ "High",  # All other categories
      TRUE ~ NA_character_
    ),
    
    
    # Demographics
    race_dichotomized = if_else(race_f == "White", 0, 1),
    sex_f = factor(
      sex,
      levels = c(0, 1),
      labels = c("Male", "Female")
    ),
    
    
    marriage_f = factor(
      marriage,
      levels = c(0, 1, 2, 3, 4),
      labels = c(
        "Single Never Married", "Divorced", "Separated",
        "Widowed", "Married/Living with Partner"
      )
    ),
    employ_status_f = factor(
      na_if(employ_status, 999),
      levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      labels = c(
        "Working Full Time", "Working Part Time",
        "Unemployed Looking for Work", "Sick Leave",
        "Disabled due to back pain", "Disabled Other",
        "Student", "Temporarily Laid Off", "Retired",
        "Keeping House", "Other"
      )
    ),
    
    # Work and pain measures
    cdc_pain_freq_f = factor(
      cdc_pain_freq,
      levels = c(0, 1, 2, 3),
      labels = c("Never", "Some Days", "Most Days", "Every Day"),
      ordered = TRUE
    ),
    cdc_pain_interfere_f = factor(
      cdc_pain_interfere,
      levels = c(0, 1, 2, 3),
      labels = c("Never", "Some Days", "Most Days", "Every Day"),
      ordered = TRUE
    ),
    startback_risk_label_f = factor(
      startback_risk_label,
      levels = c("Low Risk", "Medium Risk", "High Risk"),
      ordered = TRUE
    ),
    
    # Health and QoL
    general_health_f = factor(
      general_health,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"),
      ordered = TRUE
    ),
    qol_f = factor(
      qol,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"),
      ordered = TRUE
    ),
    mental_health_f = factor(
      mental_health,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"),
      ordered = TRUE
    ),
    physical_health_f = factor(
      physical_health,
      levels = c(5, 4, 3, 2, 1),
      labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"),
      ordered = TRUE
    ),
    
    # Convert all _d variables to factors (Low as reference)
    across(ends_with("_d"), ~ factor(.x, levels = c("Low", "High")))
  )

# Composite Social Relationships/Activites Item

BACk_data_wide_clean <- BACk_data_wide_clean |>
  mutate(
    social_relationships_num = case_when(
      social_relationships_nominal == "Excellent" ~ 5,
      social_relationships_nominal == "Very good" ~ 4,
      social_relationships_nominal == "Good"      ~ 3,
      social_relationships_nominal == "Fair"      ~ 2,
      social_relationships_nominal == "Poor"      ~ 1,
      TRUE ~ NA_real_
    ),
    
    social_activities_num = case_when(
      social_activities_nominal == "Excellent" ~ 5,
      social_activities_nominal == "Very good" ~ 4,
      social_activities_nominal == "Good"      ~ 3,
      social_activities_nominal == "Fair"      ~ 2,
      social_activities_nominal == "Poor"      ~ 1,
      TRUE ~ NA_real_
    ),
    
    social_function_num = social_relationships_num + social_activities_num
    
  )

# Create Social Relationship/Activity Composite Item
BACk_data_wide_clean <- BACk_data_wide_clean |>
  mutate(
    social_function_num = social_relationships_num + social_activities_num
  )

# Quality control check for NA's
table(BACk_data_wide_clean$social_relationships_num, useNA = "ifany")
table(BACk_data_wide_clean$social_activities_num, useNA = "ifany")
table(BACk_data_wide_clean$social_function_num, useNA = "ifany")

summary(BACk_data_wide_clean$social_function_num)


# Create Social Factor Variable
BACk_data_wide_clean <- BACk_data_wide_clean |>
  mutate(
    social_health_num = social_relationships_num + social_activities_num + 
      isolation_score,
    social_health2_d = case_when(
      social_health_num %in% c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11) ~ "Poor",   
      social_health_num %in% c(12, 13, 14) ~ "Good",
      TRUE ~ NA_character_
    ),
    social_health_d = case_when(
      social_health_num %in% c(2, 3, 4, 5, 6, 7, 8, 9, 10) ~ "Poor",   
      social_health_num %in% c(11, 12, 13, 14) ~ "Good",
      TRUE ~ NA_character_
    )
  )

# Convert ethnicity to character in both datasets before stacking
#BACk_data_wide_clean <- BACk_data_wide_clean %>%
#  mutate(ethnicity = as.character(ethnicity))


# Convert home_ownership to character in both datasets before stacking
#BACk_data_wide_clean <- BACk_data_wide_clean %>%
#  mutate(home_ownership = as.character(home_ownership))


# Add study identifier to each dataset
BACk_data_wide_clean <- BACk_data_wide_clean %>%
  mutate(study = "BACk")

str(BACk_data_wide_clean)

write.csv(BACk_data_wide_clean, here("BACk_data_wide.csv"))




