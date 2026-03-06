library(tidyverse)
library(here)

Transitions_data <- read.csv(here("transitions_merged.csv"))
BACk_data <- read.csv(here("BACkStudy_CBAim2Data.csv"))























# BACk_data <- BACk_data |> 
  # keep(
    # record_id, race, sex, educational_attainment_individual_highest_grade,
    # health_insurance_coverage_from_employer, health_insurance_coverage_purchased,
    # health_insurance_coverage_medicare, health_insurance_coverage_medicaid,
    # health_insurance_coverage_military, health_insurance_coverage_indian, 
    # health_insurance_coverage_other, health_insurance_coverage_nocoverage,
    # marriage, financialwellbeing_want, financialwellbeing_getby, 
    # financialwellbeing_notlast, financialwellbeing_mnyleft, 
    # financialwellbeing_control, financialwellbeing_score, housing_ownership,
    # employ_status, physical_activity_days, physical_activity_minutes,
    # physical_activity_score, family_history_chr_pain, family_history_lbp,
    # substance_use1, substance_use2, days_since_lbp_start, lbp_vas_current, 
    # cdc_pain_freq, cdc_pain_interfere
    
  # )