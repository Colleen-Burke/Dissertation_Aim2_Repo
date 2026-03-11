library(tidyverse)
library(here)

Transitions_data <- read.csv(here("transitions_merged.csv"))
BACk_data <- read.csv(here("BACkStudy_CBAim2Data.csv"))

write.csv(BACk_data, here("my_data.csv"))


# Split into baseline and follow-up
baseline <- BACk_data |> 
  filter(redcap_event_name == "baseline_arm_1")  |> 
  select(-redcap_event_name, -redcap_repeat_instrument, -redcap_repeat_instance)

followup <- BACk_data |>
  filter(redcap_event_name == "3_month_followup_arm_1") |>
  select(record_id, where(~!all(is.na(.)))) |>
  select(-any_of(c("redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance")))

# Identify columns that appear in BOTH splits (besides record_id)
# Followup rows duplicate some baseline cols with all NAs — those were dropped above.
# Any remaining overlap gets a suffix to avoid collision.
wide <- left_join(baseline, followup, by = "record_id", suffix = c("_bl", "_fu"))

# Preview
glimpse(wide)






















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