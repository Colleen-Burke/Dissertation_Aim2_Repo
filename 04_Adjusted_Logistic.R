library(tidyverse)
library(here)

 analytic_data <- analytic_data |> 
   mutate(
#     race_f = factor(race_f,
#                     levels = c("American Indian or Alaskan Native", "Asian",
#                                "Native Hawaiian or Other Pacific Islander",
#                                "Black or African American", "White",
#                                "Middle Eastern or North African", "Other",
#                                "Unknown", "Multiracial", "Choose Not to Respond")),
#     ethnicity_f = factor(ethnicity_f,
#                          levels = c("Non-Hispanic", "Hispanic or Latinx")),
#     race_collapsed = case_when(
#       race_f == "White" ~ "White",
#       race_f == "Black or African American" ~ "Black or African American",
#       race_f == "Asian" ~ "Asian",
#       race_f %in% c("American Indian or Alaskan Native",
#                     "Native Hawaiian or Other Pacific Islander",
#                     "Middle Eastern or North African",
#                     "Multiracial", "Other") ~ "Other/Multiracial",
#       race_f %in% c("Unknown", "Choose Not to Respond") ~ NA_character_,
#       TRUE ~ "Other/Multiracial"
#     ),
#     race_collapsed = factor(race_collapsed,
#                             levels = c("White", "Black or African American",
#                                        "Asian", "Other/Multiracial")),
     race_dichotomized = ifelse(race_f == "White", 0, 1)
   )


##--- Social Health Model (Adjusted) ---
social_health_num_model_adj <- glm(
  chronic_pain ~ social_health_num + 
    age + sex + race_dichotomized + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(social_health_num_model_adj)
exp(cbind(OR = coef(social_health_num_model_adj), confint(social_health_num_model_adj)))

##--- Social Activities Model (Adjusted) ---
social_activities_num_model_adj <- glm(
  chronic_pain ~ social_activities_num + 
    age + sex + race_dichotomized + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(social_activities_num_model_adj)
exp(cbind(OR = coef(social_activities_num_model_adj), confint(social_activities_num_model_adj)))

##--- Social Relationships Model (Adjusted) ---
social_relationships_num_model_adj <- glm(
  chronic_pain ~ social_relationships_num + 
    age + sex + race_dichotomized + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(social_relationships_num_model_adj)
exp(cbind(OR = coef(social_relationships_num_model_adj), confint(social_relationships_num_model_adj)))

##--- Social Isolation Model (Adjusted) ---
isolation_score_model_adj <- glm(
  chronic_pain ~ isolation_score + 
    age + sex + race_dichotomized + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(isolation_score_model_adj)
exp(cbind(OR = coef(isolation_score_model_adj), confint(isolation_score_model_adj)))