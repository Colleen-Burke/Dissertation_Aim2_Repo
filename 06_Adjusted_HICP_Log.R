library(tidyverse)
library(here)

##--- Social Health Model (Adjusted) ---
social_health_hicp_model_adj <- glm(
  hicp_binary ~ social_health_num + 
    age + sex + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(social_health_hicp_model_adj)
exp(cbind(OR = coef(social_health_hicp_model_adj), confint(social_health_hicp_model_adj)))

##--- Social Activities Model (Adjusted) ---
social_activities_hicp_model_adj <- glm(
  hicp_binary ~ social_activities_num + 
    age + sex + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(social_activities_hicp_model_adj)
exp(cbind(OR = coef(social_activities_hicp_model_adj), confint(social_activities_hicp_model_adj)))

##--- Social Relationships Model (Adjusted) ---
social_relationships_hicp_model_adj <- glm(
  hicp_binary ~ social_relationships_num + 
    age + sex + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(social_relationships_hicp_model_adj)
exp(cbind(OR = coef(social_relationships_hicp_model_adj), confint(social_relationships_hicp_model_adj)))

##--- Social Isolation Model (Adjusted) ---
isolation_hicp_adj <- glm(
  hicp_binary ~ isolation_score + 
    age + sex + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(isolation_hicp_adj)
exp(cbind(OR = coef(isolation_hicp_adj), confint(isolation_hicp_adj)))