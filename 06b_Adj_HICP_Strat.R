library(tidyverse)
library(here)

#--- Split data by race_dichotomized -------------------------------------------
analytic_data_race0 <- analytic_data |> filter(race_dichotomized == 0)
analytic_data_race1 <- analytic_data |> filter(race_dichotomized == 1)

##--- Social Health Model (Adjusted) + Stratified ------------------------------
social_health_hicp_model_adj_race0 <- glm(
  hicp_binary ~ social_health_num + 
    age + sex + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data_race0,
  family = binomial)
summary(social_health_hicp_model_adj_race0)
exp(cbind(OR = coef(social_health_hicp_model_adj_race0), confint.default(social_health_hicp_model_adj_race0)))

social_health_hicp_model_adj_race1 <- glm(
  hicp_binary ~ social_health_num + 
    age + sex + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data_race1,
  family = binomial)
summary(social_health_hicp_model_adj_race1)
exp(cbind(OR = coef(social_health_hicp_model_adj_race1), confint.default(social_health_hicp_model_adj_race1)))

##--- Social Activities Model (Adjusted) + Stratified --------------------------
social_activities_hicp_model_adj_race0 <- glm(
  hicp_binary ~ social_activities_num + 
    age + sex + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data_race0,
  family = binomial)
summary(social_activities_hicp_model_adj_race0)
exp(cbind(OR = coef(social_activities_hicp_model_adj_race0), confint.default(social_activities_hicp_model_adj_race0)))

social_activities_hicp_model_adj_race1 <- glm(
  hicp_binary ~ social_activities_num + 
    age + sex + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data_race1,
  family = binomial)
summary(social_activities_hicp_model_adj_race1)
exp(cbind(OR = coef(social_activities_hicp_model_adj_race1), confint.default(social_activities_hicp_model_adj_race1)))

##--- Social Relationships Model (Adjusted) + Stratified -----------------------
social_relationships_hicp_model_adj_race0 <- glm(
  hicp_binary ~ social_relationships_num + 
    age + sex + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data_race0,
  family = binomial)
summary(social_relationships_hicp_model_adj_race0)
exp(cbind(OR = coef(social_relationships_hicp_model_adj_race0), confint.default(social_relationships_hicp_model_adj_race0)))

social_relationships_hicp_model_adj_race1 <- glm(
  hicp_binary ~ social_relationships_num + 
    age + sex + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data_race1,
  family = binomial)
summary(social_relationships_hicp_model_adj_race1)
exp(cbind(OR = coef(social_relationships_hicp_model_adj_race1), confint.default(social_relationships_hicp_model_adj_race1)))

##--- Social Isolation Model (Adjusted) + Stratified ---------------------------
isolation_hicp_adj_race0 <- glm(
  hicp_binary ~ isolation_score + 
    age + sex + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data_race0,
  family = binomial)
summary(isolation_hicp_adj_race0)
exp(cbind(OR = coef(isolation_hicp_adj_race0), confint.default(isolation_hicp_adj_race0)))

isolation_hicp_adj_race1 <- glm(
  hicp_binary ~ isolation_score + 
    age + sex + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data_race1,
  family = binomial)
summary(isolation_hicp_adj_race1)
exp(cbind(OR = coef(isolation_hicp_adj_race1), confint.default(isolation_hicp_adj_race1)))