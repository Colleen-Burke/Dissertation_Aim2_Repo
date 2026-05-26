##--- Social Health Dichotomized Model (Adjusted) ---
social_health_d_model_adj <- glm(
  chronic_pain ~ social_health_d + 
    age + sex + race_f + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(social_health_d_model_adj)

##--- Social Health Dichotomized 2 Model (Adjusted) ---
social_health2_d_model_adj <- glm(
  chronic_pain ~ social_health2_d + 
    age + sex + race_f + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(social_health2_d_model_adj)

##--- Social Health Model (Adjusted) ---
social_health_num_model_adj <- glm(
  chronic_pain ~ social_health_num + 
    age + sex + race_f + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(social_health_num_model_adj)

##--- Social Function Model (Adjusted) ---
social_function_num_model_adj <- glm(
  chronic_pain ~ social_function_num + 
    age + sex + race_f + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(social_function_num_model_adj)

##--- Social Activities Model (Adjusted) ---
social_activities_num_model_adj <- glm(
  chronic_pain ~ social_activities_num + 
    age + sex + race_f + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(social_activities_num_model_adj)

##--- Social Relationships Model (Adjusted) ---
social_relationships_num_model_adj <- glm(
  chronic_pain ~ social_relationships_num + 
    age + sex + race_f + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(social_relationships_num_model_adj)

##--- Social Isolation Model (Adjusted) ---
isolation_score_model_adj <- glm(
  chronic_pain ~ isolation_score + 
    age + sex + race_f + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(isolation_score_model_adj)

##--- Social Activities Dichotomized Model (Adjusted) ---
social_activities_d_model_adj <- glm(
  chronic_pain ~ social_activities_d + 
    age + sex + race_f + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(social_activities_d_model_adj)

##--- Social Relationships Dichotomized Model (Adjusted) ---
social_relationships_d_model_adj <- glm(
  chronic_pain ~ social_relationships_d + 
    age + sex + race_f + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(social_relationships_d_model_adj)

##--- Social Isolation Dichotomized Model (Adjusted) ---
isolation_cat_d_model_adj <- glm(
  chronic_pain ~ isolation_cat_d + 
    age + sex + race_f + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(isolation_cat_d_model_adj)

##--- Social Activities Dichotomized 2 Model (Adjusted) ---
social_activities2_d_model_adj <- glm(
  chronic_pain ~ social_activities2_d + 
    age + sex + race_f + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(social_activities2_d_model_adj)

##--- Social Relationships Dichotomized 2 Model (Adjusted) ---
social_relationships2_d_model_adj <- glm(
  chronic_pain ~ social_relationships2_d + 
    age + sex + race_f + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(social_relationships2_d_model_adj)

##--- Social Isolation Dichotomized 2 Model (Adjusted) ---
isolation_cat2_d_model_adj <- glm(
  chronic_pain ~ isolation_cat2_d + 
    age + sex + race_f + ethnicity_f + bmi_measured + general_health + 
    mental_health + psqi_global_score + lbp_vas_current,
  data = analytic_data,
  family = binomial)
summary(isolation_cat2_d_model_adj)