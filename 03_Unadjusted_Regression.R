library(here)
analytic_data <- read.csv(here("Aim2_data.csv"))

##--- Social Health Dichotomized Model ---
social_health_d_model <- glm(
  chronic_pain ~ social_health_d, 
  data = analytic_data, 
  family = binomial)

summary(social_health_d_model)


##--- Social Health Dichotomized 2 Model ---
social_health2_d_model <- glm(
  chronic_pain ~ social_health2_d, 
  data = analytic_data, 
  family = binomial)

summary(social_health2_d_model)


##--- Social Health Number Model ---
social_health_num_model <- glm(
  chronic_pain ~ social_health_num, 
  data = analytic_data, 
  family = binomial)

summary(social_health_num_model)


##--- Social Activities Number Model ---
social_activities_num_model <- glm(
  chronic_pain ~ social_activities_num, 
  data = analytic_data, 
  family = binomial)

summary(social_activities_num_model)



##--- Social Relationships Number Model ---
social_relationships_num_model <- glm(
  chronic_pain ~ social_relationships_num, 
  data = analytic_data, 
  family = binomial)

summary(social_relationships_num_model)



##--- Social Relationships Number Model ---
isolation_cat_nominal_model <- glm(
  chronic_pain ~ isolation_cat_nominal, 
  data = analytic_data, 
  family = binomial)

summary(isolation_cat_nominal_model)





##--- Social Activities Dichotomized Model ---
social_activities_d_model <- glm(
  chronic_pain ~ social_activities_d, 
  data = analytic_data, 
  family = binomial)

summary(social_activities_d_model)



##--- Social Relationships Dichotomized Model ---
social_relationships_d_model <- glm(
  chronic_pain ~ social_relationships_d, 
  data = analytic_data, 
  family = binomial)

summary(social_relationships_d_model)



##--- Social Isolation Dichotomized Model ---
isolation_cat_d_model <- glm(
  chronic_pain ~ isolation_cat_d, 
  data = analytic_data, 
  family = binomial)

summary(isolation_cat_d_model)



##--- Social Activities Dichotomized Model ---
social_activities2_d_model <- glm(
  chronic_pain ~ social_activities2_d, 
  data = analytic_data, 
  family = binomial)

summary(social_activities2_d_model)



##--- Social Relationships Dichotomized 2 Model ---
social_relationships2_d_model <- glm(
  chronic_pain ~ social_relationships2_d, 
  data = analytic_data, 
  family = binomial)

summary(social_relationships2_d_model)



##--- Social Isolation Dichotomized 2 Model ---
isolation_cat2_d_model <- glm(
  chronic_pain ~ isolation_cat2_d, 
  data = analytic_data, 
  family = binomial)

summary(isolation_cat2_d_model)





