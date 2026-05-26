library(here)
analytic_data <- read.csv(here("Aim2_data.csv"))

##--- Social Health Model ---
social_health_num_model <- glm(
  chronic_pain ~ social_health_num, 
  data = analytic_data, 
  family = binomial)
summary(social_health_num_model)
confint(social_health_num_model)
exp(cbind(OR = coef(social_health_num_model), confint(social_health_num_model)))

##--- Social Function Model ---
social_function_model <- glm(
  chronic_pain ~ social_function_num,
  data = analytic_data,
  family = binomial)
summary(social_function_model)
confint(social_function_model)
exp(cbind(OR = coef(social_function_model), confint(social_function_model)))

##--- Social Activities Model ---
social_activities_num_model <- glm(
  chronic_pain ~ social_activities_num, 
  data = analytic_data, 
  family = binomial)
summary(social_activities_num_model)
confint(social_activities_num_model)
exp(cbind(OR = coef(social_activities_num_model), confint(social_activities_num_model)))

##--- Social Relationships Model ---
social_relationships_num_model <- glm(
  chronic_pain ~ social_relationships_num, 
  data = analytic_data, 
  family = binomial)
summary(social_relationships_num_model)
confint(social_relationships_num_model)
exp(cbind(OR = coef(social_relationships_num_model), confint(social_relationships_num_model)))

##--- Social Isolation Model ---
isolation_score_model <- glm(
  chronic_pain ~ isolation_score, 
  data = analytic_data, 
  family = binomial)
summary(isolation_score_model)
confint(isolation_score_model)
exp(cbind(OR = coef(isolation_score_model), confint(isolation_score_model)))