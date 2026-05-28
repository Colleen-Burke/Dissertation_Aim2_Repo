library(here)
analytic_data <- read.csv(here("Aim2_data.csv"))

##--- Social Health Model ---
social_health_hicp_model <- glm(
  hicp_binary ~ social_health_num, 
  data = analytic_data, 
  family = binomial)
summary(social_health_hicp_model)
confint(social_health_hicp_model)
exp(cbind(OR = coef(social_health_hicp_model), confint(social_health_hicp_model)))


##--- Social Activities Model ---
social_activities_hicp_model <- glm(
  hicp_binary ~ social_activities_num, 
  data = analytic_data, 
  family = binomial)
summary(social_activities_hicp_model)
confint(social_activities_hicp_model)
exp(cbind(OR = coef(social_activities_hicp_model), confint(social_activities_hicp_model)))

##--- Social Relationships Model ---
social_relationships_hicp_model <- glm(
  hicp_binary ~ social_relationships_num, 
  data = analytic_data, 
  family = binomial)
summary(social_relationships_hicp_model)
confint(social_relationships_hicp_model)
exp(cbind(OR = coef(social_relationships_hicp_model), confint(social_relationships_hicp_model)))

##--- Social Isolation Model ---
isolation_score_hicp_model <- glm(
  hicp_binary ~ isolation_score, 
  data = analytic_data, 
  family = binomial)
summary(isolation_score_hicp_model)
confint(isolation_score_hicp_model)
exp(cbind(OR = coef(isolation_score_hicp_model), confint(isolation_score_hicp_model)))