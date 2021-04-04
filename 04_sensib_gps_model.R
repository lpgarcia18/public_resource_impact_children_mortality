# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)


# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(lubridate)
library(WeightIt)
library(cobalt)
library(SuperLearner)
library(jtools)
library(survey)

# Bases -----------------------------------------------------------------
dose_resp <- read_csv("bases/completed_base.csv")
dose_resp$LOCATION <- as.factor(dose_resp$LOCATION)

# Bivariate analysis for match -----------------------
#Problems with imputation
# HOSPITAL_BEDS_LAGGED
# MALARIA_INCIDENCE_LAGGED
# UNDERNOURISHMENT_LAGGED
# OUT_OF_SCHOOL_LAGGED
# POVERTY_GAP_LAGGED
# GINI_LAGGED
external_factors <- c("FERTILITY_RATE_LAGGED", "PLUS_65_YEARS_LAGGED", "LOG_GDP_PER_CAP_LAGGED", "LOG_OOP_PER_CAP_LAGGED",
			"INFLATION_LAGGED", "UNEMPLOYMENT_LAGGED", "BASIC_SANITATION_LAGGED", "BASIC_WATER_LAGGED",
			"UNEMPLOYMENT_FEM_LAGGED", "SCHOOL_FEM_LAGGED", "WOMEN_PARLIAMENT_LAGGED", "SCHOOL_LIFE_EXP_LAGGED",
			"CONTROL_CORRUPTION_LAGGED", "GOV_EFFECTIVENESS_LAGGED", "POLITICAL_STABILITY_LAGGED", "REGULATORY_QUALITY_LAGGED", "RULE_OF_LAW_LAGGED",
			"DOCTORS_LAGGED", "DELIVERY_ASSISTANCE_LAGGED", "AIDS_PREVALENCE_LAGGED",
			"DPT_LAGGED", "ELECTRICITY_LAGGED", "URBAN_RATE_LAGGED", "pop", "POP_DENS", "LONG", "LAT")


p_cor_ext <- function(compare){ #function to calculate p value of pearson correlationg and extract external factors with p <= 0.25
	biv_compare <-list()
	for(i in 1:length(external_factors)){
		biv_compare[[i]] <- cor.test(x = as.matrix(dose_resp[,colnames(dose_resp) == compare]), 
					       y = as.matrix(dose_resp[,colnames(dose_resp) == external_factors[i]]))$p.value
	}	
	biv_compare <- do.call(cbind, biv_compare)
	biv_compare <- t(biv_compare) %>% as.data.frame()
	names(biv_compare) <- "p_value"
	biv_compare$ext_factor <- external_factors
	biv_compare <- subset(biv_compare, biv_compare$p_value <= 0.25)
	biv_compare <- biv_compare$ext_factor
	a <- paste(head(biv_compare,-1), " + ")
	a <- c(paste(compare," ~"), a, tail(biv_compare,1))
	return(a)
}


p_cor_public <- p_cor_ext("LOG_PUBLIC_EXP_PER_CAP_LAGGED")
p_cor_health <- p_cor_ext("LOG_HEALTH_EXP_LAGGED")
p_cor_other <- p_cor_ext("LOG_OTHER_EXP_LAGGED")



# Match -------------------------------------------------------------------
#SuperLearner::listWrappers()
#Public Expenditure
set.seed(123)
m.out_public_super <- weightit(formula = as.formula(p_cor_public),
		  		data= dose_resp, method = "super",  				
		 		SL.library = c(
					       "SL.glm",
					       "SL.nnet",
					       "SL.nnls",
					       "SL.ranger",
					       "SL.gbm",
					       "SL.xgboost"
					       )) %>% trim(at = .9, lower = TRUE)
m.out_public_ps <- weightit(formula = as.formula(p_cor_public),
		  		data= dose_resp, method = "ps") %>% trim(at = .9, lower = TRUE)
m.out_public_cbps <- weightit(formula = as.formula(p_cor_public),
		  		data= dose_resp, method = "cbps") %>% trim(at = .9, lower = TRUE)
m.out_public_npcbps <- weightit(formula = as.formula(p_cor_public),
		  		data= dose_resp, method = "npcbps") %>% trim(at = .9, lower = TRUE)

coef_var_public_super <- summary(m.out_public_super)$coef.of.var
coef_var_public_ps <- summary(m.out_public_ps)$coef.of.var
coef_var_public_cbps <- summary(m.out_public_cbps)$coef.of.var
coef_var_public_npcbps <- summary(m.out_public_npcbps)$coef.of.var

coef_var_public <- rbind(coef_var_public_super, coef_var_public_ps, coef_var_public_cbps, coef_var_public_npcbps) %>% as.data.frame()

d.w_public <- svydesign(ids = ~1, 
		 weights = m.out_public_super$weights, data = dose_resp)



#Health Expenditure
set.seed(123)
m.out_health_super <- weightit(formula = as.formula(p_cor_health),
		  		data= dose_resp, method = "super",  				
		 		SL.library = c(
					       "SL.glm",
					       "SL.nnet",
					       "SL.nnls",
					       "SL.ranger",
					       "SL.gbm",
					       "SL.xgboost"
					       )) %>% trim(at = .9, lower = TRUE)
m.out_health_ps <- weightit(formula = as.formula(p_cor_health),
		  		data= dose_resp, method = "ps") %>% trim(at = .9, lower = TRUE)
m.out_health_cbps <- weightit(formula = as.formula(p_cor_health),
		  		data= dose_resp, method = "cbps") %>% trim(at = .9, lower = TRUE)
m.out_health_npcbps <- weightit(formula = as.formula(p_cor_health),
		  		data= dose_resp, method = "npcbps") %>% trim(at = .9, lower = TRUE)

coef_var_health_super <- summary(m.out_health_super)$coef.of.var
coef_var_health_ps <- summary(m.out_health_ps)$coef.of.var
coef_var_health_cbps <- summary(m.out_health_cbps)$coef.of.var
coef_var_health_npcbps <- summary(m.out_health_npcbps)$coef.of.var

coef_var_health <- rbind(coef_var_health_super, coef_var_health_ps, coef_var_health_cbps, coef_var_health_npcbps) %>% as.data.frame()

d.w_health <- svydesign(ids = ~1, 
		 weights = m.out_health_super$weights, data = dose_resp)

#Other Expenditure
set.seed(123)
m.out_other_super <- weightit(formula = as.formula(p_cor_other),
		  		data= dose_resp, method = "super",  				
		 		SL.library = c(
					       "SL.glm",
					       "SL.nnet",
					       "SL.nnls",
					       "SL.ranger",
					       "SL.gbm",
					       "SL.xgboost"
					       )) %>% trim(at = .9, lower = TRUE)
m.out_other_ps <- weightit(formula = as.formula(p_cor_other),
		  		data= dose_resp, method = "ps") %>% trim(at = .9, lower = TRUE)
m.out_other_cbps <- weightit(formula = as.formula(p_cor_other),
		  		data= dose_resp, method = "cbps") %>% trim(at = .9, lower = TRUE)
m.out_other_npcbps <- weightit(formula = as.formula(p_cor_other),
		  		data= dose_resp, method = "npcbps") %>% trim(at = .9, lower = TRUE)


coef_var_other_super <- summary(m.out_other_super)$coef.of.var
coef_var_other_ps <- summary(m.out_other_ps)$coef.of.var
coef_var_other_cbps <- summary(m.out_other_cbps)$coef.of.var
coef_var_other_npcbps <- summary(m.out_other_npcbps)$coef.of.var

coef_var_other <- rbind(coef_var_other_super, coef_var_other_ps, coef_var_other_cbps, coef_var_other_npcbps) %>% as.data.frame()

d.w_other <- svydesign(ids = ~1, 
		 weights = m.out_other_super$weights, data = dose_resp)

# Bivariate analysis for regression -----------------------

p_cor_ext_reg <- function(compare, treatment){ #function to calculate p value of pearson correlationg and extract external factors with p <= 0.25
	biv_compare <-list()
	for(i in 1:length(external_factors)){
		biv_compare[[i]] <- cor.test(x = as.matrix(dose_resp[,colnames(dose_resp) == compare]), 
					       y = as.matrix(dose_resp[,colnames(dose_resp) == external_factors[i]]))$p.value
	}	
	biv_compare <- do.call(cbind, biv_compare)
	biv_compare <- t(biv_compare) %>% as.data.frame()
	names(biv_compare) <- "p_value"
	biv_compare$ext_factor <- external_factors
	biv_compare <- subset(biv_compare, biv_compare$p_value <= 0.25)
	biv_compare <- biv_compare$ext_factor
	a <- paste(head(biv_compare,-1), " + ")
	a <- c(paste(compare," ~"),paste(treatment, " + "), a, tail(biv_compare,1))
	return(a)
}


p_cor_public_neo <- p_cor_ext_reg("MEAN_RATE_NEO", "LOG_PUBLIC_EXP_PER_CAP_LAGGED")
p_cor_health_neo <- p_cor_ext_reg("MEAN_RATE_NEO", "LOG_HEALTH_EXP_LAGGED")
p_cor_other_neo <- p_cor_ext_reg("MEAN_RATE_NEO", "LOG_OTHER_EXP_LAGGED")

p_cor_public_neo_u5 <- p_cor_ext_reg("MEAN_RATE_NEO_U5", "LOG_PUBLIC_EXP_PER_CAP_LAGGED")
p_cor_health_neo_u5 <- p_cor_ext_reg("MEAN_RATE_NEO_U5", "LOG_HEALTH_EXP_LAGGED")
p_cor_other_neo_u5 <- p_cor_ext_reg("MEAN_RATE_NEO_U5", "LOG_OTHER_EXP_LAGGED")



# Regression --------------------------------------------------------------
#Neo - Public
fit_public_neo <- svyglm(as.formula(p_cor_public_neo), design = d.w_public)
fit_public_neo_step <- stepAIC(fit_public_neo, k = 2)
public_neo_impact <- summ(fit_public_neo_step, confint = TRUE,
model.fit_public_neo = FALSE, model.info = FALSE, digits = 7) 

#Neo - Health
fit_health_neo <- svyglm(as.formula(p_cor_health_neo), design = d.w_health)
fit_health_neo_step <- stepAIC(fit_health_neo, k = 2)
health_neo_impact <- summ(fit_health_neo_step, confint = TRUE,
model.fit_health_neo = FALSE, model.info = FALSE, digits = 7) 

#Neo - Other
fit_other_neo <- svyglm(as.formula(p_cor_other_neo), design = d.w_other)
fit_other_neo_step <- stepAIC(fit_other_neo, k = 2)
other_neo_impact <- summ(fit_other_neo_step, confint = TRUE,
model.fit_other_neo = FALSE, model.info = FALSE, digits = 7) 

#Neo_u5 - Public
fit_public_neo_u5 <- svyglm(as.formula(p_cor_public_neo_u5), design = d.w_public)
fit_public_neo_u5_step <- stepAIC(fit_public_neo_u5, k = 2)
public_neo_u5_impact <- summ(fit_public_neo_u5_step, confint = TRUE,
model.fit_public_neo_u5 = FALSE, model.info = FALSE, digits = 7) 

#Neo_u5 - Health
fit_health_neo_u5 <- svyglm(as.formula(p_cor_health_neo_u5), design = d.w_health)
fit_health_neo_u5_step <- stepAIC(fit_health_neo_u5, k = 2)
health_neo_u5_impact <- summ(fit_health_neo_u5_step, confint = TRUE,
model.fit_health_neo_u5 = FALSE, model.info = FALSE, digits = 7) 

#Neo_u5 - Other
fit_other_neo_u5 <- svyglm(as.formula(p_cor_other_neo_u5), design = d.w_other)
fit_other_neo_u5_step <- stepAIC(fit_other_neo_u5,  k=2)
other_neo_u5_impact <- summ(fit_other_neo_u5_step, confint = TRUE,
model.fit_other_neo_u5 = FALSE, model.info = FALSE, digits = 7) 


#Extracting data from fit models
impact_sensib_gps_model <- rbind(public_neo_impact$coeftable[2,c(1,2,3)] %>% data.frame() %>% t(),
      health_neo_impact$coeftable[2,c(1,2,3)] %>% data.frame() %>% t(),
      other_neo_impact$coeftable[2,c(1,2,3)] %>% data.frame() %>% t(),
      public_neo_u5_impact$coeftable[2,c(1,2,3)] %>% data.frame() %>% t(),
      health_neo_u5_impact$coeftable[2,c(1,2,3)] %>% data.frame() %>% t(),
      other_neo_u5_impact$coeftable[2,c(1,2,3)] %>% data.frame() %>% t()) %>% as.data.frame(row.names = F)
impact_sensib_gps_model$treatment <- c("Total public expenditure", 
			    "Health public expenditure",
			    "Public expenditure in other sectores")
impact_sensib_gps_model$mortality_rate <- c(rep("NeoRt",3),rep("NeoU5Rt",3))

write.csv(impact_sensib_gps_model, "bases/impact_sensib_gps_model.csv", row.names = F)
