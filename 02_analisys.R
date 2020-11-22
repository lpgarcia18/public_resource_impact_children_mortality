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
external_factors <- c("FERTILITY_RATE_LAGGED", "PLUS_65_YEARS_LAGGED", "LOG_GDP_PER_CAP_LAGGED", "LOG_OOP_PER_CAP_LAGGED", "GINI_LAGGED", 
			"POVERTY_GAP_LAGGED", "INFLATION_LAGGED", "UNEMPLOYMENT_LAGGED", "BASIC_SANITATION_LAGGED", "BASIC_WATER_LAGGED", 
			"UNEMPLOYMENT_FEM_LAGGED", "SCHOOL_FEM_LAGGED", "WOMEN_PARLIAMENT_LAGGED", "SCHOOL_LIFE_EXP_LAGGED", "OUT_OF_SCHOOL_LAGGED", 
			"CONTROL_CORRUPTION_LAGGED", "GOV_EFFECTIVENESS_LAGGED", "POLITICAL_STABILITY_LAGGED", "REGULATORY_QUALITY_LAGGED", "RULE_OF_LAW_LAGGED", 
			"UNDERNOURISHMENT_LAGGED", "DOCTORS_LAGGED", "DELIVERY_ASSISTANCE_LAGGED", "AIDS_PREVALENCE_LAGGED", "MALARIA_INCIDENCE_LAGGED", 
			"DPT_LAGGED", "HOSPITAL_BEDS_LAGGED", "ELECTRICITY_LAGGED", "URBAN_RATE_LAGGED", "pop", "POP_DENS", "LONG", "LAT")

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
m.out_public <- weightit(formula = as.formula(p_cor_public),
		  		data= dose_resp, method = "super",  				
		 		SL.library = c("SL.bayesglm",
					       "SL.glm",
					       "SL.nnet",
					       "SL.nnls",
					       "SL.ranger",
					       "SL.gbm",
					       "SL.xgboost"
					       ))
summary(m.out_public)
bal.tab(m.out_public, un = TRUE, m.threshold = .05)
m.out_public_trim <- trim(m.out_public, at = .9, lower = TRUE)
summary(m.out_public_trim)
bal.tab(m.out_public_trim, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")

d.w_public <- svydesign(ids = ~1, 
		 weights = m.out_public_trim$weights, data = dose_resp)


#Health Expenditure
set.seed(123)
m.out_health <- weightit(formula = as.formula(p_cor_health),
		  		data= dose_resp, method = "super",  				
		 		SL.library = c("SL.bayesglm",
					       "SL.glm",
					       "SL.nnet",
					       "SL.nnls",
					       "SL.ranger",
					       "SL.gbm",
					       "SL.xgboost"
					       ))
summary(m.out_health)
bal.tab(m.out_health, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")
m.out_health_trim <- trim(m.out_health, at = .9, lower = TRUE)
summary(m.out_health_trim)
bal.tab(m.out_health_trim, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")
d.w_health <- svydesign(ids = ~1, 
		 weights = m.out_health_trim$weights, data = dose_resp)


#Other Expenditure
set.seed(123)
m.out_other <- weightit(formula = as.formula(p_cor_other),
		  		data= dose_resp, method = "super",  				
		 		SL.library = c("SL.bayesglm",
					       "SL.glm",
					       "SL.nnet",
					       "SL.nnls",
					       "SL.ranger",
					       "SL.gbm",
					       "SL.xgboost"
					       ))
summary(m.out_other)
bal.tab(m.out_other, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")
m.out_other_trim <- trim(m.out_other, at = .9, lower = TRUE)
summary(m.out_other_trim)
bal.tab(m.out_other_trim, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")
d.w_other <- svydesign(ids = ~1, 
		 weights = m.out_other_trim$weights, data = dose_resp)


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
summ(fit_public_neo, confint = TRUE,
model.fit_public_neo = FALSE, model.info = FALSE, digits = 7) 

#Neo - Health
fit_health_neo <- svyglm(as.formula(p_cor_health_neo), design = d.w_health)
summ(fit_health_neo, confint = TRUE,
model.fit_health_neo = FALSE, model.info = FALSE, digits = 7) 

#Neo - Other
fit_other_neo <- svyglm(as.formula(p_cor_other_neo), design = d.w_other)
summ(fit_other_neo, confint = TRUE,
model.fit_other_neo = FALSE, model.info = FALSE, digits = 7) 


#Neo_u5 - Public
fit_public_neo_u5 <- svyglm(as.formula(p_cor_public_neo_u5), design = d.w_public)
summ(fit_public_neo_u5, confint = TRUE,
model.fit_public_neo_u5 = FALSE, model.info = FALSE, digits = 7) 

#Neo_u5 - Health
fit_health_neo_u5 <- svyglm(as.formula(p_cor_health_neo_u5), design = d.w_health)
summ(fit_health_neo_u5, confint = TRUE,
model.fit_health_neo_u5 = FALSE, model.info = FALSE, digits = 7) 

#Neo_u5 - Other
fit_other_neo_u5 <- svyglm(as.formula(p_cor_other_neo_u5), design = d.w_other)
summ(fit_other_neo_u5, confint = TRUE,
model.fit_other_neo_u5 = FALSE, model.info = FALSE, digits = 7) 

