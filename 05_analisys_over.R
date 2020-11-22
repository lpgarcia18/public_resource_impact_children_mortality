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
dose_resp_over <- read_csv("bases/completed_base.csv")
dose_resp_over$LOCATION <- as.factor(dose_resp_over$LOCATION)
dose_resp_over <- subset(dose_resp_over, dose_resp_over$GDP_PER_CAP_LAGGED >= quantile(dose_resp_over$GDP_PER_CAP_LAGGED,0.5))

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
		biv_compare[[i]] <- cor.test(x = as.matrix(dose_resp_over[,colnames(dose_resp_over) == compare]), 
					       y = as.matrix(dose_resp_over[,colnames(dose_resp_over) == external_factors[i]]))$p.value
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


p_cor_public_over <- p_cor_ext("LOG_PUBLIC_EXP_PER_CAP_LAGGED")
p_cor_health_over <- p_cor_ext("LOG_HEALTH_EXP_LAGGED")
p_cor_other_over <- p_cor_ext("LOG_OTHER_EXP_LAGGED")



# Match -------------------------------------------------------------------
#SuperLearner::listWrappers()
#Public Expenditure
set.seed(123)
m.out_public_over <- weightit(formula = as.formula(p_cor_public_over),
		  		data= dose_resp_over, method = "super",  				
		 		SL.library = c("SL.bayesglm",
					       "SL.glm",
					       "SL.nnet",
					       "SL.nnls",
					       "SL.ranger",
					       "SL.gbm",
					       "SL.xgboost"
					       ))
summary(m.out_public_over)
bal.tab(m.out_public_over, un = TRUE, m.threshold = .05)
m.out_public_over_trim <- trim(m.out_public_over, at = .9, lower = TRUE)
summary(m.out_public_over_trim)
bal.tab(m.out_public_over_trim, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")

d.w_public_over <- svydesign(ids = ~1, 
		 weights = m.out_public_over_trim$weights, data = dose_resp_over)


#Health Expenditure
set.seed(123)
m.out_health_over <- weightit(formula = as.formula(p_cor_health_over),
		  		data= dose_resp_over, method = "super",  				
		 		SL.library = c("SL.bayesglm",
					       "SL.glm",
					       "SL.nnet",
					       "SL.nnls",
					       "SL.ranger",
					       "SL.gbm",
					       "SL.xgboost"
					       ))
summary(m.out_health_over)
bal.tab(m.out_health_over, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")
m.out_health_over_trim <- trim(m.out_health_over, at = .9, lower = TRUE)
summary(m.out_health_over_trim)
bal.tab(m.out_health_over_trim, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")
d.w_health_over <- svydesign(ids = ~1, 
		 weights = m.out_health_over_trim$weights, data = dose_resp_over)


#Other Expenditure
set.seed(123)
m.out_other_over <- weightit(formula = as.formula(p_cor_other_over),
		  		data= dose_resp_over, method = "super",  				
		 		SL.library = c("SL.bayesglm",
					       "SL.glm",
					       "SL.nnet",
					       "SL.nnls",
					       "SL.ranger",
					       "SL.gbm",
					       "SL.xgboost"
					       ))
summary(m.out_other_over)
bal.tab(m.out_other_over, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")
m.out_other_over_trim <- trim(m.out_other_over, at = .9, lower = TRUE)
summary(m.out_other_over_trim)
bal.tab(m.out_other_over_trim, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")
d.w_other_over <- svydesign(ids = ~1, 
		 weights = m.out_other_over_trim$weights, data = dose_resp_over)


# Bivariate analysis for regression -----------------------

p_cor_ext_reg <- function(compare, treatment){ #function to calculate p value of pearson correlationg and extract external factors with p <= 0.25
	biv_compare <-list()
	for(i in 1:length(external_factors)){
		biv_compare[[i]] <- cor.test(x = as.matrix(dose_resp_over[,colnames(dose_resp_over) == compare]), 
					       y = as.matrix(dose_resp_over[,colnames(dose_resp_over) == external_factors[i]]))$p.value
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


p_cor_public_over_neo <- p_cor_ext_reg("MEAN_RATE_NEO", "LOG_PUBLIC_EXP_PER_CAP_LAGGED")
p_cor_health_over_neo <- p_cor_ext_reg("MEAN_RATE_NEO", "LOG_HEALTH_EXP_LAGGED")
p_cor_other_over_neo <- p_cor_ext_reg("MEAN_RATE_NEO", "LOG_OTHER_EXP_LAGGED")

p_cor_public_over_neo_u5 <- p_cor_ext_reg("MEAN_RATE_NEO_U5", "LOG_PUBLIC_EXP_PER_CAP_LAGGED")
p_cor_health_over_neo_u5 <- p_cor_ext_reg("MEAN_RATE_NEO_U5", "LOG_HEALTH_EXP_LAGGED")
p_cor_other_over_neo_u5 <- p_cor_ext_reg("MEAN_RATE_NEO_U5", "LOG_OTHER_EXP_LAGGED")



# Regression --------------------------------------------------------------
#Neo - Public
fit_public_over_neo <- svyglm(as.formula(p_cor_public_over_neo), design = d.w_public_over)
summ(fit_public_over_neo, confint = TRUE,
model.fit_public_over_neo = FALSE, model.info = FALSE, digits = 7) 

#Neo - Health
fit_health_over_neo <- svyglm(as.formula(p_cor_health_over_neo), design = d.w_health_over)
summ(fit_health_over_neo, confint = TRUE,
model.fit_health_over_neo = FALSE, model.info = FALSE, digits = 7) 

#Neo - Other
fit_other_over_neo <- svyglm(as.formula(p_cor_other_over_neo), design = d.w_other_over)
summ(fit_other_over_neo, confint = TRUE,
model.fit_other_over_neo = FALSE, model.info = FALSE, digits = 7) 


#Neo_u5 - Public
fit_public_over_neo_u5 <- svyglm(as.formula(p_cor_public_over_neo_u5), design = d.w_public_over)
summ(fit_public_over_neo_u5, confint = TRUE,
model.fit_public_over_neo_u5 = FALSE, model.info = FALSE, digits = 7) 

#Neo_u5 - Health
fit_health_over_neo_u5 <- svyglm(as.formula(p_cor_health_over_neo_u5), design = d.w_health_over)
summ(fit_health_over_neo_u5, confint = TRUE,
model.fit_health_over_neo_u5 = FALSE, model.info = FALSE, digits = 7) 

#Neo_u5 - Other
fit_other_over_neo_u5 <- svyglm(as.formula(p_cor_other_over_neo_u5), design = d.w_other_over)
summ(fit_other_over_neo_u5, confint = TRUE,
model.fit_other_over_neo_u5 = FALSE, model.info = FALSE, digits = 7) 

