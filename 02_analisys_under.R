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
dose_resp_under <- read_csv("bases/completed_base.csv")
dose_resp_under$LOCATION <- as.factor(dose_resp_under$LOCATION)
dose_resp_under <- subset(dose_resp_under, dose_resp_under$GDP_PER_CAP_LAGGED < quantile(dose_resp_under$GDP_PER_CAP_LAGGED,0.5))

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
		biv_compare[[i]] <- cor.test(x = as.matrix(dose_resp_under[,colnames(dose_resp_under) == compare]), 
					       y = as.matrix(dose_resp_under[,colnames(dose_resp_under) == external_factors[i]]))$p.value
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


p_cor_public_under <- p_cor_ext("LOG_PUBLIC_EXP_PER_CAP_LAGGED")
p_cor_health_under <- p_cor_ext("LOG_HEALTH_EXP_LAGGED")
p_cor_other_under <- p_cor_ext("LOG_OTHER_EXP_LAGGED")



# Match -------------------------------------------------------------------
#SuperLearner::listWrappers()
#Public Expenditure
set.seed(123)
m.out_public_under <- weightit(formula = as.formula(p_cor_public_under),
		  		data= dose_resp_under, method = "super",  				
		 		SL.library = c("SL.bayesglm",
					       "SL.glm",
					       "SL.nnet",
					       "SL.nnls",
					       "SL.ranger",
					       "SL.gbm",
					       "SL.xgboost"
					       ))
summary(m.out_public_under)
bal.tab(m.out_public_under, un = TRUE, m.threshold = .05)
m.out_public_under_trim <- trim(m.out_public_under, at = .9, lower = TRUE)
summary(m.out_public_under_trim)
bal.tab(m.out_public_under_trim, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")

d.w_public_under <- svydesign(ids = ~1, 
		 weights = m.out_public_under_trim$weights, data = dose_resp_under)


#Health Expenditure
set.seed(123)
m.out_health_under <- weightit(formula = as.formula(p_cor_health_under),
		  		data= dose_resp_under, method = "super",  				
		 		SL.library = c("SL.bayesglm",
					       "SL.glm",
					       "SL.nnet",
					       "SL.nnls",
					       "SL.ranger",
					       "SL.gbm",
					       "SL.xgboost"
					       ))
summary(m.out_health_under)
bal.tab(m.out_health_under, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")
m.out_health_under_trim <- trim(m.out_health_under, at = .9, lower = TRUE)
summary(m.out_health_under_trim)
bal.tab(m.out_health_under_trim, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")
d.w_health_under <- svydesign(ids = ~1, 
		 weights = m.out_health_under_trim$weights, data = dose_resp_under)


#Other Expenditure
set.seed(123)
m.out_other_under <- weightit(formula = as.formula(p_cor_other_under),
		  		data= dose_resp_under, method = "super",  				
		 		SL.library = c("SL.bayesglm",
					       "SL.glm",
					       "SL.nnet",
					       "SL.nnls",
					       "SL.ranger",
					       "SL.gbm",
					       "SL.xgboost"
					       ))
summary(m.out_other_under)
bal.tab(m.out_other_under, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")
m.out_other_under_trim <- trim(m.out_other_under, at = .9, lower = TRUE)
summary(m.out_other_under_trim)
bal.tab(m.out_other_under_trim, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")
d.w_other_under <- svydesign(ids = ~1, 
		 weights = m.out_other_under_trim$weights, data = dose_resp_under)


# Bivariate analysis for regression -----------------------

p_cor_ext_reg <- function(compare, treatment){ #function to calculate p value of pearson correlationg and extract external factors with p <= 0.25
	biv_compare <-list()
	for(i in 1:length(external_factors)){
		biv_compare[[i]] <- cor.test(x = as.matrix(dose_resp_under[,colnames(dose_resp_under) == compare]), 
					       y = as.matrix(dose_resp_under[,colnames(dose_resp_under) == external_factors[i]]))$p.value
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


p_cor_public_under_neo <- p_cor_ext_reg("MEAN_RATE_NEO", "LOG_PUBLIC_EXP_PER_CAP_LAGGED")
p_cor_health_under_neo <- p_cor_ext_reg("MEAN_RATE_NEO", "LOG_HEALTH_EXP_LAGGED")
p_cor_other_under_neo <- p_cor_ext_reg("MEAN_RATE_NEO", "LOG_OTHER_EXP_LAGGED")

p_cor_public_under_neo_u5 <- p_cor_ext_reg("MEAN_RATE_NEO_U5", "LOG_PUBLIC_EXP_PER_CAP_LAGGED")
p_cor_health_under_neo_u5 <- p_cor_ext_reg("MEAN_RATE_NEO_U5", "LOG_HEALTH_EXP_LAGGED")
p_cor_other_under_neo_u5 <- p_cor_ext_reg("MEAN_RATE_NEO_U5", "LOG_OTHER_EXP_LAGGED")



# Regression --------------------------------------------------------------
#Neo - Public
fit_public_under_neo <- svyglm(as.formula(p_cor_public_under_neo), design = d.w_public_under)
public_under_neo_impact <- summ(fit_public_under_neo, confint = TRUE,
model.fit_public_under_neo = FALSE, model.info = FALSE, digits = 7) 

#Neo - Health
fit_health_under_neo <- svyglm(as.formula(p_cor_health_under_neo), design = d.w_health_under)
health_under_neo_impact <- summ(fit_health_under_neo, confint = TRUE,
model.fit_health_under_neo = FALSE, model.info = FALSE, digits = 7) 

#Neo - Other
fit_other_under_neo <- svyglm(as.formula(p_cor_other_under_neo), design = d.w_other_under)
other_under_neo_impact <- summ(fit_other_under_neo, confint = TRUE,
model.fit_other_under_neo = FALSE, model.info = FALSE, digits = 7) 


#Neo_u5 - Public
fit_public_under_neo_u5 <- svyglm(as.formula(p_cor_public_under_neo_u5), design = d.w_public_under)
public_under_neo_u5_impact <- summ(fit_public_under_neo_u5, confint = TRUE,
model.fit_public_under_neo_u5 = FALSE, model.info = FALSE, digits = 7) 

#Neo_u5 - Health
fit_health_under_neo_u5 <- svyglm(as.formula(p_cor_health_under_neo_u5), design = d.w_health_under)
health_under_neo_u5_impact <- summ(fit_health_under_neo_u5, confint = TRUE,
model.fit_health_under_neo_u5 = FALSE, model.info = FALSE, digits = 7) 

#Neo_u5 - Other
fit_other_under_neo_u5 <- svyglm(as.formula(p_cor_other_under_neo_u5), design = d.w_other_under)
other_under_neo_u5_impact <- summ(fit_other_under_neo_u5, confint = TRUE,
model.fit_other_under_neo_u5 = FALSE, model.info = FALSE, digits = 7) 


#Extracting data from fit models
impact_under <- rbind(public_under_neo_impact$coeftable[2,c(1,2,3)] %>% data.frame() %>% t(),
      health_under_neo_impact$coeftable[2,c(1,2,3)] %>% data.frame() %>% t(),
      other_under_neo_impact$coeftable[2,c(1,2,3)] %>% data.frame() %>% t(),
      public_under_neo_u5_impact$coeftable[2,c(1,2,3)] %>% data.frame() %>% t(),
      health_under_neo_u5_impact$coeftable[2,c(1,2,3)] %>% data.frame() %>% t(),
      other_under_neo_u5_impact$coeftable[2,c(1,2,3)] %>% data.frame() %>% t()) %>% as.data.frame(row.names = F)
impact_under$treatment <- c("Total public expenditure", 
			    "Health public expenditure",
			    "Public expenditure in other sectures")
impact_under$mortality_rate <- c(rep("NeoRt",3),rep("NeoU5Rt",3))
impact_under$gdp_range <- "Under median"

write.csv(impact_under, "bases/impact_under.csv", row.names = F)
