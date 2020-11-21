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
#dose_resp <- subset(dose_resp, dose_resp$LOCATION != "Vanuatu")
dose_resp$LOCATION <- as.factor(dose_resp$LOCATION)

exclude <- c("MEAN_RATE_NEO", "MEAN_RATE_NEO_U5", "LOG_PUBLIC_EXP_PER_CAP_LAGGED", "LOG_HEALTH_EXP_LAGGED", "LOG_OTHER_EXP_LAGGED", "LOCATION")



# Transformation of monetary variables ------------------------------------
dose_resp$LOG_PUBLIC_EXP_PER_CAP_LAGGED <- log(dose_resp$PUBLIC_EXP_PER_CAP_LAGGED)
dose_resp$LOG_HEALTH_EXP_LAGGED <- log(dose_resp$HEALTH_EXP_LAGGED)
dose_resp$LOG_OTHER_EXP_LAGGED <- log(dose_resp$OTHER_EXP_LAGGED)
dose_resp$LOG_GDP_PER_CAP_LAGGED <- log(dose_resp$GDP_PER_CAP_LAGGED)
dose_resp$LOG_OOP_PER_CAP_LAGGED <- log(dose_resp$OOP_PER_CAP_LAGGED)
dose_resp$LOG_MEAN_RATE_NEO <- log(dose_resp$MEAN_RATE_NEO)
dose_resp$LOG_MEAN_RATE_NEO_U5 <- log(dose_resp$MEAN_RATE_NEO_U5)


dose_resp <- subset(dose_resp, dose_resp$LOG_GDP_PER_CAP_LAGGED >= quantile(dose_resp$LOG_GDP_PER_CAP_LAGGED,0.5))

# Bivariate analysis for match -----------------------
external_factors <- c("FERTILITY_RATE_LAGGED", "PLUS_65_YEARS_LAGGED", "LOG_GDP_PER_CAP_LAGGED", "LOG_OOP_PER_CAP_LAGGED", "GINI_LAGGED", 
			"POVERTY_GAP_LAGGED", "INFLATION_LAGGED", "UNEMPLOYMENT_LAGGED", "BASIC_SANITATION_LAGGED", "BASIC_WATER_LAGGED", 
			"UNEMPLOYMENT_FEM_LAGGED", "SCHOOL_FEM_LAGGED", "WOMEN_PARLIAMENT_LAGGED", "SCHOOL_LIFE_EXP_LAGGED", "OUT_OF_SCHOOL_LAGGED", 
			"CONTROL_CORRUPTION_LAGGED", "GOV_EFFECTIVENESS_LAGGED", "POLITICAL_STABILITY_LAGGED", "REGULATORY_QUALITY_LAGGED", "RULE_OF_LAW_LAGGED", 
			"UNDERNOURISHMENT_LAGGED", "DOCTORS_LAGGED", "DELIVERY_ASSISTANCE_LAGGED", "AIDS_PREVALENCE_LAGGED", "MALARIA_INCIDENCE_LAGGED", 
			"DPT_LAGGED", "HOSPITAL_BEDS_LAGGED", "ELECTRICITY_LAGGED", "URBAN_RATE_LAGGED", "pop", "POP_DENS", "LONG", "LAT", "UNDER_50_GDP")

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
					       "SL.gbm",
					       "SL.glm",
					       "SL.nnet",
					       "SL.nnls",
					       "SL.ranger",
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
					       "SL.gbm",
					       "SL.glm",
					       "SL.nnet",
					       "SL.nnls",
					       "SL.ranger",
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
					       "SL.gbm",
					       "SL.glm",
					       "SL.nnet",
					       "SL.nnls",
					       "SL.ranger",
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


p_cor_public_neo <- p_cor_ext_reg("LOG_MEAN_RATE_NEO", "LOG_PUBLIC_EXP_PER_CAP_LAGGED")
p_cor_health_neo <- p_cor_ext_reg("LOG_MEAN_RATE_NEO", "LOG_HEALTH_EXP_LAGGED")
p_cor_other_neo <- p_cor_ext_reg("LOG_MEAN_RATE_NEO", "LOG_OTHER_EXP_LAGGED")

p_cor_public_neo_u5 <- p_cor_ext_reg("LOG_MEAN_RATE_NEO_U5", "LOG_PUBLIC_EXP_PER_CAP_LAGGED")
p_cor_health_neo_u5 <- p_cor_ext_reg("LOG_MEAN_RATE_NEO_U5", "LOG_HEALTH_EXP_LAGGED")
p_cor_other_neo_u5 <- p_cor_ext_reg("LOG_MEAN_RATE_NEO_U5", "LOG_OTHER_EXP_LAGGED")



# Regression --------------------------------------------------------------
#Neo - Public
fit_public_neo <- svyglm(as.formula(p_cor_public_neo), design = d.w_public)
coef(fit_public_neo)
summary(fit_public_neo)
summ(fit_public_neo, confint = TRUE,
model.fit_public_neo = FALSE, model.info = FALSE, digits = 7) 

#Neo - Health
fit_health_neo <- svyglm(as.formula(p_cor_health_neo), design = d.w_health)
coef(fit_health_neo)
summary(fit_health_neo)
summ(fit_health_neo, confint = TRUE,
model.fit_health_neo = FALSE, model.info = FALSE, digits = 7) 

#Neo - Other
fit_other_neo <- svyglm(as.formula(p_cor_other_neo), design = d.w_other)
coef(fit_other_neo)
summary(fit_other_neo)
summ(fit_other_neo, confint = TRUE,
model.fit_other_neo = FALSE, model.info = FALSE, digits = 7) 


#Neo_u5 - Public
fit_public_neo_u5 <- svyglm(as.formula(p_cor_public_neo_u5), design = d.w_public)
coef(fit_public_neo_u5)
summary(fit_public_neo_u5)
summ(fit_public_neo_u5, confint = TRUE,
model.fit_public_neo_u5 = FALSE, model.info = FALSE, digits = 7) 

#Neo_u5 - Health
fit_health_neo_u5 <- svyglm(as.formula(p_cor_health_neo_u5), design = d.w_health)
coef(fit_health_neo_u5)
summary(fit_health_neo_u5)
summ(fit_health_neo_u5, confint = TRUE,
model.fit_health_neo_u5 = FALSE, model.info = FALSE, digits = 7) 

#Neo_u5 - Other
fit_other_neo_u5 <- svyglm(as.formula(p_cor_other_neo_u5), design = d.w_other)
coef(fit_other_neo_u5)
summary(fit_other_neo_u5)
summ(fit_other_neo_u5, confint = TRUE,
model.fit_other_neo_u5 = FALSE, model.info = FALSE, digits = 7) 


# Prediction --------------------------------------------------------------
dose_resp_contrafact <- dose_resp
dose_resp_contrafact$LOG_HEALTH_EXP_LAGGED <- dose_resp$LOG_HEALTH_EXP_LAGGED * 1.5 

neo_predict <- predict(fit_health_neo, newdata= dose_resp_contrafact, total=NULL,
                         type=c("response")) %>% as.data.frame()
dose_resp_contrafact$LOG_MEAN_RATE_NEO_PREDICTED <- neo_predict$response
ggplot(dose_resp_contrafact, aes(exp(LOG_MEAN_RATE_NEO), exp(LOG_MEAN_RATE_NEO_PREDICTED)))+
	geom_jitter()+
	geom_abline(intercept = 0, slope = 1)+
	geom_text(label = dose_resp_contrafact$LOCATION,check_overlap = F, size = 3)
	
neo_predict_u5 <- predict(fit_health_neo_u5, newdata= dose_resp_contrafact, total=NULL,
                         type=c("response")) %>% as.data.frame()
dose_resp_contrafact$LOG_MEAN_RATE_NEO_PREDICTED_U5 <- neo_predict_u5$response
ggplot(dose_resp_contrafact, aes(exp(LOG_MEAN_RATE_NEO_U5), exp(LOG_MEAN_RATE_NEO_PREDICTED_U5)))+
	geom_jitter()+
	geom_abline(intercept = 0, slope = 1)+
	geom_text(label = dose_resp_contrafact$LOCATION,check_overlap = F, size = 3)
