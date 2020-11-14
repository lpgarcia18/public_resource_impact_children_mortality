# Ambiente ----------------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)


# Pacotes -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(lubridate)
library(MatchIt)
#devtools::install_github('IQSS/Zelig')
library(Zelig)
library(WeightIt)
library(cobalt)
library(SuperLearner)
library(jtools)
library(survey)

# Bases -----------------------------------------------------------------
dose_resp <- read_csv("bases/completed_base.csv")
dose_resp <- subset(dose_resp, dose_resp$LOCATION != "Vanuatu")
dose_resp$LOCATION <- as.factor(dose_resp$LOCATION)

exclude <- c("MEAN_RATE_NEO", "MEAN_RATE_NEO_U5", "PUBLIC_EXP_PER_CAP_LAGGED2", "HEALTH_EXP_LAGGED2", "OTHER_EXP_LAGGED2", "LOCATION")

corr_publ <- cor(x = dose_resp[,!(colnames(dose_resp) %in% exclude)], y = dose_resp$PUBLIC_EXP_PER_CAP_LAGGED2) %>%
	as.data.frame()
corr_publ <- subset(corr_publ, corr_publ$V1 >= 0.2 | corr_publ$V1 <= -0.2)


corr_health <- cor(x = dose_resp[,!(colnames(dose_resp) %in% exclude)], y = dose_resp$HEALTH_EXP_LAGGED2) %>%
	as.data.frame()
corr_health <- subset(corr_health, corr_health$V1 >= 0.2 | corr_health$V1 <= -0.2)


corr_other <- cor(x = dose_resp[,!(colnames(dose_resp) %in% exclude)], y = dose_resp$OTHER_EXP_LAGGED2) %>%
	as.data.frame()
corr_other <- subset(corr_other, corr_other$V1 >= 0.2 | corr_other$V1 <= -0.2)

# Match -------------------------------------------------------------------
#SuperLearner::listWrappers()

#Public Expenditure
set.seed(123)
m.out_public <- weightit(PUBLIC_EXP_PER_CAP_LAGGED2 ~ 
			GDP_PER_CAP_LAGGED1 +
			OOP_PER_CAP_LAGGED3 +
			POLITICAL_STABILITY_LAGGED,
		  		data= dose_resp, method = "super",  				
		 		SL.library = c("SL.nnls"))
summary(m.out_public)

bal.tab(m.out_public, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")


library("survey")
d.w_public <- svydesign(ids = ~1, 
		 weights = m.out_public$weights, data = dose_resp)
fit_public <- svyglm(MEAN_RATE_NEO_U5 ~ PUBLIC_EXP_PER_CAP_LAGGED2  +
	   	          	FERTILITY_RATE_LAGGED +          
				PLUS_65_YEARS_LAGGED +          
				GDP_PER_CAP_LAGGED1 +
				OOP_PER_CAP_LAGGED3 +
				GINI_LAGGED + 
				POVERTY_GAP_LAGGED +            
				INFLATION_LAGGED + 
				UNEMPLOYMENT_LAGGED +
				BASIC_SANITATION_LAGGED +       
				BASIC_WATER_LAGGED +  
				UNEMPLOYMENT_FEM_LAGGED + 
				SCHOOL_FEM_LAGGED +             
				WOMEN_PARLIAMENT_LAGGED +  
				SCHOOL_LIFE_EXP_LAGGED +  
				OUT_OF_SCHOOL_LAGGED +          
				CONTROL_CORRUPTION_LAGGED +  
				GOV_EFFECTIVENESS_LAGGED + 
				POLITICAL_STABILITY_LAGGED +    
				REGULATORY_QUALITY_LAGGED +  
				RULE_OF_LAW_LAGGED +            
				UNDERNOURISHMENT_LAGGED +       
				DOCTORS_LAGGED +          
				DELIVERY_ASSISTANCE_LAGGED +  
				AIDS_PREVALENCE_LAGGED +        
				MALARIA_INCIDENCE_LAGGED +    
				DPT_LAGGED +                   
				HOSPITAL_BEDS_LAGGED +          
				ELECTRICITY_LAGGED +       
				URBAN_RATE_LAGGED +       
				SURFACE +                       
				POP_DENS +               
				LONG +                          
				LAT, design = d.w_public)
coef(fit_public)
summary(fit_public)
summ(fit_public, confint = TRUE, 
model.fit_public = FALSE, model.info = FALSE, digits = 7) 

#Health Expenditure
set.seed(123)
m.out_health <- weightit(HEALTH_EXP_LAGGED2 ~ 
			GDP_PER_CAP_LAGGED1 +        
			OOP_PER_CAP_LAGGED3 +        
			BASIC_SANITATION_LAGGED +    
			SCHOOL_FEM_LAGGED +         
			SCHOOL_LIFE_EXP_LAGGED +     
			CONTROL_CORRUPTION_LAGGED +  
			GOV_EFFECTIVENESS_LAGGED +  
			POLITICAL_STABILITY_LAGGED + 
			REGULATORY_QUALITY_LAGGED + 
			RULE_OF_LAW_LAGGED +        
			URBAN_RATE_LAGGED +         
			LAT,
		  	data= dose_resp, method = "super",  				
		 	SL.library = c("SL.nnls"))
summary(m.out_health)

bal.tab(m.out_health, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")



d.w_health <- svydesign(ids = ~1, 
		 weights = m.out_health$weights, data = dose_resp)
fit_health <- svyglm(MEAN_RATE_NEO_U5 ~ HEALTH_EXP_LAGGED2  +
	   	          	FERTILITY_RATE_LAGGED +          
				PLUS_65_YEARS_LAGGED +          
				GDP_PER_CAP_LAGGED1 +
				OOP_PER_CAP_LAGGED3 +
				GINI_LAGGED + 
				POVERTY_GAP_LAGGED +            
				INFLATION_LAGGED + 
				UNEMPLOYMENT_LAGGED +
				BASIC_SANITATION_LAGGED +       
				BASIC_WATER_LAGGED +  
				UNEMPLOYMENT_FEM_LAGGED + 
				SCHOOL_FEM_LAGGED +             
				WOMEN_PARLIAMENT_LAGGED +  
				SCHOOL_LIFE_EXP_LAGGED +  
				OUT_OF_SCHOOL_LAGGED +          
				CONTROL_CORRUPTION_LAGGED +  
				GOV_EFFECTIVENESS_LAGGED + 
				POLITICAL_STABILITY_LAGGED +    
				REGULATORY_QUALITY_LAGGED +  
				RULE_OF_LAW_LAGGED +            
				UNDERNOURISHMENT_LAGGED +       
				DOCTORS_LAGGED +          
				DELIVERY_ASSISTANCE_LAGGED +  
				AIDS_PREVALENCE_LAGGED +        
				MALARIA_INCIDENCE_LAGGED +    
				DPT_LAGGED +                   
				HOSPITAL_BEDS_LAGGED +          
				ELECTRICITY_LAGGED +       
				URBAN_RATE_LAGGED +       
				SURFACE +                       
				POP_DENS +               
				LONG +                          
				LAT, design = d.w_health)
coef(fit_health)
summary(fit_health)
summ(fit_health, confint = TRUE, 
model.fit_health = FALSE, model.info = FALSE, digits = 7) 


#Health Other
set.seed(123)
m.out_other <- weightit(OTHER_EXP_LAGGED2 ~ 
			GDP_PER_CAP_LAGGED1 +
			OOP_PER_CAP_LAGGED3 +
			POLITICAL_STABILITY_LAGGED,
		  	data= dose_resp, method = "super",  				
		 	SL.library = c("SL.nnls"))
summary(m.out_other)

bal.tab(m.out_other, un = TRUE, disp.v.ratio = TRUE, m.threshold = .05,continuous = "std")



d.w_other <- svydesign(ids = ~1, 
		 weights = m.out_other$weights, data = dose_resp)
fit_other <- svyglm(MEAN_RATE_NEO_U5 ~ OTHER_EXP_LAGGED2  +
	   	          	FERTILITY_RATE_LAGGED +          
				PLUS_65_YEARS_LAGGED +          
				GDP_PER_CAP_LAGGED1 +
				OOP_PER_CAP_LAGGED3 +
				GINI_LAGGED + 
				POVERTY_GAP_LAGGED +            
				INFLATION_LAGGED + 
				UNEMPLOYMENT_LAGGED +
				BASIC_SANITATION_LAGGED +       
				BASIC_WATER_LAGGED +  
				UNEMPLOYMENT_FEM_LAGGED + 
				SCHOOL_FEM_LAGGED +             
				WOMEN_PARLIAMENT_LAGGED +  
				SCHOOL_LIFE_EXP_LAGGED +  
				OUT_OF_SCHOOL_LAGGED +          
				CONTROL_CORRUPTION_LAGGED +  
				GOV_EFFECTIVENESS_LAGGED + 
				POLITICAL_STABILITY_LAGGED +    
				REGULATORY_QUALITY_LAGGED +  
				RULE_OF_LAW_LAGGED +            
				UNDERNOURISHMENT_LAGGED +       
				DOCTORS_LAGGED +          
				DELIVERY_ASSISTANCE_LAGGED +  
				AIDS_PREVALENCE_LAGGED +        
				MALARIA_INCIDENCE_LAGGED +    
				DPT_LAGGED +                   
				HOSPITAL_BEDS_LAGGED +          
				ELECTRICITY_LAGGED +       
				URBAN_RATE_LAGGED +       
				SURFACE +                       
				POP_DENS +               
				LONG +                          
				LAT, design = d.w_other)
coef(fit_other)
summary(fit_other)
summ(fit_other, confint = TRUE, 
model.fit_other = FALSE, model.info = FALSE, digits = 7) 