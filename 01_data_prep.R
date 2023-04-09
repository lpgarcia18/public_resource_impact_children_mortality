#########################################################################
#Settings
#########################################################################
options(scipen=999)
set.seed(233)

#########################################################################
#Libraries
#########################################################################
library(readr)
library(tidyverse)
library(reshape2)
library(matrixStats)
library(wbstats)
library(sf)
library(spData)
library(mice)
library(VIM)
#########################################################################
#Data extraction
#########################################################################
# GBD -----------------------------------------------------------------
#Population
population <-
	list.files(path = "bases/IHME/IHME_GBD_2019_POP_2010_2019_0",
		   pattern = "*.CSV", 
		   full.names = T) %>% 
	map_df(~read_csv(., col_types = cols(.default = "c"))) 


#Number of death
number_deaths <- read_csv("bases/IHME/IHME-GBD_2019_DATA-0aa7f1cf-1/IHME-GBD_2019_DATA-0aa7f1cf-1.csv")

#Fertility rate
fertility <- read_csv("bases/IHME/IHME_GBD_2019_FERTILITY_1950_2019_TFR/IHME_GBD_2019_FERTILITY_1950_2019_TFR_Y2020M08D05.CSV")



# WHO DATA ----------------------------------------------------------------
#http://apps.who.int/nha/database/Select/Indicators/en
#Current Health Expenditure (CHE) as % Gross Domestic Product (GDP)
#Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)
#Domestic General Government Health Expenditure (GGHE-D) as % Gross Domestic Product (GDP)
#General Government Expenditure (GGE) as % Gross Domestic Product (GDP)
#Gross Domestic Product (GDP) per Capita in PPP Int$

#Relation
#in million constant (2017) US$
who <- read_csv("bases/WHO/EXP_WHO.csv")

# World Bank Database -----------------------------------------------------
new_cache <- wb_cache()
wbsearch(pattern = "GINI")

#ECONOMIC/INCOME
#GINI index (World Bank estimate)
GINI <- wb_data(indicator = "SI.DST.10TH.10", start_date = 2010, end_date = 2012)
#Poverty gap at $1.90 a day (2011 PPP) (%)
POVERTY_GAP <- wb_data(indicator = "SI.POV.GAPS", start_date = 2010, end_date = 2012)
#Inflation, consumer prices (annual %)
INFLATION <- wb_data(indicator = "FP.CPI.TOTL.ZG", start_date = 2010, end_date = 2012)
#Unemployment, total (% of total labor force) (modeled ILO estimate)
UNEMPLOYMENT <- wb_data(indicator = "SL.UEM.TOTL.ZS", start_date = 2010, end_date = 2012)

#SANITATION
#People using basic sanitation services (% of population)
BASIC_SANITATION <- wb_data(indicator = "SH.STA.BASS.ZS", start_date = 2010, end_date = 2012)
#People using basic drinking water services (% of population)
BASIC_WATER <- wb_data(indicator = "SH.H2O.BASW.ZS", start_date = 2010, end_date = 2012)

#FEMALE EMPOWERMENT
#Unemployment, female (% of female labor force) (modeled ILO estimate)
UNEMPLOYMENT_FEM <- wb_data(indicator = "SL.UEM.TOTL.FE.ZS", start_date = 2010, end_date = 2012)
#School life expectancy, secondary, female (years)
SCHOOL_FEM <- wb_data(indicator = "UIS.SLE.23.F", start_date = 2010, end_date = 2012)
#Proportion of seats held by women in national parliaments (%)
WOMEN_PARLIAMENT <- wb_data(indicator = "SG.GEN.PARL.ZS", start_date = 2010, end_date = 2012)

#SCHOOL
#School life expectancy, secondary, both sexes (years)
SCHOOL_LIFE_EXP <- wb_data(indicator = "UIS.SLE.23", start_date = 2010, end_date = 2012)
#Rate of out-of-school adolescents of lower secondary school age, both sexes (%)
OUT_OF_SCHOOL <- wb_data(indicator = "UIS.ROFST.2", start_date = 2010, end_date = 2012)

#GOVERNANCE
#Control of Corruption: Estimate	
CONTROL_CORRUPTION <- wb_data(indicator = "CC.EST", start_date = 2010, end_date = 2012)
#Government Effectiveness: Estimate	
GOV_EFFECTIVENESS <- wb_data(indicator = "GE.EST", start_date = 2010, end_date = 2012)
#Political Stability and Absence of Violence/Terrorism: Estimate	
POLITICAL_STABILITY <- wb_data(indicator = "PV.EST", start_date = 2010, end_date = 2012)
#Regulatory Quality: Estimate	
REGULATORY_QUALITY <- wb_data(indicator = "RQ.EST", start_date = 2010, end_date = 2012)
#Rule of Law: Estimate	
RULE_OF_LAW <- wb_data(indicator = "RL.EST", start_date = 2010, end_date = 2012)

#NUTRITION
#Prevalence Of Undernourishment (% Of Population)
UNDERNOURISHMENT <- wb_data(indicator = "SN.ITK.DEFC.ZS", start_date = 2010, end_date = 2012)

#HEALTH
#Physicians (per 1,000 people)
DOCTORS <- wb_data(indicator = "SH.MED.PHYS.ZS", start_date = 2010, end_date = 2012)
#Births attended by skilled health staff (% of total)	
DELIVERY_ASSISTANCE <- wb_data(indicator = "SH.STA.BRTC.ZS", start_date = 2010, end_date = 2012)
#Prevalence of HIV, total (% of population ages 15-49)
AIDS_PREVALENCE <- wb_data(indicator = "SH.DYN.AIDS.ZS", start_date = 2010, end_date = 2012)
#Incidence of malaria (per 1,000 population at risk)	
MALARIA_INCIDENCE <- wb_data(indicator = "SH.MLR.INCD.P3", start_date = 2010, end_date = 2012)
#Immunization, DPT (% of children ages 12-23 months)
DPT <- wb_data(indicator = "SH.IMM.IDPT", start_date = 2010, end_date = 2012)
#Hospital beds (per 1,000 people)
HOSPITAL_BEDS <- wb_data(indicator = "SH.MED.BEDS.ZS", start_date = 2010, end_date = 2012)

#ENGERGY
#Access to electricity (% of total population)
ELECTRICITY <- wb_data(indicator = "1.1_ACCESS.ELECTRICITY.TOT", start_date = 2010, end_date = 2012)

#DEMOGRAPHY
#Urban population (% of total)
URBAN_RATE <- wb_data(indicator = "SP.URB.TOTL.IN.ZS", start_date = 2010, end_date = 2012)
#Surface area (sq. km)
SURFACE <- wb_data(indicator = "AG.SRF.TOTL.K2", start_date = 2010, end_date = 2012)


#Geografical centroid
coord_countries <- world
xy <- st_coordinates(st_centroid(coord_countries))
coord_countries <- cbind(coord_countries, xy)
coord_countries <- dplyr::select(coord_countries, name_long,X,Y)
coord_countries$geom <- NULL
names(coord_countries)[1] <- "LOCATION"

#########################################################################
#Variable construction
#########################################################################
#Mortality rates
population <- subset(population, population$year_id %in% c(2010:2019))
population <- subset(population, population$location_id != "533")#Excluding Georigia-US
population <- dplyr::select(population, c('location_name','sex_name','age_group_name', 'year_id', 'val'))
population <- subset(population,population$sex_name == "both")
population$sex_name <- NULL
names(population) <- c('location','age', 'year', 'val')
population_birth <- subset(population,population$age == "Birth")
population_birth$age <- NULL

number_deaths <- dplyr::select(number_deaths, c('location','sex','age', 'year', 'val'))
number_deaths <- subset(number_deaths,number_deaths$sex == "Both")
number_deaths$sex <- NULL
number_deaths_neo <- subset(number_deaths, number_deaths$age %in% c("Early Neonatal","Late Neonatal"))
number_deaths_neo <- number_deaths_neo %>%
	group_by(location, year) %>%
	dplyr::summarize(deaths_neo = sum(val))

number_deaths_under_5 <- subset(number_deaths, number_deaths$age %in% c("Under 5"))
names(number_deaths_under_5) <- c("location", "age", "year", "deaths_u5")
number_deaths <- merge(number_deaths_neo, number_deaths_under_5, by = c("location", "year"), all = T)
number_deaths$neo_u5 <- number_deaths$deaths_u5 - number_deaths$deaths_neo

deaths <- merge(number_deaths, population_birth, by = c('location','year'))
deaths$deaths_u5 <- NULL
deaths$age <- NULL
names(deaths) <- c('LOCATION', 'YEAR', 'DEATHS_NEO', 'DEATHS_NEO_U5', 'BIRTHS')
deaths$BIRTHS <- as.numeric(deaths$BIRTHS)
deaths$RATE_NEO <- deaths$DEATHS_NEO/deaths$BIRTHS * 1000
deaths$RATE_NEO_U5 <- deaths$DEATHS_NEO_U5/deaths$BIRTHS * 1000
deaths$RATE_NEO <- deaths$RATE_NEO 
deaths$RATE_NEO_U5 <- deaths$RATE_NEO_U5 

deaths <- subset(deaths[,-c(3,4,5)], deaths$YEAR %in% c(2018,2019))
deaths <- deaths %>%
	group_by(LOCATION) %>%
	summarize(MEAN_RATE_NEO = mean(RATE_NEO, na.rm = T),
		  MEAN_RATE_NEO_U5 = mean(RATE_NEO_U5, na.rm = T))

#Fertility rate
fertility <- dplyr::select(fertility, c('year_id', 'val', 'location_name'))
fertility$Total <- as.numeric(fertility$val)
fertility$val <- NULL
names(fertility) <- c('YEAR', 'LOCATION', 'FERTILITY_RATE')

fertility <- subset(fertility, fertility$YEAR %in% c(2010:2012))
fertility <- fertility %>%
	group_by(LOCATION) %>%
	summarize(FERTILITY_RATE_LAGGED = mean(FERTILITY_RATE, na.rm = T))
#65 years old + 
population_65 <- subset(population,population$age %in% c("65 to 69", "70+ years", "All Ages"))
population_65 <- subset(population_65, population_65$year %in% c(2010:2012))
population_65$val <- as.numeric(population_65$val)
population_65 <- dcast(population_65, location  ~ age, value.var = "val", fun.aggregate = mean)
population_65$PLUS_65_YEARS_LAGGED <- (population_65$`65 to 69` + population_65$`70+ years`)/population_65$`All Ages`
population_65$`65 to 69` <- NULL
population_65$`70+ years` <- NULL
population_65$`All Ages` <- NULL
names(population_65)[1] <- "LOCATION"

#Merging
base <- merge(deaths, fertility, by = "LOCATION", all.x = T)
base <- merge(base, population_65, by = "LOCATION", all.x = T)


#########################################################################
#Variable construction
#########################################################################
#Adjusting countries names from WB to merge with IHME 
countries_ihme <- data.frame(IHME = unique(number_deaths$location))
countries_wb <- data.frame(WB = unique(base$LOCATION),
			   WB_2 = unique(base$LOCATION))


countries <- merge(countries_ihme, countries_wb, by.x = "IHME", by.y = "WB_2", all = T) 
#The names were the same in boath databases
countries <- NULL

#Adjusting countries names from who to merge with IHME
countries_who <- data.frame(WHO = unique(who$Countries),
			    WHO_2 = unique(who$Countries))
countries <- merge(countries_ihme, countries_who, by.x = "IHME", by.y = "WHO_2", all = T)

who[which(who$Countries == "Samoa"), 1] <- "American Samoa"
who[which(who$Countries == "Bolivia Plurinational States of"), 1] <- "Bolivia (Plurinational State of)"
who[which(who$Countries == "Cabo Verde Republic of"), 1] <- "Cabo Verde"
who[which(who$Countries == "Czech Republic"), 1] <- "Czechia"
who[which(who$Countries == "Republic of Korea"), 1] <- "Democratic People's Republic of Korea"
who[which(who$Countries == "Iran"), 1] <- "Iran (Islamic Republic of)"
who[which(who$Countries == "The Republic of North Macedonia"), 1] <- "North Macedonia"
who[which(who$Countries == "Syria"), 1] <- "Syrian Arab Republic"

#Countries in IHME database but not in who's

# "Bermuda"
# "Greenland"
# "Guam"
# "Lybia"
# "Montenegro"
# "Northern Mariana Islands"
# "Palestine"
# "Puerto Rico"
# "Somalia"
# "Taiwan (Province of China)"
# "Tokelau"
# "United States Virgin Islands"
countries <- NULL


#Adjusting countries names from Countries Coord to merge with IHME
countries_coord <- data.frame(COORD_COUNTRIES = unique(coord_countries$LOCATION),
			      COORD_COUNTRIES_2 = unique(coord_countries$LOCATION))

countries <- merge(countries_ihme, countries_coord, by.x = "IHME", by.y = "COORD_COUNTRIES_2", all = T)

coord_countries[which(coord_countries$LOCATION == "Bolivia"), 1] <- "Bolivia (Plurinational State of)"
coord_countries[which(coord_countries$LOCATION == "Republic of the Congo"), 1] <- "Congo"
coord_countries[which(coord_countries$LOCATION == "Czech Republic"), 1] <- "Czechia"
coord_countries[which(coord_countries$LOCATION == "Republic of Korea"), 1] <- "Democratic People's Republic of Korea"
coord_countries[which(coord_countries$LOCATION == "eSwatini"), 1] <- "Eswatini"
coord_countries[which(coord_countries$LOCATION == "The Gambia"), 1] <- "Gambia"
coord_countries[which(coord_countries$LOCATION == "Iran"), 1] <- "Iran (Islamic Republic of)"
coord_countries[which(coord_countries$LOCATION == "Lao PDR"), 1] <- "Lao People's Democratic Republic"
coord_countries[which(coord_countries$LOCATION == "Macedonia"), 1] <- "North Macedonia"
coord_countries[which(coord_countries$LOCATION == "Moldova"), 1] <- "Republic of Moldova"
coord_countries[which(coord_countries$LOCATION == "Syria"), 1] <- "Syrian Arab Republic"
coord_countries[which(coord_countries$LOCATION == "Taiwan"), 1] <- "Taiwan (Province of China)"
coord_countries[which(coord_countries$LOCATION == "Tanzania"), 1] <- "United Republic of Tanzania"
coord_countries[which(coord_countries$LOCATION == "United States"), 1] <- "United States of America"
coord_countries[which(coord_countries$LOCATION == "Venezuela"), 1] <- "Venezuela (Bolivarian Republic of)"
coord_countries[which(coord_countries$LOCATION == "Vietnam"), 1] <- "Viet Nam"


#Countries in IHME database but not in coord coutries's
# "American Samoa"
# "Andorra"
# "Antigua and Barbuda"
# "Bahrain"
# "Barbados"
# "Bermuda"
# "Cabo Verde"
# "Comoros"
# "Cook Islands"
# "Dominica"
# "Grenada"
# "Guam"
# "Kiribati"
# "Maldives"
# "Malta"
# "Marshall Islands"
# "Mauritius"
# "Micronesia (Federated States of)"
# "Monaco"
# "Nauru"
# "Niue"
# "Northern Mariana Islands"
# "Palau"
# "Saint Kitts and Nevis"
# "Saint Lucia"
# "Saint Vincent and the Grenadine"
# "Samoa"
# "San Marino"
# "Sao Tome and Principe"
# "Seychelles"
# "Tokelau"
# "Tonga"
# "Tuvalu"
# "United States Virgin Islands"
countries <- NULL


#Adjusting the variables from WHO
who[which(who$Indicators == "Current Health Expenditure (CHE) as % Gross Domestic Product (GDP)"), 2] <- "CHE_PERC_GDP"
who[which(who$Indicators == "Out-of-pocket (OOPS) as % of Current Health Expenditure (CHE)"), 2] <- "OOP_PERC_CHE"
who[which(who$Indicators == "Domestic General Government Health Expenditure (GGHE-D) as % Gross Domestic Product (GDP)"), 2] <- "DGGHE_PERC_GDP"
who[which(who$Indicators == "General Government Expenditure (GGE) as % Gross Domestic Product (GDP)"), 2] <- "GGE_PERC_GDP"
who[which(who$Indicators == "Gross Domestic Product (GDP) per Capita in PPP Int$"), 2] <- "GDP"
who <- who[,-3]
who <- melt(who,id.vars = c("Countries","Indicators"))
names(who)[3] <- "YEAR" 
who$value <- as.numeric(gsub("\\,", "", who$value))
who <- dcast(who, Countries + YEAR ~ Indicators, value.var = "value")
who$GDP_PER_CAP <- who$GDP #Adjusting the name of the variable
who$CHE <- who$CHE_PERC_GDP*who$GDP_PER_CAP/100 #Current Health Expenditure
who$OOP_PER_CAP <- who$OOP_PERC_CHE*who$CHE/100 #Out-of-pocket
who$DGGHE <- who$DGGHE_PERC_GDP*who$GDP_PER_CAP/100 #Domestic General Government Health Expenditure
who$GGE <- who$GGE_PERC_GDP*who$GDP_PER_CAP/100 #General Government Expenditure
who <- who %>% dplyr::select("Countries", "YEAR", "GDP_PER_CAP", "OOP_PER_CAP","DGGHE","GGE")

who[is.na(who)] <- 0
who$HEALTH_EXP_PER_CAP <- who$DGGHE 
who$EHE <- NULL
who$DGGHE <- NULL
names(who)[1] <- "LOCATION" 
who <- subset(who, who$YEAR %in% c(2010:2017)) #Public expenditure and GDP


#External factors
GINI <- dplyr::select(GINI, YEAR = date, GINI = SI.DST.10TH.10, LOCATION = country)
POVERTY_GAP <- dplyr::select(POVERTY_GAP, YEAR = date, POVERTY_GAP = SI.POV.GAPS, LOCATION = country)
INFLATION <- dplyr::select(INFLATION, YEAR = date, INFLATION = FP.CPI.TOTL.ZG, LOCATION = country)
UNEMPLOYMENT <- dplyr::select(UNEMPLOYMENT, YEAR = date, UNEMPLOYMENT = SL.UEM.TOTL.ZS, LOCATION = country)
BASIC_SANITATION <- dplyr::select(BASIC_SANITATION, YEAR = date, BASIC_SANITATION = SH.STA.BASS.ZS, LOCATION = country)
BASIC_WATER <- dplyr::select(BASIC_WATER, YEAR = date, BASIC_WATER = SH.H2O.BASW.ZS, LOCATION = country)
UNEMPLOYMENT_FEM <- dplyr::select(UNEMPLOYMENT_FEM, YEAR = date, UNEMPLOYMENT_FEM = SL.UEM.TOTL.FE.ZS, LOCATION = country)
SCHOOL_FEM <- dplyr::select(SCHOOL_FEM, YEAR = date, SCHOOL_FEM = UIS.SLE.23.F, LOCATION = country)
WOMEN_PARLIAMENT <- dplyr::select(WOMEN_PARLIAMENT, YEAR = date, WOMEN_PARLIAMENT = SG.GEN.PARL.ZS, LOCATION = country)
SCHOOL_LIFE_EXP <- dplyr::select(SCHOOL_LIFE_EXP, YEAR = date, SCHOOL_LIFE_EXP = UIS.SLE.23, LOCATION = country)
OUT_OF_SCHOOL <- dplyr::select(OUT_OF_SCHOOL, YEAR = date, OUT_OF_SCHOOL = UIS.ROFST.2, LOCATION = country)
CONTROL_CORRUPTION <- dplyr::select(CONTROL_CORRUPTION, YEAR = date, CONTROL_CORRUPTION  = CC.EST, LOCATION = country)
GOV_EFFECTIVENESS <- dplyr::select(GOV_EFFECTIVENESS, YEAR = date, GOV_EFFECTIVENESS = GE.EST, LOCATION = country)
POLITICAL_STABILITY <- dplyr::select(POLITICAL_STABILITY, YEAR = date, POLITICAL_STABILITY = PV.EST, LOCATION = country)
REGULATORY_QUALITY <- dplyr::select(REGULATORY_QUALITY, YEAR = date, REGULATORY_QUALITY = RQ.EST, LOCATION = country)
RULE_OF_LAW <- dplyr::select(RULE_OF_LAW, YEAR = date, RULE_OF_LAW = RL.EST, LOCATION = country)
UNDERNOURISHMENT <- dplyr::select(UNDERNOURISHMENT, YEAR = date, UNDERNOURISHMENT = SN.ITK.DEFC.ZS, LOCATION = country)
DOCTORS <- dplyr::select(DOCTORS, YEAR = date, DOCTORS = SH.MED.PHYS.ZS, LOCATION = country)
DELIVERY_ASSISTANCE <- dplyr::select(DELIVERY_ASSISTANCE, YEAR = date, DELIVERY_ASSISTANCE = SH.STA.BRTC.ZS, LOCATION = country)
AIDS_PREVALENCE <- dplyr::select(AIDS_PREVALENCE, YEAR = date, AIDS_PREVALENCE = SH.DYN.AIDS.ZS, LOCATION = country)
MALARIA_INCIDENCE <- dplyr::select(MALARIA_INCIDENCE, YEAR = date, MALARIA_INCIDENCE = SH.MLR.INCD.P3, LOCATION = country)
DPT <- dplyr::select(DPT, YEAR = date, DPT = SH.IMM.IDPT, LOCATION = country)
HOSPITAL_BEDS <- dplyr::select(HOSPITAL_BEDS, YEAR = date, HOSPITAL_BEDS = SH.MED.BEDS.ZS, LOCATION = country)
ELECTRICITY <- dplyr::select(ELECTRICITY, YEAR = date, ELECTRICITY = `1.1_ACCESS.ELECTRICITY.TOT`, LOCATION = country)
URBAN_RATE <- dplyr::select(URBAN_RATE, YEAR = date, URBAN_RATE = SP.URB.TOTL.IN.ZS, LOCATION = country)
SURFACE <- dplyr::select(SURFACE, YEAR = date, SURFACE = AG.SRF.TOTL.K2, LOCATION = country)

wb1 <- Reduce(function(x, y) merge(x, y, by = c("LOCATION", "YEAR"),  all=TRUE), 
	      list(GINI, POVERTY_GAP, INFLATION,
	           UNEMPLOYMENT, BASIC_SANITATION, BASIC_WATER,
	           UNEMPLOYMENT_FEM, SCHOOL_FEM, WOMEN_PARLIAMENT,
	           SCHOOL_LIFE_EXP, OUT_OF_SCHOOL, CONTROL_CORRUPTION,
	           GOV_EFFECTIVENESS, POLITICAL_STABILITY, REGULATORY_QUALITY,
	           RULE_OF_LAW, UNDERNOURISHMENT, DOCTORS, DELIVERY_ASSISTANCE,
	           AIDS_PREVALENCE, MALARIA_INCIDENCE, DPT, HOSPITAL_BEDS,
	           ELECTRICITY, URBAN_RATE, SURFACE))

wb1 <- wb1 %>%
	group_by(LOCATION) %>%
	summarize(
		GINI_LAGGED = mean(GINI,na.rm = T), 
		POVERTY_GAP_LAGGED = mean(POVERTY_GAP,na.rm = T), 
		INFLATION_LAGGED = mean(INFLATION,na.rm = T),
		UNEMPLOYMENT_LAGGED = mean(UNEMPLOYMENT,na.rm = T), 
		BASIC_SANITATION_LAGGED = mean(BASIC_SANITATION,na.rm = T), 
		BASIC_WATER_LAGGED = mean(BASIC_WATER,na.rm = T),
		UNEMPLOYMENT_FEM_LAGGED = mean(UNEMPLOYMENT_FEM,na.rm = T), 
		SCHOOL_FEM_LAGGED = mean(SCHOOL_FEM,na.rm = T), 
		WOMEN_PARLIAMENT_LAGGED = mean(WOMEN_PARLIAMENT,na.rm = T),
		SCHOOL_LIFE_EXP_LAGGED = mean(SCHOOL_LIFE_EXP,na.rm = T), 
		OUT_OF_SCHOOL_LAGGED = mean(OUT_OF_SCHOOL,na.rm = T), 
		CONTROL_CORRUPTION_LAGGED = mean(CONTROL_CORRUPTION,na.rm = T),
		GOV_EFFECTIVENESS_LAGGED = mean(GOV_EFFECTIVENESS,na.rm = T), 
		POLITICAL_STABILITY_LAGGED = mean(POLITICAL_STABILITY,na.rm = T), 
		REGULATORY_QUALITY_LAGGED = mean(REGULATORY_QUALITY,na.rm = T),
		RULE_OF_LAW_LAGGED = mean(RULE_OF_LAW,na.rm = T), 
		UNDERNOURISHMENT_LAGGED = mean(UNDERNOURISHMENT,na.rm = T), 
		DOCTORS_LAGGED = mean(DOCTORS,na.rm = T), 
		DELIVERY_ASSISTANCE_LAGGED = mean(DELIVERY_ASSISTANCE,na.rm = T),
		AIDS_PREVALENCE_LAGGED = mean(AIDS_PREVALENCE,na.rm = T), 
		MALARIA_INCIDENCE_LAGGED = mean(MALARIA_INCIDENCE,na.rm = T), 
		DPT_LAGGED = mean(DPT,na.rm = T), 
		HOSPITAL_BEDS_LAGGED = mean(HOSPITAL_BEDS,na.rm = T),
		ELECTRICITY_LAGGED = mean(ELECTRICITY,na.rm = T),
		URBAN_RATE_LAGGED = mean(URBAN_RATE,na.rm = T), 
		SURFACE = mean(SURFACE,na.rm = T)
	)

wb1_loc <- data.frame(WB1_COUNTRIES = unique(wb1$LOCATION),
		      WB1_COUNTRIES_2 = unique(wb1$LOCATION))

countries <- merge(countries_ihme, wb1_loc, by.x = "IHME", by.y = "WB1_COUNTRIES_2", all = T)

wb1[which(wb1$LOCATION == "Bahamas, The"), 1] <- "Bahamas"
wb1[which(wb1$LOCATION == "Bolivia"), 1] <- "Bolivia (Plurinational State of)"
wb1[which(wb1$LOCATION == "Congo, Rep."), 1] <- "Congo"
wb1[which(wb1$LOCATION == "Congo, Dem. Rep."), 1] <- "Democratic Republic of the Congo"
wb1[which(wb1$LOCATION == "Cote d'Ivoire"), 1] <- "Côte d'Ivoire"
wb1[which(wb1$LOCATION == "Czech Republic"), 1] <- "Czechia"
wb1[which(wb1$LOCATION == "Korea, Dem. People’s Rep."), 1] <- "Democratic People's Republic of Korea"
wb1[which(wb1$LOCATION == "Egypt, Arab Rep."), 1] <- "Egypt"
wb1[which(wb1$LOCATION == "Gambia, The"), 1] <- "Gambia"
wb1[which(wb1$LOCATION == "Iran, Islamic Rep."), 1] <- "Iran (Islamic Republic of)"
wb1[which(wb1$LOCATION == "Kyrgyz Republic"), 1] <- "Kyrgyzstan"
wb1[which(wb1$LOCATION == "Lao PDR"), 1] <- "Lao People's Democratic Republic"
wb1[which(wb1$LOCATION == "Micronesia, Fed. Sts."), 1] <- "Micronesia (Federated States of)"
wb1[which(wb1$LOCATION == "Moldova"), 1] <- "Republic of Moldova"
wb1[which(wb1$LOCATION == "St. Kitts and Nevis"), 1] <- "Saint Kitts and Nevis"
wb1[which(wb1$LOCATION == "St. Lucia"), 1] <- "Saint Lucia"
wb1[which(wb1$LOCATION == "St. Vincent and the Grenadines"), 1] <- "Saint Vincent and the Grenadines"
wb1[which(wb1$LOCATION == "Slovak Republic"), 1] <- "Slovakia"
wb1[which(wb1$LOCATION == "Taiwan, China"), 1] <- "Taiwan (Province of China)"
wb1[which(wb1$LOCATION == "Tanzania"), 1] <- "United Republic of Tanzania"
wb1[which(wb1$LOCATION == "United States"), 1] <- "United States of America"
wb1[which(wb1$LOCATION == "Virgin Islands (U.S.)"), 1] <- "United States Virgin Islands"
wb1[which(wb1$LOCATION == "Venezuela, RB"), 1] <- "Venezuela (Bolivarian Republic of)"
wb1[which(wb1$LOCATION == "Vietnam"), 1] <- "Viet Nam"
wb1[which(wb1$LOCATION == "Yemen, Rep."), 1] <- "Yemen"
countries <- NULL


all_pop <- subset(population, population$age == "All Ages")
all_pop <- subset(all_pop, all_pop$year %in% c(2010:2012))
all_pop$val <- as.numeric(all_pop$val)
all_pop <- all_pop %>%
	group_by(location) %>%
	summarise(pop = mean(val, na.rm = T))
names(all_pop)[1] <- "LOCATION"

wb1 <- merge(wb1, all_pop, by = "LOCATION", all.x = T)

wb1$POP_DENS <- wb1$pop/wb1$SURFACE #Populational density
who$PUBLIC_EXP_PER_CAP <- who$GGE 
who <- who %>% dplyr::select(LOCATION, YEAR, GDP_PER_CAP, PUBLIC_EXP_PER_CAP, HEALTH_EXP_PER_CAP, OOP_PER_CAP)
#Excluding countries without information
who <- na.omit(who)
who <- subset(who, who$GDP_PER_CAP != 0)
who <- subset(who, who$PUBLIC_EXP != 0)
who <- subset(who, who$HEALTH_EXP != 0)


#Estimanting lagged 
# Averages
who_lagged1 <- subset(who, who$YEAR %in% c(2013:2017))
who_lagged1 <- who_lagged1 %>%
	group_by(LOCATION) %>%
	summarize(
		PUBLIC_EXP_PER_CAP_LAGGED = mean(PUBLIC_EXP_PER_CAP, na.rm = T),
		HEALTH_EXP_LAGGED = mean(HEALTH_EXP_PER_CAP, na.rm = T))


who_lagged2 <- subset(who, who$YEAR %in% c(2010:2012))
who_lagged2 <- who_lagged2 %>% 
	group_by(LOCATION) %>%
	summarize(
		GDP_PER_CAP_LAGGED = mean(GDP_PER_CAP, na.rm = T),
		OOP_PER_CAP_LAGGED = mean(OOP_PER_CAP, na.rm = T))



who <- merge(who_lagged1, who_lagged2, by = "LOCATION")


#Mergin all the indicators
base <- merge(base, who, by = "LOCATION", all.y = T)
base <- merge(base, wb1, by = "LOCATION", all.x = T)
base <- merge(base, coord_countries, by = "LOCATION", all.x = T)
#Excluding countries without mortality and centroid data
base <- base[which(!is.na(base$MEAN_RATE_NEO)), ]
base <- base[which(!is.na(base$X)), ]
names(base)[which(names(base) == "X")] <- "LONG" 
names(base)[which(names(base) == "Y")] <- "LAT" 
#Excluding coutries without population
base <- subset(base, !is.na(base$pop))

#Calculating expenditure with other sectors than health
base$OTHER_EXP_LAGGED <- base$PUBLIC_EXP_PER_CAP_LAGGED - base$HEALTH_EXP_LAGGED

#Selecting countries with more than 1000000 inhab
base <- subset(base, base$pop > 1000000)

#Adjusting Czechia
czechia <- subset(base, LOCATION == "Czechia")

for (i in 1:ncol(czechia)){
	czechia[1,i] <- ifelse(czechia[1,i] == "NaN",czechia[2,i],czechia[1,i])
}
czechia <- czechia[1,]
base <- subset(base, LOCATION  != "Czechia")
base <- rbind(base, czechia) %>% as.data.frame()


# Description -------------------------------------------------------------
description <- describe(base) %>% as.data.frame()
description$variables <- rownames(description)
writexl::write_xlsx(description, "bases/description.xlsx",col_names = T)

#########################################################################
#Imputation
#########################################################################
#Imputing values
base_under_matrix <- as.matrix(dplyr::select(base, -c(LOCATION))) 
temp_base<- mice::mice(base_under_matrix, m=20, maxit= 30, meth= "cart", diagnostics = T, seed=233)
completed_base<- mice::complete(temp_base)
completed_base<- cbind(dplyr::select(base, c(LOCATION, OTHER_EXP_LAGGED)), completed_base) %>% as.data.frame()
densityplot(temp_base)


#########################################################################
#Variables'transformation
#########################################################################
#Log transformation
completed_base$LOG_GDP_PER_CAP_LAGGED <- log(completed_base$GDP_PER_CAP_LAGGED)
completed_base$LOG_OOP_PER_CAP_LAGGED <- log(completed_base$OOP_PER_CAP_LAGGED)
completed_base$LOG_PUBLIC_EXP_PER_CAP_LAGGED <- log(completed_base$PUBLIC_EXP_PER_CAP_LAGGED)
completed_base$LOG_HEALTH_EXP_LAGGED <- log(completed_base$HEALTH_EXP_LAGGED)
completed_base$LOG_OTHER_EXP_LAGGED <- log(completed_base$OTHER_EXP_LAGGED)

#########################################################################
#Saving database_under
#########################################################################
write.csv(completed_base, "bases/completed_base.csv", row.names = F)



