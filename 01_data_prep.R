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
#Domestic General Government Health Expenditure (GGHE-D) per Capita in PPP Int$
#External Health Expenditure (EXT) per Capita in PPP Int$
#General government expenditure - in current PPP per capita
#Gross Domestic Product - in current PPP per capita
#Out-of-Pocket Expenditure (OOPS) per Capita in PPP Int$
who <- read_csv("bases/WHO/EXP_WHO.csv")


# World Bank Database -----------------------------------------------------
new_cache <- wbcache()
#wbsearch(pattern = "Strength of legal rights index")

#OUTCOME
#Net ODA received (% of GDP)
ODA <- wb(indicator = "DT.ODA.ODAT.GN.ZS", startdate = 2012, enddate = 2019)
#GDP deflator: linked series (base year varies by country)
deflator <- wb(indicator = "NY.GDP.DEFL.ZS.AD", startdate = 2012, enddate = 2019)


#Tax revenue (% of GDP)
TAX <- wb(indicator = "GC.TAX.TOTL.GD.ZS", startdate = 2012, enddate = 2019)

#ECONOMIC/INCOME
#GINI index (World Bank estimate)
GINI <- wb(indicator = "SI.DST.10TH.10", startdate = 2012, enddate = 2014)
#Poverty gap at $1.90 a day (2011 PPP) (%)
POVERTY_GAP <- wb(indicator = "SI.POV.GAPS", startdate = 2012, enddate = 2014)
#Inflation, consumer prices (annual %)
INFLATION <- wb(indicator = "FP.CPI.TOTL.ZG", startdate = 2012, enddate = 2014)
#Unemployment, total (% of total labor force) (modeled ILO estimate)
UNEMPLOYMENT <- wb(indicator = "SL.UEM.TOTL.ZS", startdate = 2012, enddate = 2014)

#SANITATION
#People using basic sanitation services (% of population)
BASIC_SANITATION <- wb(indicator = "SH.STA.BASS.ZS", startdate = 2012, enddate = 2014)
#People using basic drinking water services (% of population)
BASIC_WATER <- wb(indicator = "SH.H2O.BASW.ZS", startdate = 2012, enddate = 2014)

#FEMALE EMPOWERMENT
#Unemployment, female (% of female labor force) (modeled ILO estimate)
UNEMPLOYMENT_FEM <- wb(indicator = "SL.UEM.TOTL.FE.ZS", startdate = 2012, enddate = 2014)
#School life expectancy, secondary, female (years)
SCHOOL_FEM <- wb(indicator = "UIS.SLE.23.F", startdate = 2012, enddate = 2014)
#Proportion of seats held by women in national parliaments (%)
WOMEN_PARLIAMENT <- wb(indicator = "SG.GEN.PARL.ZS", startdate = 2012, enddate = 2014)

#SCHOOL
#School life expectancy, secondary, both sexes (years)
SCHOOL_LIFE_EXP <- wb(indicator = "UIS.SLE.23", startdate = 2012, enddate = 2014)
#Rate of out-of-school adolescents of lower secondary school age, both sexes (%)
OUT_OF_SCHOOL <- wb(indicator = "UIS.ROFST.2", startdate = 2012, enddate = 2014)

#GOVERNANCE
#Control of Corruption: Estimate	
CONTROL_CORRUPTION <- wb(indicator = "CC.EST", startdate = 2012, enddate = 2014)
#Government Effectiveness: Estimate	
GOV_EFFECTIVENESS <- wb(indicator = "GE.EST", startdate = 2012, enddate = 2014)
#Political Stability and Absence of Violence/Terrorism: Estimate	
POLITICAL_STABILITY <- wb(indicator = "PV.EST", startdate = 2012, enddate = 2014)
#Regulatory Quality: Estimate	
REGULATORY_QUALITY <- wb(indicator = "RQ.EST", startdate = 2012, enddate = 2014)
#Rule of Law: Estimate	
RULE_OF_LAW <- wb(indicator = "RL.EST", startdate = 2012, enddate = 2014)

#NUTRITION
#Prevalence Of Undernourishment (% Of Population)
UNDERNOURISHMENT <- wb(indicator = "SN.ITK.DEFC.ZS", startdate = 2012, enddate = 2014)

#HEALTH
#Physicians (per 1,000 people)
DOCTORS <- wb(indicator = "SH.MED.PHYS.ZS", startdate = 2012, enddate = 2014)
#Births attended by skilled health staff (% of total)	
DELIVERY_ASSISTANCE <- wb(indicator = "SH.STA.BRTC.ZS", startdate = 2012, enddate = 2014)
#Prevalence of HIV, total (% of population ages 15-49)
AIDS_PREVALENCE <- wb(indicator = "SH.DYN.AIDS.ZS", startdate = 2012, enddate = 2014)
#Incidence of malaria (per 1,000 population at risk)	
MALARIA_INCIDENCE <- wb(indicator = "SH.MLR.INCD.P3", startdate = 2012, enddate = 2014)
#Immunization, DPT (% of children ages 12-23 months)
DPT <- wb(indicator = "SH.IMM.IDPT", startdate = 2012, enddate = 2014)
#Hospital beds (per 1,000 people)
HOSPITAL_BEDS <- wb(indicator = "SH.MED.BEDS.ZS", startdate = 2012, enddate = 2014)

#ENGERGY
#Access to electricity (% of total population)
ELECTRICITY <- wb(indicator = "1.1_ACCESS.ELECTRICITY.TOT", startdate = 2012, enddate = 2014)

#DEMOGRAPHY
#Urban population (% of total)
URBAN_RATE <- wb(indicator = "SP.URB.TOTL.IN.ZS", startdate = 2012, enddate = 2014)
#Surface area (sq. km)
SURFACE <- wb(indicator = "AG.SRF.TOTL.K2", startdate = 2012, enddate = 2014)


#Economic development data
#https://datahelpdesk.worldbank.org/knowledgebase/articles/378834-how-does-the-world-bank-classify-countries
#http://databank.worldbank.org/data/download/site-content/OGHIST.xls
# 2019
# Low income (L) <= 1,035
# Lower middle income (LM) 1,036 - 4,045
# Upper middle income (UM) 4,046 - 12,535
# High income (H) > 12,535
income_class <- read_csv("bases/WB/income_class_serie.csv")


#########################################################################
#Variable construction
#########################################################################
#Mortality rates
population <- subset(population, population$year_id %in% c(2012:2019))
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

fertility <- subset(fertility, fertility$YEAR %in% c(2012:2019))
fertility <- fertility %>%
   group_by(LOCATION) %>%
   summarize(FERTILITY_RATE_LAGGED = mean(FERTILITY_RATE, na.rm = T))
#65 years old + 
population_65 <- subset(population,population$age %in% c("65 to 69", "70+ years", "All Ages"))
population_65 <- subset(population_65, population_65$year %in% c(2012:2014))
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

#Adjusting countries names to merge with World Bank data
base[which(base$LOCATION == "The Bahamas"), 1] <- "Bahamas, The"
base[which(base$LOCATION == "Cape Verde"), 1] <- "Cabo Verde"
base[which(base$LOCATION == "Congo"), 1] <- "Congo, Rep."
base[which(base$LOCATION == "Egypt"), 1] <- "Egypt, Arab Rep."
base[which(base$LOCATION == "South Korea"), 1] <- "Korea, Rep."
base[which(base$LOCATION == "Kyrgyzstan"), 1] <- "Kyrgyz Republic"
base[which(base$LOCATION == "Federated States of Micronesia"), 1] <- "Micronesia, Fed. Sts."
base[which(base$LOCATION == "Macedonia"), 1] <- "North Macedonia"
base[which(base$LOCATION == "Slovakia"), 1] <- "Slovak Republic"
base[which(base$LOCATION == "Saint Lucia"), 1] <- "St. Lucia"
base[which(base$LOCATION == "Saint Vincent and the Grenadines"), 1] <- "St. Vincent and the Grenadines"


#General government expenditure, Public health expenditure
who[which(who$Indicators == "Domestic General Government Health Expenditure (GGHE-D) per Capita in PPP Int$"), 2] <- "DGGHE_PPP"
who[which(who$Indicators == "External Health Expenditure (EXT) per Capita in PPP Int$"), 2] <- "EXT_PPP"
who[which(who$Indicators == "General government expenditure"), 2] <- "GGE_PPP"
who[which(who$Indicators == "Gross Domestic Product"), 2] <- "GDP_PPP"
who[which(who$Indicators == "Out-of-Pocket Expenditure (OOPS) per Capita in PPP Int$"), 2] <- "OOP_PPP"
who <- who[,-3]
who <- melt(who,id.vars = c("Countries","Indicators"))
names(who)[3] <- "YEAR" 
who$value <- as.numeric(gsub("\\,", "", who$value))
who <- dcast(who, Countries + YEAR ~ Indicators, value.var = "value")
who[is.na(who)] <- 0
who$HEALTH_PPP <- who$EXT_PPP + who$DGGHE_PPP #Domestic and external health expenditure per capita PPP
names(who)[1] <- "LOCATION" 
who<- subset(who, who$YEAR %in% c(2012:2017))
#Adjusting countries names to merge with world bank data
who[which(who$LOCATION == "Bahamas"), 1] <- "Bahamas, The"
who[which(who$LOCATION == "Bolivia Plurinational States of"), 1] <- "Bolivia"
who[which(who$LOCATION == "Cabo Verde Republic of"), 1] <- "Cabo Verde"
who[which(who$LOCATION == "Congo"), 1] <- "Congo, Rep."
who[which(who$LOCATION == "Democratic Republic of the Congo"), 1] <- "Congo, Dem. Rep"
who[which(who$LOCATION == "Egypt"), 1] <- "Egypt, Arab Rep."
who[which(who$LOCATION == "Gambia"), 1] <- "Gambia, The"
who[which(who$LOCATION == "Iran"), 1] <- "Iran, Islamic Rep."
who[which(who$LOCATION == "Kyrgyzstan"), 1] <- "Kyrgyz Republic"
who[which(who$LOCATION == "Lao People's Democratic Republic"), 1] <- "Lao PDR"
who[which(who$LOCATION == "Micronesia (Federated States of)"), 1] <- "Micronesia, Fed. Sts."
who[which(who$LOCATION == "Republic of Korea"), 1] <- "Korea, Rep."
who[which(who$LOCATION == "Republic of Moldova"), 1] <- "Moldova"
who[which(who$LOCATION == "Saint Kitts and Nevis"), 1] <- "St. Kitts and Nevis"
who[which(who$LOCATION == "Saint Lucia"), 1] <- "St. Lucia"
who[which(who$LOCATION == "Saint Vincent and the Grenadines"), 1] <- "St. Vincent and the Grenadines"
who[which(who$LOCATION == "Slovakia"), 1] <- "Slovak Republic"
who[which(who$LOCATION == "Syria"), 1] <- "Syrian Arab Republic"
who[which(who$LOCATION == "The former Yugoslav Republic of Macedonia"), 1] <- "Macedonia, FYR"
who[which(who$LOCATION == "United States of America"), 1] <- "United States"
who[which(who$LOCATION == "United Republic of Tanzania"), 1] <- "Tanzania"
who[which(who$LOCATION == "Venezuela (Bolivarian Republic of)"), 1] <- "Venezuela, RB"
who[which(who$LOCATION == "Viet Nam"), 1] <- "Vietnam"
who[which(who$LOCATION == "Yemen"), 1] <- "Yemen, Rep."
who[which(who$LOCATION == "Congo, Dem. Rep"), 1] <- "Congo, Dem. Rep."
who[which(who$LOCATION == "Côte d'Ivoire"), 1] <- "Cote d'Ivoire"
who[which(who$LOCATION == "Macedonia, FYR"), 1] <- "North Macedonia"


#ODA, Deflactor, Tax revenue and external factors
ODA <- dplyr::select(ODA, YEAR = date, ODA = value, LOCATION = country)
deflator <- dplyr::select(deflator, YEAR = date, deflator = value, LOCATION = country)
TAX <- dplyr::select(TAX, YEAR = date, TAX = value, LOCATION = country)
GINI <- dplyr::select(GINI, YEAR = date, GINI = value, LOCATION = country)
POVERTY_GAP <- dplyr::select(POVERTY_GAP, YEAR = date, POVERTY_GAP = value, LOCATION = country)
INFLATION <- dplyr::select(INFLATION, YEAR = date, INFLATION = value, LOCATION = country)
UNEMPLOYMENT <- dplyr::select(UNEMPLOYMENT, YEAR = date, UNEMPLOYMENT = value, LOCATION = country)
BASIC_SANITATION <- dplyr::select(BASIC_SANITATION, YEAR = date, BASIC_SANITATION = value, LOCATION = country)
BASIC_WATER <- dplyr::select(BASIC_WATER, YEAR = date, BASIC_WATER = value, LOCATION = country)
UNEMPLOYMENT_FEM <- dplyr::select(UNEMPLOYMENT_FEM, YEAR = date, UNEMPLOYMENT_FEM = value, LOCATION = country)
SCHOOL_FEM <- dplyr::select(SCHOOL_FEM, YEAR = date, SCHOOL_FEM = value, LOCATION = country)
WOMEN_PARLIAMENT <- dplyr::select(WOMEN_PARLIAMENT, YEAR = date, WOMEN_PARLIAMENT = value, LOCATION = country)
SCHOOL_LIFE_EXP <- dplyr::select(SCHOOL_LIFE_EXP, YEAR = date, SCHOOL_LIFE_EXP = value, LOCATION = country)
OUT_OF_SCHOOL <- dplyr::select(OUT_OF_SCHOOL, YEAR = date, OUT_OF_SCHOOL = value, LOCATION = country)
CONTROL_CORRUPTION <- dplyr::select(CONTROL_CORRUPTION, YEAR = date, CONTROL_CORRUPTION  = value, LOCATION = country)
GOV_EFFECTIVENESS <- dplyr::select(GOV_EFFECTIVENESS, YEAR = date, GOV_EFFECTIVENESS = value, LOCATION = country)
POLITICAL_STABILITY <- dplyr::select(POLITICAL_STABILITY, YEAR = date, POLITICAL_STABILITY = value, LOCATION = country)
REGULATORY_QUALITY <- dplyr::select(REGULATORY_QUALITY, YEAR = date, REGULATORY_QUALITY = value, LOCATION = country)
RULE_OF_LAW <- dplyr::select(RULE_OF_LAW, YEAR = date, RULE_OF_LAW = value, LOCATION = country)
UNDERNOURISHMENT <- dplyr::select(UNDERNOURISHMENT, YEAR = date, UNDERNOURISHMENT = value, LOCATION = country)
DOCTORS <- dplyr::select(DOCTORS, YEAR = date, DOCTORS = value, LOCATION = country)
DELIVERY_ASSISTANCE <- dplyr::select(DELIVERY_ASSISTANCE, YEAR = date, DELIVERY_ASSISTANCE = value, LOCATION = country)
AIDS_PREVALENCE <- dplyr::select(AIDS_PREVALENCE, YEAR = date, AIDS_PREVALENCE = value, LOCATION = country)
MALARIA_INCIDENCE <- dplyr::select(MALARIA_INCIDENCE, YEAR = date, MALARIA_INCIDENCE = value, LOCATION = country)
DPT <- dplyr::select(DPT, YEAR = date, DPT = value, LOCATION = country)
HOSPITAL_BEDS <- dplyr::select(HOSPITAL_BEDS, YEAR = date, HOSPITAL_BEDS = value, LOCATION = country)
ELECTRICITY <- dplyr::select(ELECTRICITY, YEAR = date, ELECTRICITY = value, LOCATION = country)
URBAN_RATE <- dplyr::select(URBAN_RATE, YEAR = date, URBAN_RATE = value, LOCATION = country)
SURFACE <- dplyr::select(SURFACE, YEAR = date, SURFACE = value, LOCATION = country)

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

all_pop <- subset(population, population$age == "All Ages")
all_pop <- subset(all_pop, all_pop$year %in% c(2012:2014))
all_pop$val <- as.numeric(all_pop$val)
all_pop <- all_pop %>%
   group_by(location) %>%
   summarise(pop = mean(val, na.rm = T))
names(all_pop)[1] <- "LOCATION"
all_pop[which(all_pop$LOCATION == "Republic of the Congo"), 1] <- "Congo, Rep."
all_pop[which(all_pop$LOCATION == "Côte d'Ivoire"), 1] <- "Cote d'Ivoire"
all_pop[which(all_pop$LOCATION == "Egypt"), 1] <- "Egypt, Arab Rep."
all_pop[which(all_pop$LOCATION == "Republic of Korea"), 1] <- "Korea, Rep."
all_pop[which(all_pop$LOCATION == "Macedonia"), 1] <- "North Macedonia"
all_pop[which(all_pop$LOCATION == "Slovakia"), 1] <- "Slovak Republic"
all_pop[which(all_pop$LOCATION == "Democratic Republic of the Congo"), 1] <- "Congo, Dem. Rep."
all_pop[which(all_pop$LOCATION == "The Gambia"), 1] <- "Gambia, The"
all_pop[which(all_pop$LOCATION == "Dominican Republic"), 1] <- "Dominica"
all_pop[which(all_pop$LOCATION == "Iran"), 1] <- "Iran, Islamic Rep."
all_pop[which(all_pop$LOCATION == "Kyrgyzstan"), 1] <- "Kyrgyz Republic"
all_pop[which(all_pop$LOCATION == "Dem. Rep. Korea"), 1] <- "North Korea"
all_pop[which(all_pop$LOCATION == "Bahamas"), 1] <- "Bahamas, The"
all_pop[which(all_pop$LOCATION == "Yemen"), 1] <- "Yemen, Rep."
all_pop[which(all_pop$LOCATION == "The Bahamas"), 1] <- "Bahamas, The"
all_pop[which(all_pop$LOCATION == "North Korea"), 1] <- "Korea, Dem. People’s Rep."
all_pop[which(all_pop$LOCATION == "South Korea"), 1] <- "Korea, Rep."
all_pop[which(all_pop$LOCATION == "Laos"), 1] <- "Lao PDR"
all_pop[which(all_pop$LOCATION == "Macedonia, FYR"), 1] <- "North Macedonia"
all_pop[which(all_pop$LOCATION == "Federated States of Micronesia"), 1] <- "Micronesia, Fed. Sts."
all_pop[which(all_pop$LOCATION == "Venezuela"), 1] <- "Venezuela, RB"
all_pop[which(all_pop$LOCATION == "Congo"), 1] <- "Congo, Rep."
all_pop[which(all_pop$LOCATION == "Brunei"), 1] <- "Brunei Darussalam"

wb1 <- merge(wb1, all_pop, by = "LOCATION", all.x = T)

wb1$POP_DENS <- wb1$pop/wb1$SURFACE #Populational density
wb2 <- Reduce(function(x, y) merge(x, y, by = c("LOCATION", "YEAR"),  all=TRUE), 
      list(ODA, deflator, TAX))


#Merging World Bank database with WHO database
who <- merge(who, wb2, by = c("LOCATION", "YEAR"), all.x = T) #https://www.khanacademy.org/economics-finance-domain/macroeconomics/macro-economic-indicators-and-the-business-cycle/macro-real-vs-nominal-gdp/a/lesson-summary-real-vs-nominal-gdp
#Calculating Public Expenditure and Proportion of Public Health Expenditure
who[which(is.na(who$ODA)), 9] <- 0 #Changing NA for 0 in ODA colum, to sum with Government general expenditure
who$PUBLIC_EXP <- who$GGE_PPP + who$ODA

#Passing monetarie value to constant 2017 PPP int$
who_deflac <- dplyr::select(who, LOCATION, YEAR, deflator, GDP_PPP, TAX, PUBLIC_EXP, HEALTH_PPP, OOP_PPP)
who_deflac[which(is.na(who_deflac$TAX)), 5] <- 0 #Changing NA for 0 in TAX colum, to calculate constant 2017 value
who_deflac$TAX_PPP <- who_deflac$TAX * who_deflac$GDP_PPP/10
who_deflac$TAX <- NULL
who_deflac <- na.omit(who_deflac) #excluding lines whith deflactor == NA
who_deflac$deflator <- who_deflac$deflator/100 #Transformando em centésimo


who_deflac <- who_deflac %>%
   group_by(LOCATION) %>%
   nest()
for(i in seq_along(who_deflac$LOCATION)){
   who_deflac$data[[i]][2] <- who_deflac$data[[i]][which(who_deflac$data[[i]][1] == 2017),2] #Para setar um ano como constante
   who_deflac$data[[i]][3] <- who_deflac$data[[i]][3]/who_deflac$data[[i]][2]
   who_deflac$data[[i]][4] <- who_deflac$data[[i]][4]/who_deflac$data[[i]][2]
   who_deflac$data[[i]][5] <- who_deflac$data[[i]][5]/who_deflac$data[[i]][2]
   who_deflac$data[[i]][6] <- who_deflac$data[[i]][6]/who_deflac$data[[i]][2]
   who_deflac$data[[i]][7] <- who_deflac$data[[i]][7]/who_deflac$data[[i]][2]
}
who <- unnest(who_deflac)

who <- dplyr::select(who, LOCATION, YEAR, GDP_PPP, PUBLIC_EXP, HEALTH_PPP, OOP_PPP, TAX_PPP)
#Excluding countries without information
who <- na.omit(who)
who <- subset(who, who$GDP_PPP != 0)
who <- subset(who, who$PUBLIC_EXP != 0)
who <- subset(who, who$HEALTH_PPP != 0)
who <- subset(who, who$TAX_PPP != 0)

#Estimanting lagged 
who_lagged1 <- subset(who, who$YEAR %in% c(2017))
who_lagged1 <- who_lagged1 %>% 
   group_by(LOCATION) %>%
   summarize(GDP_PPP_LAGGED1 = GDP_PPP)


#Calculating Proportion of Public Health Expenditure
who_lagged2 <- subset(who, who$YEAR %in% c(2015:2016))
who_lagged2$PROP_PUBLIC_HEALTH_EXP <- who_lagged2$HEALTH_PPP/who_lagged2$PUBLIC_EXP
who_lagged2 <- who_lagged2 %>%
   group_by(LOCATION) %>%
   summarize(TAX_PPP_LAGGED2 = mean(TAX_PPP, na.rm = T),
      PUBLIC_EXP_LAGGED2 = mean(PUBLIC_EXP, na.rm = T),
      PROP_PUBLIC_HEALTH_EXP_LAGGED2 = mean(PROP_PUBLIC_HEALTH_EXP, na.rm = T))


who_lagged3 <- subset(who, who$YEAR %in% c(2012:2014))
who_lagged3 <- who_lagged3 %>% 
   group_by(LOCATION) %>%
   summarize(OOP_PPP_LAGGED3 = mean(OOP_PPP, na.rm = T))


who <- merge(who_lagged1, who_lagged2, by = "LOCATION")
who <- merge(who, who_lagged3, by = "LOCATION")


#Income class
income_class <- income_class[,c(1,31)] #LOCATION and 2016
names(income_class)[2] <- "INCOME_CLASS"
income_class[which(income_class$LOCATION == "Côte d'Ivoire"), 1] <- "Cote d'Ivoire"
income_class[which(income_class$LOCATION == "São Tomé and Principe"), 1] <- "Sao Tome and Principe"
income_class[which(income_class$LOCATION == "Korea, Dem. Rep."), 1] <- "Korea, Dem. People’s Rep."
income_class[which(income_class$LOCATION == "Faeroe Islands"), 1] <- "Faroe Islands"
income_class[which(income_class$LOCATION == "Curaçao"), 1] <- "Curacao"


#Geografical centroid
coord_countries <- world
xy <- st_coordinates(st_centroid(coord_countries))
coord_countries <- cbind(coord_countries, xy)
coord_countries <- dplyr::select(coord_countries, name_long,X,Y)
coord_countries$geom <- NULL
names(coord_countries)[1] <- "LOCATION"
coord_countries[which(coord_countries$LOCATION == "Republic of the Congo"), 1] <- "Congo, Rep."
coord_countries[which(coord_countries$LOCATION == "Côte d'Ivoire"), 1] <- "Cote d'Ivoire"
coord_countries[which(coord_countries$LOCATION == "Egypt"), 1] <- "Egypt, Arab Rep."
coord_countries[which(coord_countries$LOCATION == "Republic of Korea"), 1] <- "Korea, Rep."
coord_countries[which(coord_countries$LOCATION == "Macedonia"), 1] <- "North Macedonia"
coord_countries[which(coord_countries$LOCATION == "Slovakia"), 1] <- "Slovak Republic"
coord_countries[which(coord_countries$LOCATION == "Democratic Republic of the Congo"), 1] <- "Congo, Dem. Rep."
coord_countries[which(coord_countries$LOCATION == "The Gambia"), 1] <- "Gambia, The"
coord_countries[which(coord_countries$LOCATION == "Dominican Republic"), 1] <- "Dominica"
coord_countries[which(coord_countries$LOCATION == "Iran"), 1] <- "Iran, Islamic Rep."
coord_countries[which(coord_countries$LOCATION == "Kyrgyzstan"), 1] <- "Kyrgyz Republic"
coord_countries[which(coord_countries$LOCATION == "Dem. Rep. Korea"), 1] <- "North Korea"
coord_countries[which(coord_countries$LOCATION == "Bahamas"), 1] <- "Bahamas, The"
coord_countries[which(coord_countries$LOCATION == "Yemen"), 1] <- "Yemen, Rep."


base[which(base$LOCATION == "Democratic Republic of the Congo"), 1] <- "Congo, Dem. Rep."
base[which(base$LOCATION == "Brunei"), 1] <- "Brunei Darussalam"
base[which(base$LOCATION == "The Gambia"), 1] <- "Gambia, The"
base[which(base$LOCATION == "Iran"), 1] <- "Iran, Islamic Rep."
base[which(base$LOCATION == "Laos"), 1] <- "Lao PDR"
base[which(base$LOCATION == "Yemen"), 1] <- "Yemen, Rep."


#Mergin all the indicators
base <- merge(base, who, by = "LOCATION", all.y = T)
base <- merge(base, wb1, by = "LOCATION", all.x = T)
base <- merge(base, income_class, by = "LOCATION", all.x = T)
base <- merge(base, coord_countries, by = "LOCATION", all.x = T)
#Excludina countries without mortality and centroid data
base <- base[which(!is.na(base$MEAN_RATE_NEO)), ]
base <- base[which(!is.na(base$X)), ]
names(base)[which(names(base) == "X")] <- "LONG" 
names(base)[which(names(base) == "Y")] <- "LAT" 

#########################################################################
#Imputation
#########################################################################
base_matrix <- as.matrix(dplyr::select(base, -LOCATION, -INCOME_CLASS))
missing <- md.pattern(base_matrix)
temp_base <- mice::mice(base_matrix, m=10, maxit=100, meth='cart', seed=233)
#summary(temp_base)
completed_base <- mice::complete(temp_base,1)
completed_base <- cbind(dplyr::select(base, LOCATION, INCOME_CLASS), completed_base) %>% as.data.frame()
densityplot(temp_base)


#########################################################################
#Saving the databases
#########################################################################
write.csv(base, "bases/missing_base.csv", row.names = F)
write.csv(completed_base, "bases/completed_base.csv", row.names = F)

