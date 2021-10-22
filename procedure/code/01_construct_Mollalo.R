#------------------------------------------------------------------------#
#- Author: Sarah Bardin                                                 -#
#-                                                                      -#
#- Overview: Reproduce Mollalo. Create analysis file and perform        -#
#            global regression models                                   -#
#------------------------------------------------------------------------#

#------------------------#
#--- Define Libraries ---#
#------------------------#
library("qpcR")
library("here")
library("tidyverse")
library(tidycensus)
library(readxl)
library(sf)
library(stringr)
library(car)
library(knitr)
library(spdep)
library(rgdal)
library(rgeos)
library(stargazer)
library("spatialreg")

set.seed(122)
#------------------------#
#--- Read in Data ---#
#------------------------#

# County-level boundary data 
boundaries <- get_estimates(geography = "county", 
                            product = "population", 
                            year = 2018,                
                            geometry = TRUE)

boundaries <- boundaries %>% 
  filter(variable == "POP") 

str(boundaries)

# COVID-19 Jan 22 - April 9, 2020 from USAFacts
covid <- read.csv(here("data","raw","public","covid_confirmed_usafacts.csv"))
covid_county <- covid %>% 
                    filter(countyFIPS!=0) %>% ## remove state-level data
                    transmute(FIPS = str_pad(countyFIPS, 5, pad = "0"),
                              covid_cases = select(., X1.22.2020:X4.9.2020) %>% rowSums(na.rm = T))
                             

# Income Inequality (SAIPE/ACS 5-Year Estimates)
cty_healthrankings <- read_excel(here("data","raw","public","2020 County Health Rankings Data - v2.xlsx"), sheet = "Ranked Measure Data", skip = 1)
cty_healthrankings <- cty_healthrankings %>% transmute(FIPS, 
                                                       State, 
                                                       County, 
                                                       inequality = `Income Ratio`)

# Median household income (SAIPE/ACS 5-Year Estimates)
saipe <- read_excel(here("data","raw","public","est18all.xls"), skip = 3)
saipe_restricted <- saipe %>% 
                      filter(`State FIPS Code` != "00") %>%
                      transmute(countyFIPS = paste0(`State FIPS Code`,`County FIPS Code`,""),
                                medIncome = `Median Household Income`)

# Pcnt of nurse practitioner (Pulled from May 2020 BLS)
bls <- read_excel(here("data","raw","public","OES_Report.xlsx"))

# Pcnt black females (2018 Census Pop Estimates)
pop <- read_csv(here("data","raw","public","cc-est2019-alldata.csv"))
pop_restricted <- pop %>% 
                    filter(YEAR == 11 & AGEGRP == 0) %>% ## restrict to all ages in 2018
                    transmute(FIPS = paste0(STATE, COUNTY),
                              pcntblackfem = (BA_FEMALE/TOT_FEMALE)*100,
                              TOT_POP)
str(pop_restricted)
summary(pop_restricted$pcntblackfem)

#------------------------#
#--- Merge Data ---#
#------------------------#

an <- merge(boundaries, cty_healthrankings, by.x = "GEOID", by.y = "FIPS")
an <- merge(an, bls, by = "State")
an <- merge(an, saipe_restricted, by.x = "GEOID", by.y = "countyFIPS")
an <- merge(an, covid_county, by.x = "GEOID", by.y = "FIPS")
an <- merge(an, pop_restricted, by.x = "GEOID", by.y = "FIPS")

rm(bls, cty_healthrankings, boundaries, saipe, saipe_restricted, covid, covid_county, pop, pop_restricted)

an <- an %>% filter(State != "Hawaii" & State != "Alaska")
summary(an)

#------------------------#
#--- Construct Variables ---#
#------------------------#

an_v2 <- an %>% 
          group_by(State) %>%
          mutate(state_pop = sum(TOT_POP)) %>%
          ungroup() %>%
          mutate(pcnt_pop = TOT_POP/state_pop,
                 tot_nurse = round(NursePractitioner*pcnt_pop,0),
                 covid_incidence = (covid_cases/TOT_POP)*100000,
                 medIncome = as.numeric(medIncome))

#Check construction
tot_pcnt_nurse <- an_v2 %>% group_by(State) %>% transmute(NursePractitioner,
                                                       nurses = sum(tot_nurse))
diff <- tot_pcnt_nurse$NursePractitioner - tot_pcnt_nurse$nurses
summary(diff) 

#-Export permenant analysis file-#
st_crs(an_v2)
an_v2 <- st_transform(an_v2, 5070)
st_crs(an_v2)

Mollalo_mgwr<-st_centroid(an_v2)
st_write(Mollalo_mgwr, here("data","derived","public","Mollalo_analysis.csv"), layer_options = "GEOMETRY=AS_XY")

#------------------------#
#--- Perform Analysis---#
#------------------------#
options(scipen=7) 

queen.nb = poly2nb(an_v2) 
queen.listw = nb2listw(queen.nb, zero.policy = TRUE) 
listw1= queen.listw

reg.eq1 <- scale(an_v2$covid_incidence) ~ scale(an_v2$inequality) + scale(an_v2$medIncome) +  scale(an_v2$tot_nurse) + scale(an_v2$pcntblackfem)

# OLS
OLS = lm(reg.eq1, data = an_v2)
summary(OLS)
vif(OLS)

# Spatial Error Model
SReg.SEM = errorsarlm(reg.eq1, data = an_v2, listw1, zero.policy = T)
summary(SReg.SEM)

# Spatially Lag Model
SReg.SLM = lagsarlm(reg.eq1, data = an_v2, listw1, zero.policy = T)
summary(SReg.SLM)


AICc(OLS)
AICc(SReg.SLM)
AICc(SReg.SEM)
