#### LIBRARIES ####

library(tidyverse)
library(purrr)

library(here)
library(rmarkdown)

library(tidycensus)
census_api_key("530ce361defc2c476e5b5d5626d224d8354b9b9a")

library(RODBC)

library(Hmisc) 

library(RColorBrewer) 
library(ggridges) 
library(paletteer) 

library(LaCroixColoR)

library(directlabels)
library(grid)
library(scales)
library(pals) 

library(sf)
library(tigris)

library(foreign)
library(censusapi)
library(ggrepel)


#### LOAD DATA ####

### crosswalks
NOLAcrosswalk <- read.csv('inputs/NOLAcrosswalk.csv')
colnames(NOLAcrosswalk) <- c("tract", "nbhd")
NOLAcrosswalk <- NOLAcrosswalk %>%
  mutate(tract6 = str_pad(tract,6,pad = "0"))
FIPScrosswalk <- read_csv('inputs/fips_state_county_crosswalk.csv') # aggregating LE



### geographies

tracts.la <- sf::st_read(here("inputs/tl_2010_22_tract10/tl_2010_22_tract10.shp"))

parishes_sf <- tigris::counties(state = "22", class = "sf")

Orleans.water_sf <- tigris::area_water("22", "Orleans Parish", class = "sf")
Jefferson.water_sf <- tigris::area_water("22", "Jefferson Parish", class = "sf")
StCharles.water_sf <- tigris::area_water("22", "St. Charles Parish", class = "sf")
# Plaquemines.water_sf <- tigris::area_water("22", "Plaquemines Parish", class = "sf")
# StJames.water_sf <- tigris::area_water("22", "St. James Parish", class = "sf")
# StBernard.water_sf <- tigris::area_water("22", "St. Bernard Parish", class = "sf")
# StTammany.water_sf <- tigris::area_water("22", "St. Tammany Parish", class = "sf")
otherwater.simple_sf <- sf::st_read(here("inputs/water/Otherwater_clipped_SimplifyP.shp"))
wetlands.simple_sf <- sf::st_read(here("inputs/water/Wetlands2_Clip_SimplifyPolyg.shp"))


GNO.parishfips <- c("051", "071", "075", "087", "089", "093", "095", "103")

##For Countries line graph


## Figure 1.3 Life expectancy in selected countries, 1950-2019 (source: World Bank)
LECountry <- read.csv("inputs/LE_country.csv")
## Source: World bank
## https://data.worldbank.org/indicator/SP.DYN.LE00.IN


#### USALEEP data  ####
#### Opportunity Atlas data #### 
#### ACS ####
#### MSAs ####
#### building blocks ####
#### smooshing ####

load("inputs/dataPull_data.Rdata")


alldata <- LEusb %>%
  mutate(FIPS_5 = as.numeric(substr(GEOID,1,5))) %>%
  left_join(.,MSAlist, by = c("FIPS_5")) %>%
  left_join(., OpAtlas.tract_covariates, by=c("GEOID")) %>%
  left_join(., OpAtlas.tract_outcomes_simple, by=c("GEOID")) %>%
  left_join(., acs, by=c("GEOID"))

### Aggregated LE & prob death ###
load("inputs/usLE.Rdata")
load("inputs/stateLE.Rdata")
load("inputs/msaLE.Rdata")
load("inputs/parishLE.Rdata")


load("inputs/msaProbDeath.Rdata")
load("inputs/usProbDeathagg.Rdata")




### Chapter 1 RW
###For Deaths of Despair graphics


#Figure 1.4 Deaths per 100,000 from external causes (Source: CDC Wonder)
US_Liverraw <- read.csv("inputs/Mortality/DD/US_Liver.txt", sep="\t")
US_ODraw <- read.csv("inputs/Mortality/DD/US_OD.txt", sep="\t") 
US_Suicideraw <- read.csv("inputs/Mortality/DD/US_Suicide.txt", sep="\t")
Metro_Liverraw <- read.csv("inputs/Mortality/DD/Metro_Liver.txt", sep="\t")
Metro_ODraw <- read.csv("inputs/Mortality/DD/Metro_OD.txt", sep="\t")
Metro_Suicideraw <- read.csv("inputs/Mortality/DD/Metro_Suicide.txt", sep="\t")

#Figure 1.5: Deaths per 100,000 from chronic diseases (Source: CDC Wonder)
Metro_Cancerraw <- read.csv("inputs/Mortality/DD/Metro_Cancer.txt", sep="\t") 
Metro_Heartraw <- read.csv("inputs/Mortality/DD/Metro_Heart.txt", sep="\t")
Metro_HIVraw <- read.csv("inputs/Mortality/DD/Metro_HIV.txt", sep="\t")
US_Cancerraw <- read.csv("inputs/Mortality/DD/US_Cancer.txt", sep="\t")
US_Heartraw <- read.csv("inputs/Mortality/DD/US_Heart.txt", sep="\t") 
US_HIVraw <- read.csv("inputs/Mortality/DD/US_HIV.txt", sep="\t") 


##Source: CDC Wonder


## Figure1.7:Middleagemortalityratesper100,000(source:CDCWonder)
## Figure 1.8:Mortality trends for youth and elderly per 100,000 (source: CDCWonder)
Metro_Totalraw <- read.csv("inputs/Mortality/Death_Rates/Metro_Total.txt", sep="\t")
US_Totalraw <- read.csv("inputs/Mortality/Death_Rates/US_Total.txt", sep="\t")

## Figure1.10:Blackandwhitemortalityratesper100,000,NewOrleansmetro (source:CDCWonder)
Metro_Blackraw <- read.csv("inputs/Mortality/Death_Rates/Metro_Black.txt", sep="\t")
Metro_Whiteraw <- read.csv("inputs/Mortality/Death_Rates/Metro_White.txt", sep="\t")

## Figure1.11:Blackandwhitemortalityratesper100,000,UnitedStates(source: CDCWonder)
US_Blackraw <- read.csv("inputs/Mortality/Death_Rates/US_Black.txt", sep="\t")
US_Whiteraw <- read.csv("inputs/Mortality/Death_Rates/US_White.txt", sep="\t")


## Figure 1.9 decline in all0cause, age-adjusted mortality rates per 100,000 (Source: CDC Wonder)
OrleansMortraw <- read.csv("inputs/Mortality/MortalityOrleans.txt", sep="\t")
USMortraw <- read.csv("inputs/Mortality/MortalityUS.txt", sep="\t")
MetroMortraw <- read.csv("inputs/Mortality/MortalityMetro.txt", sep="\t")
AllgeoMortraw <- read.csv("inputs/Mortality/MortalityMetrro.txt", sep="\t") #This one does not include Orleans



### Chapter 2 RW

black1970_2010 <- read.dbf("inputs/Sampson/popblack1970_2010.dbf")
NOLAcrosswalk<- read.csv("inputs/Crosswalk2010full.csv") %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  mutate(Neighborhood = as.character(Neighborhood)) 
pov1970_2010 <- read.dbf("inputs/Sampson/poppov1970_2010.dbf")


#Income segregation
incomebuckets1970raw <- read.csv("inputs/Sampson/incomebuckets1970.csv")
incomebuckets1980raw <- read.csv("inputs/Sampson/incomebuckets1980.csv")
incomebuckets1990raw <- read.csv("inputs/Sampson/incomebuckets1990.csv")
incomebuckets2000raw <- read.csv("inputs/Sampson/incomebuckets2000.csv")
incomebuckets2010raw <- read.csv("inputs/Sampson/incomebuckets2010.csv")

