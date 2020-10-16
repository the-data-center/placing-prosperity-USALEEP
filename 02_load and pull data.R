
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



### Aggregated LE & prob death ###
load("inputs/usLE.Rdata")
load("inputs/stateLE.Rdata")
load("inputs/msaLE.Rdata")
load("inputs/parishLE.Rdata")


load("inputs/msaProbDeath.Rdata")
load("inputs/usProbDeathagg.Rdata")




### Chapter 1 mortality
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



### Chapter 2 historical census

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

#### PULL DATA ####
#### USALEEP data ####
## download tusing this link and store locally: https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/US_B.CSV
#LEusbRaw <- ## your local USALEEP file ##
LEusb <- LEusbRaw %>%
  dplyr::select(-ValidFrom, -ValidTo) %>%
  mutate(
    ageGrp = AgeGroup,
    probDeath = as.numeric(ProbDyingInRange),
    numSurv = as.numeric(SurvivingToGroup),
    numDying = as.numeric(DyingInRange),
    personYrs = as.numeric(PersonYearsLivedInRange),
    personYrsAbove = as.numeric(TotalYearsLivedAboveGroup),
    LE_x = as.numeric(LifeExpectancyAtRange),
    LE_SE_probDeath = as.numeric(ProbDyingInRangeSE),
    LE_SE_x = as.numeric(LifeExpectancyAtRangeSE),
    GEOID = str_pad(TractID, 11, pad="0")) %>%
  dplyr::select(-AgeGroup,-ProbDyingInRange,-SurvivingToGroup,-DyingInRange,-PersonYearsLivedInRange,-TotalYearsLivedAboveGroup,-LifeExpectancyAtRange,-ProbDyingInRangeSE,-LifeExpectancyAtRangeSE)

LEusbLA <- LEusb %>%
  filter(str_sub(GEOID,1,2)=="22")

#### Opportunity Atlas data ####

OpAtlas.tract_outcomes_simple <- ## your local file ##
  mutate(state = str_pad(state,2, pad="0"),
         county = str_pad(county,3, pad= "0"),
         tract = str_pad(tract,6, pad="0"),
         GEOID = paste0(state, county, tract))

OpAtlas.tract_covariates <- ## your local file ##
  mutate(state = str_pad(state,2, pad="0"),
         county = str_pad(county,3, pad= "0"),
         tract = str_pad(tract,6, pad="0"),
         GEOID = paste0(state, county, tract),
         FIPS_5 = as.numeric(paste0(state, county)))

#### acs ####

# us <- unique(fips_codes$state)[1:51]
# acs15 <- map_df(us, function(x) {
#   get_acs(geography = "tract",
#           year = 2015,
#           variables = c("B03002_001", "B03002_004"), # tot pop and Black non-Hispanic pop
#           state = x,
#           survey = "acs5")
# })
#
# acs <- acs15 %>%
#   dplyr::select(-NAME, -moe) %>%
#   spread(key = variable, value = estimate) %>%
#   transmute(GEOID=GEOID, ACS.pctBlack = B03002_004/B03002_001)  %>%
#   mutate(ACS.majBlack = ifelse(ACS.pctBlack >= .5,TRUE, FALSE))  # is the tract 50%+ Black?

# save(acs,file = "inputs/acs.Rdata")
load("inputs/acs.Rdata")

# acs15.NOmsa <- get_acs(geography = "tract",
#                                  year = 2015,
#                                  variables = c("B03002_001", "B03002_004", "B19013_001E"), # tot pop, Black non-Hispanic pop
#                                  state = "LA",
#                                  survey = "acs5")
#
# acs15.NOmsa.dp <- get_acs(geography = "tract",
#                                  year = 2015,
#                                  variables = c(  "DP02_0058E", bach = "DP02_0064E", gradProf = "DP02_0065E"), # ed data
#                                  state = "LA",
#                                  survey = "acs5")
#
# acs.NOmsa <- acs15.NOmsa %>%
#   bind_rows(acs15.NOmsa.dp) %>%
#     dplyr::select(-NAME, -moe) %>%
#   filter(str_sub(GEOID,3,5) %in% GNO.parishfips) %>%
#   spread(key = variable, value = estimate) %>%
#   transmute(GEOID=GEOID, ACS.pctBlack = B03002_004/B03002_001, ACS.medInc = B19013_001, ACS.pctBachPlus = (DP02_0064 + DP02_0065)/DP02_0058)
#
#  save(acs.NOmsa,file = "inputs/acsNOmsa.Rdata")
load("inputs/acsNOmsa.Rdata")

#### MSAs ####
## this table comes originally from the Census at this site: https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
## Sept 2018 was the most recent available
## due to formatting, the file has to be downloaded and cleaned up - that's why it's stored in inputs
MSAlist <- read_csv("inputs/MSA list.csv")
MSAlist$FIPS_5 <- as.numeric(MSAlist$FIPS_5)
names(MSAlist) <- c("city", "metroORmicro", "county", "stateName", "stateCode", "countyCode", "centralORoutlying", "FIPS_5")


#### building blocks ####

## tract & LE
geoLE <- LEusb %>%
  filter(ageGrp == "Under 1") %>%
  dplyr::select(GEOID, LE_x, LE_SE_x) %>%
  rename(LE = LE_x, LE_se = LE_SE_x)

# tract & LE & MSA
geoLEMSA <- geoLE %>%
  mutate(FIPS_5 = as.numeric(substr(GEOID,1,5))) %>%
  left_join(.,MSAlist, by = c("FIPS_5")) %>%
  dplyr::select(GEOID, FIPS_5, LE, LE_se, city, metroORmicro)


#### smooshing ####
alldata <- LEusb %>%
  mutate(FIPS_5 = as.numeric(substr(GEOID,1,5))) %>%
  left_join(.,MSAlist, by = c("FIPS_5")) %>%
  left_join(., OpAtlas.tract_covariates, by=c("GEOID")) %>%
  left_join(., OpAtlas.tract_outcomes_simple, by=c("GEOID")) %>%
  left_join(., acs, by=c("GEOID"))



### Pulling 2017 black population for Chapter 2

np.pull <- function(variables, names = variables, year=2017, survey = "acs/acs5"){
  censuskey="530ce361defc2c476e5b5d5626d224d8354b9b9a"
  tract <- getCensus(name = survey,
                     vintage = year,
                     key = censuskey,
                     vars = variables,
                     region = "tract:*",
                     regionin = "state:22+county:051,071,075,087,089,093,095,103")
  #select(-state, -county)
  colnames(tract) <- c("state", "county", "tract", names)
  return(tract)
}

##pulling black 2017

black2017.vars <- c("B03002_001E", "B03002_004E")
black2017.names <- c("pop2017", "black2017")
black2017 <- np.pull(variables = black2017.vars, names = black2017.names) %>%
  mutate(GEOID = paste(state, county, tract, sep= "")) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  select(GEOID, pop2017, black2017)

pov.vars <- c("B17001_001E","B17001_002E")
pov.names <- c("pop2017","pov2017")
pov2017 <- np.pull(variables = pov.vars, names = pov.names)%>%
  mutate(GEOID = paste(state, county, tract, sep= "")) %>%
  mutate(GEOID = as.character(GEOID)) %>%
  select(GEOID, pop2017, pov2017)

incDist.vars <- c("B19001_001E","B19001_002E","B19001_003E","B19001_004E","B19001_005E","B19001_006E","B19001_007E","B19001_008E","B19001_009E","B19001_010E","B19001_011E","B19001_012E","B19001_013E","B19001_014E","B19001_015E",
                  "B19001_016E","B19001_017E")
incDist.names <- c("totfams","inc_less10","inc10_15","inc15_20","inc20_25","inc25_30","inc30_35","inc35_40","inc40_45","inc45_50","inc50_60","inc60_75","inc75_100","inc100_125","inc125_150","inc150_200","inc_200PLUS")
incomebuckets2017raw <- np.pull(variables = incDist.vars, names = incDist.names)

### Save to Rdata
# 
# save(list = c('LEusbRaw',
#               'LEusb',
#               'LEusbLA',
#               'OpAtlas.tract_outcomes_simple',
#               'OpAtlas.tract_covariates',
#               'MSAlist',
#               'geoLE',
#               'geoLEMSA',
#               'alldata',
#               'black2017',
#               'pov2017',
#               'incomebuckets2017raw'), file ="dataPull_data.Rdata")

#### Ch 3 additional Opportunity Atlas data

# Full tract outcomes for GNO MSA
temp <- tempfile()
download.file("https://opportunityinsights.org/wp-content/uploads/2018/10/tract_outcomes.zip",temp)
unzip(temp, exdir = paste0(getwd(),"/inputs/opinsights"))
unlink(temp)
OpAtlas.tract_outcomes_full <- read_csv("inputs/opinsights/tract_outcomes_early.csv",
                                        col_types = cols(county = col_character(), 
                                                         cz = col_character(), state = col_character(), 
                                                         tract = col_character())) %>% 
  mutate(state = str_pad(state,2, pad="0"),
         county = str_pad(county,3, pad= "0"),
         tract = str_pad(tract,6, pad="0"),
         GEOID = paste0(state, county, tract))

OpAtlas.tract_outcomes_full.GNO <- OpAtlas.tract_outcomes_full %>% 
  filter(state == "22") %>% 
  filter(county %in% c("051", "071", "075", "087", "089", "093", "095", "103"))

rm(OpAtlas.tract_outcomes_full)

# Income crosswalk
Op.atlas.pctinccrosswalk <- read_csv("https://opportunityinsights.org/wp-content/uploads/2018/10/pctile_to_dollar_cw.csv")

# Communiting zone outcomes
OpAtlas.CZ_outcomes_simple <- read_csv("https://opportunityinsights.org/wp-content/uploads/2018/10/cz_outcomes_simple.csv")
OpAtlas.CZ_outcomes_full <- read_csv("https://opportunityinsights.org/wp-content/uploads/2018/10/cz_outcomes.csv")


# Combine tract OpAtlas with life expectancy. 
OpAtlas.tract_outcomes_simple_LE.GNO <- OpAtlas.tract_outcomes_simple %>% 
  filter(state == "22") %>% 
  filter(county %in% c("051", "071", "075", "087", "089", "093", "095", "103")) %>% 
  #full_join(LE.GNO, by=c("GEOID"="GEOID"))
  full_join(LE.GNO, by=c("GEOID"="GEOID"))



## Save to Rdata
# 
# save(list = c('OpAtlas.tract_outcomes_simple_LE.GNO',
#               'Op.atlas.pctinccrosswalk',
#               'OpAtlas.tract_outcomes_full.GNO',
#               'OpAtlas.CZ_outcomes_simple',
#               'OpAtlas.CZ_outcomes_full'), file = "Ch3OpAtlas_data.Rdata")


