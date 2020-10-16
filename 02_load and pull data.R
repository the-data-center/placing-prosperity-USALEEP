
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

###
#### PULL DATA ####
###

#### USALEEP data ####
## download using this link and store locally: https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/CSV/US_B.CSV

#LEusbRaw <- ## your local USALEEP file ##
LEusb <- LEusbRaw %>%
  rename(ageGrp = Age.Group,
         probDeath = nq.x.,
         numSurv = l.x.,
         numDying = nd.x.,
         personYrs = nL.x.,
         personYrsAbove = T.x.,
         LE_x = e.x.,
         LE_SE_probDeath = se.nq.x..,
         LE_SE_x = se.e.x..,
         GEOID = Tract.ID) %>%
  dplyr::select(-contains('2KX')) 

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

#### AGGREGATING LE ####

### LA

popLA10 <- get_decennial(geography = "tract", 
                         variables = c("P001001"),
                         state = c("LA"))

popLA15 <- get_acs(geography = "tract",
                   year = 2015,
                   variables = c("B01003_001"), 
                   state = c("LA"),
                   survey = "acs5")
popLA <- left_join(popLA10, popLA15, by = "GEOID") %>%
  rename(pop10 = value, pop15 = estimate) %>%
  dplyr::select(-NAME.x, -NAME.y, -variable.x, -variable.y) %>%
  mutate(popPooled = pop10 + pop15)

### US

us <- unique(fips_codes$state)[1:51]

pop10 <- map_df(us, function(x) {
  get_decennial(geography = "tract", variables = "P001001", 
                state = x)
})

pop15 <- map_df(us, function(x) {
  get_acs(geography = "tract",
          year = 2015,
          variables = "B01003_001", 
          state = x,
          survey = "acs5")
})


metropop15 <-   get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                        year = 2015,
                        variables = "B01003_001",
                        survey = "acs5") %>%
  filter(grepl("Metro Area", NAME)) %>%
  filter(estimate > 1000000) %>%
  mutate(NAME = str_sub(NAME, 1,-12))

#### PROBABLILITY OF DEATH ####

### Population data
age2000.vars <-c("P001001","P012003","P012004","P012005","P012006","P012007","P012008","P012009","P012010","P012011","P012012","P012013","P012014","P012015","P012016","P012017","P012018","P012019","P012020","P012021","P012022","P012023","P012024","P012025","P012027","P012028","P012029","P012030","P012031","P012032","P012033","P012034","P012035","P012036","P012037","P012038","P012039","P012040","P012041","P012042","P012043","P012044","P012045","P012046","P012047","P012048","P012049")
age2000.names <-c("pop","m_under5","m5_9","m10_14","m15_17","m18_19","m20","m21","m22_24","m25_29","m30_34","m35_39","m40_44","m45_49","m50_54","m55_59","m60_61","m62_64","m65_66","m67_69","m70_74","m75_79","m80_84","m85over","f_under5","f5_9","f10_14","f15_17","f18_19","f20","f21","f22_24","f25_29","f30_34","f35_39","f40_44","f45_49","f50_54","f55_59","f60_61","f62_64","f65_66","f67_69","f70_74","f75_79","f80_84","f85over")

popLA10Raw <- get_decennial(geography = "tract", 
                            variables = age2000.vars,
                            state = c("LA"))


age.vars <-c("B01003_001E", "B01003_001M","B01001_003E","B01001_004E","B01001_005E","B01001_006E","B01001_007E","B01001_008E","B01001_009E","B01001_010E","B01001_011E","B01001_012E","B01001_013E","B01001_014E","B01001_015E","B01001_016E","B01001_017E","B01001_018E","B01001_019E","B01001_020E","B01001_021E","B01001_022E","B01001_023E","B01001_024E","B01001_025E","B01001_027E","B01001_028E","B01001_029E","B01001_030E","B01001_031E","B01001_032E","B01001_033E","B01001_034E","B01001_035E","B01001_036E","B01001_037E","B01001_038E","B01001_039E","B01001_040E","B01001_041E","B01001_042E","B01001_043E","B01001_044E","B01001_045E","B01001_046E","B01001_047E","B01001_048E","B01001_049E","B01001_003M","B01001_004M","B01001_005M","B01001_006M","B01001_007M","B01001_008M","B01001_009M","B01001_010M","B01001_011M","B01001_012M","B01001_013M","B01001_014M","B01001_015M","B01001_016M","B01001_017M","B01001_018M","B01001_019M","B01001_020M","B01001_021M","B01001_022M","B01001_023M","B01001_024M","B01001_025M","B01001_027M","B01001_028M","B01001_029M","B01001_030M","B01001_031M","B01001_032M","B01001_033M","B01001_034M","B01001_035M","B01001_036M","B01001_037M","B01001_038M","B01001_039M","B01001_040M","B01001_041M","B01001_042M","B01001_043M","B01001_044M","B01001_045M","B01001_046M","B01001_047M","B01001_048M","B01001_049M")
age.names <-c("pop", "popMOE", "m_under5","m5_9","m10_14","m15_17","m18_19","m20","m21","m22_24","m25_29","m30_34","m35_39","m40_44","m45_49","m50_54","m55_59","m60_61","m62_64","m65_66","m67_69","m70_74","m75_79","m80_84","m85over","f_under5","f5_9","f10_14","f15_17","f18_19","f20","f21","f22_24","f25_29","f30_34","f35_39","f40_44","f45_49","f50_54","f55_59","f60_61","f62_64","f65_66","f67_69","f70_74","f75_79","f80_84","f85over","m_under5MOE","m5_9MOE","m10_14MOE","m15_17MOE","m18_19MOE","m20MOE","m21MOE","m22_24MOE","m25_29MOE","m30_34MOE","m35_39MOE","m40_44MOE","m45_49MOE","m50_54MOE","m55_59MOE","m60_61MOE","m62_64MOE","m65_66MOE","m67_69MOE","m70_74MOE","m75_79MOE","m80_84MOE","m85overMOE","f_under5MOE","f5_9MOE","f10_14MOE","f15_17MOE","f18_19MOE","f20MOE","f21MOE","f22_24MOE","f25_29MOE","f30_34MOE","f35_39MOE","f40_44MOE","f45_49MOE","f50_54MOE","f55_59MOE","f60_61MOE","f62_64MOE","f65_66MOE","f67_69MOE","f70_74MOE","f75_79MOE","f80_84MOE","f85overMOE")
age.names.short <-c("m_under5","m5_9","m10_14","m15_17","m18_19","m20","m21","m22_24","m25_29","m30_34","m35_39","m40_44","m45_49","m50_54","m55_59","m60_61","m62_64","m65_66","m67_69","m70_74","m75_79","m80_84","m85over","f_under5","f5_9","f10_14","f15_17","f18_19","f20","f21","f22_24","f25_29","f30_34","f35_39","f40_44","f45_49","f50_54","f55_59","f60_61","f62_64","f65_66","f67_69","f70_74","f75_79","f80_84","f85over", "pop")
popLA15Raw <- get_acs(geography = "tract",
                      year = 2015,
                      variables = c(age.vars),
                      state = c("LA"),
                      survey = "acs5")






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

