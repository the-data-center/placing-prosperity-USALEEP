## This code creates "dataPull_data.Rdata," which is loaded in the 01_libraries and load data file. ##

# #### USALEEP data #### 
# 
# #LEusbRaw <- ## your local USALEEP file ##
# LEusb <- LEusbRaw %>%
#   dplyr::select(-ValidFrom, -ValidTo) %>%
#   mutate(
#     ageGrp = AgeGroup, 
#     probDeath = as.numeric(ProbDyingInRange),
#     numSurv = as.numeric(SurvivingToGroup),
#     numDying = as.numeric(DyingInRange),
#     personYrs = as.numeric(PersonYearsLivedInRange),
#     personYrsAbove = as.numeric(TotalYearsLivedAboveGroup),
#     LE_x = as.numeric(LifeExpectancyAtRange),
#     LE_SE_probDeath = as.numeric(ProbDyingInRangeSE),
#     LE_SE_x = as.numeric(LifeExpectancyAtRangeSE),
#     GEOID = str_pad(TractID, 11, pad="0")) %>%
#   dplyr::select(-AgeGroup,-ProbDyingInRange,-SurvivingToGroup,-DyingInRange,-PersonYearsLivedInRange,-TotalYearsLivedAboveGroup,-LifeExpectancyAtRange,-ProbDyingInRangeSE,-LifeExpectancyAtRangeSE)
# 
# LEusbLA <- LEusb %>%
#   filter(str_sub(GEOID,1,2)=="22")
# 
# #### Opportunity Atlas data #### 
# 
# OpAtlas.tract_outcomes_simple <- ## your local file ##
#   mutate(state = str_pad(state,2, pad="0"),
#          county = str_pad(county,3, pad= "0"),
#          tract = str_pad(tract,6, pad="0"),
#          GEOID = paste0(state, county, tract)) 
# 
# OpAtlas.tract_covariates <- ## your local file ##
#   mutate(state = str_pad(state,2, pad="0"),
#          county = str_pad(county,3, pad= "0"),
#          tract = str_pad(tract,6, pad="0"),
#          GEOID = paste0(state, county, tract),
#          FIPS_5 = as.numeric(paste0(state, county))) 
# 
# #### acs ####
# 
# # us <- unique(fips_codes$state)[1:51]
# # acs15 <- map_df(us, function(x) {
# #   get_acs(geography = "tract",
# #           year = 2015,
# #           variables = c("B03002_001", "B03002_004"), # tot pop and Black non-Hispanic pop
# #           state = x,
# #           survey = "acs5")
# # })
# # 
# # acs <- acs15 %>%
# #   dplyr::select(-NAME, -moe) %>%
# #   spread(key = variable, value = estimate) %>%
# #   transmute(GEOID=GEOID, ACS.pctBlack = B03002_004/B03002_001)  %>%
# #   mutate(ACS.majBlack = ifelse(ACS.pctBlack >= .5,TRUE, FALSE))  # is the tract 50%+ Black? 
# 
# # save(acs,file = "inputs/acs.Rdata")
# load("inputs/acs.Rdata")
# 
# # acs15.NOmsa <- get_acs(geography = "tract",
# #                                  year = 2015,
# #                                  variables = c("B03002_001", "B03002_004", "B19013_001E"), # tot pop, Black non-Hispanic pop
# #                                  state = "LA",
# #                                  survey = "acs5")
# # 
# # acs15.NOmsa.dp <- get_acs(geography = "tract",
# #                                  year = 2015,
# #                                  variables = c(  "DP02_0058E", bach = "DP02_0064E", gradProf = "DP02_0065E"), # ed data
# #                                  state = "LA",
# #                                  survey = "acs5")
# # 
# # acs.NOmsa <- acs15.NOmsa %>%
# #   bind_rows(acs15.NOmsa.dp) %>%
# #     dplyr::select(-NAME, -moe) %>%
# #   filter(str_sub(GEOID,3,5) %in% GNO.parishfips) %>%
# #   spread(key = variable, value = estimate) %>%
# #   transmute(GEOID=GEOID, ACS.pctBlack = B03002_004/B03002_001, ACS.medInc = B19013_001, ACS.pctBachPlus = (DP02_0064 + DP02_0065)/DP02_0058)
# # 
# #  save(acs.NOmsa,file = "inputs/acsNOmsa.Rdata")
# load("inputs/acsNOmsa.Rdata")
# 
# #### MSAs ####
# ## this table comes originally from the Census at this site: https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
# ## Sept 2018 was the most recent available
# ## due to formatting, the file has to be downloaded and cleaned up - that's why it's stored in inputs
# MSAlist <- read_csv("inputs/MSA list.csv")
# MSAlist$FIPS_5 <- as.numeric(MSAlist$FIPS_5)
# names(MSAlist) <- c("city", "metroORmicro", "county", "stateName", "stateCode", "countyCode", "centralORoutlying", "FIPS_5")
# 
# 
# #### building blocks ####
# 
# ## tract & LE
# geoLE <- LEusb %>% 
#   filter(ageGrp == "Under 1") %>% 
#   dplyr::select(GEOID, LE_x, LE_SE_x) %>% 
#   rename(LE = LE_x, LE_se = LE_SE_x)
# 
# # tract & LE & MSA
# geoLEMSA <- geoLE %>%
#   mutate(FIPS_5 = as.numeric(substr(GEOID,1,5))) %>%
#   left_join(.,MSAlist, by = c("FIPS_5")) %>%
#   dplyr::select(GEOID, FIPS_5, LE, LE_se, city, metroORmicro)
# 
# 
# #### smooshing ####
# alldata <- LEusb %>%
#   mutate(FIPS_5 = as.numeric(substr(GEOID,1,5))) %>%
#   left_join(.,MSAlist, by = c("FIPS_5")) %>%
#   left_join(., OpAtlas.tract_covariates, by=c("GEOID")) %>%
#   left_join(., OpAtlas.tract_outcomes_simple, by=c("GEOID")) %>%
#   left_join(., acs, by=c("GEOID"))
# 
# ### Aggregated LE & prob death ###
# load("inputs/usLE.Rdata")
# load("inputs/stateLE.Rdata")
# load("inputs/msaLE.Rdata")
# load("inputs/parishLE.Rdata")
# 
# 
# load("inputs/msaProbDeath.Rdata")
# load("inputs/usProbDeathagg.Rdata")
# 
# 
# ### Pulling 2017 black population for Chapter 2
# 
# np.pull <- function(variables, names = variables, year=2017, survey = "acs/acs5"){
#   censuskey="530ce361defc2c476e5b5d5626d224d8354b9b9a"
#   tract <- getCensus(name = survey, 
#                      vintage = year, 
#                      key = censuskey, 
#                      vars = variables, 
#                      region = "tract:*", 
#                      regionin = "state:22+county:051,071,075,087,089,093,095,103") 
#   #select(-state, -county)
#   colnames(tract) <- c("state", "county", "tract", names) 
#   return(tract)
# }
# 
# ##pulling black 2017
# 
# black2017.vars <- c("B03002_001E", "B03002_004E")
# black2017.names <- c("pop2017", "black2017")
# black2017 <- np.pull(variables = black2017.vars, names = black2017.names) %>% 
#   mutate(GEOID = paste(state, county, tract, sep= "")) %>% 
#   mutate(GEOID = as.character(GEOID)) %>% 
#   select(GEOID, pop2017, black2017)
# 
# pov.vars <- c("B17001_001E","B17001_002E")
# pov.names <- c("pop2017","pov2017")
# pov2017 <- np.pull(variables = pov.vars, names = pov.names)%>% 
#   mutate(GEOID = paste(state, county, tract, sep= "")) %>% 
#   mutate(GEOID = as.character(GEOID)) %>% 
#   select(GEOID, pop2017, pov2017)
# 
# incDist.vars <- c("B19001_001E","B19001_002E","B19001_003E","B19001_004E","B19001_005E","B19001_006E","B19001_007E","B19001_008E","B19001_009E","B19001_010E","B19001_011E","B19001_012E","B19001_013E","B19001_014E","B19001_015E",
#                   "B19001_016E","B19001_017E")
# incDist.names <- c("totfams","inc_less10","inc10_15","inc15_20","inc20_25","inc25_30","inc30_35","inc35_40","inc40_45","inc45_50","inc50_60","inc60_75","inc75_100","inc100_125","inc125_150","inc150_200","inc_200PLUS")
# incomebuckets2017raw <- np.pull(variables = incDist.vars, names = incDist.names)
# 
# ### Save to Rdata
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
# 
