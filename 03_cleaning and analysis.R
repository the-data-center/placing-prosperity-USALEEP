#### AGGREGATING LE ####

### LA

popLELA <- left_join(LEusbLA, popLA, by = "GEOID") %>%
  mutate(fips5 = str_sub(GEOID, 1, 5))

parishLE <- popLELA %>%
  filter(ageGrp == "Under 1") %>%
  mutate(weightedLE = LE_x * popPooled) %>%
  mutate(weightedProbDeath = probDeath * popPooled) %>%
  group_by(fips5) %>%
  summarise(weightedSumLE = sum(weightedLE),
            weightedSumProbDeath = sum(weightedProbDeath),
            popSum = sum(popPooled)) %>%
  mutate(weightedAvgLE = weightedSumLE / popSum,
         weightedAvgProbDeath = weightedSumProbDeath / popSum) %>%
  left_join(FIPScrosswalk %>% dplyr::select(county, fipscounty, cbsaname), by = c("fips5" = "fipscounty")) 

#save(parishLE,file="inputs/parishLE.Rdata")

### US
pop <- left_join(pop10, pop15, by = "GEOID") %>%
  rename(pop10 = value, pop15 = estimate) %>%
  dplyr::select(-NAME.x, -NAME.y, -variable.x, -variable.y) %>%
  mutate(popPooled = pop10 + pop15)


popLE <- left_join(LEusb, pop, by = "GEOID") %>%
  mutate(fips5 = str_sub(GEOID, 1, 5))

countyLE <- popLE %>%
  filter(ageGrp == "Under 1") %>%
  mutate(weightedLE = LE_x * popPooled) %>%
  group_by(fips5) %>%
  summarise(weightedSumLE = sum(weightedLE),
            popSum = sum(popPooled)) %>%
  mutate(weightedAvgLE = weightedSumLE / popSum) %>%
  left_join(FIPScrosswalk %>% dplyr::select(county, state, fipscounty, cbsaname), by = c("fips5" = "fipscounty"))


missingTracts <- anti_join(pop, LEusb, by = "GEOID")%>% # to leave only tracts that have population estimates but no LE estimate
  mutate(fips5 = str_sub(GEOID,1,5)) %>%
  group_by(fips5) %>%
  summarise(numtractsNoData = n_distinct(GEOID))
sumTracts <- pop %>%
  mutate(fips5 = str_sub(GEOID,1,5)) %>%
  group_by(fips5) %>%
  summarise(numtracts = n_distinct(GEOID))
emptiesUS <- left_join(sumTracts, missingTracts, by = "fips5") %>%
  mutate(pctEmpty = numtractsNoData/numtracts)%>%
  left_join(FIPScrosswalk %>% dplyr::select(county, fipscounty, cbsaname), by = c("fips5" = "fipscounty")) %>%
  filter(pctEmpty>=.32)

msaLE <-popLE %>%
  filter(ageGrp == "Under 1") %>%
  mutate(FIPS_5 = as.numeric(fips5)) %>%
  left_join(MSAlist, by = "FIPS_5") %>%
  filter(metroORmicro == "Metropolitan Statistical Area") %>%
  mutate(weightedLE = LE_x * popPooled) %>%
  group_by(city) %>%
  summarise(weightedSumLE = sum(weightedLE),
            popSum = sum(popPooled)) %>%
  mutate(weightedAvgLE = weightedSumLE / popSum) %>%
  right_join(MSAlist, by = "city") %>%
  filter(metroORmicro == "Metropolitan Statistical Area")%>%
  dplyr::select(FIPS_5, LE = weightedAvgLE, popSum)

# save(msaLE,file="inputs/msaLE.Rdata")

stateLE <- popLE %>%
  mutate(stateCode = str_sub(GEOID,1,2)) %>%
  filter(ageGrp == "Under 1") %>%
  mutate(weightedLE = LE_x * popPooled) %>%
  group_by(stateCode) %>%
  summarise(weightedSumLE = sum(weightedLE),
            popSum = sum(popPooled)) %>%
  mutate(weightedAvgLE = weightedSumLE / popSum) %>%
  left_join(FIPScrosswalk %>% dplyr::select(state, fipsstate), by = c("stateCode" = "fipsstate")) %>%
  distinct()

#save(stateLE,file = "inputs/stateLE.Rdata")

usLE <- popLE %>%
  filter(ageGrp == "Under 1") %>%
  mutate(weightedLE = LE_x * popPooled) %>%
  summarise(weightedSumLE = sum(weightedLE),
            popSum = sum(popPooled)) %>%
  mutate(weightedAvgLE = weightedSumLE / popSum) 

#save(usLE,file = "inputs/usLE.Rdata")

usLEblk <- popLE %>%
  filter(ageGrp == "Under 1") %>%
  left_join(alldata %>%
              dplyr::select(GEOID, ACS.majBlack)) %>%
  mutate(weightedLE = LE_x * popPooled) %>%
  group_by(ACS.majBlack) %>%
  summarise(weightedSumLE = sum(weightedLE),
            popSum = sum(popPooled)) %>%
  mutate(weightedAvgLE = weightedSumLE / popSum)
save(usLEblk,file = "inputs/usLEblk.Rdata")

NOmsaLEblk <-popLE %>%
  filter(ageGrp == "Under 1") %>%
  mutate(FIPS_5 = as.numeric(fips5))%>%
  filter(FIPSCodeState == 22 & str_sub(fips5,3,5) %in% GNO.parishfips) %>%
  left_join(alldata %>%
              dplyr::select(GEOID, ACS.majBlack)) %>%
  group_by(ACS.majBlack) %>%
  mutate(weightedLE = LE_x * popPooled) %>%
  summarise(weightedSumLE = sum(weightedLE),
            popSum = sum(popPooled)) %>%
  mutate(weightedAvgLE = weightedSumLE / popSum)
save(NOmsaLEblk,file = "inputs/NOmsaLEblk.Rdata")


#### PROBABLILITY OF DEATH ####

### Population data

popLA10 <- popLA10Raw %>%
  dplyr::select(-NAME) %>%
  tidyr::spread(key = variable, value = value)
colnames(popLA10) <- c("GEOID", age2000.names)
popLA10.summed <- popLA10 %>%
  transmute(GEOID = GEOID, 
            `Under 5` = m_under5 + f_under5,
            `5-14` = m5_9+m10_14+f5_9+f10_14,
            `15-24` = m15_17 + m18_19 + m20 + m21 + m22_24 + f15_17 + f18_19 + f20 + f21 + f22_24,
            `25-34` = m25_29 + m30_34 + f25_29 + f30_34,
            `35-44` = m35_39 + m40_44 + f35_39 + f40_44,
            `45-54` = m45_49 + m50_54 + f45_49 + f50_54,
            `55-64` = m55_59 + m60_61 + m62_64 + f55_59 + f60_61 + f62_64,
            `65-74` = m65_66 + m67_69 + m70_74 + f65_66 + f67_69 + f70_74,
            `75-84` = m75_79 + m80_84 + f75_79 + f80_84,
            `85 and older` = m85over + f85over)
popLA10.long <- popLA10.summed %>%
  gather(-GEOID, key = var, value = value)


popLA15 <- popLA15Raw %>%
  dplyr::select(-NAME, -moe) %>%
  tidyr::spread(key = variable, value = estimate)
colnames(popLA15) <- c("GEOID", age.names.short)
popLA15.summed <- popLA15 %>%
  transmute(GEOID = GEOID,
            `Under 5` = m_under5 + f_under5,
            `5-14` = m5_9+m10_14+f5_9+f10_14,
            `15-24` = m15_17 + m18_19 + m20 + m21 + m22_24 + f15_17 + f18_19 + f20 + f21 + f22_24,
            `25-34` = m25_29 + m30_34 + f25_29 + f30_34,
            `35-44` = m35_39 + m40_44 + f35_39 + f40_44,
            `45-54` = m45_49 + m50_54 + f45_49 + f50_54,
            `55-64` = m55_59 + m60_61 + m62_64 + f55_59 + f60_61 + f62_64,
            `65-74` = m65_66 + m67_69 + m70_74 + f65_66 + f67_69 + f70_74,
            `75-84` = m75_79 + m80_84 + f75_79 + f80_84,
            `85 and older` = m85over + f85over)
popLA15.long <- popLA15.summed %>%
  gather(-GEOID, key = var, value = estimate)

popLA <- left_join(popLA10.long, popLA15.long, by = c("GEOID", "var")) %>%
  rename(pop10 = value, pop15 = estimate) %>%
  mutate(popPooled = pop10 + pop15) %>%
  filter(var != "Under 5") %>%
  rename(ageGrp = var)

#### LE data #### 

LEusbLA.abb <- LEusb %>% # abb = abbreviated - we don't have the 2 youngest age groups' pops
  filter(str_sub(GEOID,1,2)=="22") %>%
  mutate(ageGrp = as.character(ageGrp)) %>%
  filter(ageGrp != "Under 1" & ageGrp != "1-4") # filtering out 2 youngest age groups


popLELA <- left_join(LEusbLA.abb, popLA, by = c("GEOID", "ageGrp")) %>%
  mutate(fips5 = str_sub(GEOID, 1, 5))


parishProbDeath <- popLELA %>%
  mutate(weightedProbDeath = probDeath * popPooled) %>%
  group_by(fips5, ageGrp) %>%
  summarise(weightedSumProbDeath = sum(weightedProbDeath),
            popSum = sum(popPooled)) %>%
  mutate(weightedAvgProbDeath = weightedSumProbDeath / popSum) %>%
  left_join(FIPScrosswalk %>% dplyr::select(county, fipscounty, cbsaname), by = c("fips5" = "fipscounty")) 


pop10 <- pop10Raw %>%
  dplyr::select(-NAME) %>%
  tidyr::spread(key = variable, value = value)
colnames(pop10) <- c("GEOID", age2000.names)

pop10.summed <- pop10 %>%
  transmute(GEOID = GEOID,
            `Under 5` = m_under5 + f_under5,
            `5-14` = m5_9+m10_14+f5_9+f10_14,
            `15-24` = m15_17 + m18_19 + m20 + m21 + m22_24 + f15_17 + f18_19 + f20 + f21 + f22_24,
            `25-34` = m25_29 + m30_34 + f25_29 + f30_34,
            `35-44` = m35_39 + m40_44 + f35_39 + f40_44,
            `45-54` = m45_49 + m50_54 + f45_49 + f50_54,
            `55-64` = m55_59 + m60_61 + m62_64 + f55_59 + f60_61 + f62_64,
            `65-74` = m65_66 + m67_69 + m70_74 + f65_66 + f67_69 + f70_74,
            `75-84` = m75_79 + m80_84 + f75_79 + f80_84,
            `85 and older` = m85over + f85over)

pop10.long <- pop10.summed %>%
  gather(-GEOID, key = var, value = value)




pop15 <- pop15Raw %>%
  dplyr::select(-NAME, -moe) %>%
  tidyr::spread(key = variable, value = estimate)
colnames(pop15) <- c("GEOID", age.names.short)

pop15.summed <- pop15 %>%
  transmute(GEOID = GEOID,
            `Under 5` = m_under5 + f_under5,
            `5-14` = m5_9+m10_14+f5_9+f10_14,
            `15-24` = m15_17 + m18_19 + m20 + m21 + m22_24 + f15_17 + f18_19 + f20 + f21 + f22_24,
            `25-34` = m25_29 + m30_34 + f25_29 + f30_34,
            `35-44` = m35_39 + m40_44 + f35_39 + f40_44,
            `45-54` = m45_49 + m50_54 + f45_49 + f50_54,
            `55-64` = m55_59 + m60_61 + m62_64 + f55_59 + f60_61 + f62_64,
            `65-74` = m65_66 + m67_69 + m70_74 + f65_66 + f67_69 + f70_74,
            `75-84` = m75_79 + m80_84 + f75_79 + f80_84,
            `85 and older` = m85over + f85over)
pop15.long <- pop15.summed %>%
  gather(-GEOID, key = var, value = estimate)

pop <- left_join(pop10.long, pop15.long, by = "GEOID") %>%
  rename(pop10 = value, pop15 = estimate) %>%
  dplyr::select(-NAME.x, -NAME.y, -variable.x, -variable.y) %>%
  mutate(popPooled = pop10 + pop15)%>%
  filter(var != "Under 5") %>%
  rename(ageGrp = var)

popLE.pd <- left_join((LEusb %>%
                         filter(ageGrp != "Under 1" & ageGrp != "1-4") ), pop, by = "GEOID") %>%
  mutate(fips5 = str_sub(GEOID, 1, 5))

usProbDeath.agg <- popLE.pd %>%
  mutate(weightedProbDeath = probDeath * popPooled) %>%
  group_by(ageGrp) %>%
  summarise(weightedSumProbDeath = sum(weightedProbDeath),
            popSum = sum(popPooled)) %>%
  mutate(weightedAvgProbDeath = weightedSumProbDeath / popSum) 

#save(usProbDeath.agg,file = "inputs/usProbDeathagg.Rdata")


msaProbDeath <-popLE.pd %>%
  mutate(FIPS_5 = as.numeric(fips5)) %>%
  left_join(MSAlist, by = "FIPS_5") %>%
  filter(metroORmicro == "Metropolitan Statistical Area") %>%
  mutate(weightedProbDeath = probDeath * popPooled) %>%
  group_by(city, ageGrp) %>%
  summarise(weightedSumProbDeath = sum(weightedProbDeath),
            popSum = sum(popPooled)) %>%
  mutate(weightedAvgProbDeath = weightedSumProbDeath / popSum)

#save(msaProbDeath,file="inputs/msaProbDeath.Rdata")

#### INEQUALITY DATA ####

metros <- alldata %>% 
  filter(grepl("Metro",metroORmicro)) %>% 
  group_by(city) %>% 
  summarise(pop = sum(pooled_pooled_count)) %>%
  top_n(100) %>%
  mutate(state.abb = str_sub(city,-2), metro.abb = paste0("metro",str_sub(city, 1, 6), state.abb,".LE"), poorchild.pop.rank = 101-rank(pop)) 
metro.popLE <- popLE %>%
  filter(ageGrp == "Under 1") %>%
  mutate(FIPS_5 = as.numeric(fips5)) %>%
  left_join(MSAlist, by = "FIPS_5") %>%
  filter(city %in% metros$city)

ineq.metro <- metro.popLE %>% 
  filter(ageGrp == "Under 1") %>%
  mutate(weightedLE = LE_x * popPooled) %>%
  group_by(city) %>%
  summarise(p1 = wtd.quantile(LE_x, weights = popPooled, probs = .01, na.rm = TRUE),
            p10 = wtd.quantile(LE_x, weights = popPooled, probs = .1, na.rm = TRUE),
            p20 = wtd.quantile(LE_x, weights = popPooled, probs = .2, na.rm = TRUE),
            p25 = wtd.quantile(LE_x, weights = popPooled, probs = .25, na.rm = TRUE),
            p30 = wtd.quantile(LE_x, weights = popPooled, probs = .3, na.rm = TRUE),
            p40 = wtd.quantile(LE_x, weights = popPooled, probs = .4, na.rm = TRUE),
            p50 = wtd.quantile(LE_x, weights = popPooled, probs = .5, na.rm = TRUE),
            p60 = wtd.quantile(LE_x, weights = popPooled, probs = .6, na.rm = TRUE),
            p70 = wtd.quantile(LE_x, weights = popPooled, probs = .7, na.rm = TRUE),
            p75 = wtd.quantile(LE_x, weights = popPooled, probs = .75, na.rm = TRUE),
            p80 = wtd.quantile(LE_x, weights = popPooled, probs = .8, na.rm = TRUE),
            p90 = wtd.quantile(LE_x, weights = popPooled, probs = .9, na.rm = TRUE),
            p99 = wtd.quantile(LE_x, weights = popPooled, probs = .99, na.rm = TRUE)) %>%
  mutate(rel8020 = p80/p20, 
         abs8020 = p80-p20,
         rel991 =  p99/p1,
         abs991 =  p99-p1) %>%
  right_join(MSAlist, by = "city") %>%
  filter(!is.na(p1))%>% # best way to get rid of non-top 100 metros 
  dplyr::select(1:18, FIPS_5) %>%
  rename(geo = city) %>%
  left_join(msaLE, by = "FIPS_5") %>% 
  dplyr::select(geo, p20, p80, abs8020, LE)%>% 
  distinct()

#save(ineq.metro,file="inputs/ineqMetro.Rdata")
write.csv(ineq.metro,file="outputs/CSVs/absIneq_metro.csv")


## CDC Wonder Tool
### Figure 4 & 5
### Deaths by despair by cause of death charts
## First have to transform a ton of data!

Metro_Cancer <- Metro_Cancerraw %>% 
  select(-Notes, -Year.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Age.Adjusted.Rate = as.numeric(as.character(Age.Adjusted.Rate)),
         COD = "Cancer",
         Geography = "Metro New Orleans") 

Metro_Heart <- Metro_Heartraw %>% 
  select(-Notes, -Year.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Age.Adjusted.Rate = as.numeric(as.character(Age.Adjusted.Rate)),
         COD = "Heart Disease",
         Geography = "Metro New Orleans") 

Metro_HIV <- Metro_HIVraw %>% 
  select(-Notes, -Year.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Age.Adjusted.Rate = as.numeric(as.character(Age.Adjusted.Rate)),
         COD = "HIV",
         Geography = "Metro New Orleans") 

Metro_Liver <- Metro_Liverraw %>% 
  select(-Notes, -Year.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Age.Adjusted.Rate = as.numeric(as.character(Age.Adjusted.Rate)),
         COD = "Alcoholic Liver Disease",
         Geography = "Metro New Orleans") 

Metro_OD <- Metro_ODraw %>% 
  select(-Notes, -Year.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Age.Adjusted.Rate = as.numeric(as.character(Age.Adjusted.Rate)),
         COD = "Drug Poisoning",
         Geography = "Metro New Orleans") 


Metro_Suicide <- Metro_Suicideraw %>% 
  select(-Notes, -Year.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Age.Adjusted.Rate = as.numeric(as.character(Age.Adjusted.Rate)),
         COD = "Suicide",
         Geography = "Metro New Orleans")



#### United States

US_Cancer <- US_Cancerraw %>% 
  select(-Notes, -Year.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Age.Adjusted.Rate = as.numeric(as.character(Age.Adjusted.Rate)),
         COD = "Cancer",
         Geography = "United States") 

US_Heart <- US_Heartraw %>% 
  select(-Notes, -Year.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Age.Adjusted.Rate = as.numeric(as.character(Age.Adjusted.Rate)),
         COD = "Heart Disease",
         Geography = "United States") 

US_HIV <- US_HIVraw %>% 
  select(-Notes, -Year.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Age.Adjusted.Rate = as.numeric(as.character(Age.Adjusted.Rate)),
         COD = "HIV",
         Geography = "United States") 

US_Liver <- US_Liverraw %>% 
  select(-Notes, -Year.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Age.Adjusted.Rate = as.numeric(as.character(Age.Adjusted.Rate)),
         COD = "Alcoholic Liver Disease",
         Geography = "United States")

US_OD <- US_ODraw %>% 
  select(-Notes, -Year.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Age.Adjusted.Rate = as.numeric(as.character(Age.Adjusted.Rate)),
         COD = "Drug Poisoning",
         Geography = "United States") 

US_Suicide <- US_Suicideraw %>% 
  select(-Notes, -Year.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Age.Adjusted.Rate = as.numeric(as.character(Age.Adjusted.Rate)),
         COD = "Suicide",
         Geography = "United States")


DD <- bind_rows(Metro_Cancer, Metro_Heart, Metro_HIV, Metro_Liver, Metro_OD, Metro_Suicide, US_Cancer, US_Heart, US_HIV, US_Liver, US_OD, US_Suicide) %>% 
  select(-Deaths, -Population, -Crude.Rate) %>% 
  pivot_wider(., names_from = Year, values_from = Age.Adjusted.Rate) %>% 
  mutate(`1999-2001` = (`1999` + `2000` + `2001`)/3,
         `2002-2004` = (`2002` + `2003` + `2004`) / 3,
         `2005-2007` = (`2005` + `2006` + `2007`) / 3,
         `2008-2010` = (`2008` + `2009` + `2010`) / 3,
         `2011-2013` = (`2011` + `2012` + `2013`) / 3,
         `2014-2016` = (`2014` + `2015` + `2016`) / 3,
         `2017-2018` = (`2017` + `2018`) / 2) %>% 
  select(COD,
         Geography,
         `1999-2001`,
         `2002-2004`,
         `2005-2007`,
         `2008-2010`,
         `2011-2013`,
         `2014-2016`,
         `2017-2018`) %>% 
  pivot_longer(names_to = "Year", cols = 3:9) %>% 
  rename(Age.Adjusted.Rate = "value") 

























##Figure 1.7: Middle age mortality rates per 100,000 (Source: CDC Wonder)
##Figure 1.8 Mortality trends for yourh and elderly per 100,000 (Source: CDC Wonder)
## Creating Metro Death Rates


Metro_Total <- Metro_Totalraw %>% 
  select(-Notes, -Year.Code, -Ten.Year.Age.Groups.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Ten.Year.Age.Groups = as.character(Ten.Year.Age.Groups),
         Race = "Tot",
         Geography = "Metro New Orleans") %>% 
  rename(Age.Group = "Ten.Year.Age.Groups") %>% 
  rename(Rate = "Crude.Rate") %>% 
  filter(Age.Group != "Not Stated")

Metro_Black <- Metro_Blackraw %>% 
  select(-Notes, -Year.Code, -Ten.Year.Age.Groups.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Ten.Year.Age.Groups = as.character(Ten.Year.Age.Groups),
         Race = "Black",
         Geography = "Metro New Orleans") %>% 
  rename(Age.Group = "Ten.Year.Age.Groups")%>% 
  rename(Rate = "Crude.Rate") %>% 
  filter(Age.Group != "Not Stated")

Metro_White <- Metro_Whiteraw %>% 
  select(-Notes, -Year.Code, -Ten.Year.Age.Groups.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Ten.Year.Age.Groups = as.character(Ten.Year.Age.Groups),
         Race = "White",
         Geography = "Metro New Orleans") %>% 
  rename(Age.Group = "Ten.Year.Age.Groups")%>% 
  rename(Rate = "Crude.Rate") %>% 
  filter(Age.Group != "Not Stated")

US_Total <- US_Totalraw %>% 
  select(-Notes, -Year.Code, -Ten.Year.Age.Groups.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Ten.Year.Age.Groups = as.character(Ten.Year.Age.Groups),
         Race = "Tot",
         Geography = "United States") %>% 
  rename(Age.Group = "Ten.Year.Age.Groups")%>% 
  rename(Rate = "Crude.Rate") %>% 
  filter(Age.Group != "Not Stated")


US_Black <- US_Blackraw %>% 
  select(-Notes, -Year.Code, -Ten.Year.Age.Groups.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Ten.Year.Age.Groups = as.character(Ten.Year.Age.Groups),
         Race = "Black",
         Geography = "United States") %>% 
  rename(Age.Group = "Ten.Year.Age.Groups")%>% 
  rename(Rate = "Crude.Rate") %>% 
  filter(Age.Group != "Not Stated")

US_White <- US_Whiteraw %>% 
  select(-Notes, -Year.Code, -Ten.Year.Age.Groups.Code) %>% 
  na.omit() %>% 
  mutate(Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate)),
         Ten.Year.Age.Groups = as.character(Ten.Year.Age.Groups),
         Race = "White",
         Geography = "United States") %>% 
  rename(Age.Group = "Ten.Year.Age.Groups")%>% 
  rename(Rate = "Crude.Rate") %>% 
  filter(Age.Group != "Not Stated")


Metro_deathrates <- bind_rows(Metro_Total, Metro_Black, Metro_White, US_Total, US_Black, US_White)









### Figure 7 
### Middle age mortality rates over time



middleage <- Metro_deathrates %>% 
  filter(Race == "Tot") %>% 
  select(Year, Geography, Age.Group, Rate) %>% 
  pivot_wider(., names_from = Year, values_from = Rate) %>% 
  mutate(`1999-2001` = (`1999` + `2000` + `2001`)/3,
         `2002-2004` = (`2002` + `2003` + `2004`) / 3,
         `2005-2007` = (`2005` + `2006` + `2007`) / 3,
         `2008-2010` = (`2008` + `2009` + `2010`) / 3,
         `2011-2013` = (`2011` + `2012` + `2013`) / 3,
         `2014-2016` = (`2014` + `2015` + `2016`) / 3,
         `2017-2018` = (`2017` + `2018`) / 2) %>% 
  select(Age.Group,
         Geography,
         `1999-2001`,
         `2002-2004`,
         `2005-2007`,
         `2008-2010`,
         `2011-2013`,
         `2014-2016`,
         `2017-2018`) %>% 
  pivot_longer(names_to = "Year", cols = 3:9) %>% 
  rename(Rate = "value") 





## Fig 9 
## Death Rates by race

### Metro death rates by race


Metro_race <- Metro_deathrates %>% 
  filter(Age.Group %in% c( "15-24 years", "25-34 years", "35-44 years", "45-54 years", "55-64 years")) %>% 
  filter(Geography == "Metro New Orleans") %>% 
  select(Year, Race, Age.Group, Rate) %>% 
  pivot_wider(names_from = Year, values_from = Rate) %>% 
  mutate(`1999-2001` = (`1999` + `2000` + `2001`)/3,
         `2002-2004` = (`2002` + `2003` + `2004`) / 3,
         `2005-2007` = (`2005` + `2006` + `2007`) / 3,
         `2008-2010` = (`2008` + `2009` + `2010`) / 3,
         `2011-2013` = (`2011` + `2012` + `2013`) / 3,
         `2014-2016` = (`2014` + `2015` + `2016`) / 3,
         `2017-2018` = (`2017` + `2018`) / 2) %>% 
  select(Age.Group,
         Race,
         `1999-2001`,
         `2002-2004`,
         `2005-2007`,
         `2008-2010`,
         `2011-2013`,
         `2014-2016`,
         `2017-2018`) %>% 
  pivot_longer(names_to = "Year", cols = 3:9) %>% 
  mutate(Race = ifelse(Race == "Tot", "Total", Race)) %>% 
  mutate(Year = ifelse(Year == "1999-2001", "2000", Year),
         Year = ifelse(Year == "2008-2010", "2010", Year),
         Year = ifelse(Year == "2017-2018", "2018", Year))%>% 
  rename(Rate = "value") 




## Fig 10 US Race

US_race <- Metro_deathrates %>% 
  filter(Age.Group %in% c( "15-24 years", "25-34 years", "35-44 years", "45-54 years", "55-64 years")) %>% 
  filter(Geography == "United States") %>% 
  select(Year, Race, Age.Group, Rate) %>% 
  pivot_wider(names_from = Year, values_from = Rate) %>% 
  mutate(`1999-2001` = (`1999` + `2000` + `2001`)/3,
         `2002-2004` = (`2002` + `2003` + `2004`) / 3,
         `2005-2007` = (`2005` + `2006` + `2007`) / 3,
         `2008-2010` = (`2008` + `2009` + `2010`) / 3,
         `2011-2013` = (`2011` + `2012` + `2013`) / 3,
         `2014-2016` = (`2014` + `2015` + `2016`) / 3,
         `2017-2018` = (`2017` + `2018`) / 2) %>% 
  select(Age.Group,
         Race,
         `1999-2001`,
         `2002-2004`,
         `2005-2007`,
         `2008-2010`,
         `2011-2013`,
         `2014-2016`,
         `2017-2018`) %>% 
  pivot_longer(names_to = "Year", cols = 3:9) %>% 
  mutate(Race = ifelse(Race == "Tot", "Total", Race)) %>% 
  mutate(Year = ifelse(Year == "1999-2001", "2000", Year),
         Year = ifelse(Year == "2008-2010", "2010", Year),
         Year = ifelse(Year == "2017-2018", "2018", Year))%>% 
  rename(Rate = "value") 




## Fig 11
### All cause mortality Metro and US

USMort <- USMortraw %>% 
  na.omit() %>% 
  mutate(Notes = as.character(Notes),
         Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Age.Adjusted.Rate = as.numeric(as.character(Age.Adjusted.Rate )),
         Geography = "United States") %>% 
  select(-Notes, -Year.Code)

OrleansMort <- OrleansMortraw %>% 
  na.omit() %>% 
  mutate(Notes = as.character(Notes),
         Year = as.numeric(as.character(Year)),
         County = as.character(County),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Age.Adjusted.Rate  = as.numeric(as.character(Age.Adjusted.Rate ))) %>% 
  select(-Notes, -Year.Code, -County.Code) %>% 
  rename(Geography = "County")

MetroMort <- MetroMortraw %>% 
  na.omit() %>% 
  mutate(Notes = as.character(Notes),
         Year = as.numeric(as.character(Year)),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Age.Adjusted.Rate  = as.numeric(as.character(Age.Adjusted.Rate )),
         Geography = "Metro New Orleans") %>% 
  select(-Notes, -Year.Code) 

AllgeoMort <- AllgeoMortraw %>% 
  mutate(Notes = as.character(Notes),
         Year = as.numeric(as.character(Year)),
         County = as.character(County),
         Deaths = as.numeric(as.character(Deaths)),
         Population = as.numeric(as.character(Population)),
         Crude.Rate = as.numeric(as.character(Crude.Rate))) %>% 
  mutate(County = County,
         County = ifelse(Notes == "Total", "Metro (Minus Orleans)", County)) %>% 
  rename(Geography = "County") %>%
  select(-Notes, -Year.Code, -County.Code) %>% 
  na.omit() %>% 
  bind_rows(OrleansMort) %>% 
  bind_rows(USMort) %>% 
  bind_rows(MetroMort)







###Chapter 2



#####Fig4_ch2_pov_7000

Orleans_pov_1970_2010 <- pov1970_2010 %>% 
  filter(str_detect(tolower(GEO2010), pattern = "22071")) %>% 
  mutate(GEO2010 = as.numeric(as.character(GEO2010)),
         pop1970 = as.numeric(as.character(POVRAT7D)),
         pov1970 = as.numeric(as.character(POVRAT7N)),
         pop1980 = as.numeric(as.character(POVRAT8D)),
         pov1980 = as.numeric(as.character(POVRAT8N)),
         pop1990 = as.numeric(as.character(POVRAT9D)),
         pov1990 = as.numeric(as.character(POVRAT9N)),
         pop2000 = as.numeric(as.character(POVRAT0D)),
         pov2000 = as.numeric(as.character(POVRAT0N)),
         pop2010 = as.numeric(as.character(POVRAT1AD)),
         pov2010 = as.numeric(as.character(POVRAT1AN))) %>% 
  rename(GEOID = "GEO2010") %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  left_join(., pov2017, by = "GEOID") %>% 
  select(GEOID, pop1970, pov1970, pop1980, pov1980, pop1990, pov1990, pop2000, pov2000, pop2010, pov2010, pop2017, pov2017) 

Orleans_neighborhood_pov_1970_2017 <- merge(Orleans_pov_1970_2010, NOLAcrosswalk, by = "GEOID" ) %>% 
  select(-GEOID) %>% 
  group_by(Neighborhood) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(pctpov1970 = pov1970 / pop1970,
         pctpov1980 = pov1980 / pop1980,
         pctpov1990 = pov1990 / pop1990,
         pctpov2000 = pov2000 / pop2000,
         pctpov2010 = pov2010 / pop2010,
         pctpov2017 = pov2017 / pop2017) 







### Fig5_ch2_black_7000


Orleans_black_1970_2017 <- black1970_2010 %>% 
  filter(str_detect(tolower(GEO2010), pattern = "22071")) %>% 
  mutate(pop1970 = as.numeric(as.character(SHR7D)),
         black1970 = as.numeric(as.character(SHRBLK7N)),
         pop1980 = as.numeric(as.character(SHR8D)),
         black1980 = as.numeric(as.character(SHRBLK8N)),
         pop1990 = as.numeric(as.character(SHR9D)),
         black1990 = as.numeric(as.character(SHRBLK9N)),
         pop2000 = as.numeric(as.character(SHR0D)),
         black2000 = as.numeric(as.character(SHRBLK0N)),
         pop2010 = as.numeric(as.character(SHR1D)),
         black2010 = as.numeric(as.character(SHRBLK1N))) %>% 
  rename(GEOID = 'GEO2010') %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  left_join(., black2017, by = "GEOID") %>% 
  select(GEOID, pop1970, black1970, pop1980, black1980, pop1990, black1990, pop2000, black2000, pop2010, black2010, pop2017, black2017) 

Orleans_neighborhood_black_1970_2017 <- merge(Orleans_black_1970_2017, NOLAcrosswalk, by = "GEOID" ) %>%
  filter(Neighborhood != "Z-no land mass-all Lake") %>% 
  mutate(GEOID = as.numeric(GEOID)) %>%
  group_by(Neighborhood) %>%
  summarise_all(funs(sum)) %>%
  mutate(pctblk1970 = black1970 / pop1970,
         pctblk1980 = black1980 / pop1980,
         pctblk1990 = black1990 / pop1990,
         pctblk2000 = black2000 / pop2000,
         pctblk2010 = black2010 / pop2010,
         pctblk2017 = black2017 / pop2017) %>%
  mutate(group_1970_2000 = "Other",
         group_1970_2000 = ifelse(pctblk1970 < .3 & pctblk2000 < .3, "Stable White",
                                  ifelse(pctblk1970 >.4 & pctblk1970 < .6 & pctblk2000 > .4 & pctblk2000 < .6, "Stably Integrated",
                                         ifelse(pctblk1970 < .3 & pctblk2000 >.3, "White Flight",
                                                ifelse(pctblk1970 >.3 & pctblk1970 <.7 & pctblk2000 > .7, "Partially Black to Segregated Black",
                                                       ifelse(pctblk1970 > .7 & pctblk2000 > .7, "Stable Black", "Other" )))))) %>%
  mutate(group_1970_2010 = "Other",
         group_1970_2010 = ifelse(pctblk1970 < .3 & pctblk2010 < .3, "Stable White",
                                  ifelse(pctblk1970 >.4 & pctblk1970 < .6 & pctblk2010 > .4 & pctblk2010 < .6, "Stably Integrated",
                                         ifelse(pctblk1970 < .3 & pctblk2010 >.3, "White Flight",
                                                ifelse(pctblk1970 >.3 & pctblk1970 <.7 & pctblk2010 > .7, "Partially Black to Segregated Black",
                                                       ifelse(pctblk1970 > .7 & pctblk2010 > .7, "Stable Black", "Other" )))))) %>%
  mutate(group_2000_2010 = "Other",
         group_2000_2010 = ifelse(pctblk2000 < .3 & pctblk2010 < .3, "Stable White",
                                  ifelse(pctblk2000 >.4 & pctblk1970 < .6 & pctblk2010 > .4 & pctblk2010 < .6, "Stably Integrated",
                                         ifelse(pctblk2000 < .3 & pctblk2010 >.3, "White Flight",
                                                ifelse(pctblk2000 >.3 & pctblk1970 <.7 & pctblk2010 > .7, "Partially Black to Segregated Black",
                                                       ifelse(pctblk2000 > .7 & pctblk2010 > .7, "Stable Black", "Other" )))))) %>%
  mutate(group_2010_2017 = "Other",
         group_2010_2017 = ifelse(pctblk2010 < .3 & pctblk2017 < .3, "Stable White",
                                  ifelse(pctblk2010 >.4 & pctblk2010 < .6 & pctblk2017 > .4 & pctblk2017 < .6, "Stably Integrated",
                                         ifelse(pctblk2010 < .3 & pctblk2017 >.3, "White Flight",
                                                ifelse(pctblk2010 >.3 & pctblk2010 <.7 & pctblk2017 > .7, "Partially Black to Segregated Black",
                                                       ifelse(pctblk2010 > .7 & pctblk2017 > .7, "Stable Black", "Other" )))))) %>%
  mutate(group_1970_2017 = "Other",
         group_1970_2017 = ifelse(pctblk1970 < .3 & pctblk2017 < .3, "Stable White",
                                  ifelse(pctblk1970 >.4 & pctblk1970 < .6 & pctblk2017 > .4 & pctblk2017 < .6, "Stably Integrated",
                                         ifelse(pctblk1970 < .3 & pctblk2017 >.3, "White Flight",
                                                ifelse(pctblk1970 >.3 & pctblk1970 <.7 & pctblk2017 > .7, "Partially Black to Segregated Black",
                                                       ifelse(pctblk1970 > .7 & pctblk2017 > .7, "Stable Black", "Other" )))))) %>%
  mutate(group_2000_2017 = "Other",
         group_2000_2017 = ifelse(pctblk2000 < .3 & pctblk2017 < .3, "Stable White",
                                  ifelse(pctblk2000 >.4 & pctblk2000 < .6 & pctblk2017 > .4 & pctblk2017 < .6, "Stably Integrated",
                                         ifelse(pctblk2000 < .3 & pctblk2017 >.3, "White Flight",
                                                ifelse(pctblk2000 >.3 & pctblk2000 <.7 & pctblk2017 > .7, "Partially Black to Segregated Black",
                                                       ifelse(pctblk2000 > .7 & pctblk2017 > .7, "Stable Black", "Other" ))))))







### Percent of people living in census tracts with concentrated poverty, 1970-2010

Metro_pov_1970_2017 <- pov1970_2010 %>% 
  filter(str_detect(tolower(GEO2010), pattern = "22071")| str_detect(tolower(GEO2010), pattern = "22051") |
           str_detect(tolower(GEO2010), pattern = "22075") | str_detect(tolower(GEO2010), pattern = "22087") |
           str_detect(tolower(GEO2010), pattern = "22089") | str_detect(tolower(GEO2010), pattern = "22093") |
           str_detect(tolower(GEO2010), pattern = "22095") | str_detect(tolower(GEO2010), pattern = "22103")) %>% 
  mutate(GEO2010 = as.numeric(as.character(GEO2010)),
         pop1970 = as.numeric(as.character(POVRAT7D)),
         pov1970 = as.numeric(as.character(POVRAT7N)),
         pop1980 = as.numeric(as.character(POVRAT8D)),
         pov1980 = as.numeric(as.character(POVRAT8N)),
         pop1990 = as.numeric(as.character(POVRAT9D)),
         pov1990 = as.numeric(as.character(POVRAT9N)),
         pop2000 = as.numeric(as.character(POVRAT0D)),
         pov2000 = as.numeric(as.character(POVRAT0N)),
         pop2010 = as.numeric(as.character(POVRAT1AD)),
         pov2010 = as.numeric(as.character(POVRAT1AN))) %>% 
  rename(GEOID = "GEO2010") %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  left_join(., pov2017, by = "GEOID") %>% 
  select(GEOID, pop1970, pov1970, pop1980, pov1980, pop1990, pov1990, pop2000, pov2000, pop2010, pov2010, pop2017, pov2017) %>% 
  mutate(pctpov1970 = pov1970 / pop1970,
         pctpov1980 = pov1980 / pop1980,
         pctpov1990 = pov1990 / pop1990,
         pctpov2000 = pov2000 / pop2000,
         pctpov2010 = pov2010 / pop2010,
         pctpov2017 = pov2017/ pop2017) %>% 
  mutate(povcount1970 = ifelse(pctpov1970 > .39999999, pop1970, 0),
         povcount1980 = ifelse(pctpov1980 > .39999999, pop1980, 0),
         povcount1990 = ifelse(pctpov1990 > .39999999, pop1990, 0),
         povcount2000 = ifelse(pctpov2000 > .39999999, pop2000, 0),
         povcount2010 = ifelse(pctpov2010 > .39999999, pop2010, 0),
         povcount2017 = ifelse(pctpov2017 > .39999999, pop2010, 0))

povsumsOrleans <- Metro_pov_1970_2017 %>% 
  filter(str_detect(tolower(GEOID), pattern = "22071")) %>% 
  select(pop1970, pop1980, pop1990, pop2000, pop2010, pop2017, povcount1970, povcount1980, povcount1990, povcount2000, povcount2010, povcount2017) %>%  
  summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
  mutate(pctinvpoornbhd1970 = povcount1970 / pop1970,
         pctinvpoornbhd1980 = povcount1980 / pop1980,
         pctinvpoornbhd1990 = povcount1990 / pop1990,
         pctinvpoornbhd2000 = povcount2000 / pop2000,
         pctinvpoornbhd2010 = povcount2010 / pop2010,
         pctinvpoornbhd2017 = povcount2017 / pop2017)

povsums <- Metro_pov_1970_2017 %>% 
  select(pop1970, pop1980, pop1990, pop2000, pop2010, pop2017, povcount1970, povcount1980, povcount1990, povcount2000, povcount2010, povcount2017) %>%  
  summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
  mutate(pctinvpoornbhd1970 = povcount1970 / pop1970,
         pctinvpoornbhd1980 = povcount1980 / pop1980,
         pctinvpoornbhd1990 = povcount1990 / pop1990,
         pctinvpoornbhd2000 = povcount2000 / pop2000,
         pctinvpoornbhd2010 = povcount2010 / pop2010,
         pctinvpoornbhd2017 = povcount2017 / pop2017)


povsumsForChartOrleans <- povsumsOrleans %>% 
  select(pctinvpoornbhd1970,
         pctinvpoornbhd1980,
         pctinvpoornbhd1990,
         pctinvpoornbhd2000,
         pctinvpoornbhd2010,
         pctinvpoornbhd2017) %>% 
  gather(., key = "year", value = "pctinvpoornbhd" ) %>% 
  mutate(year.fac = NA,
         year.fac = ifelse(grepl("1970", year), "1970", year.fac),
         year.fac = ifelse(grepl("1980", year), "1980", year.fac),
         year.fac = ifelse(grepl("1990", year), "1990", year.fac),
         year.fac = ifelse(grepl("2000", year), "2000", year.fac),
         year.fac = ifelse(grepl("2010", year), "2010", year.fac),
         year.fac = ifelse(grepl("2017", year), "2017", year.fac)) %>% 
  mutate(year.fac = as.numeric(year.fac),
         pctinvpoornbhd = round(pctinvpoornbhd, 3)) %>% 
  mutate(Geography = "Orleans Parish")


povsumsForChartMetro <- povsums %>% 
  select(pctinvpoornbhd1970,
         pctinvpoornbhd1980,
         pctinvpoornbhd1990,
         pctinvpoornbhd2000,
         pctinvpoornbhd2010,
         pctinvpoornbhd2017) %>% 
  gather(., key = "year", value = "pctinvpoornbhd" ) %>% 
  mutate(year.fac = NA,
         year.fac = ifelse(grepl("1970", year), "1970", year.fac),
         year.fac = ifelse(grepl("1980", year), "1980", year.fac),
         year.fac = ifelse(grepl("1990", year), "1990", year.fac),
         year.fac = ifelse(grepl("2000", year), "2000", year.fac),
         year.fac = ifelse(grepl("2010", year), "2010", year.fac),
         year.fac = ifelse(grepl("2017", year), "2017", year.fac)) %>% 
  mutate(year.fac = as.numeric(year.fac),
         pctinvpoornbhd = round(pctinvpoornbhd, 3)) %>% 
  mutate(Geography = "Metro New Orleans")

povsumsForChart <- povsumsForChartMetro %>% 
  bind_rows(povsumsForChartOrleans) %>% 
  filter(year.fac != 2010)


















### Figure 8 

### Income Segregation analysis
### 1970

incomebuckets1970 <- incomebuckets1970raw %>% 
  filter(COUNTY %in% c(51, 71, 75, 87, 89, 93, 95, 103)) %>% 
  rename(totfams = "FAVINC7D",
         inc_less1 = "FALT17", 
         inc1_2 = "FALT27",
         inc2_3 = "FALT37",
         inc3_4 = "FALT47",
         inc4_5 = "FALT57",
         inc5_6 = "FALT67",
         inc6_7 = "FALT77",
         inc7_7.5 = "FALT87",
         inc8_9 = "FALT97",
         inc9_10 = "FALT107",
         inc10_12 = "FALT127",
         inc12_15 = "FALT157",
         inc15_25 = "FALT257",
         inc25_50 = "FALT507",
         inc_50plus = "FALTMX7") 

###Metro
##Need to find Median, affluent, and poor for entire metro

metroranked1970 <- incomebuckets1970 %>% 
  select(-GEO2010, -COUNTY) %>% 
  summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
  pivot_longer(-totfams, names_to = "bins", values_to = "pop") %>% 
  mutate(order  = 1,
         order = ifelse(bins == "inc_less1", 1, order), 
         order = ifelse(bins == "inc1_2", 2, order),
         order = ifelse(bins == "inc2_3", 3, order),
         order = ifelse(bins == "inc3_4", 4, order),
         order = ifelse(bins == "inc4_5", 5, order),
         order = ifelse(bins == "inc5_6", 6, order),
         order = ifelse(bins == "inc6_7", 7, order),
         order = ifelse(bins == "inc7_7.5", 8, order),
         order = ifelse(bins == "inc8_9", 9, order),
         order = ifelse(bins == "inc9_10", 10, order),
         order = ifelse(bins == "inc10_12", 11, order),
         order = ifelse(bins == "inc12_15", 12, order),
         order = ifelse(bins == "inc15_25", 13, order),
         order = ifelse(bins ==  "inc25_50", 14, order),
         order = ifelse(bins ==  "inc_50plus", 15, order)) %>% 
  arrange(order) %>% 
  mutate(cumsum = ave(pop, FUN = cumsum)) %>% 
  mutate(start = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 12000, 15000, 25000, 50000),
         end = c(999, 1999, 2999, 3999, 4999, 5999, 6999, 7499, 8999, 9999, 11999, 14999, 24999, 49999, 100000))  

medianpop1970 <- (sum(metroranked1970$pop)/2)

ID1970metromedian <- metroranked1970 %>% 
  mutate(IDmediangroup = medianpop1970 - cumsum) %>% 
  filter(IDmediangroup < 0) %>% 
  mutate(rank = rank(-IDmediangroup))  %>% 
  filter(rank == 1) %>% 
  mutate(medianpopbinfrac  = (medianpop1970 - (cumsum - pop)) / pop,
         medianincome = ((end - start)*medianpopbinfrac)+start,
         affluentinc = medianincome *1.5,
         poorinc = medianincome * .67)





## ID median by neighborhood


##Finding the number of half the families in each census tract, then joining to script below
halffams1970 <-incomebuckets1970 %>% 
  select(GEO2010, totfams) %>% 
  mutate(halffams = totfams /2)


ranked1970 <- incomebuckets1970 %>% 
  select(-COUNTY) %>%
  pivot_longer(-GEO2010, names_to = "bins", values_to = "pop") %>% 
  mutate(order  = 1,
         order = ifelse(bins == "inc_less1", 1, order), 
         order = ifelse(bins == "inc1_2", 2, order),
         order = ifelse(bins == "inc2_3", 3, order),
         order = ifelse(bins == "inc3_4", 4, order),
         order = ifelse(bins == "inc4_5", 5, order),
         order = ifelse(bins == "inc5_6", 6, order),
         order = ifelse(bins == "inc6_7", 7, order),
         order = ifelse(bins == "inc7_7.5", 8, order),
         order = ifelse(bins == "inc8_9", 9, order),
         order = ifelse(bins == "inc9_10", 10, order),
         order = ifelse(bins == "inc10_12", 11, order),
         order = ifelse(bins == "inc12_15", 12, order),
         order = ifelse(bins == "inc15_25", 13, order),
         order = ifelse(bins ==  "inc25_50", 14, order),
         order = ifelse(bins ==  "inc_50plus", 15, order)) %>% 
  arrange(GEO2010,order) %>% 
  group_by(GEO2010) %>% 
  filter(bins != "totfams") %>%
  mutate(cumsum = ave(pop, FUN = cumsum)) %>% 
  left_join(halffams1970, by = "GEO2010") %>% 
  mutate(IDmediangroup = halffams - cumsum) %>% 
  filter(IDmediangroup < 0) %>% 
  mutate(rank = rank(-IDmediangroup))  %>% 
  filter(rank == 1) %>% #filtering so only income bin median falls per tract 
  mutate(mediangroup = 1) %>% #variable to identify which group median falls within
  select(GEO2010, bins, mediangroup) #I am joining this below so I remove other variables



IDmedian1970 <-incomebuckets1970  %>% 
  select(-COUNTY) %>%
  pivot_longer(-GEO2010, names_to = "bins", values_to = "pop") %>%
  filter(bins != "totfams") %>% 
  mutate(start = 0,
         start = ifelse(bins == "inc_less1", 0, start), 
         start = ifelse(bins == "inc1_2", 1000, start),
         start = ifelse(bins == "inc2_3", 2000, start),
         start = ifelse(bins == "inc3_4", 3000, start),
         start = ifelse(bins == "inc4_5", 4000, start),
         start = ifelse(bins == "inc5_6", 5000, start),
         start = ifelse(bins == "inc6_7", 6000, start),
         start = ifelse(bins == "inc7_7.5", 7000, start),
         start = ifelse(bins == "inc8_9", 8000, start),
         start = ifelse(bins == "inc9_10", 9000, start),
         start = ifelse(bins == "inc10_12", 10000, start),
         start = ifelse(bins == "inc12_15", 12000, start),
         start = ifelse(bins == "inc15_25", 15000, start),
         start = ifelse(bins ==  "inc25_50", 25000, start),
         start = ifelse(bins ==  "inc_50plus", 50000, start),
         end = 999,
         end = ifelse(bins == "inc_less1", 999, end), 
         end = ifelse(bins == "inc1_2", 1999, end),
         end = ifelse(bins == "inc2_3", 2999, end),
         end = ifelse(bins == "inc3_4", 3999, end),
         end = ifelse(bins == "inc4_5", 4999, end),
         end = ifelse(bins == "inc5_6", 5999, end),
         end = ifelse(bins == "inc6_7", 6999, end),
         end = ifelse(bins == "inc7_7.5", 7499, end),
         end = ifelse(bins == "inc8_9", 8999, end),
         end = ifelse(bins == "inc9_10", 9999, end),
         end = ifelse(bins == "inc10_12", 11999, end),
         end = ifelse(bins == "inc12_15", 14999, end),
         end = ifelse(bins == "inc15_25", 24999, end),
         end = ifelse(bins ==  "inc25_50", 49999, end),
         end = ifelse(bins ==  "inc_50plus", 100000, end)) %>% 
  left_join(ranked1970, by =c("GEO2010", "bins")) %>% 
  mutate(order  = 1,
         order = ifelse(bins == "inc_less1", 1, order), 
         order = ifelse(bins == "inc1_2", 2, order),
         order = ifelse(bins == "inc2_3", 3, order),
         order = ifelse(bins == "inc3_4", 4, order),
         order = ifelse(bins == "inc4_5", 5, order),
         order = ifelse(bins == "inc5_6", 6, order),
         order = ifelse(bins == "inc6_7", 7, order),
         order = ifelse(bins == "inc7_7.5", 8, order),
         order = ifelse(bins == "inc8_9", 9, order),
         order = ifelse(bins == "inc9_10", 10, order),
         order = ifelse(bins == "inc10_12", 11, order),
         order = ifelse(bins == "inc12_15", 12, order),
         order = ifelse(bins == "inc15_25", 13, order),
         order = ifelse(bins ==  "inc25_50", 14, order),
         order = ifelse(bins ==  "inc_50plus", 15, order)) %>% 
  arrange(GEO2010,order) %>% 
  group_by(GEO2010) %>% 
  mutate(cumsum = ave(pop, FUN = cumsum)) %>% 
  left_join(halffams1970, by = "GEO2010") %>% 
  filter(mediangroup== 1) %>% #identifying median income per census tract
  mutate(medianpopbinfrac = (halffams - (cumsum - pop)) / pop,
         medianincome = ((end - start)*medianpopbinfrac)+start) %>% 
  mutate(affluent = 0,
         poor = 0,
         affluent = ifelse(medianincome >= ID1970metromedian$affluentinc, totfams, 0),
         poor = ifelse(medianincome <= ID1970metromedian$poorinc, totfams, 0)) %>% ##Getting a count of number of families 
  ungroup(GEO2010) %>% 
  select(totfams, affluent, poor) %>% 
  summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
  mutate(middle = totfams - (affluent + poor),
         affluentpct = affluent/totfams,
         middlepct = middle/totfams,
         poorpct = poor/totfams) %>% 
  mutate(year = 1970)


### Income Segregation analysis
### 1980

incomebuckets1980 <- incomebuckets1980raw %>% 
  filter(COUNTY %in% c(51, 71, 75, 87, 89, 93, 95, 103)) %>% 
  rename(totfams = "FAVINC8D",
         inc_less2.5 = "FALT38", 
         inc2.5_5    = "FALT58",
         inc5_7.5    =  "FALT88",
         inc7.5_10   = "FALT108",
         inc10_12.5  = "FALT138",
         inc12.5_15  = "FALT158",
         inc15_17.5  = "FALT188",
         inc17.5_20  = "FALT208",
         inc20_22.5  = "FALT238",
         inc22.5_25  = "FALT258",
         inc25_27.5  = "FALT288",
         inc27.5_30  = "FALT308",
         inc30_35    = "FALT358", #missing 35-40?
         inc35_40    = "FALT408",
         inc40_50    = "FALT498",
         inc50_75    = "FALT758",
         inc_75plus  = "FALTMX8") 


###Metro
##Need to find Median, affluent, and poor for entire metro

metroranked1980 <- incomebuckets1980 %>% 
  select(-GEO2010, -COUNTY) %>% 
  summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
  pivot_longer(-totfams, names_to = "bins", values_to = "pop") %>% 
  mutate(order  = 1,
         order = ifelse(bins ==  "inc_less2.5", 1, order), 
         order = ifelse(bins ==  "inc2.5_5", 2, order),
         order = ifelse(bins ==  "inc5_7.5", 3, order),
         order = ifelse(bins ==  "inc7.5_10", 4, order),
         order = ifelse(bins ==  "inc10_12.5", 5, order),
         order = ifelse(bins ==  "inc12.5_15", 6, order),
         order = ifelse(bins ==  "inc15_17.5", 7, order),
         order = ifelse(bins ==  "inc17.5_20", 8, order),
         order = ifelse(bins ==  "inc20_22.5", 9, order),
         order = ifelse(bins ==  "inc22.5_25", 10, order),
         order = ifelse(bins ==  "inc25_27.5", 11, order),
         order = ifelse(bins ==  "inc27.5_30", 12, order),
         order = ifelse(bins ==  "inc30_35", 13, order),
         order = ifelse(bins == "inc35_40", 14, order),
         order = ifelse(bins ==  "inc40_50", 15, order),
         order = ifelse(bins ==  "inc50_75", 16, order),
         order = ifelse(bins ==  "inc_75plus", 17, order)) %>% 
  arrange(order) %>% 
  mutate(cumsum = ave(pop, FUN = cumsum)) %>% 
  filter(bins != "HFLG175") %>% 
  mutate(start = c(0, 2500, 5000, 7500, 10000, 12500, 15000, 17500, 20000, 22500, 25000, 27500, 30000, 35000, 40000, 50000, 75000),
         end = c(2499, 4999, 7499, 9999, 12499, 14999, 17499, 19999, 22499, 24999, 27499, 29999, 34999, 39999, 49999, 74999, 100000))  

medianpop1980 <- (sum(metroranked1980$pop)/2)

ID1980metromedian <- metroranked1980 %>% 
  mutate(IDmediangroup = medianpop1980 - cumsum) %>% 
  filter(IDmediangroup < 0) %>% 
  mutate(rank = rank(-IDmediangroup))  %>% 
  filter(rank == 1) %>% 
  mutate(medianpopbinfrac  = (medianpop1980 - (cumsum - pop)) / pop,
         medianincome = ((end - start)*medianpopbinfrac)+start,
         affluentinc = medianincome *1.5,
         poorinc = medianincome * .67)





## ID median by neighborhood


##Finding the number of half the families in each census tract, then joining to script below
halffams1980 <-incomebuckets1980 %>% 
  select(GEO2010, totfams) %>% 
  mutate(halffams = totfams /2)


ranked1980 <- incomebuckets1980 %>% 
  select(-COUNTY) %>%
  pivot_longer(-GEO2010, names_to = "bins", values_to = "pop") %>% 
  mutate(order  = 1,
         order = ifelse(bins ==  "inc_less2.5", 1, order), 
         order = ifelse(bins ==  "inc2.5_5", 2, order),
         order = ifelse(bins ==  "inc5_7.5", 3, order),
         order = ifelse(bins ==  "inc7.5_10", 4, order),
         order = ifelse(bins ==  "inc10_12.5", 5, order),
         order = ifelse(bins ==  "inc12.5_15", 6, order),
         order = ifelse(bins ==  "inc15_17.5", 7, order),
         order = ifelse(bins ==  "inc17.5_20", 8, order),
         order = ifelse(bins ==  "inc20_22.5", 9, order),
         order = ifelse(bins ==  "inc22.5_25", 10, order),
         order = ifelse(bins ==  "inc25_27.5", 11, order),
         order = ifelse(bins ==  "inc27.5_30", 12, order),
         order = ifelse(bins ==  "inc30_35", 13, order),
         order = ifelse(bins == "inc35_40", 14, order),
         order = ifelse(bins ==  "inc40_50", 15, order),
         order = ifelse(bins ==  "inc50_75", 16, order),
         order = ifelse(bins ==  "inc_75plus", 17, order)) %>% 
  arrange(GEO2010,order) %>% 
  group_by(GEO2010) %>% 
  filter(bins != "totfams") %>%
  mutate(cumsum = ave(pop, FUN = cumsum)) %>% 
  left_join(halffams1980, by = "GEO2010") %>% 
  mutate(IDmediangroup = halffams - cumsum) %>% 
  filter(IDmediangroup < 0) %>% 
  mutate(rank = rank(-IDmediangroup))  %>% 
  filter(rank == 1) %>% #filtering so only income bin median falls per tract 
  mutate(mediangroup = 1) %>% #variable to identify which group median falls within
  select(GEO2010, bins, mediangroup) #I am joining this below so I remove other variables



IDmedian1980 <-incomebuckets1980  %>% 
  select(-COUNTY) %>%
  pivot_longer(-GEO2010, names_to = "bins", values_to = "pop") %>%
  filter(bins != "totfams") %>% 
  mutate(start = 0,
         start = ifelse(bins =="inc_less2.5",     0, start), 
         start = ifelse(bins =="inc2.5_5",       2500, start),
         start = ifelse(bins =="inc5_7.5",       5000, start),
         start = ifelse(bins =="inc7.5_10",      7500, start),
         start = ifelse(bins =="inc10_12.5",     10000, start),
         start = ifelse(bins =="inc12.5_15",     12500, start),
         start = ifelse(bins =="inc15_17.5",     15000, start),
         start = ifelse(bins =="inc17.5_20",     17500, start),
         start = ifelse(bins =="inc20_22.5",      20000, start),
         start = ifelse(bins =="inc22.5_25",      22500, start),
         start = ifelse(bins =="inc25_27.5",       25000, start),
         start = ifelse(bins =="inc27.5_30",       27500, start),
         start = ifelse(bins =="inc30_35",         30000, start),
         start = ifelse(bins == "inc35_40",         35000, start),
         start = ifelse(bins == "inc40_50",         40000, start),
         start = ifelse(bins =="inc50_75",          50000, start),
         start = ifelse(bins =="inc_75plus",         75000, start),
         end = 999,
         end = ifelse(bins == "inc_less2.5",          2499, end), 
         end = ifelse(bins == "inc2.5_5",            4999, end),
         end = ifelse(bins == "inc5_7.5",            7499, end),
         end = ifelse(bins == "inc7.5_10",           9999, end),
         end = ifelse(bins == "inc10_12.5",          12499, end),
         end = ifelse(bins == "inc12.5_15",          14999, end),
         end = ifelse(bins == "inc15_17.5",          17499, end),
         end = ifelse(bins == "inc17.5_20",          19999, end),
         end = ifelse(bins == "inc20_22.5",           22499, end),
         end = ifelse(bins == "inc22.5_25",          24999, end),
         end = ifelse(bins == "inc25_27.5",           27499, end),
         end = ifelse(bins == "inc27.5_30",           29999, end),
         end = ifelse(bins == "inc30_35",             34999, end),
         end = ifelse(bins == "inc35_40",         39999, end),
         end = ifelse(bins == "inc40_50",         49999, end),
         end = ifelse(bins ==  "inc50_75",              74999, end),
         end = ifelse(bins ==  "inc_75plus",            199999, end)) %>% 
  left_join(ranked1980, by =c("GEO2010", "bins")) %>% 
  mutate(order  = 1,
         order = ifelse(bins ==  "inc_less2.5", 1, order), 
         order = ifelse(bins ==  "inc2.5_5", 2, order),
         order = ifelse(bins ==  "inc5_7.5", 3, order),
         order = ifelse(bins ==  "inc7.5_10", 4, order),
         order = ifelse(bins ==  "inc10_12.5", 5, order),
         order = ifelse(bins ==  "inc12.5_15", 6, order),
         order = ifelse(bins ==  "inc15_17.5", 7, order),
         order = ifelse(bins ==  "inc17.5_20", 8, order),
         order = ifelse(bins ==  "inc20_22.5", 9, order),
         order = ifelse(bins ==  "inc22.5_25", 10, order),
         order = ifelse(bins ==  "inc25_27.5", 11, order),
         order = ifelse(bins ==  "inc27.5_30", 12, order),
         order = ifelse(bins ==  "inc30_35", 13, order),
         order = ifelse(bins == "inc35_40", 14, order),
         order = ifelse(bins ==  "inc40_50", 15, order),
         order = ifelse(bins ==  "inc50_75", 16, order),
         order = ifelse(bins ==  "inc_75plus", 17, order)) %>% 
  arrange(GEO2010,order) %>% 
  group_by(GEO2010) %>% 
  mutate(cumsum = ave(pop, FUN = cumsum)) %>% 
  left_join(halffams1980, by = "GEO2010") %>% 
  filter(mediangroup== 1) %>% #identifying median income per census tract
  mutate(medianpopbinfrac = (halffams - (cumsum - pop)) / pop,
         medianincome = ((end - start)*medianpopbinfrac)+start) %>% 
  mutate(affluent = 0,
         poor = 0,
         affluent = ifelse(medianincome >= ID1980metromedian$affluentinc, totfams, 0),
         poor = ifelse(medianincome <= ID1980metromedian$poorinc, totfams, 0)) %>% ##Getting a count of number of families 
  ungroup(GEO2010) %>% 
  select(totfams, affluent, poor) %>% 
  summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
  mutate(middle = totfams - (affluent + poor),
         affluentpct = affluent/totfams,
         middlepct = middle/totfams,
         poorpct = poor/totfams) %>% 
  mutate(year = 1980)








#Median family income 1990

### Income Segregation analysis
### 1990
incomebuckets1990 <- incomebuckets1990raw %>% 
  filter(COUNTY %in% c(51, 71, 75, 87, 89, 93, 95, 103)) %>% 
  rename(totfams = "FAVINC9D",
         medfaminc_tract = "MDFAMY9",
         inc_less5  = "FALTY59", 
         inc5_10    = "FALTY109",
         inc10_12.5 = "FALT139",
         inc12.5_15 = "FALT159",
         inc15_17.5 = "FALT189",
         inc17.5_20 = "FALT209",
         inc20_22.5 = "FALT239",
         inc22.5_25 = "FALT259",
         inc25_27.5 = "FALT289",
         inc27.5_30 = "FALT309",
         inc30_35   =   "FALT359",
         inc35_40   =   "FALT409",
         inc40_50   = "FALT499",
         inc50_60   = "FALT609A",
         inc60_75   = "FALT759A",
         inc75_100  = "FALT1009",
         inc100_125 = "FALT1259",
         inc125_150 = "FALT1509",
         inc_150plus = "FALTMXB9") 

###Metro
##Need to find Median, affluent, and poor for entire metro

metroranked1990 <- incomebuckets1990 %>% 
  select(-GEO2010, -COUNTY) %>% 
  summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
  pivot_longer(-totfams, names_to = "bins", values_to = "pop") %>% 
  mutate(order  = 1,
         order = ifelse(bins ==  "inc_less5", 1, order), 
         order = ifelse(bins ==  "inc5_10", 2, order),
         order = ifelse(bins ==  "inc10_12.5", 3, order),
         order = ifelse(bins ==  "inc12.5_15", 4, order),
         order = ifelse(bins ==  "inc15_17.5", 5, order),
         order = ifelse(bins ==  "inc17.5_20", 6, order),
         order = ifelse(bins ==  "inc20_22.5", 7, order),
         order = ifelse(bins ==  "inc22.5_25", 8, order),
         order = ifelse(bins ==  "inc25_27.5", 9, order),
         order = ifelse(bins ==  "inc27.5_30", 10, order),
         order = ifelse(bins ==  "inc30_35", 11, order),
         order = ifelse(bins ==  "inc35_40", 12, order),
         order = ifelse(bins ==  "inc40_50", 13, order),
         order = ifelse(bins ==  "inc50_60", 14, order),
         order = ifelse(bins ==  "inc60_75", 15, order),
         order = ifelse(bins ==  "inc75_100", 16, order),
         order = ifelse(bins ==  "inc100_125", 17, order),
         order = ifelse(bins ==  "inc125_150", 18, order),
         order = ifelse(bins ==  "inc_150plus", 19, order))  %>% 
  filter(bins != "FALTMXA9" & bins != "FALTMX9" & bins != "FALT759" & bins != "medfaminc_tract" & bins != "HFLG175") %>% 
  arrange(order) %>% 
  mutate(cumsum = ave(pop, FUN = cumsum)) %>% 
  mutate(start = c(0, 5000, 10000, 12500, 15000, 17500, 20000, 22500, 25000, 27500, 30000, 35000, 40000, 50000, 60000, 75000, 100000, 125000, 150000),
         end = c(4999, 9999, 12499, 14999, 17499, 19999, 22499, 24999, 27499, 29999, 34999, 39999, 49999, 59999, 74999, 99999, 124999, 149999, 200000))   

medianpop1990 <- (sum(metroranked1990$pop)/2)

ID1990metromedian <- metroranked1990 %>% 
  mutate(IDmediangroup = medianpop1990 - cumsum) %>% 
  filter(IDmediangroup < 0) %>% 
  mutate(rank = rank(-IDmediangroup))  %>% 
  filter(rank == 1) %>% 
  mutate(medianpopbinfrac  = (medianpop1990 - (cumsum - pop)) / pop,
         medianincome = ((end - start)*medianpopbinfrac)+start,
         affluentinc = medianincome *1.5,
         poorinc = medianincome * .67)





## ID median by neighborhood


##Finding the number of half the families in each census tract, then joining to script below
halffams1990 <-incomebuckets1990 %>% 
  select(GEO2010, totfams) %>% 
  mutate(halffams = totfams /2)


ranked1990 <- incomebuckets1990 %>% 
  select(-COUNTY) %>%
  pivot_longer(-GEO2010, names_to = "bins", values_to = "pop") %>% 
  mutate(order  = 1,
         order = ifelse(bins ==  "inc_less5",        1, order), 
         order = ifelse(bins ==  "inc5_10",        2, order),
         order = ifelse(bins ==  "inc10_12.5",        3, order),
         order = ifelse(bins ==  "inc12.5_15",        4, order),
         order = ifelse(bins ==  "inc15_17.5",        5, order),
         order = ifelse(bins ==  "inc17.5_20",        6, order),
         order = ifelse(bins ==  "inc20_22.5",        7, order),
         order = ifelse(bins ==  "inc22.5_25",        8, order),
         order = ifelse(bins ==  "inc25_27.5",        9, order),
         order = ifelse(bins ==  "inc27.5_30",        10, order),
         order = ifelse(bins ==  "inc30_35",           11, order),
         order = ifelse(bins ==  "inc35_40",           12, order),
         order = ifelse(bins ==  "inc40_50",           13, order),
         order = ifelse(bins ==  "inc50_60",           14, order),
         order = ifelse(bins ==  "inc60_75",           15, order),
         order = ifelse(bins ==  "inc75_100",         16, order),
         order = ifelse(bins ==  "inc100_125",        17, order),
         order = ifelse(bins ==  "inc125_150",       18, order),
         order = ifelse(bins ==  "inc_150plus",         19, order))  %>% 
  filter(bins != "FALTMXA9" & bins != "FALTMX9" & bins != "FALT759" & bins != "medfaminc_tract" & bins != "HFLG175") %>% 
  arrange(GEO2010,order) %>% 
  group_by(GEO2010) %>% 
  filter(bins != "totfams") %>%
  mutate(cumsum = ave(pop, FUN = cumsum)) %>% 
  left_join(halffams1990, by = "GEO2010") %>% 
  mutate(IDmediangroup = halffams - cumsum) %>% 
  filter(IDmediangroup < 0) %>% 
  mutate(rank = rank(-IDmediangroup))  %>% 
  filter(rank == 1) %>% #filtering so only income bin median falls per tract 
  mutate(mediangroup = 1) %>% #variable to identify which group median falls within
  select(GEO2010, bins, mediangroup) #I am joining this below so I remove other variables



IDmedian1990 <-incomebuckets1990  %>% 
  select(-COUNTY) %>%
  pivot_longer(-GEO2010, names_to = "bins", values_to = "pop") %>%
  filter(bins != "totfams") %>% 
  filter(bins != "FALTMXA9" & bins != "FALTMX9" & bins != "FALT759" & bins != "medfaminc_tract" & bins != "HFLG175") %>% 
  mutate(start = 0,
         start = ifelse(bins =="inc_less5",     0,start), 
         start = ifelse(bins =="inc5_10",       5000,  start),
         start = ifelse(bins =="inc10_12.5",    10000,     start),
         start = ifelse(bins =="inc12.5_15",    12500,start),
         start = ifelse(bins =="inc15_17.5",    15000, start),
         start = ifelse(bins =="inc17.5_20",    17500, start),
         start = ifelse(bins =="inc20_22.5",    20000, start),
         start = ifelse(bins =="inc22.5_25",    22500,  start),
         start = ifelse(bins =="inc25_27.5",    25000, start),
         start = ifelse(bins =="inc27.5_30",    27500,  start),
         start = ifelse(bins =="inc30_35",      30000,  start),
         start = ifelse(bins =="inc35_40",      35000,  start),
         start = ifelse(bins =="inc40_50",      40000,   start),
         start = ifelse(bins =="inc50_60",      50000,    start),
         start = ifelse(bins =="inc60_75",      60000,   start),
         start = ifelse(bins =="inc75_100",     75000,    start),
         start = ifelse(bins =="inc100_125",    100000,   start),
         start = ifelse(bins =="inc125_150",    125000,   start),
         start = ifelse(bins =="inc_150plus",   150000,    start),
         
         end = 999,
         end = ifelse(bins == "inc_less5",          4999,        end), 
         end = ifelse(bins == "inc5_10",             9999,       end),
         end = ifelse(bins == "inc10_12.5",          12499,       end),
         end = ifelse(bins == "inc12.5_15",          14999,      end),
         end = ifelse(bins == "inc15_17.5",          17499,        end),
         end = ifelse(bins == "inc17.5_20",          19999,       end),
         end = ifelse(bins == "inc20_22.5",          22499,        end),
         end = ifelse(bins == "inc22.5_25",          24999,       end),
         end = ifelse(bins == "inc25_27.5",          27499,      end),
         end = ifelse(bins == "inc27.5_30",          29999,       end),
         end = ifelse(bins == "inc30_35",            34999,        end),
         end = ifelse(bins == "inc35_40",            39999,       end),
         end = ifelse(bins == "inc40_50",            49999,       end),
         end = ifelse(bins == "inc50_60",            59999,        end),
         end = ifelse(bins == "inc60_75",            74999,       end),
         end = ifelse(bins == "inc75_100",           99999,        end),
         end = ifelse(bins == "inc100_125",          124999,        end),
         end = ifelse(bins == "inc125_150",          149999,        end),
         end = ifelse(bins == "inc_150plus",         200000,          end) ) %>% 
  left_join(ranked1990, by =c("GEO2010", "bins")) %>% 
  mutate(order  = 1,
         order = ifelse(bins ==  "inc_less5",        1, order), 
         order = ifelse(bins ==  "inc5_10",        2, order),
         order = ifelse(bins ==  "inc10_12.5",        3, order),
         order = ifelse(bins ==  "inc12.5_15",        4, order),
         order = ifelse(bins ==  "inc15_17.5",        5, order),
         order = ifelse(bins ==  "inc17.5_20",        6, order),
         order = ifelse(bins ==  "inc20_22.5",        7, order),
         order = ifelse(bins ==  "inc22.5_25",        8, order),
         order = ifelse(bins ==  "inc25_27.5",        9, order),
         order = ifelse(bins ==  "inc27.5_30",        10, order),
         order = ifelse(bins ==  "inc30_35",           11, order),
         order = ifelse(bins ==  "inc35_40",           12, order),
         order = ifelse(bins ==  "inc40_50",           13, order),
         order = ifelse(bins ==  "inc50_60",           14, order),
         order = ifelse(bins ==  "inc60_75",           15, order),
         order = ifelse(bins ==  "inc75_100",         16, order),
         order = ifelse(bins ==  "inc100_125",        17, order),
         order = ifelse(bins ==  "inc125_150",       18, order),
         order = ifelse(bins ==  "inc_150plus",         19, order))  %>% 
  arrange(GEO2010,order) %>% 
  group_by(GEO2010) %>% 
  mutate(cumsum = ave(pop, FUN = cumsum)) %>% 
  left_join(halffams1990, by = "GEO2010") %>% 
  filter(mediangroup== 1) %>% #identifying median income per census tract
  mutate(medianpopbinfrac = (halffams - (cumsum - pop)) / pop,
         medianincome = ((end - start)*medianpopbinfrac)+start) %>% 
  mutate(affluent = 0,
         poor = 0,
         affluent = ifelse(medianincome >= ID1990metromedian$affluentinc, totfams, 0),
         poor = ifelse(medianincome <= ID1990metromedian$poorinc, totfams, 0)) %>% ##Getting a count of number of families 
  ungroup(GEO2010) %>% 
  select(totfams, affluent, poor) %>% 
  summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
  mutate(middle = totfams - (affluent + poor),
         affluentpct = affluent/totfams,
         middlepct = middle/totfams,
         poorpct = poor/totfams) %>% 
  mutate(year = 1990)



#Median family income 2000

### Income Segregation analysis
### 2000

incomebuckets2000 <- incomebuckets2000raw %>% 
  filter(COUNTY %in% c(51, 71, 75, 87, 89, 93, 95, 103)) %>% 
  rename(totfams = "FAVINC0D",
         medfaminc_tract = "MDFAMY0",
         inc_less10 = "FAY0100", 
         inc10_15   = "FAY0150",
         inc15_20    = "FAY0200",
         inc20_25    = "FAY0250",
         inc25_30       = "FAY0300",
         inc30_35    = "FAY0350",
         inc35_40    = "FAY0400",
         inc40_45    = "FAY0450",
         inc45_50    = "FAY0500",
         inc50_60    = "FAY0600",
         inc60_75    = "FAY0750",
         inc75_100    = "FAY01000",
         inc100_125   = "FAY01250",
         inc125_150   = "FAY01500",
         inc150_200    = "FAY02000",
         inc_200PLUS = "FAY0M200") 

###Metro
##Need to find Median, affluent, and poor for entire metro
metroranked2000 <- incomebuckets2000 %>% 
  select(-GEO2010, -COUNTY) %>% 
  summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
  pivot_longer(-totfams, names_to = "bins", values_to = "pop") %>% 
  mutate(order  = 1,
         order = ifelse(bins ==  "inc_less10", 1, order), 
         order = ifelse(bins ==  "inc10_15", 2, order),
         order = ifelse(bins ==  "inc15_20", 3, order),
         order = ifelse(bins ==  "inc20_25", 4, order),
         order = ifelse(bins ==  "inc25_30", 5, order),
         order = ifelse(bins ==  "inc30_35", 6, order),
         order = ifelse(bins ==  "inc35_40", 7, order),
         order = ifelse(bins ==  "inc40_45", 8, order),
         order = ifelse(bins ==  "inc45_50", 9, order),
         order = ifelse(bins ==  "inc50_60", 10, order),
         order = ifelse(bins ==  "inc60_75", 11, order),
         order = ifelse(bins ==  "inc75_100", 12, order),
         order = ifelse(bins ==  "inc100_125", 13, order),
         order = ifelse(bins ==  "inc125_150", 14, order),
         order = ifelse(bins ==  "inc150_200", 15, order),
         order = ifelse(bins ==  "inc_200PLUS", 16, order))  %>% 
  filter(bins != "medfaminc_tract" & bins != "HFLG175") %>% 
  arrange(order) %>% 
  mutate(cumsum = ave(pop, FUN = cumsum)) %>% 
  mutate(start = c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 60000, 75000, 100000, 125000, 150000, 200000),
         end = c(9999, 14999, 19999, 24999, 29999, 34999, 39999, 44999, 49999, 59999, 74999, 99999, 124999, 149999, 199999,1000000))   

medianpop2000 <- (sum(metroranked2000$pop)/2)

ID2000metromedian <- metroranked2000 %>% 
  mutate(IDmediangroup = medianpop2000 - cumsum) %>% 
  filter(IDmediangroup < 0) %>% 
  mutate(rank = rank(-IDmediangroup))  %>% 
  filter(rank == 1) %>% 
  mutate(medianpopbinfrac  = (medianpop2000 - (cumsum - pop)) / pop,
         medianincome = ((end - start)*medianpopbinfrac)+start,
         affluentinc = medianincome *1.5,
         poorinc = medianincome * .67)





## ID median by neighborhood


##Finding the number of half the families in each census tract, then joining to script below
halffams2000 <-incomebuckets2000 %>% 
  select(GEO2010, totfams) %>% 
  mutate(halffams = totfams /2)

ranked2000 <- incomebuckets2000 %>% 
  select(-COUNTY) %>%
  pivot_longer(-GEO2010, names_to = "bins", values_to = "pop") %>% 
  mutate(order  = 1,
         order = ifelse(bins ==  "inc_less10",            1, order), 
         order = ifelse(bins ==  "inc10_15",              2, order),
         order = ifelse(bins ==  "inc15_20",              3, order),
         order = ifelse(bins ==  "inc20_25",              4, order),
         order = ifelse(bins ==  "inc25_30",              5, order),
         order = ifelse(bins ==  "inc30_35",              6, order),
         order = ifelse(bins ==  "inc35_40",              7, order),
         order = ifelse(bins ==  "inc40_45",              8, order),
         order = ifelse(bins ==  "inc45_50",              9, order),
         order = ifelse(bins ==  "inc50_60",              10, order),
         order = ifelse(bins ==  "inc60_75",              11, order),
         order = ifelse(bins ==  "inc75_100",             12, order),
         order = ifelse(bins ==  "inc100_125",            13, order),
         order = ifelse(bins ==  "inc125_150",            14, order),
         order = ifelse(bins ==  "inc150_200",            15, order),
         order = ifelse(bins ==  "inc_200PLUS",           16, order))  %>% 
  filter(bins != "medfaminc_tract" & bins != "HFLG175") %>% 
  arrange(GEO2010,order) %>% 
  group_by(GEO2010) %>% 
  filter(bins != "totfams") %>%
  mutate(cumsum = ave(pop, FUN = cumsum)) %>% 
  left_join(halffams2000, by = "GEO2010") %>% 
  mutate(IDmediangroup = halffams - cumsum) %>% 
  filter(IDmediangroup < 0) %>% 
  mutate(rank = rank(-IDmediangroup))  %>% 
  filter(rank == 1) %>% #filtering so only income bin median falls per tract 
  mutate(mediangroup = 1) %>% #variable to identify which group median falls within
  select(GEO2010, bins, mediangroup) #I am joining this below so I remove other variables



IDmedian2000 <-incomebuckets2000  %>% 
  select(-COUNTY) %>%
  pivot_longer(-GEO2010, names_to = "bins", values_to = "pop") %>%
  filter(bins != "totfams") %>% 
  filter(bins != "FALTMXA9" & bins != "FALTMX9" & bins != "FALT759" & bins != "medfaminc_tract" & bins != "HFLG175") %>% 
  mutate(start = 0,
         start = ifelse(bins =="inc_less10",    0,    start), 
         start = ifelse(bins =="inc10_15",      10000,     start),
         start = ifelse(bins =="inc15_20",      15000,     start),
         start = ifelse(bins =="inc20_25",      20000,    start),
         start = ifelse(bins =="inc25_30",      25000,    start),
         start = ifelse(bins =="inc30_35",      30000,  start),
         start = ifelse(bins =="inc35_40",      35000, start),
         start = ifelse(bins =="inc40_45",      40000,  start),
         start = ifelse(bins =="inc45_50",      45000,  start),
         start = ifelse(bins =="inc50_60",      50000,  start),
         start = ifelse(bins =="inc60_75",      60000,   start),
         start = ifelse(bins =="inc75_100",     75000,  start),
         start = ifelse(bins =="inc100_125",    100000,   start),
         start = ifelse(bins =="inc125_150",    125000,    start),
         start = ifelse(bins =="inc150_200",    150000,   start),
         start = ifelse(bins =="inc_200PLUS",   200000,     start),
         
         end = 999,
         end = ifelse(bins == "inc_less10",        9999,    end), 
         end = ifelse(bins == "inc10_15",          14999,    end),
         end = ifelse(bins == "inc15_20",          19999,     end),
         end = ifelse(bins == "inc20_25",          24999,     end),
         end = ifelse(bins == "inc25_30",          29999,      end),
         end = ifelse(bins == "inc30_35",          34999,      end),
         end = ifelse(bins == "inc35_40",          39999,      end),
         end = ifelse(bins == "inc40_45",          44999,     end),
         end = ifelse(bins == "inc45_50",          49999,    end),
         end = ifelse(bins == "inc50_60",          59999,      end),
         end = ifelse(bins == "inc60_75",          74999,      end),
         end = ifelse(bins == "inc75_100",         99999,     end),
         end = ifelse(bins == "inc100_125",        124999,     end),
         end = ifelse(bins == "inc125_150",        149999,      end),
         end = ifelse(bins == "inc150_200",        199999,     end),
         end = ifelse(bins == "inc_200PLUS",       1000000,       end)) %>% 
  left_join(ranked2000, by =c("GEO2010", "bins")) %>% 
  mutate(order  = 1,
         order = ifelse(bins ==  "inc_less5",        1, order), 
         order = ifelse(bins ==  "inc5_10",        2, order),
         order = ifelse(bins ==  "inc10_12.5",        3, order),
         order = ifelse(bins ==  "inc12.5_15",        4, order),
         order = ifelse(bins ==  "inc15_17.5",        5, order),
         order = ifelse(bins ==  "inc17.5_20",        6, order),
         order = ifelse(bins ==  "inc20_22.5",        7, order),
         order = ifelse(bins ==  "inc22.5_25",        8, order),
         order = ifelse(bins ==  "inc25_27.5",        9, order),
         order = ifelse(bins ==  "inc27.5_30",        10, order),
         order = ifelse(bins ==  "inc30_35",           11, order),
         order = ifelse(bins ==  "inc35_40",           12, order),
         order = ifelse(bins ==  "inc40_50",           13, order),
         order = ifelse(bins ==  "inc50_60",           14, order),
         order = ifelse(bins ==  "inc60_75",           15, order),
         order = ifelse(bins ==  "inc75_100",         16, order),
         order = ifelse(bins ==  "inc100_125",        17, order),
         order = ifelse(bins ==  "inc125_150",       18, order),
         order = ifelse(bins ==  "inc_150plus",         19, order))  %>% 
  arrange(GEO2010,order) %>% 
  group_by(GEO2010) %>% 
  mutate(cumsum = ave(pop, FUN = cumsum)) %>% 
  left_join(halffams2000, by = "GEO2010") %>% 
  filter(mediangroup== 1) %>% #identifying median income per census tract
  mutate(medianpopbinfrac = (halffams - (cumsum - pop)) / pop,
         medianincome = ((end - start)*medianpopbinfrac)+start) %>% 
  mutate(affluent = 0,
         poor = 0,
         affluent = ifelse(medianincome >= ID2000metromedian$affluentinc, totfams, 0),
         poor = ifelse(medianincome <= ID2000metromedian$poorinc, totfams, 0)) %>% ##Getting a count of number of families 
  ungroup(GEO2010) %>% 
  select(totfams, affluent, poor) %>% 
  summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
  mutate(middle = totfams - (affluent + poor),
         affluentpct = affluent/totfams,
         middlepct = middle/totfams,
         poorpct = poor/totfams) %>% 
  mutate(year = 2000)




#From ACS5 2017

### Income Segregation analysis
### 2017




###Metro
##Need to find Median, affluent, and poor for entire metro
metroranked2017 <- incomebuckets2017raw %>% 
  select(-state, -county, -tract) %>% 
  summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
  pivot_longer(-totfams, names_to = "bins", values_to = "pop") %>% 
  mutate(order  = 1,
         order = ifelse(bins ==  "inc_less10", 1, order), 
         order = ifelse(bins ==  "inc10_15", 2, order),
         order = ifelse(bins ==  "inc15_20", 3, order),
         order = ifelse(bins ==  "inc20_25", 4, order),
         order = ifelse(bins ==  "inc25_30", 5, order),
         order = ifelse(bins ==  "inc30_35", 6, order),
         order = ifelse(bins ==  "inc35_40", 7, order),
         order = ifelse(bins ==  "inc40_45", 8, order),
         order = ifelse(bins ==  "inc45_50", 9, order),
         order = ifelse(bins ==  "inc50_60", 10, order),
         order = ifelse(bins ==  "inc60_75", 11, order),
         order = ifelse(bins ==  "inc75_100", 12, order),
         order = ifelse(bins ==  "inc100_125", 13, order),
         order = ifelse(bins ==  "inc125_150", 14, order),
         order = ifelse(bins ==  "inc150_200", 15, order),
         order = ifelse(bins ==  "inc_200PLUS", 16, order))  %>% 
  filter(bins != "medfaminc_tract" & bins != "HFLG175") %>% 
  arrange(order) %>% 
  mutate(cumsum = ave(pop, FUN = cumsum)) %>% 
  mutate(start = c(0, 10000, 15000, 20170, 25000, 30000, 35000, 40000, 45000, 50000, 60000, 75000, 100000, 125000, 150000, 201700),
         end = c(9999, 14999, 19999, 24999, 29999, 34999, 39999, 44999, 49999, 59999, 74999, 99999, 124999, 149999, 199999,1000000))  

medianpop2017 <- (sum(metroranked2017$pop)/2)

ID2017metromedian <- metroranked2017 %>% 
  mutate(IDmediangroup = medianpop2017 - cumsum) %>% 
  filter(IDmediangroup < 0) %>% 
  mutate(rank = rank(-IDmediangroup))  %>% 
  filter(rank == 1) %>% 
  mutate(medianpopbinfrac  = (medianpop2017 - (cumsum - pop)) / pop,
         medianincome = ((end - start)*medianpopbinfrac)+start,
         affluentinc = medianincome *1.5,
         poorinc = medianincome * .67)





## ID median by neighborhood


##Finding the number of half the families in each census tract, then joining to script below
halffams2017 <-incomebuckets2017 %>% 
  mutate(GEO2010 = paste(state, county, tract, sep="")) %>% 
  select(GEO2010, totfams) %>% 
  mutate(halffams = totfams /2)

metroranked2017 <- incomebuckets2017raw %>% 
  mutate(GEO2010 = paste(state, county, tract, sep="")) %>% 
  select(-state, -county, -tract) %>% 
  pivot_longer(-GEO2010, names_to = "bins", values_to = "pop") %>% 
  mutate(order  = 1,
         order = ifelse(bins ==  "inc_less10", 1, order), 
         order = ifelse(bins ==  "inc10_15", 2, order),
         order = ifelse(bins ==  "inc15_20", 3, order),
         order = ifelse(bins ==  "inc20_25", 4, order),
         order = ifelse(bins ==  "inc25_30", 5, order),
         order = ifelse(bins ==  "inc30_35", 6, order),
         order = ifelse(bins ==  "inc35_40", 7, order),
         order = ifelse(bins ==  "inc40_45", 8, order),
         order = ifelse(bins ==  "inc45_50", 9, order),
         order = ifelse(bins ==  "inc50_60", 10, order),
         order = ifelse(bins ==  "inc60_75", 11, order),
         order = ifelse(bins ==  "inc75_100", 12, order),
         order = ifelse(bins ==  "inc100_125", 13, order),
         order = ifelse(bins ==  "inc125_150", 14, order),
         order = ifelse(bins ==  "inc150_200", 15, order),
         order = ifelse(bins ==  "inc_200PLUS", 16, order))  %>% 
  filter(bins != "medfaminc_tract" & bins != "HFLG175") %>% 
  arrange(GEO2010,order) %>% 
  group_by(GEO2010) %>% 
  filter(bins != "totfams") %>%
  mutate(cumsum = ave(pop, FUN = cumsum)) %>% 
  left_join(halffams2017, by = "GEO2010") %>% 
  mutate(IDmediangroup = halffams - cumsum) %>% 
  filter(IDmediangroup < 0) %>% 
  mutate(rank = rank(-IDmediangroup))  %>% 
  filter(rank == 1) %>% #filtering so only income bin median falls per tract 
  mutate(mediangroup = 1) %>% #variable to identify which group median falls within
  select(GEO2010, bins, mediangroup) #I am joining this below so I remove other variables



IDmedian2017 <-incomebuckets2017  %>% 
  mutate(GEO2010 = paste(state, county, tract, sep="")) %>% 
  select(-state, -county, -tract) %>% 
  pivot_longer(-GEO2010, names_to = "bins", values_to = "pop") %>%
  filter(bins != "totfams") %>% 
  filter(bins != "FALTMXA9" & bins != "FALTMX9" & bins != "FALT759" & bins != "medfaminc_tract" & bins != "HFLG175") %>% 
  mutate(start = 0,
         start = ifelse(bins =="inc_less10",    0,    start), 
         start = ifelse(bins =="inc10_15",      10000,     start),
         start = ifelse(bins =="inc15_20",      15000,     start),
         start = ifelse(bins =="inc20_25",      20170,    start),
         start = ifelse(bins =="inc25_30",      25000,    start),
         start = ifelse(bins =="inc30_35",      30000,  start),
         start = ifelse(bins =="inc35_40",      35000, start),
         start = ifelse(bins =="inc40_45",      40000,  start),
         start = ifelse(bins =="inc45_50",      45000,  start),
         start = ifelse(bins =="inc50_60",      50000,  start),
         start = ifelse(bins =="inc60_75",      60000,   start),
         start = ifelse(bins =="inc75_100",     75000,  start),
         start = ifelse(bins =="inc100_125",    100000,   start),
         start = ifelse(bins =="inc125_150",    125000,    start),
         start = ifelse(bins =="inc150_200",    150000,   start),
         start = ifelse(bins =="inc_200PLUS",   201700,     start),
         
         end = 999,
         end = ifelse(bins == "inc_less10",        9999,    end), 
         end = ifelse(bins == "inc10_15",          14999,    end),
         end = ifelse(bins == "inc15_20",          19999,     end),
         end = ifelse(bins == "inc20_25",          24999,     end),
         end = ifelse(bins == "inc25_30",          29999,      end),
         end = ifelse(bins == "inc30_35",          34999,      end),
         end = ifelse(bins == "inc35_40",          39999,      end),
         end = ifelse(bins == "inc40_45",          44999,     end),
         end = ifelse(bins == "inc45_50",          49999,    end),
         end = ifelse(bins == "inc50_60",          59999,      end),
         end = ifelse(bins == "inc60_75",          74999,      end),
         end = ifelse(bins == "inc75_100",         99999,     end),
         end = ifelse(bins == "inc100_125",        124999,     end),
         end = ifelse(bins == "inc125_150",        149999,      end),
         end = ifelse(bins == "inc150_200",        199999,     end),
         end = ifelse(bins == "inc_200PLUS",       1000000,       end)) %>% 
  left_join(metroranked2017, by =c("GEO2010", "bins")) %>% 
  mutate(order  = 1,
         order = ifelse(bins ==  "inc_less10",       1, order), 
         order = ifelse(bins ==  "inc10_15",       2, order),
         order = ifelse(bins ==  "inc15_20",          3, order),
         order = ifelse(bins ==  "inc20_25",          4, order),
         order = ifelse(bins ==  "inc25_30",          5, order),
         order = ifelse(bins ==  "inc30_35",          6, order),
         order = ifelse(bins ==  "inc35_40",          7, order),
         order = ifelse(bins ==  "inc40_45",          8, order),
         order = ifelse(bins ==  "inc45_50",          9, order),
         order = ifelse(bins ==  "inc50_60",          10, order),
         order = ifelse(bins ==  "inc60_75",           11, order),
         order = ifelse(bins ==  "inc75_100",          12, order),
         order = ifelse(bins ==  "inc100_125",         13, order),
         order = ifelse(bins ==  "inc125_150",         14, order),
         order = ifelse(bins ==  "inc150_200",         15, order),
         order = ifelse(bins ==  "inc_200PLUS",       16, order))  %>% 
  arrange(GEO2010,order) %>% 
  group_by(GEO2010) %>% 
  mutate(cumsum = ave(pop, FUN = cumsum)) %>% 
  left_join(halffams2017, by = "GEO2010") %>% 
  filter(mediangroup== 1) %>% #identifying median income per census tract
  mutate(medianpopbinfrac = (halffams - (cumsum - pop)) / pop,
         medianincome = ((end - start)*medianpopbinfrac)+start) %>% 
  mutate(affluent = 0,
         poor = 0,
         affluent = ifelse(medianincome >= ID2017metromedian$affluentinc, totfams, 0),
         poor = ifelse(medianincome <= ID2017metromedian$poorinc, totfams, 0)) %>% ##Getting a count of number of families 
  ungroup(GEO2010) %>% 
  select(totfams, affluent, poor) %>% 
  summarize_if(is.numeric, sum, na.rm=TRUE) %>% 
  mutate(middle = totfams - (affluent + poor),
         affluentpct = affluent/totfams,
         middlepct = middle/totfams,
         poorpct = poor/totfams) %>% 
  mutate(year = 2017)




affluentpoor_long <- IDmedian1970 %>% 
  bind_rows(IDmedian1980, IDmedian1990, IDmedian2000, IDmedian2017) %>% 
  pivot_longer(-year, names_to = "income", values_to = "percent" ) %>% 
  mutate(income.fac = income,
         income.fac = ifelse(income == "affluentpct", "High-Income (>150% of Metro Median)", income.fac),
         income.fac = ifelse(income == "middlepct", "Middle-Income (67-150% Metro Median)", income.fac),
         income.fac = ifelse(income == "poorpct", "Low-Income (<67% Metro Median)", income.fac)) %>% 
  filter(income == "affluentpct" | income == "middlepct" | income == "poorpct")
