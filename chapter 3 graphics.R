#### Analysis of Opportunity Atlas and USALEEP

load(here("Ch3OpAtlas_data.Rdata"))


## map of income mobility by census tract

# Find the percentile ranges
tract.incomeMobility.pctile <- OpAtlas.tract_outcomes_simple_LE.GNO %>% 
  mutate(pctile = ntile(kfr_pooled_pooled_p25, 5)) %>% 
  select(pctile, kfr_pooled_pooled_p25) %>% 
  group_by(pctile) %>% 
  summarise(range.min = min(kfr_pooled_pooled_p25, na.rm = TRUE),
            range.max = max(kfr_pooled_pooled_p25, na.rm = TRUE)) %>% 
  mutate(pctile.range = paste0(round(100*range.min), " - ", (round(100*range.max))),
         pctile.range = ifelse(is.na(pctile), "Missing", pctile.range)) %>% 
  select(pctile, pctile.range)

# make map, with custom legend
tracts.la %>%
  mutate(GEOID = as.character(GEOID10)) %>%
  right_join(OpAtlas.tract_outcomes_simple_LE.GNO) %>% 
  mutate(incomeMob.pctile = ntile(kfr_pooled_pooled_p25,5)) %>% 
  left_join(tract.incomeMobility.pctile, by=c("incomeMob.pctile"="pctile")) %>% 
  mutate(incomeMob.pctile.fac = factor(incomeMob.pctile, levels = 1:5, ordered = T)) %>% 
  ggplot() + 
  geom_sf(aes(fill = incomeMob.pctile.fac), color = "gray70", size = .1) +
  scale_fill_manual(values = brewer.pal(6, "GnBu"),  na.value = "gray70", 
                    labels = c("22 - 32", "33 - 36", "36 - 41", "41 - 46", "46 - 70", "Missing"),
                    name = "Adult income percentile for children from low-income households (25th percentile)"
  )+ 
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  #scale_fill_manual(values = col.metroLE,  na.value = "gray70", labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing"))+ 
  #scale_fill_paletteer_d("LaCroixColoR::Lemon", na.value = "gray70",labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing")) + # "Redmonder::dPBlPuGn" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  #scale_fill_brewer(palette = "PRGn", direction = 1,  na.value = "gray70") +
  #scale_fill_viridis_c(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  coord_sf(xlim = c(-90.3, -89.85), ylim = c(29.85, 30.1), expand = FALSE) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(.3,"cm"),
        strip.text = element_text(hjust = 0)) + 
  guides(fill=guide_legend(nrow = 1, title.position = "top")) +
  labs(fill = "") +
  ggsave(filename = "outputs/graphics PDFs/ch3_metro.tract.incomeMobility.pdf", device = cairo_pdf, width = 8, height = 5)


tracts.la %>%
  mutate(GEOID = as.character(GEOID10)) %>%
  right_join(OpAtlas.tract_outcomes_simple_LE.GNO) %>% 
  mutate(incomeMob.pctile = ntile(kfr_pooled_pooled_p25,5)) %>% 
  left_join(tract.incomeMobility.pctile, by=c("incomeMob.pctile"="pctile")) %>% 
  mutate(incomeMob.pctile.fac = factor(incomeMob.pctile, levels = 1:5, ordered = T)) %>% 
  select(GEOID, incomeMob.pctile.fac, pctile.range, LE= LE_x, STATEFP10, COUNTYFP10, kfr_pooled_pooled_p25) %>% 
  st_drop_geometry() %>% 
  write.csv(file = here("outputs/CSVs/ch3_metro.tract.incomeMobility.csv"))



## Quintile plot of income mobility

incomeMob.raceQuantile.pctile <- 
  OpAtlas.tract_outcomes_simple_LE.GNO %>% 
  mutate(quantile = ntile(kfr_pooled_pooled_p25, 5),
         quantile = factor(quantile, levels = c(as.character(1:10)))) %>% 
  group_by(quantile) %>% 
  summarise(Black = mean(kfr_black_pooled_p25, na.rm = TRUE),
            White = mean(kfr_white_pooled_p25, na.rm = TRUE)) %>% 
  gather(-quantile, key = variable, value = average) %>% 
  filter(!is.na(quantile)) %>% 
  mutate(average.round = round(100*average, 0)) %>% 
  left_join(Op.atlas.pctinccrosswalk, by=c("average.round"="percentile")) %>% 
  mutate(average = 100*average) %>% 
  ggplot(aes(quantile, average, group = variable)) + 
  geom_point(aes(color = variable)) + 
  geom_line(aes(color = variable)) + 
  geom_hline(yintercept = 25) +
  #geom_hline(yintercept = 27000) +
  annotate("text", x = 5, y = 27, label = "Childhood income (25th percentile)", hjust = 1, family = "Asap") + 
  geom_dl(aes(label = variable), method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8, fontfamily = "Asap")) + 
  scale_color_manual(values = c("Black" = "#002F45", "White" = "#9EB23B"))+
  scale_x_discrete(labels = c("0-20th","20-40th","40-60th","60-80th","80-100th"))+
  scale_y_continuous(limits = c(0,75)) +
  themeDC_horizontal() +
  theme(legend.position = "none",
        strip.text.y = element_text(hjust=0, angle = 180),
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(1,1,1,1), "lines")) + 
  labs(y= "Average adult income percentile fow low-income children",
       x = "Neighborhood income mobility percentile")

ggsave(removeDirectLabelClipping(incomeMob.raceQuantile.pctile), filename = "outputs/graphics PDFS/ch3_incomeMob.raceDecile.pctile.pdf", device = cairo_pdf,
       width = 8, height = 7, units = "in")

OpAtlas.tract_outcomes_simple_LE.GNO %>% 
  mutate(quantile = ntile(kfr_pooled_pooled_p25, 5),
         quantile = factor(quantile, levels = c(as.character(1:10)))) %>% 
  group_by(quantile) %>% 
  summarise(Black = mean(kfr_black_pooled_p25, na.rm = TRUE),
            White = mean(kfr_white_pooled_p25, na.rm = TRUE)) %>% 
  gather(-quantile, key = variable, value = average) %>% 
  filter(!is.na(quantile)) %>% 
  mutate(average.round = round(100*average, 0)) %>% 
  left_join(Op.atlas.pctinccrosswalk, by=c("average.round"="percentile")) %>% 
  mutate(average = 100*average) %>% 
  select(quantile, variable, average, average.round) %>% 
  write.csv(file = here("outputs/CSVs/ch3.incomeMob.raceQuantile.pctile.csv"))






## Scatterplot of income mobility and life expectancy

incomeMobLERace.scatter <- OpAtlas.tract_outcomes_simple_LE.GNO %>% 
  left_join(OpAtlas.tract_covariates, by="GEOID") %>% 
  left_join(acs.NOmsa,by="GEOID") %>% 
  ggplot(aes(100*kfr_pooled_pooled_p25, LE_x)) + 
  geom_point(aes(color = ACS.pctBlack)) + 
  scale_color_distiller(palette = "Spectral", guide = "legend", direction = 1,
                        name = "Black %",
                        labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(name = "Life expectancy") +
  scale_x_continuous(limits = c(20,75), name = "Adult income percentile") +
  themeDC_scatter() + 
  theme(legend.position = "right",
        #axis.line.y = element_line(),
        #axis.line.x = element_line(),
        #panel.grid = element_blank(),
        legend.direction = "vertical") 

ggsave(filename = "outputs/graphics PDFs/incomeMobLERace.scatter.pdf", device = cairo_pdf, width = 8, height = 5)


OpAtlas.tract_outcomes_simple_LE.GNO %>% 
  left_join(OpAtlas.tract_covariates, by="GEOID") %>% 
  left_join(acs.NOmsa,by="GEOID") %>% 
  select(GEOID, kfr_pooled_pooled_p25, ACS.pctBlack, LE = LE_x) %>% 
  write.csv(file = here("outputs/CSVs/ch3.incomeMobLERace.scatter.csv"))


#### Analysis of rent, life expectancy, and affordability

rentNincLA <- get_acs(geography = "tract",
                      year = 2015,
                      variables = c("B19013_001E","B25056_002E","B25065_001E","B25056_002M","B25065_001M","B19025_001E","B19001_001E","B19025_001M","B19001_001M"), 
                      state = c("LA"),
                      survey = "acs5")

RILdata <- rentNincLA %>%
  dplyr::select(-NAME, -moe) %>%
  spread( key=variable, value=estimate) %>%
  magrittr::set_colnames(c("GEOID","totI","medInc", "agg", "totR","grossrent")) %>%
  mutate(rent = grossrent/totR,
         avgInc = agg/totI
  ) %>%
  left_join((LEusbLA%>%filter(ageGrp=="Under 1")), by = "GEOID") %>%
  na.omit()%>%
  filter(str_sub(GEOID,1,2) == "22" & str_sub(GEOID,3,5) %in% GNO.parishfips)



quintRILdata  <- as.data.frame(cut(as.numeric(RILdata$LE_x), breaks = quantile(as.numeric(RILdata$LE_x), probs = seq(0, 1, 0.2)), 
                                   include.lowest = TRUE, labels = 1:5))

RILdata.agg.quint <- RILdata  %>%
  bind_cols(quintRILdata) %>%
  rename(quintile = "cut(as.numeric(RILdata$LE_x), breaks = quantile(as.numeric(RILdata$LE_x), probs = seq(0, 1, 0.2)), include.lowest = TRUE, labels = 1:5)") %>%
  left_join(popLA, by="GEOID")%>%
  dplyr::select(GEOID,medInc,totI, agg, totR, grossrent,LE_x, popPooled, quintile) %>%
  group_by(quintile) %>%
  mutate(weightedLE = LE_x*popPooled) %>%
  summarise(weightedSumLE = sum(weightedLE),
            popSum = sum(popPooled),
            sumInc = sum(agg),
            sumIncPop = sum(totI),
            sumRent = sum(grossrent),
            sumRentPop = sum(totR),
            avgMedInc = mean(medInc)
  ) %>%
  mutate(weightedAvgLE = weightedSumLE / popSum,
         weightedAvgInc = sumInc / sumIncPop,
         weightedAvgInc.monthly = (sumInc / sumIncPop)/12,
         weightedMedInc.monthly = avgMedInc/12,
         weightedAvgRent = sumRent/ sumRentPop,
         avgAff = (weightedAvgInc/12)*.3,
         medAff =  (avgMedInc/12)*.3
  )

quint.an <- RILdata.agg.quint %>%
  gather(-quintile,key=var, value=val) %>%
  filter(var %in% c( "weightedAvgLE" , "avgMedInc","medAff", "weightedAvgInc" , "weightedAvgRent","aff", "weightedMedInc.monthly", "weightedAvgInc.monthly")) %>%
  spread(key=quintile, value = val) %>%
  mutate(relineq8020 = ((`5`/`1`)-1) * 100,
         absineq8020 = `5`-`1`)

fig_metro.inc.rentcsv <- RILdata.agg.quint %>%
  gather(-quintile, key = var, value = val) %>%
  filter(var %in% c("medAff", "weightedAvgRent")) 
write.csv(fig_metro.inc.rentcsv, file = "outputs/CSVs/fig_metroincrent.csv")

fig_metro.inc.rent<- RILdata.agg.quint %>%
  gather(-quintile, key = var, value = val) %>%
  filter(var %in% c("medAff", "weightedAvgRent")) %>%
  ggplot(aes(x=as.factor(quintile),y=val, group = var, color=var))+
  geom_line(size=1)+
  geom_point()+
  scale_color_manual(values = c(DCcolor.p1skyblue,DCcolor.p2teal),
                     labels = c("Affordable housing costs", "Actual housing costs")) +
  scale_x_discrete(labels = c("0-20th\n(70.6)","20-40th\n(73.8)","40-60th\n(76.1)","60-80th\n(78.4)","80-100th\n(81.3)"))+
  scale_y_continuous(labels = dollar_format(accuracy = 1))+
  themeDC_scatter() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(.3,"cm"),
        strip.text = element_text(hjust = 0)) + guides(fill = guide_legend(nrow = 1))+
  labs(x="Life expectancy percentile\n(Life expectancy)",
       y="")+
  ggsave(filename = "outputs/graphics PDFs/fig_metro.inc.rent.pdf", device = cairo_pdf, width = 8, height = 5)

### New Orleans
RILdata.NO <- rentNincLA %>%
  dplyr::select(-NAME, -moe) %>%
  spread( key=variable, value=estimate) %>%
  magrittr::set_colnames(c("GEOID","totI","medInc", "agg", "totR","grossrent")) %>%
  mutate(rent = grossrent/totR,
         avgInc = agg/totI
  ) %>%
  left_join((LEusbLA%>%filter(ageGrp=="Under 1")), by = "GEOID") %>%
  na.omit()%>%
  filter(str_sub(GEOID,1,5) == "22071")



quintRILdata.NO  <- as.data.frame(cut(as.numeric(RILdata.NO$LE_x), breaks = quantile(as.numeric(RILdata.NO$LE_x), probs = seq(0, 1, 0.2)), 
                                      include.lowest = TRUE, labels = 1:5))

RILdata.agg.quint.NO <- RILdata.NO  %>%
  bind_cols(quintRILdata.NO) %>%
  rename(quintile = "cut(as.numeric(RILdata.NO$LE_x), breaks = quantile(as.numeric(RILdata.NO$LE_x), probs = seq(0, 1, 0.2)), include.lowest = TRUE, labels = 1:5)") %>%
  left_join(popLA, by="GEOID")%>%
  dplyr::select(GEOID,medInc,totI, agg, totR, grossrent,LE_x, popPooled, quintile) %>%
  group_by(quintile) %>%
  mutate(weightedLE = LE_x*popPooled) %>%
  summarise(weightedSumLE = sum(weightedLE),
            popSum = sum(popPooled),
            sumInc = sum(agg),
            sumIncPop = sum(totI),
            sumRent = sum(grossrent),
            sumRentPop = sum(totR),
            avgMedInc = mean(medInc)
  ) %>%
  mutate(weightedAvgLE = weightedSumLE / popSum,
         weightedAvgInc = sumInc / sumIncPop,
         weightedAvgInc.monthly = (sumInc / sumIncPop)/12,
         weightedMedInc.monthly = avgMedInc/12,
         weightedAvgRent = sumRent/ sumRentPop,
         avgAff = (weightedAvgInc/12)*.3,
         medAff =  (avgMedInc/12)*.3
  )

quint.an.NO <- RILdata.agg.quint.NO %>%
  gather(-quintile,key=var, value=val) %>%
  filter(var %in% c( "weightedAvgLE" , "avgMedInc","medAff", "weightedAvgInc" , "weightedAvgRent","aff", "weightedMedInc.monthly", "weightedAvgInc.monthly")) %>%
  spread(key=quintile, value = val) %>%
  mutate(relineq8020 = ((`5`/`1`)-1) * 100,
         absineq8020 = `5`-`1`)


fig_NO.inc.rent <- RILdata.agg.quint.NO %>%
  gather(-quintile, key = var, value = val) %>%
  filter(var %in% c("medAff", "weightedAvgRent")) %>%
  ggplot(aes(x=as.factor(quintile),y=val, group = var, color=var))+
  geom_line(size=1)+
  geom_point()+
  scale_color_manual(values = c(DCcolor.p1skyblue,DCcolor.p2teal),
                     labels = c("Affordable housing costs", "Actual housing costs")) +
  scale_x_discrete(labels = c("0-20th\n(69.3)","20-40th\n(72.9)","40-60th\n(75.2)","60-80th\n(78.1)","80-100th\n(82.2)"))+
  scale_y_continuous(labels = dollar_format(accuracy = 1))+
  themeDC_scatter() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(.3,"cm"),
        strip.text = element_text(hjust = 0)) + guides(fill = guide_legend(nrow = 1))+
  labs(x="Life expectancy percentile\n(Avg. life expectancy)",
       y="")+
  ggsave(filename = "outputs/graphics PDFs/fig_NO.inc.rent.pdf", device = cairo_pdf, width = 8, height = 5)

