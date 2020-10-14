

#### Figure 3
### Life Expectancy by country over time
#Figure1.3:Lifeexpectancyinselectedcountries,1950-2019(source:World Bank)

LEUS <- LECountry %>% 
  filter(Entity == "United States") %>% 
  filter(Year > 1950) %>% 
  filter(Year <2018)

fig3_countries.line <- LECountry %>% 
  filter(Entity %in% c("United States", "United Kingdom", "Japan",  "Spain", "Brazil", "France", "Chile")) %>% 
  mutate(Year = as.numeric(Year),
         Code = as.character(Code)) %>% 
  filter(Year >1950) %>% 
  filter(Year <2018) %>% 
  ggplot(., aes(x=Year, y=Life.expectancy..years., group = Entity, color = Entity )) +
  geom_line(aes(color = Entity), size =1) +
  geom_line(data = LEUS, aes(x=Year, y = Life.expectancy..years.), color = "#EF812C", size =2) +
  scale_color_manual(values = c("Japan" = "#595959", "Spain" = "#999999", "United Kingdom" = "#b2b2b2",
                                "Brazil" = "#8c8c8c" , "France" = "#4c4c4c", "Chile" = "#808080", "United States" = "#EF812C" ))+
  # scale_color_manual(values = c("Japan" = "#8c8c8c", "Spain" = "#999999", "Sweden" = "#a6a6a6", "United Kingdom" = "#b2b2b2", "United States" = "#EF812C",
  #                              "Russia" = "#737373", "India" = "#666666", "Brazil" = "#595959", "France" = "#4c4c4c", "China" = "#333333", "Chile" = "#808080", "Isreal" = "#d8d8d8" ))+
  themeDC_horizontal()+ 
  #  geom_text(data = subset(LECountry_select, Year == 2018), aes(label = Entity, x = Year, y = Life.expectancy..years., size =1), vjust=-.45, hjust=-.1)+
  geom_dl(aes(label = Entity, size =10), method = list(dl.trans(x = x + .2), "last.bumpup", cex = .8, fontfamily = "Asap")) +
  #  geom_dl(aes(label = Entity), method = list(dl.trans(x = x + -.2), "first.bumpup", cex = .5, fontfamily = "Asap")) +
  scale_x_continuous(breaks=c(1950,1960,1970,1980,1990,2000,2010,2017), expand = c(0, 3),  limits=c(1948,2028)) +
  ylim(50, 88)+
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.key = element_blank(), legend.position = "none") +
  # theme(axis.text.x = element_text(color="black", 
  #                                 size=12),
  #     axis.text.y = element_text( color="black", 
  #                               size=12))+
  labs(y = "Life Expectancy", x = "Year")


ggsave(removeDirectLabelClipping(fig3_countries.line), filename = "outputs/graphics PDFS/fig3_countries.line.pdf", device = cairo_pdf,
       width = 8, height = 5, units = "in")

write.csv(LECountry, file = "outputs/CSVs/fig3_countries.line.csv")



### Figure 4 & 5
### Deaths by despair by cause of death charts
#Figure1.5:Deathsper 100,000fromchronic diseases(source:CDC Wonder)

### Decline of chronic diseases over time

DD$COD.fac = factor(DD$COD, levels=c('Cancer', 'Heart Disease', 'HIV', 'Drug Poisoning', "Alcoholic Liver Disease", "Suicide"))

DDFacet_disease_filter2 <- DD %>% 
  filter(COD %in% c('Heart Disease', 'Cancer', 'HIV')) %>% 
  filter(Year %in% c("1999-2001", "2017-2018", "2008-2010")) %>% 
  mutate(Year = ifelse(Year == "1999-2001", "2000", Year),
         Year = ifelse(Year == "2017-2018", "2018", Year))


fig4_DD.chronic <- DD %>% 
  filter(COD %in% c('Heart Disease', 'Cancer', 'HIV')) %>% 
  # filter(Year %in% c("1999-2001", "2017-2018")) %>% 
  mutate(Year = ifelse(Year == "1999-2001", "2000", Year),
         Year = ifelse(Year == "2017-2018", "2018", Year),
         Year = ifelse(Year == "2008-2010", "2010", Year)) %>% 
  ggplot(., aes(x=Year, y=Age.Adjusted.Rate, group=interaction(COD.fac, Geography), color = Geography)) +
  geom_line(aes(color = Geography), size =1)+
  scale_color_manual(values = c("Metro New Orleans" = "#166E95", "United States" = "#7A9957"))+
  themeDC_horizontal()+ 
  # geom_text_repel(aes(label=round(Age.Adjusted.Rate)), point.padding = NA, vjust=-1.5, hjust = .5) +
  #geom_text(data = subset(DDFacet_disease_filter , Year == "2017"), aes(label = Change, x = Year, y = Age.Adjusted.Rate ),
  #           color = "red",
  #          size = 3,  family = "Asap",
  #          hjust = -.9, vjust=.2,
  #          show.legend = FALSE)+
  geom_text(data = subset(DDFacet_disease_filter2 , Year == "2018"), aes(label = round(Age.Adjusted.Rate), x = Year, y = Age.Adjusted.Rate ),
            size =3,  family = "Asap",
            hjust = -.3, vjust=.3,
            show.legend = FALSE)+
  geom_text(data = subset(DDFacet_disease_filter2 , Year == "2000"), aes(label = round(Age.Adjusted.Rate), x = Year, y = Age.Adjusted.Rate ),
            size =3,  family = "Asap",
            hjust = 1.5, vjust=.3,
            show.legend = FALSE)+
  scale_x_discrete(breaks=c("2000", "2010", "2018", "2050")) +
  facet_wrap(~ COD.fac, scales = "free", dir = "v", strip.position = "left") +
  theme(panel.spacing = unit(1, "lines"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(), legend.position = "bottom", legend.title = element_blank(),
        strip.text.y = element_text(angle = 180)) +
  theme( axis.ticks.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size =8, color = "gray70"),
         axis.text.y=element_blank())


ggsave(removeDirectLabelClipping(fig4_DD.chronic), filename = "outputs/graphics PDFS/fig4_DD.chronic.pdf", device = cairo_pdf,
       width = 5, height = 5, units = "in")

write.csv(DD, file = "outputs/CSVs/fig4and5_DD.csv")


### Graph 5 
### Deaths by despair second chart with actual deaths by despair
# Figure1.4:Deathsper 100,000fromexternal causes(source:CDC Wonder)

DDFacet_despair_filter2 <- DD %>% 
  filter(COD %in% c('Alcoholic Liver Disease', 'Drug Poisoning', 'Suicide')) %>% 
  filter(Year %in% c("1999-2001", "2017-2018")) %>% 
  mutate(Year = ifelse(Year == "1999-2001", "2000", Year),
         Year = ifelse(Year == "2017-2018", "2018", Year))

fig5_DD.external <- DD %>% 
  filter(COD %in% c('Alcoholic Liver Disease', 'Drug Poisoning', 'Suicide')) %>% 
  # filter(Year %in% c("1999-2001", "2017-2018")) %>% 
  mutate(Year = ifelse(Year == "1999-2001", "2000", Year),
         Year = ifelse(Year == "2017-2018", "2018", Year),
         Year = ifelse(Year == "2008-2010", "2010", Year)) %>% 
  ggplot(., aes(x=Year, y=Age.Adjusted.Rate, group=interaction(COD.fac, Geography), color = Geography)) +
  geom_line(aes(color = Geography), size =1)+
  scale_color_manual(values = c("Metro New Orleans" = "#166E95", "United States" = "#7A9957"))+
  themeDC_horizontal()+ 
  # geom_text_repel(aes(label=round(Age.Adjusted.Rate)), point.padding = NA, vjust=-1.5, hjust = .5) +
  #geom_text(data = subset(DDFacet_disease_filter , Year == "2017"), aes(label = Change, x = Year, y = Age.Adjusted.Rate ),
  #           color = "red",
  #          size = 3,  family = "Asap",
  #          hjust = -.9, vjust=.2,
  #          show.legend = FALSE)+
  geom_text(data = subset(DDFacet_despair_filter2 , Year == "2018"), aes(label = round(Age.Adjusted.Rate), x = Year, y = Age.Adjusted.Rate ),
            size =3,  family = "Asap",
            hjust = -.3, vjust=.3,
            show.legend = FALSE)+
  geom_text(data = subset(DDFacet_despair_filter2 , Year == "2000"), aes(label = round(Age.Adjusted.Rate), x = Year, y = Age.Adjusted.Rate ),
            size =3,  family = "Asap",
            hjust = 1.5, vjust=.3,
            show.legend = FALSE)+
  scale_x_discrete(breaks=c("2000", "2010", "2018", "2050")) +
  facet_wrap(~ COD.fac, scales = "free", dir = "v", strip.position = "left") +
  theme(panel.spacing = unit(1, "lines"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(), legend.position = "bottom", legend.title = element_blank(),
        strip.text.y = element_text(angle = 180)) +
  theme( axis.ticks.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size =8, color = "gray70"),
         axis.text.y=element_blank())


ggsave(removeDirectLabelClipping(fig5_DD.external), filename = "outputs/graphics PDFS/fig5_DD.external.pdf", device = cairo_pdf,
       width = 5, height = 5, units = "in")







#### Figure 7
#### Middle age death rates
# Figure1.7:Middleagemortalityratesper100,000(source:CDCWonder)


DDFacet_middleage_filter2 <- middleage %>% 
  filter(Age.Group %in% c("25-34 years", "35-44 years", "55-64 years")) %>%
  mutate(Year = ifelse(Year == "1999-2001", "2000", Year),
         Year = ifelse(Year == "2008-2010", "2010", Year),
         Year = ifelse(Year == "2017-2018", "2018", Year),
         Geography = ifelse(Geography == "US", "United States", Geography)) %>% 
  filter(Year %in% c("2000", "2010", "2018")) 

# Trying to figure out how to reorder with Oldest age groups at top because they have highest rates
# Factor not working
#middleage$Age.Group.fac = factor(middleage$Age.Group, levels=c("85+ years", "75-84 years", "65-74 years", "55-64 years", "45-54 years", "35-44 years", "25-34 years", 
#                                                              "15-24 years", "5-14 years", "1-4 years", "< 1 year"))

fig7_middleage <- middleage %>% 
  filter(Age.Group %in% c("25-34 years", "35-44 years", "55-64 years")) %>% 
  # filter(Year %in% c("1999-2001", "2017-2018")) %>% 
  mutate(Year = ifelse(Year == "1999-2001", "2000", Year),
         Year = ifelse(Year == "2008-2010", "2010", Year),
         Year = ifelse(Year == "2017-2018", "2018", Year),
         Geography = ifelse(Geography == "US", "United States", Geography)) %>% 
  mutate(Age.Group.fac = factor(Age.Group, levels=c("85+ years", "75-84 years", "65-74 years", "55-64 years", "45-54 years", "35-44 years", "25-34 years", 
                                                    "15-24 years", "5-14 years", "1-4 years", "< 1 year"))) %>% 
  ggplot(., aes(x=Year, y=Rate, group=interaction(Age.Group, Geography), color = Geography)) +
  geom_line(aes(color = Geography), size =1)+
  facet_wrap(~ Age.Group, scales = "free", dir = "v", strip.position = "left") +
  scale_color_manual(values = c("Metro New Orleans" = "#166E95", "United States" = "#7A9957"))+
  themeDC_horizontal()+ 
  geom_text(data = subset(DDFacet_middleage_filter2 , Year == "2018"), aes(label = round(Rate), x = Year, y = Rate ),
            size =3,  family = "Asap",
            hjust = -.3, vjust=.3,
            show.legend = FALSE)+
  geom_text(data = subset(DDFacet_middleage_filter2 , Year == "2010"), aes(label = round(Rate), x = Year, y = Rate ),
            size =3,  family = "Asap",
            hjust = 0, vjust=-.8,
            show.legend = FALSE)+
  geom_text(data = subset(DDFacet_middleage_filter2 , Year == "2000"), aes(label = round(Rate), x = Year, y = Rate ),
            size =3,  family = "Asap",
            hjust = 1.2, vjust=.3,
            show.legend = FALSE)+
  scale_x_discrete(breaks=c("2000", "2010", "2018")) +
  theme(panel.spacing = unit(1, "lines"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(), legend.position = "bottom", legend.title = element_blank(),
        strip.text.y = element_text(angle = 180)) +
  theme( axis.ticks.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size =8, color = "gray70"),
         axis.text.y=element_blank())

ggsave(removeDirectLabelClipping(fig7_middleage), filename = "outputs/graphics PDFS/fig7_middleage.pdf", device = cairo_pdf,
       width = 5, height = 5, units = "in")



write.csv(middleage, file= "outputs/CSVs/fig7_middleage.csv")




### Figure 8 
### Young and old mortality rates over time
# Figure1.8:Mortalitytrendsforyouthandelderlyper100,000(source:CDC Wonder)



DDFacet_youngold_filter2 <- middleage %>% 
  filter(Age.Group %in% c("1-4 years", "5-14 years", "65-74 years", "75-84 years")) %>% 
  mutate(Year = ifelse(Year == "1999-2001", "2000", Year),
         Year = ifelse(Year == "2008-2010", "2010", Year),
         Year = ifelse(Year == "2017-2018", "2018", Year),
         Geography = ifelse(Geography == "US", "United States", Geography)) %>% 
  filter(Year %in% c("2000", "2010", "2018")) 

# Trying to figure out how to reorder with Oldest age groups at top because they have highest rates
# Factor not working
#middleage$Age.Group.fac = factor(middleage$Age.Group, levels=c("85+ years", "75-84 years", "65-74 years", "55-64 years", "45-54 years", "35-44 years", "25-34 years", 
#                                                              "15-24 years", "5-14 years", "1-4 years", "< 1 year"))

fig8_youngold <- middleage %>% 
  filter(Age.Group %in% c("1-4 years", "5-14 years", "65-74 years", "75-84 years")) %>% 
  # filter(Year %in% c("1999-2001", "2017-2018")) %>% 
  mutate(Year = ifelse(Year == "1999-2001", "2000", Year),
         Year = ifelse(Year == "2008-2010", "2010", Year),
         Year = ifelse(Year == "2017-2018", "2018", Year),
         Geography = ifelse(Geography == "US", "United States", Geography)) %>% 
  filter(Year %in% c("2000", "2018")) %>% 
  mutate(Age.Group.fac = factor(Age.Group, levels=c("85+ years", "75-84 years", "65-74 years", "55-64 years", "45-54 years", "35-44 years", "25-34 years", 
                                                    "15-24 years", "5-14 years", "1-4 years", "< 1 year"))) %>% 
  ggplot(., aes(x=Year, y=Rate, group=interaction(Age.Group, Geography), color = Geography)) +
  geom_line(aes(color = Geography), size =1)+
  facet_wrap(~ Age.Group, scales = "free", dir = "v", strip.position = "left") +
  scale_color_manual(values = c("Metro New Orleans" = "#166E95", "United States" = "#7A9957"))+
  themeDC_horizontal()+ 
  geom_text(data = subset(DDFacet_youngold_filter2, Year == "2018"), aes(label = comma(round(Rate)), x = Year, y = Rate ),
            size =3,  family = "Asap",
            hjust = -.1, vjust=.3,
            show.legend = FALSE)+
  geom_text(data = subset(DDFacet_youngold_filter2, Year == "2000"), aes(label = comma(round(Rate)), x = Year, y = Rate ),
            size =3,  family = "Asap",
            hjust = 1, vjust=.3,
            show.legend = FALSE)+
  scale_x_discrete(breaks=c("2000", "2010", "2018")) +
  theme(panel.spacing = unit(1, "lines"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(), legend.position = "bottom", legend.title = element_blank(),
        strip.text.y = element_text(angle = 180)) +
  theme( axis.ticks.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size =8, color = "gray70"),
         axis.text.y=element_blank())

ggsave(removeDirectLabelClipping(fig8_youngold ), filename = "outputs/graphics PDFS/fig8_youngold .pdf", device = cairo_pdf,
       width = 8, height = 5, units = "in")













### Metro death rates by race
#Figure1.10:Blackandwhitemortalityratesper100,000,NewOrleansmetro (source:CDCWonder)


DDFacet_Metrorace_filter2 <- Metro_race %>% 
  mutate(Year = ifelse(Year == "1999-2001", "2000", Year),
         Year = ifelse(Year == "2008-2010", "2010", Year),
         Year = ifelse(Year == "2017-2018", "2018", Year)) %>% 
  filter(Year %in% c("2000", "2018")) %>% 
  filter(Race != "Total")

# Trying to figure out how to reorder with Oldest age groups at top because they have highest rates
# Factor not working
#middleage$Age.Group.fac = factor(middleage$Age.Group, levels=c("85+ years", "75-84 years", "65-74 years", "55-64 years", "45-54 years", "35-44 years", "25-34 years", 
#                                                              "15-24 years", "5-14 years", "1-4 years", "< 1 year"))

fig9_MetroRace <- Metro_race %>% 
  filter(Race != "Total") %>% 
  mutate(Age.Group.fac = factor(Age.Group, levels=c("85+ years", "75-84 years", "65-74 years", "55-64 years", "45-54 years", "35-44 years", "25-34 years", 
                                                    "15-24 years", "5-14 years", "1-4 years", "< 1 year"))) %>% 
  ggplot(., aes(x=Year, y=Rate, group=interaction(Age.Group, Race), color = Race)) +
  geom_line(aes(color = Race), size =1)+
  facet_wrap(~ Age.Group, scales = "free", dir = "v", strip.position = "left", nrow=5) +
  scale_color_manual(values = c("Black" = "#002F45", "White" = "#9EB23B"))+
  themeDC_horizontal()+ 
  geom_text(data = subset(DDFacet_Metrorace_filter2, Year == "2018"), aes(label = comma(round(Rate)), x = Year, y = Rate ),
            size =3,  family = "Asap",
            hjust = -.3, vjust=.5,
            show.legend = FALSE)+
  geom_text(data = subset(DDFacet_Metrorace_filter2, Year == "2000"), aes(label = comma(round(Rate)), x = Year, y = Rate ),
            size =3,  family = "Asap",
            hjust = 1.2, vjust=.5,
            show.legend = FALSE)+
  scale_x_discrete(breaks=c("2000", "2010", "2018")) +
  theme(panel.spacing = unit(2.5, "lines"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(), legend.position = "bottom", legend.title = element_blank(),
        strip.text.y = element_text(angle = 180)) +
  theme( axis.ticks.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size =8, color = "gray70"),
         axis.text.y=element_blank())

ggsave(removeDirectLabelClipping(fig9_MetroRace), filename = "outputs/graphics PDFS/fig9_MetroRace.pdf", device = cairo_pdf,
       width = 5, height = 8, units = "in")

write.csv(Metro_race, file = "outputs/CSVs/fig9_MetroRace.csv")



### Fig 10
### US death rates by race
# Figure1.11:Blackandwhitemortalityratesper100,000,UnitedStates(source: CDCWonder)



DDFacet_USrace_filter2 <- US_race %>% 
  mutate(Year = ifelse(Year == "1999-2001", "2000", Year),
         Year = ifelse(Year == "2008-2010", "2010", Year),
         Year = ifelse(Year == "2017-2018", "2018", Year)) %>% 
  filter(Year %in% c("2000", "2018")) %>% 
  filter(Race != "Total")

# Trying to figure out how to reorder with Oldest age groups at top because they have highest rates
# Factor not working
#middleage$Age.Group.fac = factor(middleage$Age.Group, levels=c("85+ years", "75-84 years", "65-74 years", "55-64 years", "45-54 years", "35-44 years", "25-34 years", 
#                                                              "15-24 years", "5-14 years", "1-4 years", "< 1 year"))

fig10_USRace <- US_race %>% 
  filter(Race != "Total") %>% 
  mutate(Age.Group.fac = factor(Age.Group, levels=c("85+ years", "75-84 years", "65-74 years", "55-64 years", "45-54 years", "35-44 years", "25-34 years", 
                                                    "15-24 years", "5-14 years", "1-4 years", "< 1 year"))) %>% 
  ggplot(., aes(x=Year, y=Rate, group=interaction(Age.Group, Race), color = Race)) +
  geom_line(aes(color = Race), size =1)+
  facet_wrap(~ Age.Group, scales = "free", dir = "v", strip.position = "left", nrow=5) +
  scale_color_manual(values = c("Black" = "#002F45", "White" = "#9EB23B"))+
  themeDC_horizontal()+ 
  geom_text(data = subset(DDFacet_USrace_filter2 , Year == "2018"), aes(label = comma(round(Rate)), x = Year, y = Rate ),
            size =3,  family = "Asap",
            hjust = -.3, vjust=.5,
            show.legend = FALSE)+
  geom_text(data = subset(DDFacet_USrace_filter2 , Year == "2000"), aes(label = comma(round(Rate)), x = Year, y = Rate ),
            size =3,  family = "Asap",
            hjust = 1.2, vjust=.5,
            show.legend = FALSE)+
  scale_x_discrete(breaks=c("2000", "2010", "2018")) +
  theme(panel.spacing = unit(2.5, "lines"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(), legend.position = "bottom", legend.title = element_blank(),
        strip.text.y = element_text(angle = 180)) +
  theme( axis.ticks.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size =8, color = "gray70"),
         axis.text.y=element_blank())

ggsave(removeDirectLabelClipping(fig10_USRace), filename = "outputs/graphics PDFS/fig10_USRace.pdf", device = cairo_pdf,
       width = 5, height = 8, units = "in")

write.csv(US_race, file = "outputs/CSVs/fig10_USRace.csv")



## Figure 11
### All cause mortality Metro and US
# Figure1.9:Declineinall-cause,age-adjustedmortalityratesper100,000 (source:CDCWonder)


USvMetroMortfilter <- AllgeoMort %>% 
  filter(Geography == "United States" | Geography == "Metro New Orleans" ) %>% 
  filter(Year == 1999 | Year == 2018)

fig11_allcause <- AllgeoMort %>% 
  filter(Geography == "United States" | Geography == "Metro New Orleans" ) %>% 
  ggplot(., aes(x=Year, y=Age.Adjusted.Rate, group = Geography, color = Geography )) +
  geom_line(aes(color = Geography), size =1)+
  scale_color_manual(values = c("Metro New Orleans" = "#166E95", "United States" = "#5D893C"))+
  themeDC_horizontal()+ 
  geom_text(data = subset(USvMetroMortfilter  , Year == 2018), aes(label = comma(round(Age.Adjusted.Rate)), x = Year, y = Age.Adjusted.Rate ),
            size =3,  family = "Asap",
            hjust = -.3, vjust=.5,
            show.legend = FALSE)+
  geom_text(data = subset(USvMetroMortfilter  , Year == 1999), aes(label = comma(round(Age.Adjusted.Rate)), x = Year, y =Age.Adjusted.Rate ),
            size =3,  family = "Asap",
            hjust = 1.2, vjust=.5,
            show.legend = FALSE)+
  scale_x_continuous(breaks=c(1999, 2005, 2018)) +
  theme(panel.spacing = unit(2.5, "lines"), panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(), legend.position = "bottom", legend.title = element_blank(),
        strip.text.y = element_text(angle = 180)) +
  theme( axis.ticks.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_text(size =8, color = "gray70"),
         axis.text.y=element_blank())

ggsave(removeDirectLabelClipping(fig11_allcause), filename = "outputs/graphics PDFS/fig11_allcause.pdf", device = cairo_pdf,
       width = 5, height = 5, units = "in")

write.csv(AllgeoMort, file = "outputs/CSVs/fig11_allcause.csv")



# save(list = c('LEUS', 
#               'fig3_countries.line',
#               'DDFacet_disease_filter2',
#               'fig4_DD.chronic',
#               'DDFacet_despair_filter2',
#               'fig5_DD.external',
#               'DDFacet_middleage_filter2',
#               'fig7_middleage',
#               'DDFacet_youngold_filter2',
#               'fig8_youngold',
#               'DDFacet_Metrorace_filter2',
#               'fig9_MetroRace',
#               'DDFacet_USrace_filter2', 
#               'fig10_USRace',
#               'USvMetroMortfilter',
#               'fig11_allcause'), file = "CH1.rachelGraphics.Rdata")

### fig_comp.LE

flute.US <- usLE %>%
  dplyr::select(weightedAvgLE) %>%
  mutate(geo = "United States")
flute.LA <- stateLE %>%
  filter(state == "LA") %>%
  dplyr::select(weightedAvgLE) %>%
  mutate(geo = "Louisiana") %>%
  distinct()
flute.NOmetro <- msaLE %>%
  filter(FIPS_5 == "22071") %>%
  dplyr::select(weightedAvgLE = LE) %>%
  mutate(geo = "Metro New Orleans")
flute.OP <- parishLE %>%
  filter(fips5 == "22071") %>%
  dplyr::select(weightedAvgLE) %>%
  mutate(geo = "City of New Orleans")

fluteData <- bind_rows(flute.US, flute.LA, flute.NOmetro, flute.OP) %>%
  mutate(y=0)

write.csv(fluteData, file = "outputs/CSVs/fig_compLE.csv")
markers <- as.data.frame(matrix(data = c(75,76,77,78,79), ncol = 1, nrow = 5)) %>%
  mutate(y = 0)

#fluteData %>%
fig_comp.LE <- ggplot() +
  geom_segment(aes(x = flute.OP$weightedAvgLE, y = 0, xend=flute.OP$weightedAvgLE, yend = .2), color = "gray") + # lines for labels 
  geom_segment(aes(x = flute.LA$weightedAvgLE, y = 0, xend=flute.LA$weightedAvgLE, yend = .15), color = "gray") +
  geom_segment(aes(x = flute.NOmetro$weightedAvgLE, y = 0, xend=flute.NOmetro$weightedAvgLE, yend = .1), color = "gray") +
  geom_segment(aes(x = flute.US$weightedAvgLE, y = 0, xend=flute.US$weightedAvgLE, yend = .05), color = "gray") +
  geom_hline(yintercept=0, color = "gray") +
  geom_point(aes(y=c(0, 0,0,0,0), x = c(75,76,77,78,79)), color = "gray") +
  geom_text(aes(y=c(0, 0,0,0,0), x = c(75,76,77,78,79),label = c(75,76,77,78,79), color = "darkgray"), vjust = 2, size = 3.3)+
  geom_point(fluteData, mapping = aes(x=weightedAvgLE, y=y, color = geo),size = 3) +
  scale_color_manual(values = c(DCcolor.p2yellow,
                                "black",
                                DCcolor.p2magenta,
                                DCcolor.p2blue,
                                DCcolor.p2green))+
  geom_text(fluteData,  mapping = aes(x=weightedAvgLE, y=c(.05,.15,.1,.2),label = geo), hjust = -.001, vjust = -.2)+
  theme_light()+
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        plot.caption = element_text(hjust = 0),
        strip.text = element_text(color = "grey20", face = "bold"),
        strip.background = element_blank()) +
  labs(x = "",
       y="") +
  ggsave(filename = "outputs/graphics PDFs/fig_comp.LE.pdf", device = cairo_pdf, width = 8, height = 4)

#fig_comp.LE


### fig_metro.tract.LE

LE.GNO <- LEusbLA %>% 
  filter(ageGrp=="Under 1") %>% 
  filter(str_sub(GEOID,3,5) %in% GNO.parishfips)%>%
  mutate(disc=cut(x=.$LE_x,breaks = 6, include.lowest = TRUE))

LE.GNOcsv <- LE.GNO %>%
  select(GEOID, LE_x, LE_SE_x, disc)
write.csv(LE.GNOcsv,file = "outputs/CSVs/fig_metrotractLE.csv")

tomap <-tracts.la %>%
  mutate(GEOID = as.character(GEOID10)) %>%
  left_join(LE.GNO)

coul <- brewer.pal(4, "YlGnBu") 
col.metroLE <- colorRampPalette(coul)(6)

# make map
fig_metro.tract.LE <- tomap %>%
  ggplot() +
  geom_sf(aes(fill = disc), color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray90", color = "gray70", size = .1) +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=.1) +
  themeDC_map() +
  #scale_fill_manual(values = col.metroLE,  na.value = "gray70", labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing"))+ 
  scale_fill_paletteer_d("LaCroixColoR::Lemon", na.value = "gray70",labels = c("62.3 - 66.6","66.6 - 70.9", "70.9 - 75.2", "75.2 - 79.5", "79.5 - 83.8", "83.8 - 88.1", "Missing")) + # "Redmonder::dPBlPuGn" "rcartocolor::Temps"
  #scale_fill_distiller(palette = "Spectral", direction = 1, guide = "colorbar", na.value = "gray70") +
  #scale_fill_brewer(palette = "PRGn", direction = 1,  na.value = "gray70") +
  #scale_fill_viridis_c(palette = "magma", direction = 1, guide = "colorbar", na.value = "gray70") + 
  coord_sf(xlim = c(-90.3, -89.85), ylim = c(29.85, 30.1), expand = FALSE) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(.3,"cm"),
        strip.text = element_text(hjust = 0)) + guides(fill = guide_legend(nrow = 1))+ 
  
  labs(
    fill = "")+
  ggsave(filename = "outputs/graphics PDFs/fig_metro.tract.LE.pdf", device = cairo_pdf, width = 8, height = 5)


### fig_abbrev.sched

fig_abbrevschedcsv <- msaProbDeath %>%
  filter(city == "New Orleans-Metairie, LA") %>%
  mutate(geo = "New Orleans metro") %>%
  ungroup() %>%
  dplyr::select(-city) %>%
  bind_rows((usProbDeath.agg %>% mutate(geo = "United States"))) %>%
  mutate(ageGrp.fac = factor(ageGrp, levels = c("5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85 and older"))) %>%
  filter(ageGrp %in% c("5-14", "15-24", "25-34", "35-44", "45-54", "55-64")) %>%
  select(-popSum,-ageGrp.fac)

write.csv(fig_abbrevschedcsv, file = "outputs/CSVs/fig_abbrevsched.csv")

fig_abbrev.sched <- msaProbDeath %>%
  filter(city == "New Orleans-Metairie, LA") %>%
  mutate(geo = "New Orleans metro") %>%
  ungroup() %>%
  dplyr::select(-city) %>%
  bind_rows((usProbDeath.agg %>% mutate(geo = "United States"))) %>%
  mutate(ageGrp.fac = factor(ageGrp, levels = c("5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85 and older"))) %>%
  filter(ageGrp %in% c("5-14", "15-24", "25-34", "35-44", "45-54", "55-64")) %>%
  ggplot(aes(x=ageGrp.fac, y=weightedAvgProbDeath, group = geo)) +
  geom_line(aes(color = geo), show.legend = FALSE, size = 1) + 
  scale_color_manual(values = c(DCcolor.p2blue,DCcolor.p2green)) +
  geom_dl(aes(label = geo), method = list(dl.trans(x = x + .2), "last.bumpup", cex = 0.8)) +
  scale_y_continuous(labels = scales::percent)+
  expand_limits(x=7.5) +
  labs(
    x="Age group",
    y="Chance of death")+
  theme(legend.position = "none") +
  themeDC_scatter()

ggsave(removeDirectLabelClipping(fig_abbrev.sched), filename = "outputs/graphics PDFs/fig_abbrev.sched.pdf",device = cairo_pdf, width = 8, height = 5)


### fig_US.dists
fig_US.distscsv <- alldata %>%
  filter(ageGrp == "Under 1" & !is.na(ACS.majBlack)) %>%
  dplyr::select(LE = LE_x, GEOID, ACS.majBlack)
write.csv(fig_US.distscsv,file = "outputs/CSVs/fig_US.dists.csv")

fig_US.dists <- alldata %>%
  filter(ageGrp == "Under 1" & !is.na(ACS.majBlack)) %>%
  dplyr::select(LE = LE_x, GEOID, ACS.majBlack) %>%
  ggplot(., aes(x=LE, color = ACS.majBlack, fill=ACS.majBlack)) +
  geom_histogram(alpha=.5, position="identity", bins = 40,color = "#A0A0A0A0") +
  scale_fill_manual(name = "Tract demographics", labels = c("majority non-Black", "majority Black"), values = c(DCcolor.p3yellowochre,DCcolor.p1darkblue))+
  themeDC_horizontal() +
  geom_segment(aes(x=usLEblk$weightedAvgLE[2], y = 0, xend = usLEblk$weightedAvgLE[2], yend = Inf), color = DCcolor.p1darkblue) +
  geom_segment(aes(x=usLEblk$weightedAvgLE[1], y = 0, xend = usLEblk$weightedAvgLE[1], yend = Inf), color = DCcolor.p3yellowochre) +
  annotate('text',x=usLEblk$weightedAvgLE[2]-5, y=7020,label = "4.6-year gap in mean LE", size = 3, family = "Asap")+
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.key.size = unit(.75,"line"),
        legend.title = element_text(size = 9),
        panel.grid.major.y = element_blank()) +
  scale_x_continuous(limits=c(55,95)) +
  scale_y_continuous(labels = scales::comma)+
  labs(x="Life expectancy",
       y="Number of tracts") +
  ggsave(filename = "outputs/graphics PDFs/fig_US.dists.pdf", device = cairo_pdf, width = 8, height = 5)



### fig_nola.dist.race
histdataMSAcsv <- geoLEMSA %>%
  left_join(., (alldata %>% filter(ageGrp=="Under 1")), by=c("GEOID")) %>%
  dplyr::select(LE_x, GEOID, city = city.x, ACS.majBlack) %>%
  filter(city == "New Orleans-Metairie, LA")
write.csv(histdataMSAcsv, file = "outputs/CSVs/fig_noladistrace.csv")

histdataMSA <- geoLEMSA %>%
  left_join(., (alldata %>% filter(ageGrp=="Under 1")), by=c("GEOID")) %>%
  dplyr::select(LE_x, GEOID, city = city.x, ACS.majBlack) %>%
  mutate(LE = as.numeric(LE_x), GEOID=as.numeric(GEOID)) 

fig_nola.dist.race <- histdataMSA %>% 
  filter(city == "New Orleans-Metairie, LA") %>%
  ggplot(., aes(x=LE, color = ACS.majBlack, fill=ACS.majBlack)) +
  geom_histogram(alpha=.5, position="identity", bins = 40,color = "#A0A0A0A0") +
  scale_fill_manual(name = "Tract demographics", labels = c("majority non-Black", "majority Black"), values = c(DCcolor.p3yellowochre,DCcolor.p1darkblue))+
  themeDC_horizontal() +
  geom_segment(aes(x=NOmsaLEblk$weightedAvgLE[2], y = 0, xend = NOmsaLEblk$weightedAvgLE[2], yend = Inf), color = DCcolor.p1darkblue) +
  geom_segment(aes(x=NOmsaLEblk$weightedAvgLE[1], y = 0, xend = NOmsaLEblk$weightedAvgLE[1], yend = Inf), color = DCcolor.p3yellowochre) +
  annotate('text',x=NOmsaLEblk$weightedAvgLE[2]-5, y=30,label = "3.8-year gap in mean LE", size = 3)+
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.key.size = unit(.75,"line"),
        legend.title = element_text(size = 9),
        panel.grid.major.y = element_blank()) +
  scale_x_continuous(limits=c(55,95)) +
  scale_y_continuous(labels = scales::comma)+
  labs(x="Life expectancy",
       y="Number of tracts") +
  ggsave(filename = "outputs/graphics PDFs/fig_nola.dist.race.pdf", device = cairo_pdf, width = 8, height = 5)

### fig_nola.socDets

socdetsMSA <- geoLEMSA %>%
  filter(city == 'New Orleans-Metairie, LA') %>%
  left_join(acs.NOmsa,by="GEOID") 

write.csv(socdetsMSA, file = "outputs/CSVs/fig_nolasocDets.csv")

fig_nola.socDets.inc <- socdetsMSA %>% 
  ggplot(., aes(y=ACS.medInc, x= LE,  color = ACS.pctBlack*100)) +
  geom_point( size=1.5) +
  scale_color_distiller(palette = "Spectral", direction = 1) + 
  themeDC_scatter() +
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Life expectancy",
       y="Median household income",
       color = "% Black")+
  theme(legend.key.size = unit(.4,"cm")) +
  ggsave(filename = "outputs/graphics PDFs/fig_nola.socDets.ed.pdf", device = cairo_pdf, width = 8, height = 5)

fig_nola.socDets.inc <- socdetsMSA %>% 
  ggplot(., aes(x=LE, y = ACS.medInc,  color = ACS.pctBlack*100)) +
  geom_point( size=1.5) +
  scale_color_distiller(palette = "Spectral", direction = 1) + 
  themeDC_scatter() +
  scale_y_continuous(labels = scales::comma)+
  labs(x = "Life expectancy",
       y="Median household income",
       color = "% Black")+
  theme(legend.key.size = unit(.4,"cm")) +
  ggsave(filename = "outputs/graphics PDFs/fig_nola.socDets.inc.pdf", device = cairo_pdf, width = 8, height = 5)

#fig_nola.socDets.inc


fig_nola.socDets.ed <- socdetsMSA %>% 
  ggplot(., aes(x=LE, y = ACS.pctBachPlus, color = ACS.pctBlack*100)) +
  geom_point( size=1.5) +
  scale_color_distiller(palette = "Spectral", direction = 1) +
  themeDC_scatter() +
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  labs(x = "Life expectancy",
       y="Higher educational attainment",
       color = "% Black")+
  theme(legend.key.size = unit(.4,"cm")) +
  ggsave(filename = "outputs/graphics PDFs/fig_nola.socDets.ed.pdf", device = cairo_pdf, width = 8, height = 5)

#fig_nola.socDets.ed

### fig_natl.state.LE

coul <- brewer.pal(4, "YlGnBu") 
col.stateLE <- colorRampPalette(coul)(7)

acs.state_sf <- get_acs(geography = "state",
                        variables = c(medincome = "B19013_001"), # var not used in analysis, just pulling this for the shapes
                        shift_geo = TRUE, # This downloads the geog in Albers
                        geometry = TRUE)


stateLE.map <- stateLE %>%
  distinct() %>% 
  dplyr::select(stateCode, LE = weightedAvgLE, popSum) %>% 
  mutate(LE = as.character(round(LE,digits = 0)))

write.csv(stateLE.map, file = "outputs/CSVs/fig_natlstateLE.csv")

fig_natl.state.LE <- acs.state_sf %>%
  left_join(stateLE.map, by=c("GEOID"="stateCode")) %>%
  ggplot() +
  geom_sf(aes(fill=LE)) +
  scale_fill_manual(values = lacroix_palette("Lemon", n = 7, type = "continuous"), na.value = "gray70",labels = c("75","76", "77", "78", "79", "80", "81", "Missing"))+ 
  coord_sf(datum = NA) +
  themeDC_map() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(.3,"cm"),
        strip.text = element_text(hjust = 0)) + guides(fill = guide_legend(nrow = 1)) +
  ggsave(filename = "outputs/graphics PDFs/fig_natl.state.LE.pdf",device = cairo_pdf, width = 8, height = 5)

#fig_natl.state.LE



### fig_metrosComp.dist.LE   
metro.means <-  histdataMSA %>%
  filter(city %in% c('Birmingham-Hoover, AL','New Orleans-Metairie, LA','Philadelphia-Camden-Wilmington, PA-NJ-DE-MD','Los Angeles-Long Beach-Anaheim, CA','Boston-Cambridge-Newton, MA-NH','Atlanta-Sandy Springs-Roswell, GA')) %>%
  mutate(city.fac = factor(.$city, levels = c( 'Birmingham-Hoover, AL','New Orleans-Metairie, LA','Philadelphia-Camden-Wilmington, PA-NJ-DE-MD','Atlanta-Sandy Springs-Roswell, GA','Los Angeles-Long Beach-Anaheim, CA','Boston-Cambridge-Newton, MA-NH'))) %>%
  dplyr::select(GEOID, city.fac, LE_x) %>%
  mutate(FIPS_5 = as.numeric(str_sub(GEOID,1,-7))) %>%
  left_join(msaLE, by = "FIPS_5") %>%
  group_by(city.fac) %>%
  dplyr::select(city.fac, meanLE = LE) %>%
  distinct()
fig_metrosComp.dist.LEcsv <- histdataMSA %>%
  filter(city %in% c('Birmingham-Hoover, AL','New Orleans-Metairie, LA','Philadelphia-Camden-Wilmington, PA-NJ-DE-MD','Los Angeles-Long Beach-Anaheim, CA','Boston-Cambridge-Newton, MA-NH','Atlanta-Sandy Springs-Roswell, GA')) 

write.csv(metro.means,file = "outputs/CSVs/fig_metrosCompdistLEmeans.csv")  
write.csv(fig_metrosComp.dist.LEcsv,file = "outputs/CSVs/fig_metrosCompdistLE.csv")  

fig_metrosComp.dist.LE  <- histdataMSA %>%
  filter(city %in% c('Birmingham-Hoover, AL','New Orleans-Metairie, LA','Philadelphia-Camden-Wilmington, PA-NJ-DE-MD','Los Angeles-Long Beach-Anaheim, CA','Boston-Cambridge-Newton, MA-NH','Atlanta-Sandy Springs-Roswell, GA')) %>%
  mutate(city.fac = factor(.$city, levels = c( 'Birmingham-Hoover, AL','New Orleans-Metairie, LA','Philadelphia-Camden-Wilmington, PA-NJ-DE-MD','Atlanta-Sandy Springs-Roswell, GA','Los Angeles-Long Beach-Anaheim, CA','Boston-Cambridge-Newton, MA-NH'))) %>%
  ggplot(., aes(x=LE, y=city.fac, fill=factor(..quantile..))) +
  stat_density_ridges(alpha = .5, geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = c(0.2, 0.8)) +
  scale_fill_manual(name = "", values = c(DCcolor.p1skyblue, "#A0A0A0A0", DCcolor.p2violet),
                    labels = c("0-20th percentile", "20-80th ptile", "80-100th ptile")) +
  geom_text(aes(label = ".",x=meanLE, y=city.fac), data = metro.means, inherit.aes = F, size = 16, color = "white", family = "Times New Roman", vjust = -.85)+
  themeDC_horizontal() +
  theme(legend.position = "bottom",
        legend.key.size = unit(.3,"cm"), 
        legend.spacing.x = unit(.2, 'cm')) +
  #scale_x_continuous(limits=c(59,95)) +
  scale_y_discrete(labels = c( 'Birmingham','New Orleans','Philadelphia','Atlanta','Los Angeles','Boston')) +
  labs(x = "Life expectancy",
       y = "Metro")+
  ggsave(filename = "outputs/graphics PDFs/fig_metrosComp.dist.LE.pdf",device = cairo_pdf, width = 8, height = 5)



### fig_metrosComp.dist.race.LE

metrosRaceData <- histdataMSA %>% 
  filter(city %in% c("Los Angeles-Long Beach-Anaheim, CA", 'Washington-Arlington-Alexandria, DC-VA-MD-WV', 'Memphis, TN-MS-AR', 'Birmingham-Hoover, AL')) %>%
  mutate(city = gsub("[-,/].*","", city)) %>%
  mutate(city.fac = factor(.$city, levels = c("Los Angeles","Washington","Birmingham","Memphis"))) %>%
  filter(!is.na(ACS.majBlack))

write.csv(metrosRaceData, file =  "outputs/CSVs/fig_metrosCompdistraceLE.csv")

fig_metrosComp.dist.race.LE <- metrosRaceData %>%
  ggplot(., aes(x=LE, color = ACS.majBlack, fill=ACS.majBlack)) +
  geom_histogram(alpha=.5, position="identity", bins = 40,color = "#A0A0A0A0") +
  facet_wrap(~city.fac, ncol = 2, scales = "free") +
  scale_fill_manual(name = "Tract demographics", labels = c("majority non-Black", "majority Black"), values = c(DCcolor.p3yellowochre,DCcolor.p1darkblue))+
  themeDC_scatter() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(.3,"cm"), 
        legend.spacing.x = unit(.2, 'cm')) +
  scale_x_continuous(limits=c(55,95)) +
  labs(x='Life expectancy',
       y = 'Number of tracts')+
  ggsave(filename = "outputs/graphics PDFs/fig_metrosComp.dist.race.LE.pdf",device = cairo_pdf, width = 8, height = 5)



# save(list = c('fig_comp.LE',
#               'fig_metro.tract.LE',
#               'fig_abbrev.sched',
#               'fig_US.dists',
#               'fig_nola.dist.race',
#               'fig_nola.socDets.inc',
#               'fig_nola.socDets.ed',
#               'fig_natl.state.LE',
#               'fig_metrosComp.dist.LE',
#               'fig_metrosComp.dist.race.LE'), file = "jennaGraphics.Rdata")
# 
# save(flute.OP,
#      flute.NOmetro,
#      flute.LA,
#      flute.US,
#      fluteData,
#      tomap,
#      histdataMSA ,
#      socdetsMSA,
#      stateLE.map,
#      metrosRaceData, file = "jennaGraphicsData.Rdata")




