



OrleansLE <- LEusbLA %>%
  filter(ageGrp == "Under 1") %>%
  filter(str_sub(GEOID,3,5) %in% GNO.parishfips) %>%
  dplyr::select(GEOID, LE_x)

quints = as.data.frame(cut(as.numeric(OrleansLE$LE_x), breaks = quantile(as.numeric(OrleansLE$LE), probs = seq(0, 1, 0.2)), 
                           include.lowest = TRUE, labels = 1:5), col.names = names(quintiles))  
OrleansLEquints <- bind_cols(OrleansLE, quints) %>%
  rename(quintile =  "cut(as.numeric(OrleansLE$LE_x), breaks = quantile(as.numeric(OrleansLE$LE), probs = seq(0, 1, 0.2)), include.lowest = TRUE, labels = 1:5)")

OrleansLEcut <- OrleansLE %>%
  mutate(disc=cut(x=.$LE_x,breaks = 6, include.lowest = TRUE)) %>%
  left_join(OrleansLEquints, by = "GEOID")

tomap.redlining <-tracts.la %>%
  mutate(GEOID = as.character(GEOID10)) %>%
  filter(str_sub(GEOID, 1, 5) %in% c("22051", "22071", "22075", "22087")) %>%
  left_join(OrleansLEcut) 

# make map
fig_metro.tract.LE.redlining <- tomap.redlining %>%
  filter(quintile %in%c(1,5)) %>%
  ggplot() +
  geom_sf(data = tomap, fill = "gray90", color = "gray70") +
  geom_sf(aes(fill = quintile), color = "gray70", size = .1) +
  geom_sf(data = sf::st_union(Jefferson.water_sf), fill = "gray80", color = "gray70") +
  geom_sf(data = sf::st_union(Orleans.water_sf), fill = "gray80", color = "gray70") +
  geom_sf(data = sf::st_union(StCharles.water_sf), fill = "gray80", color = "gray70") +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=1) +
  themeDC_map() +
  scale_fill_manual( values = c("#F5D000","#1BB6AF"),labels = c("Bottom 20%","Top 20%")) + 
  coord_sf(xlim = c(-90.2, -89.92), ylim = c(29.88, 30.09), expand = FALSE) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(.3,"cm"),
        strip.text = element_text(hjust = 0)) + guides(fill = guide_legend(nrow = 1))+ 
  labs(title = "Census tracts in top and bottom 20th percentile of life expectancy",
       subtitle = "Greater New Orleans",
       caption = "Source: USALEEP",
       fill = "") +
  ggsave(filename = "outputs/graphics PDFs/fig_metro.tract.quintiles.pdf", device = cairo_pdf, width = 8, height = 5)

fig_metro.tract.LE.redlining


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



metroLE <- LEusbLA %>%
  filter(ageGrp == "Under 1") %>%
  filter(str_sub(GEOID,3,5) %in% GNO.parishfips) %>%
  dplyr::select(GEOID, LE_x)
metroquints = as.data.frame(cut(as.numeric(metroLE$LE_x), breaks = quantile(as.numeric(metroLE$LE), probs = seq(0, 1, 0.2)), 
                                include.lowest = TRUE, labels = 1:5), col.names = names(quintile))  
metroLEquints <- bind_cols(metroLE, metroquints) %>%
  rename(quintile =  "cut(as.numeric(metroLE$LE_x), breaks = quantile(as.numeric(metroLE$LE), probs = seq(0, 1, 0.2)), include.lowest = TRUE, labels = 1:5)")
tomap.quintiles <-tracts.la %>%
  mutate(GEOID = as.character(GEOID10)) %>%
  filter(str_sub(GEOID, 3, 5) %in% GNO.parishfips) %>%
  left_join(metroLEquints) 

# make map
fig_metro.tract.LE.quintiles <- tomap.quintiles %>%
  ggplot() +
  geom_sf(aes(fill = quintile), color = "gray90", size = .1) +
  geom_sf(data = otherwater.simple_sf, fill = "white", color = "transparent") +
  geom_sf(data = wetlands.simple_sf, fill = "grey90", color = "transparent") +
  geom_sf(data = parishes_sf, fill = "transparent", color = "grey70", size=1) +
  themeDC_map() +
  scale_fill_paletteer_d("LaCroixColoR::Lemon") + 
  coord_sf(xlim = c(-91, -89), ylim = c(29, 30.7), expand = FALSE) +
  theme(legend.position = "top",
        legend.key.size = unit(.5,"cm"),
        strip.text = element_text(hjust = 0)) +
  labs(title = "Census tracts life expectancy",
       subtitle = "Metro New Orleans",
       caption = "Source: USALEEP",
       fill = "")

fig_metro.tract.LE.quintiles


### Redlining map  

redlining <- sf::st_read(here("inputs/LANewOrleans1939/cartodb-query.shp"))

# Make sure your unique ID is a character, properly named for joining, then join
redlining.polygon <- redlining %>% 
  ggplot() + 
  geom_sf(data = tomap.redlining, fill = "gray80", color = "gray90", size = .1) +
  geom_sf(aes(fill = holc_grade),  color="gray50")+ # This is the main feature layer for this map
  scale_fill_manual(values = c("#349d4e","#1dafd7","#faf280","#f85959")) +
  geom_sf(data = otherwater.simple_sf, fill = "white", color = "transparent") +
  geom_sf(data = wetlands.simple_sf, fill = "grey90", color = "transparent") +
  # geom_sf(data = sf::st_union(Jefferson.water_sf), fill = "gray90", color = "gray70") +
  # geom_sf(data = sf::st_union(Orleans.water_sf), fill = "gray90", color = "gray70") +
  #geom_sf(data = sf::st_union(StBernard.water_sf), fill = "gray90", color = "gray70") +
  #geom_sf(data = subset(tomap.redlining, quintile ==1), fill = "transparent", color = "black") +         
  #geom_sf(data = subset(tomap.redlining, quintile ==5), fill = "transparent", color = "purple") + 
  themeDC_map()+
  coord_sf(xlim = c(-90.2, -89.92), ylim = c(29.88, 30.09), expand = FALSE) +
  theme(legend.position = "top",
        legend.key.size = unit(.5,"cm"),
        strip.text = element_text(hjust = 0)) +
  labs(title = "HOLC Redlining",
       subtitle = "New Orleans and Jefferson Parish suburbs, 1939",
       caption = "Source: Mapping Inequality",
       fill = "")

redlining.polygon
