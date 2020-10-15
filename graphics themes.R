## Colors and themes for graphics. ##

######

DCcolors <- data.frame(stringsAsFactors = TRUE, # Factors are a pain, but this might make things easier later.
                       colorName = c("p1darkblue",
                                     "p1grayblue",
                                     "p1mediumblue",
                                     "p1skyblue",
                                     "p1lightskyblue",
                                     "p2blue",
                                     "p2teal",
                                     "p2green",
                                     "p2limegreen",
                                     "p2yellow",
                                     "p2orange",
                                     "p2orangered",
                                     "p2magenta",
                                     "p2violet",
                                     "p2purple"),
                       hex = c("#002F45",
                               "#4A6576",
                               "#6892AB",
                               "#ABE1FA",
                               "#D4EFFC",
                               "#166E95",
                               "#35A39B",
                               "#5D893C",
                               "#9EB23B",
                               "#F1C62B",
                               "#EF812C",
                               "#E65E3F",
                               "#E61C43",
                               "#B13F80",
                               "#71266E"))

DCcolors <- DCcolors %>% 
  mutate(palette = ifelse(str_sub(colorName, 1,2) == "p1", "primary","secondary"))

# 2. From a defined set of values (auto-complete is nice)

DCcolor.p1darkblue <- "#002F45"
DCcolor.p1grayblue <- "#4A6576"
DCcolor.p1mediumblue <- "#6892AB"
DCcolor.p1skyblue <- "#ABE1FA"
DCcolor.p1lightskyblue <- "#D4EFFC"
DCcolor.p2blue <- "#166E95"
DCcolor.p2teal <- "#35A39B"
DCcolor.p2green <- "#5D893C"
DCcolor.p2limegreen <- "#9EB23B"
DCcolor.p2yellow <- "#F1C62B"
DCcolor.p2orange <- "#EF812C"
DCcolor.p2orangered <- "#E65E3F"
DCcolor.p2magenta <- "#E61C43"
DCcolor.p2violet <- "#B13F80"
DCcolor.p2purple <- "#71266E"

DCcolor.p3yellowochre <- "#D7892C"

# 3. Selecting values from a list (good, for instance, if you want to use the first 3 or every third value.)

DCcolorsPrimary <- as.character(DCcolors$hex[1:5])
DCcolorsSecondary <- as.character(DCcolors$hex[6:length(DCcolors$hex)])


####### Define graphic themes

library(extrafont)
loadfonts()

# Requires datacenter_colors.R
# source(here("datacenter_colors.R"))

themeDC_horizontal <- function(){
  theme_light() +
    theme(text = element_text(family = "Asap"), # Change to Asap if necessary  
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          plot.caption = element_text(hjust = 0),
          strip.text = element_text(color = "grey20", face = "bold"),
          strip.background = element_blank())
}

themeDC_vertical <- function() {
  theme_light() +
    theme(text = element_text(family = "Asap"), # Change to Asap if necessary  
          panel.grid.major.x = element_line(color = "gray90"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          plot.caption = element_text(hjust = 0),
          strip.text = element_text(color = "grey20", face = "bold"),
          strip.background = element_blank())
}

themeDC_scatter <- function() {
  theme_light() +
    theme(text = element_text(family = "Asap"), # Change to Asap if necessary  
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          plot.caption = element_text(hjust = 0),
          strip.text = element_text(color = "grey20", face = "bold"),
          strip.background = element_blank())
}


themeDC_map <- function() {
  require(ggthemes)
  theme_map() +
    theme(text = element_text(family = "Asap"), # Change to Asap if necessary  
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          plot.caption = element_text(hjust = 0),
          strip.text = element_text(color = "grey20", face = "bold"),
          strip.background = element_blank())
}


### REMOVE DIRECT LABEL CLIPPING

removeDirectLabelClipping <- function(finishedPlot) {
  finishedPlot.temp <- ggplot_build(finishedPlot) %>% ggplot_gtable()
  finishedPlot.temp$layout$clip[finishedPlot.temp$layout$name=="panel"] <- "off"
  grid.draw(finishedPlot.temp)
}

# To use, a chart object must be saved, and the output must be passed to ggsave. Example:
# ggsave(removeDirectLabelClipping(CHARTOBJECT), filename = "FILENAME.png", dpi = 300, type = "cairo",
#        width = 8, height = 5, units = "in")
# ggsave(removeDirectLabelClipping(CHARTOBJECT), filename = "FILENAME.pdf", device = cairo_pdf,
#        width = 8, height = 5, units = "in")

