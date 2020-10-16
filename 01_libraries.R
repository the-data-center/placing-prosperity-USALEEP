#### LIBRARIES ####

library(tidyverse)
library(purrr)

library(here)
library(rmarkdown)

library(tidycensus)

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

#Put your census API key here
myCensusAPIkey <- "A string with a valid census API key"
census_api_key(myCensusAPIkey)
