#------------------------------------------------------------------------#
#- Author: Sarah Bardin                                                 -#
#-                                                                      -#
#- Overview: Reproduce MGWR and GWR maps of local parameter estimates   -#
#                                             -#
#------------------------------------------------------------------------#

#------------------------#
#--- Define Libraries ---#
#------------------------#
library(tidyverse)
library(sf)
library(here)
library(tidycensus)
library(stringr)
library(RColorBrewer)
library(maps)
#------------------------#
#--- Read in Data ---#
#------------------------#

# County-level boundary data 
boundaries <- get_estimates(geography = "county", 
                            product = "population", 
                            year = 2018,                
                            geometry = TRUE)

boundaries <- boundaries %>% 
  filter(variable == "POP") 

#MGWR results
mgwr <- read_csv(here("results","other","MGWR_Mollalo_results.csv"))
mgwr$GEOID <- str_pad(mgwr$GEOID, 5, pad = "0")

#GWR results
gwr <- read_csv(here("results","other","GWR_Mollalo_results.csv"))
gwr$GEOID <- str_pad(gwr$GEOID, 5, pad = "0")

#------------------------#
#--- MGWR Map ---#
#------------------------#
mgwr_map <- inner_join(boundaries,mgwr)
mgwr_map <- st_transform(mgwr_map, 5070)
str(mgwr_map)


states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) 

# Make list of variable names to loop over.
colNames <- names(mgwr_map)[14:17]

for(i in colNames){
  
  # Save plots to jpeg Makes a separate file for each plot.  
  file_name = paste("mgwr_",i,".jpeg", sep="")
  jpeg(here("results","maps",file_name), width = 700, height = 450, quality = 500)
  
  # Make plots.
  print(ggplot() +
    geom_sf(data=mgwr_map, aes(fill=!!sym(i)), col = "gray", size = 0.01) +
    geom_sf(data=states, fill="NA") +
    scale_fill_gradient2() +
    theme_void() + 
    theme(plot.margin = unit(c(0,5,1,1),units="points"),legend.position="bottom"))
  dev.off()

}


#------------------------#
#--- GWR Map ---#
#------------------------#
gwr_map <- inner_join(boundaries,gwr)
gwr_map <- st_transform(gwr_map, 5070)


# Make list of variable names to loop over.
colNames <- names(gwr_map)[16:19]

for(i in colNames){
  
  # Save plots to jpeg Makes a separate file for each plot.  
  file_name = paste("gwr_",i,".jpeg", sep="")
  jpeg(here("results","maps",file_name), width = 700, height = 450, quality = 500)
  
  # Make plots.
  print(ggplot() +
          geom_sf(data=gwr_map, aes(fill=!!sym(i)), col = "gray", size = 0.01) +
          geom_sf(data=states, fill="NA") +
          scale_fill_gradient2() +
          theme_void() + 
          theme(plot.margin = unit(c(0,5,1,1),units="points"),legend.position="bottom"))
  dev.off()
  
}

