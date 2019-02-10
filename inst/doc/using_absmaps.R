## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ---- eval=FALSE---------------------------------------------------------
#  library(absmaps)
#  
#  mapdata <- load_absmaps("sa4", year = 2016)
#  

## ---- include=FALSE------------------------------------------------------
library(absmaps)

mapdata <- load_absmaps("sa4", year = 2016, download = FALSE)


## ------------------------------------------------------------------------
head(mapdata)

## ------------------------------------------------------------------------
library(ggplot2)

map <- 
mapdata %>% 
  filter(gcc == "Greater Melbourne") %>%   # let's just look Melbourne
  ggplot() +
  geom_sf(aes(geometry = geometry))  # use the geometry variable

map

## ------------------------------------------------------------------------
map <- 
mapdata %>% 
  filter(gcc == "Greater Melbourne") %>%   # let's just look Melbourne
  ggplot() +
  geom_sf(aes(geometry = geometry)) +   # use the geometry variable
  geom_point(aes(cent_lat, cent_long))  # use the centroid lat and longs

map

## ------------------------------------------------------------------------
map <- 
mapdata %>% 
  filter(gcc == "Greater Melbourne") %>%   # let's just look Melbourne
  ggplot() +
  geom_sf(aes(geometry = geometry,  # use the geometry variable
              fill = areasqkm),     # fill by area size
          lwd = 0,                  # remove borders
          show.legend = FALSE) +    # remove legend
  geom_point(aes(cent_lat,      
                 cent_long),        # use the centroid lat and longs
             colour = "white") +    # make the points white
  theme_void() +                    # clears other plot elements
  coord_sf(datum = NA)              # fixes a gridline bug in theme_void()

map

## ------------------------------------------------------------------------
# Read data in
ue_data <- read_csv("data/ue_data.csv")

# This data contains a variable of sa4 that can be joined to the mapdata set:
ue_data <- ue_data %>% left_join(mapdata)

## ------------------------------------------------------------------------
map <- 
ue_data %>% 
  filter(gcc == "Greater Melbourne") %>%   # let's just look Melbourne
  ggplot() +
  geom_sf(aes(geometry = geometry,  # use the geometry variable
              fill = ue_pc),        # fill by unemployment rate
          lwd = 0) +                # remove borders
  theme_void() +                    # clears other plot elements
  coord_sf(datum = NA)              # fixes a gridline bug in theme_void()

map

## ------------------------------------------------------------------------
map <- 
ue_data %>% 
  filter(gcc == "Greater Melbourne") %>%   # let's just look Melbourne
  ggplot() +
  geom_sf(aes(geometry = geometry)) +  # use the geometry variable
  geom_point(aes(cent_lat, 
                 cent_long,
                 size = ue_pc,
                 colour = ue_pc)) +
  theme_void() +                     # clears other plot elements
  coord_sf(datum = NA)               # fixes a gridline bug in theme_void()

map

