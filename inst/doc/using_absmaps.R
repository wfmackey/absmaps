## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(dplyr)
library(ggplot2)
library(readr)


## ---- eval=FALSE---------------------------------------------------------
#  library(absmaps)
#  
#  load_absmaps("sa4", year = 2016, download = TRUE)
#  

## ---- include=FALSE------------------------------------------------------
library(absmaps)

load_absmaps("sa4", year = 2016, download = FALSE)


## ------------------------------------------------------------------------
head(sa42016)

## ------------------------------------------------------------------------
map <- 
sa42016 %>% 
  filter(gcc == "Greater Melbourne") %>%   # let's just look Melbourne
  ggplot() +
  geom_sf(aes(geometry = geometry))  # use the geometry variable

map

## ------------------------------------------------------------------------
map <- 
sa42016 %>% 
  filter(gcc == "Greater Melbourne") %>%   # let's just look Melbourne
  ggplot() +
  geom_sf(aes(geometry = geometry)) +   # use the geometry variable
  geom_point(aes(cent_lat, cent_long))  # use the centroid lat and longs

map

## ------------------------------------------------------------------------
map <- 
sa42016 %>% 
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

# This data contains a variable of sa4 that can be joined to the sa42016 set:
ue_data <- ue_data %>% left_join(sa42016)

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
  filter(gcc == "Greater Melbourne") %>% # still looking at Melbourne
  ggplot() +
  geom_sf(aes(geometry = geometry),      # use the geometry variable
          lwd = 0) +                     # remove borders
  geom_point(aes(cent_lat, 
                 cent_long,
                 colour = ue_pc)) +
  scale_size_continuous(limits = c(.1, 10)) +
  theme_void() +                         # clears other plot elements
  coord_sf(datum = NA)                   # fixes a gridline bug in theme_void()

map

