## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ------------------------------------------------------------------------
library(absmaps)
library(ggplot2)
library(dplyr)


mapdata <- load_absmaps("sa4", year = 2016)


## ------------------------------------------------------------------------
head(mapdata)

## ------------------------------------------------------------------------

mapdata %>% 
  filter(gcc == "Greater Melbourne") %>%   # let's just look Melbourne
  ggplot() +
  geom_sf(aes(geometry = geometry,  # use the geometry variable
              fill = areasqkm),     # fill by area size
          lwd = 0,                  # remove borders
          show.legend = FALSE) +    # remove legend
  theme_void() +                    # clears other plot elements
  coord_sf(datum = NA)              # fixes a gridline bug in theme_void()



