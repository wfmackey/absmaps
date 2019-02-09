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

head(mapdata)


## ------------------------------------------------------------------------

mapdata %>% 
  filter(state == "Victoria") %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry,
              fill = areasqkm),     # fill by area size
          lwd = 0,                  # remove borders
          show.legend = FALSE) +    # remove legend
  theme_void() +                    # clears other plot elements
  coord_sf(datum = NA)              # fixes a gridline bug in theme_void()



## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

