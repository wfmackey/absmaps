source("R/download_ABSmaps.R")
source("R/load_ABSmaps.R")


#--- Read and show ----
library(tidyverse)
library(tidylog)
library(ggthemes)
library(grattantheme)

my_data <- read_csv("data/IER_by_sa2.csv")

sa2geometry <- load_ABSmaps("sa2")

sa2_data <- left_join(my_data, sa2geometry)

p <-
  sa2_data %>%
  filter(gcc_code == "1GSYD") %>%
  ggplot(aes(fill = ier)) +
  geom_sf(lwd = 0) +
  coord_sf(datum=NA) +
  scale_fill_gradientn(name = "",
                       colours = c(grattan_lightyellow, grattan_lightorange, grattan_red),
                       values = scales::rescale(c(700, 900, 1100, 1300)),
                       # limits = c(0,70),
                       na.value = "grey90") +
  theme_void() +
  NULL

p

ggsave("atlas/map.pdf", height = 10, width = 10)

order <- c("Australia",
           "Brisbane",
           "Sydney",
           "Canberra",
           "Perth",
           "Adelaide",
           "Melbourne")

grid <- c(1, 1, 1, 1, 1, 2, 2, 0,
          1, 1, 1, 1, 1, 2, 2, 0,
          1, 1, 1, 1, 1, 0, 3, 3,
          1, 1, 1, 1, 1, 4, 3, 3,
          5, 5, 6, 6, 0, 7, 7, 7,
          5, 5, 6, 6, 0, 7, 7, 7)


# bin ----------------------------------------

statisticalArea = "sa4"
x = statisticalArea
saveDirectory = "~/Documents/test_ABS"
download = TRUE
mapCompression = .1
removeSourceFiles = TRUE

source("../R/download_ABS.R")
MAP <- load_ABSmaps(statisticalArea,
                    saveDirectory,
                    download = TRUE)

