

# Reading correspondences

  # Using 'ASGS Correspondences (2016) - 2016 Population Weighted(ZIP)' from
  # https://data.gov.au/dataset/23fe168c-09a7-42d2-a2f9-fd08fbd0a4ce/resource/951e18c7-f187-4c86-a73f-fcabcd19af16/download/asgs2016_2016gridcorrespondences.zip


library(readr)
library(dplyr)
  # Note the pattern is:
    # CG_FROM_FROMYEAR_TO_TOYEAR.xlsx

convert_correspondences <- function(filename, onetomany = TRUE) {
  
  print(paste0("Processing ", filename))
  
datapath <- "data/asgs2016_2016gridcorrespondences/"

sheetnumber <- 4

# The state-based mesh block tables are on sheet 5 because of course they are.
states <- c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")

state_mb <- sum(
  grepl(paste0(states[1], "\\_MB"), filename),
  grepl(paste0(states[2], "\\_MB"), filename),
  grepl(paste0(states[3], "\\_MB"), filename),
  grepl(paste0(states[4], "\\_MB"), filename),
  grepl(paste0(states[5], "\\_MB"), filename),
  grepl(paste0(states[6], "\\_MB"), filename),
  grepl(paste0(states[7], "\\_MB"), filename),
  grepl(paste0(states[8], "\\_MB"), filename)
)

if (state_mb) sheetnumber <- 5

data <-
readxl::read_xls(paste0(datapath, filename),
                 sheet = sheetnumber,   # data are always kept in 'Table 3' on sheet 4.
                 skip = 5) %>%  # variable names are on line 6
        filter(!is.na(RATIO)) %>%  # remove all non-ratio observations
        select(-starts_with("PERCENT")) %>% 
        # Filter repeat columns
        select(-matches("\\.\\.2")) %>% 
        select(-matches("\\.\\.4"))

names(data) <- gsub("\\.\\.[0-9]", "", names(data))

names(data) <- tolower(names(data))

if (onetomany) {
 
firstvar <- names(data)[1]
names(data)[1] <- "HOLD"

# Keep 'best match' for one-to-one conversion
data <- data %>% 
        group_by(HOLD) %>% 
        filter(ratio == max(ratio)) %>% 
        ungroup()

names(data)[1] <- firstvar
}


outputname <- paste0(gsub("\\.xls", "", filename), ".rds")

write_rds(data, outputname, compress = "gz")

}

files <- list.files(datapath)

purrr::map(files, .f = convert_correspondences, onetomany = FALSE)


read_rds("data/rds/CG_MB_2011_STATE_2016.rds")
read_rds("data/rds/CG_CD_2006_POA_2016.rds")

