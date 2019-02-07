

library(readr)
library(sf)
library(rmapshaper) # ms_simplify

download_absmaps <- function(statisticalArea,
                             saveDirectory = "..",  # change when package is built
                             mapCompression = .1, removeSourceFiles = TRUE,
                             ...) {

  # Warn if compression is too low
  if (mapCompression > 0.2) warning("Note: map compression makes plotting data faster without (substantial) loss of quality. Consider a map compression value of 0.2 or less.")

  # Tidy save directory
  saveDirectory <- gsub("\\/$", "", saveDirectory)

  # Set all of 'off'
  sa1 = FALSE
  sa2 = FALSE
  sa3 = FALSE
  sa4 = FALSE
  gcc = FALSE
  state = FALSE


if (sum(grepl("sa1", statisticalArea)) > 0)  sa1 = TRUE
if (sum(grepl("sa2", statisticalArea)) > 0)  sa2 = TRUE
if (sum(grepl("sa3", statisticalArea)) > 0)  sa3 = TRUE
if (sum(grepl("sa4", statisticalArea)) > 0)  sa4 = TRUE
if (sum(grepl("gcc", statisticalArea)) > 0)  gcc = TRUE
if (sum(grepl("state", statisticalArea)) > 0)  state = TRUE

if (sa1) warning("Note that the compression of the SA1 file will take about 10 minutes.")



  # URLs for ABS shapefile data
  sa1_url <-   "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa1_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&6F308688D810CEF3CA257FED0013C62D&0&July%202016&12.07.2016&Latest"
  sa2_url <-   "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa2_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&A09309ACB3FA50B8CA257FED0013D420&0&July%202016&12.07.2016&Latest"
  sa3_url <-   "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa3_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&43942523105745CBCA257FED0013DB07&0&July%202016&12.07.2016&Latest"
  sa4_url <-   "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa4_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&C65BC89E549D1CA3CA257FED0013E074&0&July%202016&12.07.2016&Latest"
  gcc_url <-   "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_gccsa_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&FD348608563DBFEACA257FED0013E500&0&July%202016&12.07.2016&Latest"
  state_url <- "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_ste_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&65819049BE2EB089CA257FED0013E865&0&July%202016&12.07.2016&Latest"


  get_absmaps_and_save <- function(x) {

    message(paste0("Downloading ", x, " data from abs.gov.au"))

    download.file(get(paste0(x, "_url")),
                  paste0(saveDirectory, "/", x, ".zip"),
                  "auto")

    unzip(paste0(saveDirectory, "/", x, ".zip"), exdir = paste0(saveDirectory, "/", x))

    if (removeSourceFiles) file.remove(paste0(saveDirectory, "/", x, ".zip"))

    if (x == "state")               prefix <- "ste"
    if (x == "gcc")                 prefix <- "gccsa"
    if (x != "state" && x != "gcc") prefix <- x


    message(paste0("Reading ", x, " shapefile"))
    shape <- sf::st_read(paste0(saveDirectory, "/", x, "/", toupper(prefix), "_2016_AUST.shp"))


    message(paste0("Compressing ", x, " shapefile to ",
                   mapCompression, " (", mapCompression * 100,
                   "% of original detail)"))

    shape <- rmapshaper::ms_simplify(shape, keep = mapCompression, keep_shapes = T)

    message("Compressed")

    data_loc <- path.expand(paste0(saveDirectory, "/absmaps"))
    if (!dir.exists(data_loc)) dir.create(data_loc)
    data_loc <- path.expand(paste0(saveDirectory, "/absmaps/", x))
    if (!dir.exists(data_loc)) dir.create(data_loc)

    # Rename data
    names(shape) <- gsub("16", "", tolower(names(shape)))
    names(shape) <- gsub("ste", "state", names(shape))
    names(shape) <- gsub("_main", "_code", names(shape))

    if (grepl("[0-9]", names(shape)[1])) {
      org <- names(shape)[1]
      names(shape)[1] <- "MAIN"
      shape$MAIN <- as.numeric(shape$MAIN)
      names(shape)[1] <- org
    }


    data_loc <- paste0(data_loc, "/", x, ".rds")

    message(paste0("Writing ", x, " shapefile to ", data_loc))

    write_rds(shape, data_loc)

    message("Done")

    if(removeSourceFiles) {
      message("Removing source files")
      unlink(paste0(saveDirectory, "/", x), recursive = TRUE)
      message("Done")
    }

    message(paste0(x, ".rds has been downloaded, cleaned and stored in ",
                   data_loc))

  }

  if (sa1) get_absmaps_and_save("sa1")

  if (sa2) get_absmaps_and_save("sa2")

  if (sa3) get_absmaps_and_save("sa3")

  if (sa4) get_absmaps_and_save("sa4")

  if (gcc) get_absmaps_and_save("gcc")

  if (state) get_absmaps_and_save("state")

}


