#' Download ABS ASGS and/or ASGC shapefile data, compress and save as an \code{sf} object.
#' @name download_absmaps
#' @param statisticalArea The statistical area you want to load. One or more (combined with \code{c("","","etc")}) of: "sa1", "sa2, "sa3", "sa4", "gcc", "state".
#' @param year The year of the ASGS/ASGC data. Defaults to 2016, but 2011 will be useful for older data.
#' @param saveDirectory The path to which your map data is saved. Default is the current working directory.
#' @param mapCompression The compression level of your map data. Default is 0.1 (10\% of original detail), which makes clear, detailed maps. Higher compression leads to greater map file size with (in most cases) little visual benefit.
#' @param removeSourceFiles Remove the original ABS shapefile data after compression. Defaults to TRUE.
#' @examples
#' \dontrun{
#' download_absmaps("sa3", "myfile/data")
#' download_absmaps(c("sa3", "gcc"), "myfile/data")
#' }
#' @export


# library(readr)
# library(sf)
# library(rmapshaper) # ms_simplify

download_absmaps <- function(statisticalArea,
                             year = 2016,
                             saveDirectory = ".",
                             mapCompression = .1,
                             removeSourceFiles = TRUE) {

  # Check if there is internet connection
  if(!curl::has_internet()) stop("Oop -- you are not connected to the internet to download ABS map data.")

  # Check if statisticalArea is appropriate
  valid_areas <- c("sa1", "sa2", "sa3", "sa4", "gcc", "state", "ra")

  if (mean(statisticalArea %in% valid_areas) < 1) {
    valid_areas_char <- paste(valid_areas, collapse = ', ')

    stop(
      paste0("Woah you have an invalid (or not yet implemented) statistical code, '",
             statisticalArea[!statisticalArea %in% valid_areas],
             "'. Try one of ",
             valid_areas_char)
    )
  }


  # Warn if compression is too low
  if (mapCompression > 0.2) warning("Note: map compression makes plotting data faster without (substantial) loss of quality. Consider a map compression value of 0.2 or less.")

  # Tidy save directory
  saveDirectory <- gsub("\\/$", "", saveDirectory)

  # # Set all of FALSE
  # sa1 = FALSE
  # sa2 = FALSE
  # sa3 = FALSE
  # sa4 = FALSE
  # gcc = FALSE
  # state = FALSE
  # ra = FALSE
  #
  # # Set requested areas to TRUE:
  # if (sum(grepl("sa1", statisticalArea)) > 0)  sa1 = TRUE
  # if (sum(grepl("sa2", statisticalArea)) > 0)  sa2 = TRUE
  # if (sum(grepl("sa3", statisticalArea)) > 0)  sa3 = TRUE
  # if (sum(grepl("sa4", statisticalArea)) > 0)  sa4 = TRUE
  # if (sum(grepl("gcc", statisticalArea)) > 0)  gcc = TRUE
  # if (sum(grepl("state", statisticalArea)) > 0)  state = TRUE
  # if (sum(grepl("ra", statisticalArea)) > 0)   ra = TRUE

  if (sum(grepl("sa1", statisticalArea)) > 0) {
    warning("Note that the compression of the SA1 file will take about 10 minutes.")
  }


  get_absmaps_and_save <- function(x, year) {

    # URLs for ABS shapefile data
    sa12016_url <-   "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa1_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&6F308688D810CEF3CA257FED0013C62D&0&July%202016&12.07.2016&Latest"
    sa22016_url <-   "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa2_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&A09309ACB3FA50B8CA257FED0013D420&0&July%202016&12.07.2016&Latest"
    sa32016_url <-   "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa3_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&43942523105745CBCA257FED0013DB07&0&July%202016&12.07.2016&Latest"
    sa42016_url <-   "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa4_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&C65BC89E549D1CA3CA257FED0013E074&0&July%202016&12.07.2016&Latest"
    gcc2016_url <-   "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_gccsa_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&FD348608563DBFEACA257FED0013E500&0&July%202016&12.07.2016&Latest"
    state2016_url <- "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_ste_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&65819049BE2EB089CA257FED0013E865&0&July%202016&12.07.2016&Latest"

    ra2016_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055005_ra_2016_aust_shape.zip&1270.0.55.005&Data%20Cubes&ACAA23F3B41FA7DFCA258251000C8004&0&July%202016&16.03.2018&Latest"

    sa12011_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_sa1_2011_aust_shape.zip&1270.0.55.001&Data%20Cubes&24A18E7B88E716BDCA257801000D0AF1&0&July%202011&23.12.2010&Latest"
    sa22011_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_sa2_2011_aust_shape.zip&1270.0.55.001&Data%20Cubes&7130A5514535C5FCCA257801000D3FBD&0&July%202011&23.12.2010&Latest"
    sa32011_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_sa3_2011_aust_shape.zip&1270.0.55.001&Data%20Cubes&AD2BD90E5DC0F4C7CA257801000D59E3&0&July%202011&23.12.2010&Latest"
    sa42011_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_sa4_2011_aust_shape.zip&1270.0.55.001&Data%20Cubes&B18D49356F3FDA5FCA257801000D6D2E&0&July%202011&23.12.2010&Latest"
    gcc2011_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_gccsa_2011_aust_shape.zip&1270.0.55.001&Data%20Cubes&E0FC2223AF731E0ACA257801000D7B54&0&July%202011&23.12.2010&Latest"
    state2011_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_ste_2011_aust_shape.zip&1270.0.55.001&Data%20Cubes&1D26EC44E6ABD911CA257801000D8779&0&July%202011&23.12.2010&Latest"

    ra2011_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055005_ra_2001_aust_shape.zip&1270.0.55.005&Data%20Cubes&C712776994895856CA257B03000D7599&0&July%202011&31.01.2013&Latest"

    message(paste0("Downloading ", x, year, " data from abs.gov.au"))

    utils::download.file(get(paste0(x, year, "_url")),
                         paste0(saveDirectory, "/", x, year, ".zip"),
                         "auto")

    utils::unzip(paste0(saveDirectory, "/", x, year, ".zip"), exdir = paste0(saveDirectory, "/", x, year))

    if (removeSourceFiles) file.remove(paste0(saveDirectory, "/", x, year, ".zip"))

    if (x == "state")               prefix <- "ste"
    if (x == "gcc")                 prefix <- "gccsa"
    if (x != "state" && x != "gcc") prefix <- x


    message(paste0("Reading ", x, year, " shapefile"))
    shape <- sf::st_read(paste0(saveDirectory, "/", x, year, "/", toupper(prefix), "_", year, "_AUST.shp"))

    message(paste0("Compressing ", x, year, " shapefile to ",
                   mapCompression, " (", mapCompression * 100,
                   "% of original detail)"))

    shape <- rmapshaper::ms_simplify(shape, keep = mapCompression, keep_shapes = TRUE)

    message("Compressed")

    data_loc <- path.expand(paste0(saveDirectory, "/absmaps"))
    if (!dir.exists(data_loc)) dir.create(data_loc)
    data_loc_sa <- paste0(data_loc, "/", x, year)
    if (!dir.exists(data_loc_sa)) dir.create(data_loc_sa)

    # Rename data
    names(shape) <- gsub(year, "", tolower(names(shape)))
    names(shape) <- gsub("ste", "state", names(shape))
    names(shape) <- gsub("_main", "_code", names(shape))


    # Some 2011 use albers_sqm instead of albers_sqkm; convert
    if (sum(grepl("sqm", names(shape))) > 0) {
      message("Converting albers sqm to sqkm for consistency with 2016 files")
      shape$albers_sqm <- shape$albers_sqm * 1e-6
      names(shape) <- gsub("sqm", "sqkm", names(shape))
    }


    # Add to main code
    names(shape) <- gsub("5dig", "shortcode", names(shape))
    names(shape) <- gsub("[0-9]{2}", "", names(shape))
    names(shape) <- gsub("_name", "", names(shape))

    # Convert factors to characters, and numbers to numerics
    shape <- dplyr::mutate_if(shape, is.factor, as.character)
    shape <- dplyr::mutate_at(shape, dplyr::vars(dplyr::matches("[^g][^c][^c]\\code")), as.numeric)


    # Add centroids
    suppressWarnings(
    shape <- cbind(shape, sf::st_coordinates(sf::st_centroid(shape))) %>%
             dplyr::rename(cent_lat = X,
                           cent_long = Y)
    )

    # Write data
    data_loc_sa <- paste0(data_loc_sa, "/", x, year, ".rds")

    message(paste0("Writing ", x, year, " sf object to ", data_loc_sa))

    readr::write_rds(shape, data_loc_sa)

    message("Done")

    if(removeSourceFiles) {
      message("Removing source files")
      unlink(paste0(saveDirectory, "/", x, year), recursive = TRUE)
      message("Done")
    }

    message(paste0(x, year, ".rds has been downloaded, cleaned and stored in ",
                   data_loc_sa))

  }

  # Apply get_absmaps_and_save to each sa and year
  purrr::map(statisticalArea,
             year,
             .f = get_absmaps_and_save)

}
