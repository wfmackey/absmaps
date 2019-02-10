#' Download and load ABS ASGS and/or ASGC shapefile data, compress and save as an \code{sf} object.
#' @name load_absmaps
#' @param statisticalArea The statistical area you want to load. One of: "sa1", "sa2, "sa3", "sa4", "gcc", "state".
#' @param download Download ASGS/ASGC data from the ABS. Default is TRUE. Set to FALSE if data are already downloaded and compressed.
#' @inheritParams download_absmaps
#' @return A \code{sf} object containing statistical area name, number and geometry; the size of the area; and higher-level statistical areas (eg the state and gcc of an sa2).
#' @examples
#' \dontrun{
#' sa4_sf <- load_absmaps("sa4", "myfile/data")
#' }
#' @export

load_absmaps <- function(statisticalArea,
                         year = 2016,
                         saveDirectory = ".",
                         download = TRUE,
                         mapCompression = .1,
                         removeSourceFiles = TRUE
                         ) {

if (download) {
  download_absmaps(statisticalArea,
                   year = year,
                   saveDirectory = saveDirectory,
                   mapCompression = mapCompression,
                   removeSourceFiles = removeSourceFiles)
}



  read_absmaps <- function(statisticalArea, year) {

    this_message <- paste0("Reading your ", statisticalArea,
                           year, " sf object")

    print(this_message)

    path <- paste0(saveDirectory,
                   "/absmaps/",
                    statisticalArea, year, "/",
                    statisticalArea, year, ".rds")

    assign(paste0(statisticalArea, year),
           readr::read_rds(path),
           envir = .GlobalEnv)

  }

  paths <- purrr::map2(statisticalArea,
                       year,
                       .f = read_absmaps)

}
