#' Download and load ABS ASGS shapefile data, compress and save as an \code{sf} object.
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

  # Checks
  if (length(statisticalArea) > 1) {
    stop(paste("Sorry, you can't load more than one file at a time.",
               "But you can _download_ more than one at a time",
               "using `download_abs()`"))
  }

  if (mapCompression <= 0 | mapCompression > 1) {
    stop(paste("Map compression needs to be in the range (0, 1].",
               "For example, 0.1 compresses to 10% of the file size.",
               "Your compression value of ", mapCompression,
               "is outside that range."))
  }

if (download) {

  this_message <- paste0("Downloading and processing your ", statisticalArea,
                         year, " sf object")

  print(this_message)

  download_absmaps(statisticalArea,
                   year = year,
                   saveDirectory = saveDirectory,
                   mapCompression = mapCompression,
                   removeSourceFiles = removeSourceFiles)

  this_message <- paste0(
    "Download and processing complete.",
    "Reading your ", statisticalArea, year,
    " sf object")

    print(this_message)
}

if (!download) {
  this_message <- paste0(
    "Reading your ", statisticalArea, year,
    " sf object")

  print(this_message)

}


    path <- paste0(saveDirectory,
                   "/absmaps/",
                    statisticalArea, year, "/",
                    statisticalArea, year, ".rds")


    print(path)
    readr::read_rds(path)

}

