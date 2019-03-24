#' Download and load ABS ASGS shapefile data, compress and save as an \code{sf} object.
#' @name load_absmaps
#' @param download Download ASGS data from the ABS. Default is TRUE. Set to FALSE if data are already downloaded and compressed.
#' @inheritParams download_absmaps
#' @return A \code{sf} object containing statistical area name, number and geometry; the size of the area; and higher-level statistical areas (eg the state and gcc of an sa2).
#' @examples
#' \dontrun{
#' sa4_sf <- load_absmaps("sa4", "myfile/data")
#' }
#' @export

load_absmaps <- function(area,
                         year = 2016,
                         saveDirectory = ".",
                         download = TRUE,
                         mapCompression = .1,
                         removeSourceFiles = TRUE
                         ) {

  # Check area length
  if (length(area) > 1) {
    stop(paste("Sorry, you can't _load_ more than one file at a time.",
               "But you can _download_ more than one at a time",
               "using `download_abs()`"))
  }

  # Check map compression
  if (mapCompression != "off") {
  if (mapCompression <= 0 | mapCompression > 1) {
    stop(paste("Map compression needs to be in the range (0, 1].",
               "For example, 0.1 compresses to 10% of the file size.",
               "Your compression value of ", mapCompression,
               "is outside that range."))
  }}


  # If download is specified:
  objectpath <- paste0(saveDirectory,
                       "/absmaps/",
                       area, year, "/",
                       area, year, ".rds")

  if (!download) {
    this_message <- paste0(
      "Reading your ", area, year,
      " sf object from ", objectpath)

    print(this_message)

    if (!file.exists(objectpath)) {
      stop(paste("The file", objectpath, "does not exist.",
                 "You can download it using download = TRUE"))
    }

    # Otherwise, all good to read
    readr::read_rds(objectpath)
  }


  if (download) {

    this_message <- paste0("Downloading and processing your ", area,
                           year, " sf object")

    print(this_message)

    download_absmaps(area,
                     year = year,
                     saveDirectory = saveDirectory,
                     mapCompression = mapCompression,
                     removeSourceFiles = removeSourceFiles)

    this_message <- paste0(
      "Download and processing complete.",
      "Reading your ", area, year,
      " sf object")

      print(this_message)

      readr::read_rds(objectpath)
  }

}


