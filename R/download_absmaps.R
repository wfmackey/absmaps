#' Download ABS ASGS and/or ASGC shapefile data, compress and save as an \code{sf} object.
#' @name download_absmaps
#' @param statisticalArea The statistical area you want to load.
#' One or more (combined with \code{c("","","etc")}) of: "sa1",
#' "sa2, "sa3", "sa4", "gcc", "state".
#' @param year The year of the ASGS/ASGC data. Defaults to 2016, but 2011
#' will be useful for older data.
#' @param saveDirectory The path to which your map data is saved. Default
#' is the current working directory.
#' @param mapCompression The compression level of your map data. Default
#' is 0.1 (10\% of original detail), which makes clear, detailed maps.
#' Higher compression leads to greater map file size with (in most cases)
#' little visual benefit.
#' @param removeSourceFiles Remove the original ABS shapefile data after
#' compression. Defaults to TRUE.
#'
#' @importFrom curl has_internet
#'
#' @examples
#' \dontrun{
#' download_absmaps("sa3", "myfile/data")
#' download_absmaps(c("sa3", "gcc"), "myfile/data")
#' }
#' @export



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

    # Tidy save directory
    saveDirectory <- gsub("\\/$", "", saveDirectory)


    # Warn if compression is too low
    if (mapCompression > 0.2) warning(paste0("Note: map compression makes plotting data",
                                             "faster without (substantial) loss of quality.",
                                             "Consider a map compression value of 0.2 or less."))

    # Warn about lengthy sa1 compression time
      if (sum(grepl("sa1", statisticalArea)) > 0) {
        warning("Note that the compression of the SA1 file will take about 10 minutes.")
      }


    # Map all statisticalAreas and all years to the download function:
    purrr::map(statisticalArea,
               year,
               .f = download_abs_shapefiles,
               saveDirectory1 = saveDirectory,
               removeSourceFiles1 = removeSourceFiles)

    # Map all statisticalAreas and all years to the read and compress function:
    purrr::map(statisticalArea,
               year,
               .f = read_and_compress_shapefile,
               saveDirectory1 = saveDirectory,
               removeSourceFiles1 = removeSourceFiles,
               mapCompression1 = mapCompression)

}
