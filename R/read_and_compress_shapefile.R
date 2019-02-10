#' Read and compress ABS ASGS and/or ASGC shapefile
#' data, compress and save as an \code{sf} object.
#'
#' Currently, this function will only work inside
#' \code{download_absmaps} and \code{load_absmaps}.
#'
#' @name read_and_compress_shapefile
#'
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate_if mutate_at vars matches rename "%>%"
#' @importFrom readr write_rds
#' @importFrom curl has_internet
#' @importFrom utils download.file unzip
#' @importFrom sf st_read st_coordinates st_centroid
#' @importFrom rmapshaper ms_simplify
#'
#' @export


read_and_compress_shapefile <- function(statisticalArea1,
                                        year1,
                                        saveDirectory1 = saveDirectory,
                                        mapCompression1 = mapCompression,
                                        removeSourceFiles1 = removeSourceFiles
                                        ) {

  message(paste0("Reading ", statisticalArea1, year1, " shapefile"))

  if (statisticalArea1 == "state")               prefix <- "ste"
  if (statisticalArea1 == "gcc")                 prefix <- "gccsa"
  if (statisticalArea1 != "state" && statisticalArea1 != "gcc") prefix <- statisticalArea1

  shape <- sf::st_read(paste0(saveDirectory1, "/",
                              statisticalArea1, year1, "/",
                              toupper(prefix), "_", year1, "_AUST.shp"))

  message(paste0("Compressing ", statisticalArea1, year1, " shapefile to ",
                 mapCompression1, " (", mapCompression1 * 100,
                 "% of original detail)"))

  shape <- rmapshaper::ms_simplify(shape, keep = mapCompression1, keep_shapes = TRUE)

  message("Compressed")

  data_loc <- path.expand(paste0(saveDirectory1, "/absmaps"))
  if (!dir.exists(data_loc)) dir.create(data_loc)
  data_loc_sa <- paste0(data_loc, "/", statisticalArea1, year1)
  if (!dir.exists(data_loc_sa)) dir.create(data_loc_sa)

  # Rename data
  names(shape) <- gsub(year1, "", tolower(names(shape)))
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
  shape <- dplyr::mutate_if(shape,
                            is.factor,
                            as.character)

  shape <- dplyr::mutate_at(shape,
                            dplyr::vars(dplyr::matches("[^g][^c][^c]\\code")),
                            as.numeric)


  # Add centroids
  suppressWarnings(
    shape <- cbind(shape, sf::st_coordinates(sf::st_centroid(shape))) %>%
      dplyr::rename(cent_lat = X,
                    cent_long = Y)
  )

  # Write data
  data_loc_sa <- paste0(data_loc_sa, "/", statisticalArea1, year1, ".rds")

  message(paste0("Writing ", statisticalArea1, year1, " sf object to ", data_loc_sa))

  readr::write_rds(shape, data_loc_sa)

  message("Done")

  if(removeSourceFiles) {
    message("Removing source files")
    unlink(paste0(saveDirectory1, "/", statisticalArea1, year1), recursive = TRUE)
    message("Done")
  }

  message(paste0(statisticalArea1, year1, ".rds has been downloaded, cleaned and stored in ",
                 data_loc_sa))

}
