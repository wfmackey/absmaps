# This function loads ABS maps (with the option to download them, too)
# The download capability is a simple wrapper around download_absmaps

load_absmaps <- function(statisticalArea,
                         saveDirectory = ".",
                         download = TRUE,
                         mapCompression = .1,
                         removeSourceFiles = TRUE
                         ) {

if (download) {
  download_absmaps(statisticalArea,
                   saveDirectory = saveDirectory,
                   mapCompression = mapCompression,
                   removeSourceFiles = removeSourceFiles)
}

path <- paste0(saveDirectory, "/absmaps")

read_rds(paste0(path, "/",
                statisticalArea, "/",
                statisticalArea, ".rds"))

}
