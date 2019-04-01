#' Download ABS ASGS shapefile data, compress and save
#' as an \code{sf} object.
#'
#' @name download_absmaps
#' @param area The area you want to download/convert/compress. One of:
#' "sa1", "sa2, "sa3", "sa4", "gcc", "state", "ra" (remoteness area),
#' "mesh_[STATE]" (eg mesh_nsw, mesh_tas, mesh_wa), "indigenous_locations", 
#' "indigenous_area", "indigenous_regions", "ced" (Commonwealth electoral 
#' district), "sed" (State electoral district), "lga" (local government 
#' area) 
#' @param year The year of the ASGS data. Defaults to 2016, but 2011 will
#' be useful for older data.
#' @param saveDirectory The path to which your map data is saved.
#' @param mapCompression The compression level of your map data. Default
#' is 0.1 -- 10 per cent of original detail -- which makes clear, detailed maps. Higher
#' compression leads to greater map file size with, in most cases, little visual benefit.
#' Map compression can be set to "off".
#' @param removeSourceFiles Remove the original ABS shapefile data after
#' compression. Defaults to TRUE.
#'
#' @importFrom dplyr filter mutate_if mutate_at vars matches rename "%>%"
#' @importFrom readr write_rds
#' @importFrom curl has_internet
#' @importFrom utils download.file unzip
#' @importFrom sf st_read st_coordinates st_centroid
#' @importFrom rmapshaper ms_simplify
#' @importFrom purrr map2
#'
#' @examples
#' \dontrun{ 
#' download_absmaps("state", 2016, saveDirectory = "data")
#' }
#' @export

## quiets concerns of R CMD check re: the out-of-nowhere 'X' and 'Y' variables
globalVariables(c("X", "Y"))


download_absmaps <- function(area,
                             year = 2016,
                             saveDirectory,
                             mapCompression = .1,
                             removeSourceFiles = TRUE) {

  # Check if there is internet connection
  if(!curl::has_internet()) {
    stop("Oop -- you are not connected to the internet to download ABS map data.")
    }

  # Check if area is appropriate
  valid_areas <- c("sa1", "sa2", "sa3", "sa4", "gcc", "state", "ra",
                   "mesh_",
                   "indigenous_locations", "indigenous_area", "indigenous_regions",
                   "lga", "sed", "ced", "postal")

  input_area_to_check <- area
  input_area_to_check <- gsub("mesh_.*", "mesh_", input_area_to_check)


  if (!input_area_to_check %in% valid_areas) {
    valid_areas_char <- paste(valid_areas, collapse = ', ')

    stop(
      paste0("Woah you have an invalid (or not yet implemented) statistical code, '",
             input_area_to_check[!input_area_to_check %in% valid_areas],
             "'. See the documentation, or try one of :",
             valid_areas_char)
    )
  }


  # Warn if compression is too low
  if (mapCompression != "off" && mapCompression > 0.2) {
    warning(paste("Note: map compression makes plotting data faster without (substantial)",
                  "loss of quality. Consider a map compression value of 0.2 or less.",
                  "But, I'm a warning, not a cop: proceed as you want.")
            )
  }

  # Tidy save directory
  saveDirectory <- gsub("\\/$", "", saveDirectory)

  # Warn of compression time if downloading SA1
  if (grepl("(sa1)|(mesh_)", area)) {
    warning("Note that the compression of mesh block and SA1 files will take AGES (about 10 minutes on my testing. You may well have a better computer).")
  }


  # URLs for ASGS shapefile data

    # 2016 ASGS
    mesh_nsw2016_url   <-  "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_mb_2016_nsw_shape.zip&1270.0.55.001&Data%20Cubes&E9FA17AFA7EB9FEBCA257FED0013A5F5&0&July%202016&12.07.2016&Latest"
    mesh_vic2016_url   <-  "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_mb_2016_vic_shape.zip&1270.0.55.001&Data%20Cubes&04F12B9E465AE765CA257FED0013B20F&0&July%202016&12.07.2016&Latest"
    mesh_qld2016_url   <-  "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_mb_2016_qld_shape.zip&1270.0.55.001&Data%20Cubes&A17EA45AB7CC5D5CCA257FED0013B7F6&0&July%202016&12.07.2016&Latest"
    mesh_sa2016_url   <-   "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_mb_2016_sa_shape.zip&1270.0.55.001&Data%20Cubes&793662F7A1C04BD6CA257FED0013BCB0&0&July%202016&12.07.2016&Latest"
    mesh_wa2016_url   <-  "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_mb_2016_wa_shape.zip&1270.0.55.001&Data%20Cubes&2634B61773C82931CA257FED0013BE47&0&July%202016&12.07.2016&Latest"
    mesh_tas2016_url   <-  "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_mb_2016_tas_shape.zip&1270.0.55.001&Data%20Cubes&854152CB547DE707CA257FED0013C180&0&July%202016&12.07.2016&Latest"
    mesh_nt2016_url   <-  "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_mb_2016_nt_shape.zip&1270.0.55.001&Data%20Cubes&31364C9DFE4CC667CA257FED0013C4F6&0&July%202016&12.07.2016&Latest"
    mesh_act2016_url   <-  "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_mb_2016_act_shape.zip&1270.0.55.001&Data%20Cubes&21B8D5684405A2A7CA257FED0013C567&0&July%202016&12.07.2016&Latest"
    mesh_other2016_url   <-  "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_mb_2016_ot_shape.zip&1270.0.55.001&Data%20Cubes&9001CEC5D0573AF4CA257FED0013C5F0&0&July%202016&12.07.2016&Latest"

    sa12016_url <-   "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa1_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&6F308688D810CEF3CA257FED0013C62D&0&July%202016&12.07.2016&Latest"
    sa22016_url <-   "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa2_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&A09309ACB3FA50B8CA257FED0013D420&0&July%202016&12.07.2016&Latest"
    sa32016_url <-   "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa3_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&43942523105745CBCA257FED0013DB07&0&July%202016&12.07.2016&Latest"
    sa42016_url <-   "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_sa4_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&C65BC89E549D1CA3CA257FED0013E074&0&July%202016&12.07.2016&Latest"
    gcc2016_url <-   "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_gccsa_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&FD348608563DBFEACA257FED0013E500&0&July%202016&12.07.2016&Latest"
    state2016_url <- "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055001_ste_2016_aust_shape.zip&1270.0.55.001&Data%20Cubes&65819049BE2EB089CA257FED0013E865&0&July%202016&12.07.2016&Latest"

    ra2016_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055005_ra_2016_aust_shape.zip&1270.0.55.005&Data%20Cubes&ACAA23F3B41FA7DFCA258251000C8004&0&July%202016&16.03.2018&Latest"

    indigenous_locations2016_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055002_iloc_2016_aust_shape.zip&1270.0.55.002&Data%20Cubes&8EBE82D4E1C6990CCA25802C0013FB58&0&July%202016&13.09.2016&Latest"
    indigenous_area2016_url      <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055002_iare_2016_aust_shape.zip&1270.0.55.002&Data%20Cubes&97C87A1ECEF91649CA25802C00140050&0&July%202016&13.09.2016&Latest"
    indigenous_regions2016_url   <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055002_ireg_2016_aust_shape.zip&1270.0.55.002&Data%20Cubes&DC766D359CEDF5ECCA25802C00140496&0&July%202016&13.09.2016&Latest"

    # 2011 ASGS
    mesh_nsw2011_url   <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_mb_2011_nsw_shape.zip&1270.0.55.001&Data%20Cubes&24D5D6CF2AE953EECA257801000C80C3&0&July%202011&23.12.2010&Latest"
    mesh_vic2011_url   <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_mb_2011_vic_shape.zip&1270.0.55.001&Data%20Cubes&85F5B2ED8E3DC957CA257801000CA953&0&July%202011&23.12.2010&Latest"
    mesh_qld2011_url   <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_mb_2011_qld_shape.zip&1270.0.55.001&Data%20Cubes&28231DC5E634B991CA257801000CC024&0&July%202011&23.12.2010&Latest"
    mesh_sa2011_url   <-  "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_mb_2011_sa_shape.zip&1270.0.55.001&Data%20Cubes&80D17614D3554120CA257801000CCDCF&0&July%202011&23.12.2010&Latest"
    mesh_wa2011_url   <-  "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_mb_2011_wa_shape.zip&1270.0.55.001&Data%20Cubes&E1F9D7EA5D7FE609CA257801000CD692&0&July%202011&23.12.2010&Latest"
    mesh_tas2011_url   <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_mb_2011_tas_shape.zip&1270.0.55.001&Data%20Cubes&2A51C083AC8C54F5CA257801000CE3B5&0&July%202011&23.12.2010&Latest"
    mesh_nt2011_url   <-  "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_mb_2011_nt_shape.zip&1270.0.55.001&Data%20Cubes&28330E3BC3BD9664CA257801000CED32&0&July%202011&23.12.2010&Latest"
    mesh_act2011_url   <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_mb_2011_act_shape.zip&1270.0.55.001&Data%20Cubes&1911A225354236D2CA257801000CEECB&0&July%202011&23.12.2010&Latest"
    mesh_other2011_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_mb_2011_ot_shape.zip&1270.0.55.001&Data%20Cubes&2DCDFCEB2683BAE8CA257801000CEFE7&0&July%202011&23.12.2010&Latest"


    sa12011_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_sa1_2011_aust_shape.zip&1270.0.55.001&Data%20Cubes&24A18E7B88E716BDCA257801000D0AF1&0&July%202011&23.12.2010&Latest"
    sa22011_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_sa2_2011_aust_shape.zip&1270.0.55.001&Data%20Cubes&7130A5514535C5FCCA257801000D3FBD&0&July%202011&23.12.2010&Latest"
    sa32011_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_sa3_2011_aust_shape.zip&1270.0.55.001&Data%20Cubes&AD2BD90E5DC0F4C7CA257801000D59E3&0&July%202011&23.12.2010&Latest"
    sa42011_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_sa4_2011_aust_shape.zip&1270.0.55.001&Data%20Cubes&B18D49356F3FDA5FCA257801000D6D2E&0&July%202011&23.12.2010&Latest"
    gcc2011_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_gccsa_2011_aust_shape.zip&1270.0.55.001&Data%20Cubes&E0FC2223AF731E0ACA257801000D7B54&0&July%202011&23.12.2010&Latest"
    state2011_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055001_ste_2011_aust_shape.zip&1270.0.55.001&Data%20Cubes&1D26EC44E6ABD911CA257801000D8779&0&July%202011&23.12.2010&Latest"

    ra2011_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055005_ra_2001_aust_shape.zip&1270.0.55.005&Data%20Cubes&C712776994895856CA257B03000D7599&0&July%202011&31.01.2013&Latest"

    indigenous_locations2011_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055002_iloc_2011_aust_shape.zip&1270.0.55.002&Data%20Cubes&6EE37FF7D6F9B0A8CA2579A70017A494&0&July%202011&20.02.2012&Latest"
    indigenous_area2011_url      <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055002_iare_2011_aust_shape.zip&1270.0.55.002&Data%20Cubes&30B219BC2C1BA5DDCA2579A7001790DC&0&July%202011&20.02.2012&Latest"
    indigenous_regions2011_url   <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055002_ireg_2011_aust_shape.zip&1270.0.55.002&Data%20Cubes&5C2F21B87633B4B6CA2579A70017B643&0&July%202011&20.02.2012&Latest"


    # URLs for non-ASGS structures
    lga2016_url <- "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055003_lga_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&7951843398FB3F4ECA25833D000EAE34&0&July%202016&07.11.2018&Previous"
    ced2016 <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_ced_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&447BE1AE2E3E7A3ACA25802C00144C3C&0&July%202016&13.09.2016&Previous"
    postal2016_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_poa_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&4FB811FA48EECA7ACA25802C001432D0&0&July%202016&13.09.2016&Previous"
    lga2018_url <- "http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055003_lga_2018_aust_shape.zip&1270.0.55.003&Data%20Cubes&FCDD3670BE71AA90CA258339000D8477&0&July%202018&05.11.2018&Latest"
    sed2018_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_sed_2018_aust_shp.zip&1270.0.55.003&Data%20Cubes&A340810D128D5531CA2582DA00118272&0&July%202018&31.07.2018&Latest"
    ced2018_url <- "http://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_ced_2018_aust_shp.zip&1270.0.55.003&Data%20Cubes&BF4D23C712D492CFCA2582F600180556&0&July%202018&28.08.2018&Latest"




    # Download
    message(paste0("Downloading ", area, year, " data from abs.gov.au"))

    utils::download.file(get(paste0(area, year, "_url")),
                         paste0(saveDirectory, "/", area, year, ".zip"),
                         "auto")

    # Unzip and clean up
    utils::unzip(paste0(saveDirectory, "/", area, year, ".zip"), exdir = paste0(saveDirectory, "/", area, year))

    if (removeSourceFiles) file.remove(paste0(saveDirectory, "/", area, year, ".zip"))

    # Read shapefile
    if (!grepl("mesh_", area)) {

      prefix <- dplyr::case_when(
        area == "state" ~ "ste",
        area == "gcc"   ~ "gccsa",
        area == "postal"   ~ "poa",
        TRUE ~ area
      )

      message(paste0("Reading ", area, year, " shapefile"))
      shape <- sf::st_read(paste0(saveDirectory, "/", area, year, "/", toupper(prefix), "_", year, "_AUST.shp"))

    }

    if (grepl("mesh_", area)) {
    sF_name <- gsub("mesh_([a-z]{2,5})([0-9]{4})", "MB_\\2_\\1", paste0(area, year)) %>% toupper()

    message(paste0("Reading ", area, year, " shapefile"))
    shape <- sf::st_read(paste0(saveDirectory, "/", area, year, "/", sF_name, ".shp"))

    }



    if (mapCompression != "off") {
    # Compress shapefile
    message(paste0("Compressing ", area, year, " shapefile to ",
                   mapCompression, " (", mapCompression * 100,
                   "% of original detail)"))

    shape <- rmapshaper::ms_simplify(shape, keep = mapCompression, keep_shapes = TRUE)

    message("Compressed")
    }

    # Set up directory
    data_loc <- path.expand(paste0(saveDirectory, "/absmaps"))

    if (!dir.exists(data_loc)) dir.create(data_loc)

    data_loc_sa <- paste0(data_loc, "/", area, year)

    if (!dir.exists(data_loc_sa)) dir.create(data_loc_sa)

    # Rename data to be consistent with correspondence tables
    names(shape) <- tolower(names(shape))
    names(shape) <- gsub("11", "_2011", names(shape))
    names(shape) <- gsub("16", "_2016", names(shape))
    names(shape) <- gsub("18", "_2018", names(shape))


    # Rename ste to state for ease of use (no state correspondences tables...yet)
    names(shape) <- gsub("ste", "state", names(shape))

    # Some 2011 use albers_sqm instead of albers_sqkm; convert
    if (sum(grepl("sqm", names(shape))) > 0) {
      message("Converting albers sqm to sqkm for consistency with 2016 files")
      shape$albers_sqm <- shape$albers_sqm * 1e-6
      names(shape) <- gsub("sqm", "sqkm", names(shape))
    }


    # Convert factors to characters, and numbers to numerics
    shape <- dplyr::mutate_if(shape, is.factor, as.character)
    # shape <- dplyr::mutate_at(shape, dplyr::vars(dplyr::matches("[^g][^c][^c]\\code")), as.numeric)


    # Add centroids
    suppressWarnings(
    shape <- cbind(shape, sf::st_coordinates(sf::st_centroid(shape)))
    )

    shape <- dplyr::rename(shape,
                           cent_lat = X,
                           cent_long = Y)


    # Write data
    data_loc_sa <- paste0(data_loc_sa, "/", area, year, ".rds")

    message(paste0("Writing ", area, year, " sf object to ", data_loc_sa))

    readr::write_rds(shape, data_loc_sa)

    message("Done")

    if(removeSourceFiles) {
      message("Removing source files")
      unlink(paste0(saveDirectory, "/", area, year), recursive = TRUE)
      message("Done")
    }

    message(paste0(area, year, ".rds has been downloaded, cleaned and stored in ",
                   data_loc_sa))


}
