#' Download ABS ASGS and/or ASGC shapefiles to a specified path
#'
#' Currently, this function will only work inside
#' \code{download_absmaps} and \code{load_absmaps}.
#'
#' @name download_abs_shapefile
#'
#' @importFrom utils download.file unzip
#'
#' @export



download_abs_shapefiles <- function(statisticalArea1,
                                    year1,
                                    saveDirectory1 = saveDirectory,
                                    removeSourceFiles1 = removeSourceFiles) {

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

  message(paste0("Downloading ", statisticalArea1, year1, " data from abs.gov.au"))

  utils::download.file(get(paste0(statisticalArea1, year1, "_url")),
                       paste0(saveDirectory1, "/", statisticalArea1, year1, ".zip"),
                       "auto")

  utils::unzip(paste0(saveDirectory1, "/", statisticalArea1, year1, ".zip"),
               exdir = paste0(saveDirectory1, "/", statisticalArea1, year1))

  if (removeSourceFiles1) file.remove(paste0(saveDirectory1, "/", statisticalArea1, year1, ".zip"))

}
