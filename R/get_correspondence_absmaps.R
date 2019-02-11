#' Read in ASGC/ASGS population-weighted correspondence tables from the ABS.
#' @name get_correspondence_absmaps
#' @param from The area you want to correspond FROM (ie the areas your data are currently in). For example: "sa1", "sa2, "sa3", "sa4".
#' @param fromyear The year you want to correspond FROM. For example: 2011, 2016.
#' @param to The area you want to correspond TO (ie the areas you want your data to be in).
#' @param toyear The year you want to correspond TO.
#' @param type Either 'onetomany' (the default) or 'manytomany'. 'onetomany' filters the observations to keep the best single match for each FROM area.
#' 
#' @return A \code{tibble} object.
#' @examples
#' \dontrun{
#' sa4_corr <- get_correspondence_absmaps("sa4", 2011, "sa4", 2011)
#' }
#' @export

get_correspondence_absmaps <- function(from, fromyear, to, toyear, type = "onetomany") {

  path <- paste0("correspondences/", type, "/")
  
  filename <- paste(
    "CG",
    toupper(from), toupper(fromyear),
    toupper(to), toupper(toyear),
    sep = "_"
  )

readr::read_rds(paste0(path, filename, ".rds"))

}


