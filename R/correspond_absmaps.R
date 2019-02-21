#' Read in ASGC/ASGS population-weighted correspondence tables from the ABS.
#' @name correspond_absmaps
#' @param data The original data.frame or tibble containing  that you want 'corresponded'.
#' @param fromArea The original area to correspond from ("sa1", "sa2", etc; see documentation for the full list.)
#' @param fromYear The year to correspond from (2011, 2016, etc; see documentation for full list.)
#' @param toArea The original area to correspond to ("sa1", "sa2", etc; see documentation for the full list.)
#' @param toYear The year to correspond to (2011, 2016, etc; see documentation for full list.)
#' @param nVar The variable you want distributed according to the correspondence ratio.
#' For example, \code{count} or \code{n} that represent the number of people in a given area.
#'
#'
#' @return A \code{tibble} object.
#' @examples
#' \dontrun{
#'
#' }
#' @export

# Note that this should have better flow from get_correspond_absmaps
correspond_absmaps <- function(data,
                               fromArea,
                               fromYear,
                               toArea,
                               toYear ,
                               nVar) {

  correspondence <- get_correspondence_absmaps(
    fromArea = fromArea,
    fromYear = fromYear,
    toArea   = toArea,
    toYear   = toYear
  )

  # Enquote nVar for tidyeval
  nVar <- dplyr::enquo(nVar)
  toVar <- paste(toArea, "name", toYear, sep = "_")
  toVar <- dplyr::sym(toVar)


  d <-
    # This will create a new row for each combination
    dplyr::left_join(data, correspondence) %>%
    # Re-distribute n evenly according to pc
    dplyr::mutate(q = !!nVar * ratio) %>%
    # Summarise over the 'to' variable
    dplyr::group_by(!!toVar) %>%
    dplyr::summarise(q = sum(q)) %>%
    # Rename to the original variable name
    dplyr::rename(!!nVar := q)

  return(d)

}
