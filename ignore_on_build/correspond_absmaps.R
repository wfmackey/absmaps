#' Read in ASGC/ASGS population-weighted correspondence tables from the ABS.
#' @name correspond_absmaps
#' @param data The original data.frame or tibble containing  that you want 'corresponded'.
#' @param correspondFrom The variable to correspond from.
#' @param fromArea The original area to correspond from ("sa1", "sa2", etc; see documentation for the full list.)
#' @param fromYear The year to correspond from (2011, 2016, etc; see documentation for full list.)
#' @param toArea The original area to correspond to ("sa1", "sa2", etc; see documentation for the full list.)
#' @param toYear The year to correspond to (2011, 2016, etc; see documentation for full list.)
#' @param nVar The variable you want distributed according to the correspondence ratio.
#' @param summarise TRUE if you would like to collapse your data to be one observation per new area.
#' If FALSE, one observation per original area will be returned.
#' @param groupVar If you would like to group your summary by an additional variable.
#' For example, \code{count} or \code{n} that represent the number of people in a given area.
#'
#'
#' @importFrom rlang :=
#'
#' @return A \code{tibble} object.
#' @examples
#' \dontrun{
#' lga %>%
#' correspond_absmaps(correspondFrom = lga_name_2017,
#'                    nVar = employees,
#'                    fromArea = "lga", fromYear = 2017,
#'                    toArea = "sa3", toYear = 2016,
#'                    groupVar = year)
#' }
#' @export


## quiets concerns of R CMD check re: the out-of-nowhere 'ratio' variable
globalVariables(c("ratio"))


# Note that this should have better flow from get_correspond_absmaps
correspond_absmaps <- function(data,
                               correspondFrom,
                               nVar,
                               fromArea,
                               fromYear,
                               toArea,
                               toYear ,
                               groupVar,
                               summarise = TRUE
                               ) {

  correspondence <- get_correspondence_absmaps(
    fromArea = fromArea,
    fromYear = fromYear,
    toArea   = toArea,
    toYear   = toYear
  )


  # Enquote nVar for tidyeval
  nVar <- dplyr::enquo(nVar)
  groupVar <- dplyr::enquo(groupVar)

  # Get FROM variable
  fromVarChar <- paste(fromArea, "name", fromYear, sep = "_")
  fromVar <- dplyr::sym(fromVarChar)

  # Get TO variable
  toVarChar <- paste(toArea, "name", toYear, sep = "_")
  toVar <- dplyr::sym(toVarChar)

  # Set up by(c(...)) for join
  join_cols = c(fromVarChar)
  names(join_cols) <- deparse(substitute(correspondFrom))

  d <-
    # This will create a new row for each combination
    dplyr::left_join(data, correspondence,
                     by = join_cols) %>%
    # Re-distribute n evenly according to pc
    dplyr::mutate(q = !!nVar * ratio) %>%
    dplyr::filter(!is.na(!!toVar))

  # Summarise if requested
  if (summarise) {
  d <- d %>%
    # Summarise over the 'to' variable
    dplyr::group_by(!!toVar, !!groupVar) %>%
    dplyr::summarise(q = sum(q))
  }

  # Rename back to originals
  d <- d %>% dplyr::rename(!!nVar := q)

  return(d)

}
