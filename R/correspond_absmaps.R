#' Read in ASGC/ASGS population-weighted correspondence tables from the ABS.
#' @name correspond_absmaps
#' @param data The original data.frame or tibble containing  that you want 'corresponded'.
#' @inheritDotParams
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

  # # Need to rename data to fit with ABS standards elsewhere
  # names(correspondence) <- gsub(paste0("_", fromYear), "", names(correspondence))
  #
  # # And grab second 'name' variable name
  # # Surely not the neatest way to do this but works for now
  # second_name_var <- names(corr) %>%
  #   grep("name", .) %>%
  #   max() %>%
  #   names(corr)[.]
  #
  # second_name_var <- enquo(second_name_var)
  #
  #
  # # Enquote nVar for tidyeval
  # nVar <- dplyr::enquo(nVar)
  #
  # # This will create a new row for each combination
  # d <-
  # left_join(data, correspondence) %>%
  #   # Re-distribute n evenly according to pc
  #   mutate(q = !!nVar * ratio) %>%
  #   # Summarise over the 'to' variable
  #   group_by(!!second_name_var) %>%
  #   summarise(q = sum(q)) %>%
  #   # Rename to the original variable name
  #   rename(!!nVar := q)
  #
  # return(d)

}
