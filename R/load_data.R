#' Get toy Portal data
#' Get an arbitrary year of Portal control plot data - for use in developing & testing methods, not for actual analysis
#' @return Portal control rodents
#' @param years vector of years to pull
#' @export
#' @importFrom portalr summarise_individual_rodents
#' @importFrom dplyr filter select
get_toy_portal_data <- function(years = c(1994, 1995)) {
  portal_data <- portalr::summarise_individual_rodents(clean = T, type = "Granivores", unknowns = F, time = "date") %>%
    dplyr::filter(year %in% years, treatment == "control", !is.na(wgt)) %>%
    dplyr::select(species, wgt)

  return(portal_data)
}

# Load Ernest 2005 data (future)


# Load NEON data (future, or leave this in neonbecs)
