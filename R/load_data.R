#' Get toy Portal data
#' Get an arbitrary year of Portal control plot data - for use in developing & testing methods, not for actual analysis
#' @return Portal control rodents
#' @param years vector of years to pull
#' @param download T/F, download or load from working_data/toy_portal
#' @export
#' @importFrom portalr summarise_individual_rodents
#' @importFrom dplyr filter select
get_toy_portal_data <- function(years = c(1994, 1995), download = F) {

  if(download) {
  portal_data <- portalr::summarise_individual_rodents(clean = T, type = "Granivores", unknowns = F, time = "date")
  } else {
    portal_data <- read.csv(here::here("working-data", "toy_portal", "portal_all.csv"), stringsAsFactors = F)
    }

  portal_data = portal_data %>%
    dplyr::filter(year %in% years, treatment == "control", !is.na(wgt)) %>%
    dplyr::select(species, wgt) %>%
    dplyr::mutate(species = as.character(species))

  return(portal_data)
}

# Load Ernest 2005 data (future)


# Load NEON data (future, or leave this in neonbecs)
