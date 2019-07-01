#' Integrate density based on GMM
#' @param gmm result of `fit_gmm`
#' @param interval_size width, in log units, of intervals to integrate over
#' @param min_size minimum of window to integrate - v. v. small
#' @param max_size maximum of window to integrate - v. v. large compared to rodents
#' @param type "sim" or "empirical", for collecting results later
#' @param dat_name source dataset, for collecting results later
#' @param stdev if sim, standard deviation of intraspecific body size used to generate sim
#'
#' @return dataframe with density, turnpoints marked
#' @export
#' @importFrom stats predict
#' @importFrom dplyr mutate
#' @importFrom pastecs turnpoints
get_integrated_density <- function(gmm, interval_size = 0.0001, min_size = 0, max_size = 8, type = "sim", dat_name, stdev = NA) {

  density_evalpoints <- seq(min_size, max_size, by = interval_size)

  density_estimates <- predict(gmm, newdata = density_evalpoints)

  integrated_density <- data.frame(start = density_evalpoints[1:length(density_evalpoints) -1],
                                   stop = density_evalpoints[2:length(density_evalpoints)],
                                   start_density = density_estimates[1:length(density_evalpoints) -1]) %>%
    dplyr::mutate(density = start_density * (stop - start),
                  by_max = density / max(density))

  p_turnpoints <- pastecs::turnpoints(integrated_density$by_max)

  pits <- c()
  peaks <- c()

  tppos <- p_turnpoints$tppos
  if(length(tppos) > 0) {
    if(p_turnpoints$firstispeak) {
      peaks <- tppos[ seq(1, length(tppos), by = 2)]
      if(length(tppos) > 1) {
        pits <- tppos[ seq(2, length(tppos), by =2)]
      }
    } else {
      pits <- tppos[ seq(1, length(tppos), by =2)]
      if(length(tppos) > 1) {
        peaks <- tppos[ seq(2, length(tppos), by =2)]
      }
    }
  }
  integrated_density$start_is_peak <- c(1:(length(density_evalpoints) - 1)) %in% peaks
  integrated_density$start_is_pit <- c(1:(length(density_evalpoints) - 1)) %in% pits
  integrated_density$start_is_turnpoint <- (integrated_density$start_is_peak | integrated_density$start_is_pit)
  integrated_density$type = type
  integrated_density$dat_name = dat_name
  integrated_density$stdev = stdev

  return(integrated_density)
}
