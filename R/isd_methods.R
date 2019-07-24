#' Wrapper for get_id
#'
#' @param datasets list of datasets
#'
#' @return list of ids
#' @export
#'
get_id_wrapped <- function(datasets) {
  ids <- lapply(datasets, get_id)
  return(ids)
}

#' Get id from community data
#'
#' @param dataset list of community data, pars
#'
#' @return list of id, pars
#' @export
get_id <- function(dataset) {
  isd <- make_isd(dataset$community)

  gmm <- fit_gmm(isd)

  id <- get_integrated_density(gmm)

  id <- list(integrated_density = id,
               pars = dataset$pars)
  return(id)
}

#' @title Make ISD
#' @param community_dat community df with energy & sizeclass columns added
#' @return ISD table
#' @export
make_isd <- function(community_dat){
  this_isd <- community_dat %>%
    dplyr::mutate(ln_size = log(wgt))

  return(this_isd)
}



#' Fit GMM to individual size distribution
#' Using `mclust::Mclust`
#' @param isd result of `make_isd`
#' @return mclust fit
#' @export
#' @importFrom mclust Mclust mclustBIC mclust.options emControl densityMclust
#' @importFrom dplyr filter
fit_gmm <- function(isd){
  library(mclust)
  isd <- dplyr::filter(isd, !is.na(ln_size))
  this_fit <- mclust::densityMclust(isd$ln_size, G = 1:15, modelNames = "V",
                                    prior = NULL,
                                    control = emControl(),
                                    initialization = NULL,
                                    warn = mclust.options("warn"),
                                    x =  NULL,
                                    verbose = FALSE)
  return(this_fit)
}

#' Integrate density based on GMM
#' @param gmm result of `fit_gmm`
#' @param interval_size width, in log units, of intervals to integrate over
#' @param min_size minimum of window to integrate - v. v. small
#' @param max_size maximum of window to integrate - v. v. large compared to rodents
#' @return dataframe with density, turnpoints marked
#' @export
#' @importFrom stats predict
#' @importFrom dplyr mutate
#' @importFrom pastecs turnpoints
get_integrated_density <- function(gmm, interval_size = 0.0001, min_size = 0, max_size = 8) {

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

  return(integrated_density)
}
