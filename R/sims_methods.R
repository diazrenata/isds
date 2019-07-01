#' Get community parameters
#' @param raw_dat community data frame with columns `individual_species_ids`', `individual_sizes`
#'
#' @return list of community parameters for sim draws: `S` = number of species, `N` = number of individuals, `min_size` = minimum body size, `max_size` = maximum body size
#' @export
get_community_pars <- function(raw_dat) {
  S <- length(unique(raw_dat$individual_species_ids))
  N <- nrow(raw_dat)
  min_size <- min(raw_dat$individual_sizes)
  max_size <- max(raw_dat$individual_sizes)
  community_pars <- list(S = S, N = N, min_size = min_size, max_size = max_size)
  return(community_pars)
}

#' Get sim pars
#' Add standard deviations to `community_pars`
#' @param community_pars community_pars from get_community_pars
#' @param stdev stdev to use in sims
#' @return  community_pars with stdev
#' @export
get_sim_pars <- function(community_pars, stdev) {
  sim_pars <- append(community_pars, list(stdev = stdev))
  return(sim_pars)
}

#' Draw SAD from logseries given S and N
#'
#' @param community_pars result of get_sim_pars
#'
#' @return draw from the logseries SAD predicted by METE
#' @export
#' @importFrom meteR meteESF sad
draw_sad <- function(community_pars) {

  maxentESF <- meteR::meteESF(S0 = community_pars$S, N0 = community_pars$N)

  maxentSAD <- meteR::sad(maxentESF)

  simsad <- NULL
  sum_to_n <- FALSE
    while(!sum_to_n) {
      simsad <- maxentSAD$r(community_pars$S)
      if(sum(simsad) == community_pars$N) {
       sum_to_n <- TRUE
      }
    }

  return(simsad)
}

#' Draw BSD from uniform
#'
#' @param community_pars result of get_sim_pars or get_community_pars
#'
#' @return vector of means for BSD drawn from runif
#' @export
#'
draw_bsd <- function(community_pars) {
  means <- runif(n = community_pars$S, min = .8 * (community_pars$min_size), max = 1.2* (community_pars$max_size))
  return(means)
}

#' Add standard deviations to BSD
#' @param bsd_means vector of BSD means
#' @param stdev stdev proportion to BSD mean value
#' @return dataframe of mean sizes + scaled sds
#' @export
add_sd <- function(bsd_means, stdev) {
  sds <- stdev * bsd_means
  bsd <- data.frame(
    means = bsd_means,
    sd = sds
  )
  return(bsd)
}


#' Assign sizes to all individuals of a species given abundance, mean, and sd
#' Assumes a normal distribution
#' @param bsd_index row of bsd - corresponds to species
#' @param bsd bsd
#' @return data frame of `individual_species_ids` and `individual_sizes` for a single species
#' @export
assign_ind_sizes <- function(bsd_index, bsd){
  this_species <- data.frame(
    individual_species_ids = rep(x = bsd_index, times = bsd$abundance[bsd_index]),
    individual_sizes = rnorm(n = bsd$abundance[bsd_index],
                             mean = bsd$means[bsd_index],
                             sd = bsd$sd[bsd_index])
  )

  return(this_species)
}

#' Combine BSD and SAD to produce ISD
#' Assign abundances to body sizes
#' Current: randomly combine one time
#' Possible future: incoporate size-abundance relationship
#' Possible future: use all possible combinations
#' @param sad draw from METE SAD (draw_sad)
#' @param bsd draw from uniform bsd (draw_bsd)
#'
#' @return bsd with `abundance` column
#' @export
#'
combine_abds <- function(sad, bsd) {

  bsd$abundance <- sample(sad, size = length(sad), replace = FALSE)

  return(bsd)
}


#' Draw a sim
#'
#' @param community_pars result of get_sim_pars
#' @param sim_index for drake
#'
#' @return dataframe of `individual_species_ids` and `individual_sizes` drawn using `community_pars`
#' @export
#' @importFrom dplyr bind_rows
draw_sim <- function(community_pars, sim_index = 1){

  sad <- draw_sad(community_pars = community_pars)
  bsd <- draw_bsd(community_pars = community_pars)
  bsd <- add_sd(bsd, stdev = community_pars$stdev)
  bsd <- combine_abds(sad = sad, bsd = bsd)

  sim <- lapply(1:nrow(bsd),FUN = assign_ind_sizes, bsd = bsd)
  sim <- dplyr::bind_rows(sim)
  return(sim)
}
