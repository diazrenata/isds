#' Get community parameters
#' @param raw_dat community data frame with columns `species`', `wgt`
#' @param dat_name name of dataset
#'
#' @return list of community parameters for sim draws: `S` = number of species, `N` = number of individuals, `min_size` = minimum body size, `max_size` = maximum body size
#' @export
get_community_pars <- function(raw_dat, dat_name) {
  S <- length(unique(raw_dat$species))
  N <- nrow(raw_dat)
  min_size <- min(raw_dat$wgt)
  max_size <- max(raw_dat$wgt)
  community_pars <- list(S = S, N = N, min_size = min_size, max_size = max_size, dat_name = dat_name, sim = FALSE)
  return(community_pars)
}

#' Get sim pars
#' Add standard deviations to `community_pars`
#' @param community_pars community_pars from get_community_pars
#' @param stdev stdev to use in sims
#' @return  community_pars with stdev
#' @export
get_sim_pars <- function(community_pars, stdev = NULL) {
  sim_pars <- append(community_pars, list(stdev = stdev))
  if(any(!is.na(c(stdev)))) {
    sim_pars$sim <- TRUE
  }
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
#' @param stdev stdev proportion to BSD mean value: numeric, vector, or "norm"
#' @return dataframe of mean sizes + scaled sds
#' @export
add_sd <- function(bsd_means, stdev) {
  if(is.numeric(stdev)) {
    if(length(stdev) == 1) {
      sds <- bsd_means * stdev
    } else {
      sds <- bsd_means * (runif(n = length(bsd_means), min = stdev[1], max = stdev[2]))
    }
  } else {
    sds <- bsd_means * -1
    while(any(sds <= 0)) {
    sds <- bsd_means * (rnorm(n = length(bsd_means), mean = .25, sd = .11))
    }
  }
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
#' @return data frame of `species` and `wgt` for a single species
#' @importFrom dplyr mutate if_else
#' @export
assign_ind_sizes <- function(bsd_index, bsd){
  this_species <- data.frame(
    species = rep(x = bsd_index, times = bsd$abundance[bsd_index]),
    wgt = rnorm(n = bsd$abundance[bsd_index],
                mean = bsd$means[bsd_index],
                sd = bsd$sd[bsd_index])
  )

  this_species <- this_species %>%
    dplyr::mutate(wgt = dplyr::if_else((wgt <= 0), 1, wgt))

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
#' @return dataframe of `species` and `wgt` drawn using `community_pars`
#' @export
#' @importFrom dplyr bind_rows
draw_sim <- function(community_pars, sim_index = 1){

  sad <- draw_sad(community_pars = community_pars)
  bsd <- draw_bsd(community_pars = community_pars)
  bsd <- add_sd(bsd, stdev = community_pars$stdev)
  bsd <- combine_abds(sad = sad, bsd = bsd)

  sim <- lapply(1:nrow(bsd),FUN = assign_ind_sizes, bsd = bsd)
  sim <- dplyr::bind_rows(sim)
  sim <- list(community = sim, pars = community_pars)
  return(sim)
}


#' draw sims all at once
#'
#' @param base_dat data
#' @param dat_name to add to pars
#' @param sds sd pars
#' @param nsim nb sim
#'
#' @return list of sims
#' @export
generate_sim_draws <- function(base_dat, dat_name = NULL, sds, nsim) {

  cpars <- get_community_pars(base_dat, dat_name = dat_name)

  sd_sim_pars <- lapply(sds, get_sim_pars, community_pars = cpars)

  sim_calls <- list()
  for(i in 1:length(sd_sim_pars)) {
    sim_calls[[i]] <- call("draw_sim", community_pars = sd_sim_pars[[i]])
  }

  all_sim_draws <- lapply(sim_calls, replicate_sim_calls, nsim = nsim)
  all_sim_draws <- unlist(all_sim_draws, recursive = F)

  empirical = list(list(community = base_dat, pars = get_sim_pars(community_pars = cpars,stdev = NA)))

  all_sims <- append(empirical, all_sim_draws)

  return(all_sims)
}

#' Replicte sim call
#'
#' To draw sims.
#'
#' @param sim_call sim call
#' @param nsim nb replicates
#'
#' @return list of sims
#' @export
replicate_sim_calls <- function(sim_call, nsim) {
  sim_reps <- replicate(nsim, expr = eval(sim_call), simplify = FALSE)
  return(sim_reps)
}
