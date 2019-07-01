#' Make community pars pipeline
#'
#' @param dats_pipeline pipeline of loading raw data
#'
#' @return pipeline to extract communty pars from raw datasets
#' @export
#' @importFrom drake drake_plan target
make_cp_pipeline <- function(dats_pipeline) {
  dat_targets <- list()
  for(i in 1:length(dats_pipeline$target)) {
    dat_targets <- c(dat_targets, as.name(dats_pipeline$target[i]))
  }
  cp_pipeline <- drake::drake_plan(
    cp = target(get_community_pars(raw_dat),
                transform = map(raw_dat = !!dat_targets))
  )
  return(cp_pipeline)
}


#' Make sim pars pipeline
#'
#' @param cp_pipeline pipeline of extracting community pars from raw data
#'
#' @return pipeline to add standard deviations to community pars
#' @export
#' @importFrom drake drake_plan target
make_sp_pipeline <- function(cp_pipeline, stdevs) {
  cp_targets <- list()
  for(i in 1:length(cp_pipeline$target)) {
    cp_targets <- c(cp_targets, as.name(cp_pipeline$target[i]))
  }
  sp_pipeline <- drake::drake_plan(
    sp = target(get_sim_pars(comm_pars, stdev),
                transform = cross(comm_pars = !!cp_targets,
                                  stdev = !!stdevs))
  )
  return(sp_pipeline)
}

#' Make draw sims pipeline
#'
#' @param sp_pipeline pipeline of community pars including standard deviations
#' @param sim_indices for drake
#' @return pipeline to extract draw sims based on community pars
#' @export
#' @importFrom drake drake_plan target
make_draw_pipeline <- function(sp_pipeline, sim_indices) {
  sp_targets <- list()
  for(i in 1:length(sp_pipeline$target)) {
    sp_targets <- c(sp_targets, as.name(sp_pipeline$target[i]))
  }
  draw_pipeline <- drake::drake_plan(
    sim = target(draw_sim(sim_pars, sim_index),
                 transform = cross(sim_pars = !!sp_targets,
                                   sim_index = !!sim_indices))
  )
  return(draw_pipeline)
}

#' Make community pars pipeline
#'
#' @param community_dat_pipeline pipeline to either get raw data or draw sims
#' @param community_type "sim" or "empirical"
#' @return pipeline to get integrated density of GMM fit to community data
#' @export
#' @importFrom drake drake_plan target
#' @importFrom replicatebecs add_energy_sizeclass
#' @importFrom neonbecs make_isd fit_gmm
#'
make_id_pipeline <- function(community_dat_pipeline, community_type = "sim") {
  cd_targets <- list()
  dat_names <- vector(length=nrow(community_dat_pipeline))
  stdevs <- vector(length=nrow(community_dat_pipeline))
  for(i in 1:length(community_dat_pipeline$target)) {
    cd_targets <- c(cd_targets, as.name(community_dat_pipeline$target[i]))
    if(community_type == "sim") {
    dat_names[i] <- strsplit(community_dat_pipeline$target[i], "_")[[1]][4]
    stdevs[i] <- as.numeric(strsplit(community_dat_pipeline$target[i], "_")[[1]][5])
    } else {
      dat_names[i] <- community_dat_pipeline$target[i]
      stdevs[i] <- NA
    }
  }
  id_pipeline <- drake::drake_plan(
    e = target(replicatebecs::add_energy_sizeclass(community_dat),
               transform = map(community_dat = !!cd_targets)),
    isd = target(neonbecs::make_isd(e),
                 transform = map(e)),
    gmm = target(neonbecs::fit_gmm(isd),
                 transform = map(isd)),
    id = target(get_integrated_density(gmm, type = community_type,
                                       dat_name = dat_names, stdev = stdevs),
                transform = map(gmm, community_type = !!community_type,
                                dat_names = !!dat_names,
                                stdevs = !!stdevs))
  )
  return(id_pipeline)

}


#' Make thresholds pipeline
#' Make pipeline to evaluate number of gaps/modes in an integrated density given a particular threshold density
#' @param id_pipeline pipeline to make integrated densities
#' @param thresholds_to_try thresholds for drake
#' @return pipeline to evaluate at thresholds
#' @export
#' @importFrom drake drake_plan target
#' @importFrom dplyr bind_rows
make_thresholds_pipeline <- function(id_pipeline,
                                     thresholds_to_try) {
  id_lines <- which(grepl("id_", id_pipeline$target))

  id_targets <- list()
  for(i in 1:length(id_lines)) {
    id_targets <- c(id_targets, as.name(id_pipeline$target[id_lines[i]]))
  }
  thresholds_pipeline <- drake::drake_plan(
    t = target(find_gaps(threshold, id),
               transform = cross(threshold = !!thresholds_to_try,
                                 id = !!id_targets)
    ),
    r = target(get_result(t),
               transform = map(t))
    ,
    result = target(
      dplyr::bind_rows(r),
      transform = combine(r)
    )
  )
  return(thresholds_pipeline)
}

