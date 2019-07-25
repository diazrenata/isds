library(drake)
library(isds)
#expose_imports(isds)

stdevs = seq(0.05, 0.35, by = 0.1)
#stdevs = c(0.01, 0.25)
# thresholds_to_try = seq(.01, .31, by = 0.02)
stdev_range = list(c(0.01, 0.4))
nsim = 25
dats <- drake_plan(
  dat1  = target(get_toy_portal_data()),
  dat2 = target(get_toy_portal_data(years = c(1985, 1986)))#,
  # dat3 = target(get_toy_portal_data(years = c(2000, 2001))),
  # dat4 = target(get_toy_portal_data(years = c(2014, 2015)))
)

dat_targets <- list()

for(i in 1:nrow(dats)) {
  dat_targets[[i]] <- as.name(dats$target[i])
}

sims_pipeline <- drake_plan(
  sims = target(generate_sim_draws(community_dat, dat_name, stdevs, stdev_range, nsim),
                transform = map(community_dat = !!dat_targets,
                                dat_name = !!dats$target,
                                stdevs = stdevs,
                                stdev_range = stdev_range,
                                nsim =nsim)
  )
)

for(i in 1:nrow(sims_pipeline)) {
  sims_pipeline$target[i] <- unlist(strsplit(sims_pipeline$target[i], split = "sims_"))[2]
  sims_pipeline$target[i] <- unlist(strsplit(sims_pipeline$target[i], split = "_"))[1]
  sims_pipeline$target[i] <- paste0("sims_", sims_pipeline$target[i])
}

sim_targets <- list()

for(i in 1:nrow(sims_pipeline)) {
  sim_targets[[i]] <- as.name(sims_pipeline$target[i])
}

ids_pipeline <- drake_plan(
  ids = target(get_id_wrapped(datasets = datasets),
                transform = map(datasets = !!sim_targets)
  )
)

for(i in 1:nrow(ids_pipeline)) {
  ids_pipeline$target[i] <- unlist(strsplit(ids_pipeline$target[i], split = "sims_"))[2]
  ids_pipeline$target[i] <- paste0("ids_", ids_pipeline$target[i])
}


ids_targets <- list()

for(i in 1:nrow(ids_pipeline)) {
  ids_targets[[i]] <- as.name(ids_pipeline$target[i])
}

id_plots_pipeline <- drake_plan(
  ids_plots = target(plot_dataset_ids(dataset_ids = dat_ids),
                     transform = map(dat_ids = !!ids_targets))
)

for(i in 1:nrow(id_plots_pipeline)) {
  id_plots_pipeline$target[i] <- unlist(strsplit(id_plots_pipeline$target[i], split = "ids_plots_ids_"))[2]
  id_plots_pipeline$target[i] <- paste0("ids_plots_", id_plots_pipeline$target[i])
}

results_pipeline <- drake_plan(
  results = target(make_results(id_list = dat_ids),
                           transform = map(dat_ids = !!ids_targets)),
  all_results = target(dplyr::bind_rows(results),
                       transform = combine(results))
)

all <- rbind(dats, sims_pipeline, ids_pipeline,
             #id_plots_pipeline,
             results_pipeline)


## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake", "drake-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)

## View the graph of the plan
if (interactive())
{
  config <- drake_config(all, cache = cache)
  sankey_drake_graph(config, build_times = "none")  # requires "networkD3" package
  vis_drake_graph(config, build_times = "none")     # requires "visNetwork" package
}

## Run the pipeline
nodename <- Sys.info()["nodename"]
if(grepl("ufhpc", nodename)) {
  library(future.batchtools)
  print("I know I am on SLURM!")
  ## Run the pipeline parallelized for HiPerGator
  future::plan(batchtools_slurm, template = "slurm_batchtools.tmpl")
  make(all,
       force = TRUE,
       cache = cache,
       cache_log_file = here::here("drake", "cache_log.txt"),
       verbose = 2,
       parallelism = "future",
       jobs = 64,
       caching = "master") # Important for DBI caches!
} else {
  # Run the pipeline on a single local core
  make(all, cache = cache, cache_log_file = here::here("drake", "cache_log.txt"))
}


print("Completed OK")
