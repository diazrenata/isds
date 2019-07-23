library(drake)
library(isds)
expose_imports(isds)

sim_indices = as.numeric(c(1:2))
#stdevs = seq(0.05, 0.35, by = 0.1)
stdevs = c(0.05)
stdevs = c(0.01, 0.25)
# thresholds_to_try = seq(.01, .31, by = 0.02)
stdev_range = list(c(0.01, 0.04))
nsim = 2
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

sims_pipeline <- drake_plan(sims = target(generate_sim_draws(community_dat, dat_name, stdevs, stdev_range, nsim),
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

all <- rbind(dats, sims_pipeline)


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

