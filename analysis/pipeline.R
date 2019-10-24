library(drake)
library(dplyr)
library(isds)
expose_imports(isds)
source(here::here("analysis", "fxns.R"))

nsim <- 2

sims_to_run <- c("uniform", "constrained", "constrained_density", "none")
#
# isds <- drake_plan(
#   emp = target(get_clumps(get_toy_portal_data(years = some_years, download = F)),
#                   transform = map(some_years = !!years_to_use)),
#   s = target(repeat_sims(ntimes = !!nsim, isd = emp, type = some_types),
#              transform = cross(emp, some_types = !!sims_to_run)),
#   all = target(dplyr::bind_rows(s),
#                transform = combine(s, .by = emp)),
#   summary = target(summarize_clumps(all),
#                    transform = map(all))
# )


isds <- drake_plan(
  emp = target(get_clumps(get_toy_portal_data(years = 1980:1985, download = F))),
             #  transform = map(some_years = !!years_to_use)),
  s = target(repeat_sims(ntimes = !!nsim, isd = emp, type = some_types),
         #    transform = cross(emp, some_types = !!sims_to_run)),
         transform = map(some_types = !!sims_to_run)),
  all = target(dplyr::bind_rows(s),
               transform = combine(s)),#, .by = emp)),
  summary = target(summarize_clumps(all),
                   transform = map(all))
)

pipeline <- isds

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake", "drake-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)

## View the graph of the plan
if (interactive())
{
  config <- drake_config(pipeline, cache = cache)
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
  make(pipeline,
       force = TRUE,
       cache = cache,
       cache_log_file = here::here("drake", "cache_log.txt"),
       verbose = 2,
       parallelism = "future",
       jobs = 64,
       caching = "master") # Important for DBI caches!
} else {
  # Run the pipeline on a single local core
  make(pipeline, cache = cache, cache_log_file = here::here("drake", "cache_log.txt"))
}


print("Completed OK")
