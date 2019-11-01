library(drake)
library(dplyr)
library(isds)
expose_imports(isds)
source(here::here("analysis", "fxns.R"))

nsim <- 200

sims_to_run <- c("uniform", "constrained", "constrained_density", "none")

isds <- drake_plan(
  emp = target(get_clumps(get_toy_portal_data(years = 1980:1985, download = F))),
  #  transform = map(some_years = !!years_to_use)),
  s = target(repeat_sims(ntimes = !!nsim, isd = emp, type = some_types),
             #    transform = cross(emp, some_types = !!sims_to_run)),
             transform = map(some_types = !!sims_to_run)),
  all = target(dplyr::distinct(dplyr::bind_rows(s)),
               transform = combine(s)),#, .by = emp)),
  summary = target(summarize_clumps(all),
                   transform = map(all))
)

pipeline <- isds

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake", "drake-cache-cdf.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)

## View the graph of the plan
if (interactive())
{
  config <- drake_config(pipeline, cache = cache)
  sankey_drake_graph(config, build_times = "none")  # requires "networkD3" package
  vis_drake_graph(config, build_times = "none")     # requires "visNetwork" package
}

make(pipeline, cache = cache, cache_log_file = here::here("drake", "cache_log-cdf.txt"))


print("Completed OK")
