library(drake)
library(isds)
expose_imports(isds)

sim_indices = as.numeric(c(1:2))
#stdevs = seq(0.01, 0.21, by = 0.1)
stdevs = c(0.01, 0.25)
# thresholds_to_try = seq(.01, .31, by = 0.02)

dats <- drake_plan(
  dat1  = target(get_toy_portal_data()),
  dat2 = target(get_toy_portal_data(years = c(1985, 1986))),
  dat3 = target(get_toy_portal_data(years = c(2000, 2001))),
  dat4 = target(get_toy_portal_data(years = c(2014, 2015)))
)

cp_pipeline <- make_cp_pipeline(dats)

sp_pipeline <- make_sp_pipeline(cp_pipeline = cp_pipeline,
                                stdevs = stdevs)

draw_pipeline <- make_draw_pipeline(sp_pipeline = sp_pipeline,
                                    sim_indices = sim_indices)
sim_id_pipeline <- make_id_pipeline(draw_pipeline, "sim")

empirical_id_pipeline <- make_id_pipeline(dats, "emp")

id_pipelines <- rbind(sim_id_pipeline, empirical_id_pipeline)
#
# id_plots_pipeline <- make_id_plots_pipeline(id_pipelines, sim_index = c(1:5))
#
# thresholds_pipeline <- make_thresholds_pipeline(id_pipelines,
#                                                 thresholds_to_try)
#
# summary_plot_pipeline <- make_summary_plots_pipeline(dats)
#
# reports_pipeline <- drake_plan(
#   stdev_report = target(rmarkdown::render(here::here("analysis", "reports", "sim_stdev_report.Rmd")))
# )

full_pipeline <- rbind(dats, cp_pipeline, sp_pipeline, draw_pipeline, id_pipelines)
                       #, thresholds_pipeline, id_plots_pipeline, summary_plot_pipeline)
                       #, reports_pipeline)


## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake", "drake-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)

## View the graph of the plan
if (interactive())
{
  config <- drake_config(full_pipeline, cache = cache)
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
  make(full_pipeline,
       force = TRUE,
       cache = cache,
       cache_log_file = here::here("drake", "cache_log.txt"),
       verbose = 2,
       parallelism = "future",
       jobs = 64,
       caching = "master") # Important for DBI caches!
} else {
  # Run the pipeline on a single local core
  make(full_pipeline, cache = cache, cache_log_file = here::here("drake", "cache_log.txt"))
}

