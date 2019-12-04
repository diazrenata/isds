library(isds)

year_chunks <- list(eighties = c(1980:1985), nineties = c(1990:1995), aughts = c(2000:2005))

sample_time_chunk <- function(time_chunk) {
  portal_isd <- get_toy_portal_data(years = time_chunk, chosen_treatment = "control") %>%
    dplyr::mutate(sim = NA, source = "empirical")

  nsamples = 95

  set.seed(1)
  uniform_isds <- replicate(nsamples, expr = sample_uniform_isd(portal_isd), simplify = F)
  constrained_isds <- replicate(nsamples, expr = sample_constrained_isd(portal_isd, size_relationship = "none"), simplify = F)
  constrained_s_isds <- replicate(nsamples, expr = sample_constrained_isd(portal_isd, size_relationship = "negative"), simplify = F)

  names(uniform_isds) <- 1:nsamples
  uniform_isds <- dplyr::bind_rows(uniform_isds, .id = "sim") %>%
    dplyr::mutate(source = "uniform")
  names(constrained_isds) <- 1:nsamples
  constrained_isds <- dplyr::bind_rows(constrained_isds, .id = "sim") %>%
    dplyr::mutate(source = "constrained")
  names(constrained_s_isds) <- 1:nsamples
  constrained_s_isds <- dplyr::bind_rows(constrained_s_isds, .id = "sim") %>%
    dplyr::mutate(source = "constrained_s")


  all_isds <- dplyr::bind_rows(uniform_isds, constrained_isds, constrained_s_isds, portal_isd)

  return(all_isds)

}

samples <- lapply(year_chunks, FUN = sample_time_chunk)

names(samples) <- names(year_chunks)

samples <- dplyr::bind_rows(samples, .id = "time_chunk")
head(samples)

write.csv(samples, file = here::here("analysis", "sims.csv"), row.names = F)

samples_nounif <- dplyr::filter(samples, source != "uniform", sim != 95) # just to squeak in under 100 MB
write.csv(samples_nounif, file = here::here("analysis", "sims_nounif.csv"), row.names = F)
