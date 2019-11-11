library(isds)
library(ggplot2)
library(dplyr)

source(here::here("analysis", "fxns2.r"))
portal_isd <- get_toy_portal_data(years = c(1990:1995), chosen_treatment = "control") %>%
  mutate(sim = NA, source = "empirical")
portal_kde <- integrate_kde(portal_isd, band = 10) %>%
  mutate(source = "empirical", sim = NA)
portal_id <- integrate_gmm(fit_gmm(portal_isd)) %>%
  mutate(source = "empirical", sim = NA)

nsamples = 100

set.seed(1)
uniform_isds <- replicate(nsamples, expr = sample_uniform_isd(portal_isd), simplify = F)
constrained_isds <- replicate(nsamples, expr = sample_constrained_isd(portal_isd, size_relationship = "none"), simplify = F)
constrained_s_isds <- replicate(nsamples, expr = sample_constrained_isd(portal_isd, size_relationship = "negative"), simplify = F)

uniform_kdes <- lapply(uniform_isds, FUN = integrate_kde, band = 10)
constrained_kdes <- lapply(constrained_isds, FUN = integrate_kde, band = 10)
constrained_s_kdes <- lapply(constrained_s_isds, FUN = integrate_kde, band = 10)

names(uniform_kdes) <- 1:nsamples
uniform_kdes <- bind_rows(uniform_kdes, .id = "sim") %>%
  mutate(source = "uniform")
names(constrained_kdes) <- 1:nsamples
constrained_kdes <- bind_rows(constrained_kdes, .id = "sim") %>%
  mutate(source = "constrained")
names(constrained_s_kdes) <- 1:nsamples
constrained_s_kdes <- bind_rows(constrained_s_kdes, .id = "sim") %>%
  mutate(source = "constrained_s")

all_kdes <- bind_rows(uniform_kdes, constrained_kdes, constrained_s_kdes, portal_kde)


uniform_gmms <- lapply(uniform_isds, FUN = function(an_isd) return(integrate_gmm(fit_gmm(an_isd))))
constrained_gmms <- lapply(constrained_isds, FUN = function(an_isd) return(integrate_gmm(fit_gmm(an_isd))))
constrained_s_gmms <- lapply(constrained_s_isds, FUN = function(an_isd) return(integrate_gmm(fit_gmm(an_isd))))

names(uniform_gmms) <- 1:nsamples
uniform_gmms <- bind_rows(uniform_gmms, .id = "sim") %>%
  mutate(source = "uniform")
names(constrained_gmms) <- 1:nsamples
constrained_gmms <- bind_rows(constrained_gmms, .id = "sim") %>%
  mutate(source = "constrained")
names(constrained_s_gmms) <- 1:nsamples
constrained_s_gmms <- bind_rows(constrained_s_gmms, .id = "sim") %>%
  mutate(source = "constrained_s")

all_gmms <- bind_rows(uniform_gmms, constrained_gmms, constrained_s_gmms, portal_id)


names(uniform_isds) <- 1:nsamples
uniform_isds <- bind_rows(uniform_isds, .id = "sim") %>%
  mutate(source = "uniform")
names(constrained_isds) <- 1:nsamples
constrained_isds <- bind_rows(constrained_isds, .id = "sim") %>%
  mutate(source = "constrained")
names(constrained_s_isds) <- 1:nsamples
constrained_s_isds <- bind_rows(constrained_s_isds, .id = "sim") %>%
  mutate(source = "constrained_s")

all_isds <- bind_rows(uniform_isds, constrained_isds, constrained_s_isds, portal_isd)


save.image("sim_bank_1990s.RData")

write.csv(all_isds, file = "all_isds_1990s.csv", row.names = F)
write.csv(all_kdes, file = "all_kdes_1990s.csv", row.names = F)
write.csv(all_gmms, file = "all_gmms_1990s.csv", row.names = F)
