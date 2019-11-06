ibrary(isds)
library(ggplot2)
library(dplyr)

portal_isd <- get_toy_portal_data(years = c(1980:1985), chosen_treatment = "control")
head(portal_isd)

raw_isd_plot <- ggplot(data = portal_isd, aes(x = wgt)) +
  geom_histogram(binwidth = 5) +
  theme_bw()

raw_isd_plot

# Try taking combinations at random?

source(here::here("analysis", "fxns2.r"))

portal_kde_id <- integrate_kde(portal_isd, band = 100) %>%
  mutate(sim = "1", source = "empirical")
portal_kde_turnpoints <- count_peaks(portal_kde_id)


nsamples = 10

set.seed(1)
uniform_isds <- replicate(nsamples, expr = sample_uniform_isd(portal_isd), simplify = F)
constrained_isds <- replicate(nsamples, expr = sample_constrained_isd(portal_isd, size_relationship = "none"), simplify = F)
constrained_s_isds <- replicate(nsamples, expr = sample_constrained_isd(portal_isd, size_relationship = "negative"), simplify = F)

uniform_kdes <- lapply(uniform_isds, FUN = integrate_kde, band = 100)
constrained_kdes <- lapply(constrained_isds, FUN = integrate_kde, band = 100)
constrained_s_kdes <- lapply(constrained_s_isds, FUN = integrate_kde, band = 100)

uniform_kdes_turns <- data.frame(source = "uniform", turns = vapply(uniform_kdes, FUN = count_peaks, FUN.VALUE = 10), sim = 1:nsamples)
constrained_kdes_turns <- data.frame(source = "constrained", turns =vapply(constrained_kdes, FUN = count_peaks, FUN.VALUE = 10), sim = 1:nsamples)
constrained_s_kdes_turns <- data.frame(source = "constrained_s", turns = vapply(constrained_s_kdes, FUN = count_peaks, FUN.VALUE = 10), sim = 1:nsamples)

names(uniform_kdes) <- 1:nsamples
uniform_kdes <- bind_rows(uniform_kdes, .id = "sim") %>%
  mutate(source = "uniform")
names(constrained_kdes) <- 1:nsamples
constrained_kdes <- bind_rows(constrained_kdes, .id = "sim") %>%
  mutate(source = "constrained")
names(constrained_s_kdes) <- 1:nsamples
constrained_s_kdes <- bind_rows(constrained_s_kdes, .id = "sim") %>%
  mutate(source = "constrained_s")

all_kdes <- bind_rows(uniform_kdes, constrained_kdes, constrained_s_kdes, portal_kde_id)

kdes_plot <- ggplot(data = all_kdes, aes(x = wgt, y = density, color = source)) +
  geom_point() +
  scale_color_viridis_d() +
  facet_grid(rows = vars(source), cols = vars(sim)) +
  theme_bw()
kdes_plot

all_kdes_turns <- bind_rows(uniform_kdes_turns, constrained_kdes_turns, constrained_s_kdes_turns)
