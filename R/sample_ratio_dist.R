library(isds)
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
#
# nsamples = 1
#
# set.seed(1)
# uniform_isds <- replicate(nsamples, expr = sample_uniform_isd(portal_isd), simplify = F)
# constrained_isds <- replicate(nsamples, expr = sample_constrained_isd(portal_isd, size_relationship = "none"), simplify = F)
# constrained_s_isds <- replicate(nsamples, expr = sample_constrained_isd(portal_isd, size_relationship = "negative"), simplify = F)

u <- sample_uniform_isd(portal_isd)
c <- sample_constrained_isd(portal_isd, size_relationship = "none")
cs <- sample_constrained_isd(portal_isd, size_relationship = "negative")


u_ratios <- sample_ratio(u, nsamples = 10000) %>%
  mutate(source = "uniform")
portal_ratios <- sample_ratio(portal_isd, nsamples = 10000) %>%
  mutate(source = "portal")
c_ratios <- sample_ratio(c, nsamples = 10000) %>%
  mutate(source = "constrained")
cs_ratios <- sample_ratio(cs, nsamples = 10000) %>%
  mutate(source = "constrained_size")


all_ratios <- bind_rows(u_ratios, portal_ratios, c_ratios, cs_ratios)

ratios_plot <- ggplot(data = all_ratios, aes(x = source, y = ratio)) +
  geom_violin() +
  theme_bw()

ratios_plot


strongly_three_kinds <- portal_isd %>%
  mutate(wgt = sample(x = c(4, 97, 190), size = nrow(portal_isd), replace = TRUE))

strongly_three_kinds_ratios <- sample_ratio(strongly_three_kinds, nsamples = 10000) %>%
  mutate(source = "three")

all_ratios <-bind_rows(all_ratios, strongly_three_kinds_ratios)

ratios_plot <- ggplot(data = all_ratios, aes(x = ratio)) +
  geom_histogram(binwidth = .1, alpha = .2) +
  geom_histogram(data = filter(all_ratios, !(between(ratio, .9, 1.1))), binwidth = .1) +
  facet_wrap(vars(source), ncol = 1) +
  theme_bw()# +
#  geom_hline(yintercept = 1)
#geom_vline(xintercept = 1, color = "blue")
ratios_plot

ratios <- list(portal_ratios, u_ratios, c_ratios, cs_ratios)
ratios <- lapply(ratios, FUN = function(a_df) return(mutate(a_df, wgt = ratio)))
ratio_gmms <- lapply(ratios, FUN = fit_gmm, max_g = 4)
ratio_ids <- lapply(ratio_gmms, FUN = integrate_gmm2)
ratio_peaks <- lapply(ratio_ids, FUN = get_peaks)
id_plots <- lapply(ratio_ids, FUN = function(an_id) return(ggplot(data = an_id, aes(x = ratio, y = density)) + geom_point(size = .3) + theme_bw()))
id_plots
ratio_peaks
