library(ggplot2)
library(isds)
library(dplyr)
source(here::here("analysis", "fxns.R"))


portal_isd <- get_toy_portal_data(years = c(1980:1985), download = F)


portal_cdf <- make_cdf(portal_isd)

cdf_plot <- ggplot(data = portal_cdf, aes(x = wgt, y = cum_ind)) +
  geom_point() +
  theme_bw()
cdf_plot

uniform_isd <- sample_uniform_isd(portal_isd)
uniform_cdf <- make_cdf(uniform_isd)
ucdf_plot <- ggplot(data = uniform_cdf, aes(x = wgt, y = cum_ind)) +
  geom_point() +
  theme_bw()
ucdf_plot

c_isd <- sample_constrained_isd(portal_isd, size_relationship = "none")
c_cdf <- make_cdf(c_isd)
ccdf_plot <- ggplot(data = c_cdf, aes(x = wgt, y = cum_ind)) +
  geom_point() +
  theme_bw()
ccdf_plot

c_s_isd <- sample_constrained_isd(portal_isd, size_relationship = "negative")
c_s_cdf <- make_cdf(c_s_isd)
csdf_plot <- ggplot(data = c_s_cdf, aes(x = wgt, y = cum_ind)) +
  geom_point() +
  theme_bw()
csdf_plot


portal_staircase <- portal_cdf
portal_staircase$cum_ind_s <- NA
portal_staircase$cum_ind_s[ which(portal_staircase$wgt <= 48)] <- .125
portal_staircase$cum_ind_s[ which(portal_staircase$wgt > 48)] <- .75
portal_staircase$cum_ind_s[ which(portal_staircase$wgt>= 125)] <- 1
portal_staircase$unif <- portal_cdf$wgt / max(portal_cdf$wgt)

stairplot <-ggplot(data = portal_staircase, aes(x = wgt, y = cum_ind)) +
  geom_point() +
  geom_line(aes(x = wgt, y = cum_ind_s), color = "blue") +
  geom_line(aes(x = wgt, y = unif), color = "red") +
  theme_bw()


stairplot

steps_error <- sum( (portal_staircase$cum_ind_s - portal_staircase$cum_ind) ^ 2)
unif_error <- sum( (portal_staircase$cum_ind_s - portal_staircase$unif) ^ 2)

steps_error
unif_error
