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

portal_gmm <- fit_gmm(portal_isd)
portal_id <- integrate_gmm(portal_gmm)
head(portal_id)

portal_id <- portal_id %>%
  arrange(desc(density)) %>%
  mutate(rowindex = row_number())

running_sum <- 0
for(i in 1:nrow(portal_id)) {
  running_sum <- running_sum + portal_id$density[i]
  if(running_sum >= .9) {
    break
  }
}

portal_id$in_most <- (portal_id$rowindex <= i)

portal_id_plot <- ggplot(data = portal_id, aes(x = wgt, y = density, color = in_most)) +
  geom_point() +
  scale_color_viridis_d(end = .6) +
  theme_bw()
portal_id_plot
