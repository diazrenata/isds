library(isds)
library(ggplot2)
library(dplyr)

portal_isd <- get_toy_portal_data(years = c(1980:1985), chosen_treatment = "control")
head(portal_isd)

raw_isd_plot <- ggplot(data = portal_isd, aes(x = wgt)) +
  geom_histogram(binwidth = 5) +
  theme_bw()

raw_isd_plot

sequenced <- portal_isd %>%
  arrange((wgt)) %>%
  mutate(ranked_wgt = row_number(),
         comparison_rank = ranked_wgt)

sequence_plot <- ggplot(data = sequenced, aes(x = ranked_wgt, y = wgt)) +
  geom_point() +
  theme_bw()

sequence_plot

offset = 1

comparison <- sequenced %>%
  mutate(comparison_rank = ranked_wgt - offset) %>%
  select(-species, -ranked_wgt) %>%
  rename(comparison_wgt = wgt)

sequenced_comparison <- left_join(sequenced, comparison, by = "comparison_rank")

sequenced_comparison_plot <- ggplot(data = sequenced_comparison, aes(x = ranked_wgt, y = wgt)) +
  geom_line(size = .1) +
  geom_line(aes(y = comparison_wgt), color = "red", size = .1) +
  theme_bw()
sequenced_comparison_plot

sequenced_comparison <- sequenced_comparison %>%
  mutate(ratio = comparison_wgt / wgt,
         difference = comparison_wgt - wgt)

hist(sequenced_comparison$ratio)
hist(sequenced_comparison$difference)

difference_plot <- ggplot(data = sequenced_comparison, aes(x = ranked_wgt, y = difference)) +
  geom_point() +
  theme_bw()

difference_plot

