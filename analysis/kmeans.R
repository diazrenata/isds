library(ggplot2)
library(isds)
library(dplyr)
source(here::here("analysis", "fxns.R"))


portal_isd <- get_toy_portal_data(years = c(1980:1985), download = F)

raw_isd_plot <- ggplot(data = portal_isd, aes(x = wgt)) +
  geom_histogram(binwidth = 5) +
  theme_bw()

portal_cdf <- make_cdf(portal_isd)


cdf_plot <- ggplot(data = portal_cdf, aes(x = wgt, y = cum_ind)) +
  geom_point() +
  #  geom_vline(xintercept = cluster_breaks$clustermax) +
  # geom_vline(xintercept = isd_means$centers, color= "green") +
  theme_bw()
cdf_plot

isd_means <- kmeans(portal_isd$wgt, centers = 2)

portal_clusters <- data.frame(
  wgt = portal_isd$wgt,
  cluster = as.factor(isd_means$cluster)
)

cluster_breaks <- portal_clusters %>%
  group_by(cluster) %>%
  summarize(clustermax = max(wgt), clustercenter = mean(wgt)) %>%
  ungroup()  %>%
  arrange(clustermax)

kmeans_plot <- ggplot(data = portal_clusters, aes(x = wgt, fill = as.factor(cluster))) +
  geom_histogram(binwidth = 5) +
 # geom_vline(xintercept = cluster_breaks$clustermax, linetype = "dotted") +
  geom_vline(xintercept = cluster_breaks$clustercenter, linetype = "dashed") +
  theme_bw() +
  scale_fill_viridis_d(end = .7) +
  theme(legend.position = "none")

kmeans_plot

cluster_stepfun <- stepfun(x = cluster_breaks$clustermax, y = c(cluster_breaks$cluster, cluster_breaks$cluster[ which(cluster_breaks$clustercenter == max(cluster_breaks$clustercenter))]))

cdf_clusters <-portal_cdf %>%
  mutate(cluster = as.factor(cluster_stepfun(wgt))) %>%
  left_join(cluster_breaks, by = "cluster") %>%
  group_by(cluster) %>%
  mutate(mean_cum_ind = mean(cum_ind),
         max_cum_ind = max(cum_ind),
         min_cum_ind = min(cum_ind)) %>%
  ungroup() %>%
  mutate(unif_cum_ind = ((1/(max(wgt) - min(wgt))) * (wgt - min(wgt))))# + (max(wgt) - min(wgt)))

cdf_cluster_plot <- ggplot(data = cdf_clusters, aes(x = wgt, y = cum_ind, color = cluster)) +
  geom_point() +
  geom_line(aes(x = wgt, y = mean_cum_ind, color = cluster), size = .5) +
  scale_color_viridis_d(end = .7) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_abline(intercept = -((1/max(cdf_clusters$wgt) * min(cdf_clusters$wgt))), slope = 1 / (max(cdf_clusters$wgt)))
cdf_cluster_plot

cdf_unif_plot <- ggplot(data = cdf_clusters, aes(x = wgt, y = cum_ind)) +
  geom_point() +
  geom_line(aes(x = wgt, y = unif_cum_ind)) +
  theme_bw() +
  theme(legend.position = "none")
cdf_unif_plot

stairs_ssq = sum ((cdf_clusters$mean_cum_ind - cdf_clusters$cum_ind) ^ 2)
unif_ssq = sum ((cdf_clusters$unif_cum_ind - cdf_clusters$cum_ind) ^ 2)

