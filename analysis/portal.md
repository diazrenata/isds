Toy overlap
================

``` r
sample_community <- get_toy_portal_data(years = 1994,chosen_treatment = "control")

isd_plot <- ggplot(data = sample_community, aes(x = wgt)) +
  geom_density() +
  theme_bw()

sbsds_plot <- ggplot(data = sample_community, aes(x = wgt,fill = species, group = species)) +
  geom_density(alpha = .3) +
  theme_bw() +
  scale_fill_viridis_c(end = .8) +
  theme(legend.position = "top")

gridExtra::grid.arrange(grobs = list(isd_plot, sbsds_plot), nrow = 1)
```

![](portal_files/figure-markdown_github/real%20data-1.png)

``` r
real_overlap <- community_overlap(sample_community)

r_o_h <- ggplot(data = real_overlap, aes(x = overlap)) +
  stat_ecdf() +
  theme_bw() +
  xlim(-.2, 1.2)

real_overlap_weighted <- data.frame(
  weighted_overlap = rep(real_overlap$overlap, times = round(real_overlap$hm)))

r_o_w_h <- ggplot(data = real_overlap_weighted, aes(x = weighted_overlap)) +
  stat_ecdf() +
  theme_bw() +
  xlim(-.2, 1.2)

real_overlap_n_weighted <- data.frame(
  weighted_overlap = rep(real_overlap$overlap, times = round(real_overlap$total_n)))
r_o_w_n_h <- ggplot(data = real_overlap_n_weighted, aes(x = weighted_overlap)) +
  stat_ecdf() +
  theme_bw() +
  xlim(-.2, 1.2)
gridExtra::grid.arrange(grobs = list(r_o_h, r_o_w_h, r_o_w_n_h), nrow = 1)
```

![](portal_files/figure-markdown_github/overlap%20real-1.png)

``` r
real_overlap_p_weighted <- data.frame(
  weighted_overlap = rep(real_overlap$overlap, times = round(real_overlap$prod_n)))
r_o_w_p_h <- ggplot(data = real_overlap_p_weighted, aes(x = weighted_overlap)) +
  stat_ecdf() +
  theme_bw() +
  xlim(-.2, 1.2)
gridExtra::grid.arrange(grobs = list(r_o_h, r_o_w_h, r_o_w_n_h, r_o_w_p_h), nrow = 1)
```

![](portal_files/figure-markdown_github/overlap%20real-2.png)

``` r
sad <- sample_community %>%
  group_by(species) %>%
  summarize(abund = dplyr::n(),
            meanwgt = mean(wgt)) %>%
  ungroup() %>%
  mutate(abund = round(nrow(sample_community) / length(unique(species))))

sample_overlapped <- function(sad) {
  mean_size <- runif(n = 1, min = min(sad$meanwgt), max = max(sad$meanwgt))
  
  sampled <- data.frame(
    species = rep(1:nrow(sad), times = sad$abund),
    wgt = rnorm(mean = mean_size, sd = .05 * mean_size, n = sum(sad$abund))
  )
}

set.seed(1977)

overlapped_samples <- replicate(n = 25, expr = sample_overlapped(sad), simplify = F)
names(overlapped_samples) <- 1:25


samples_df <- bind_rows(overlapped_samples, .id = "sim") %>%
  mutate(sim = as.numeric(sim))

isd_s_plot <- ggplot(data = samples_df, aes(x = wgt)) +
  geom_density() +
  theme_bw() +
  facet_wrap(vars(sim), scales = "free_y") +
  xlim(0, 200)

sbsds_s_plot <- ggplot(data = samples_df, aes(x = wgt,fill = species, group = species)) +
  geom_density(alpha = .3) +
  theme_bw() +
  scale_fill_viridis_c(end = .8) +
  theme(legend.position = "top") +
    facet_wrap(vars(sim), scales = "free_y") +
  xlim(0, 200)
isd_s_plot
```

![](portal_files/figure-markdown_github/complete%20overlap-1.png)

``` r
sbsds_s_plot
```

![](portal_files/figure-markdown_github/complete%20overlap-2.png)

``` r
#gridExtra::grid.arrange(grobs = list(isd_s_plot, sbsds_s_plot), nrow = 1)
overlapped_overlaps <- lapply(overlapped_samples, FUN = community_overlap)

names(overlapped_overlaps) <- 1:25

overlapped_overlaps <- bind_rows(overlapped_overlaps, .id = "sim") %>%
  mutate(sim = as.numeric(sim))


o_s_h <- ggplot(data = overlapped_overlaps, aes(x = overlap)) +
  stat_ecdf() +
  theme_bw() +
  xlim(-.2, 1.2) +
  facet_wrap(vars(sim), scales = "free_y")

overlapped_overlaps_hm_weighted <- data.frame(
  sim = rep(overlapped_overlaps$sim, times = round(overlapped_overlaps$hm)),
  overlap_w = rep(overlapped_overlaps$overlap, times = round(overlapped_overlaps$hm))
)

o_s_w_h <- ggplot(data = overlapped_overlaps_hm_weighted, aes(x = overlap_w)) +
    stat_ecdf() +
  theme_bw() +
  xlim(-.2, 1.2) +
  facet_wrap(vars(sim), scales = "free_y")

overlapped_overlaps_n_weighted <- data.frame(
  sim = rep(overlapped_overlaps$sim, times = round(overlapped_overlaps$total_n)),
  overlap_w = rep(overlapped_overlaps$overlap, times = round(overlapped_overlaps$total_n))
)
o_s_w_n_h <- ggplot(data = overlapped_overlaps_n_weighted, aes(x = overlap_w)) +
    stat_ecdf() +
  theme_bw() +
  xlim(-.2, 1.2) +
  facet_wrap(vars(sim), scales = "free_y")
o_s_h
```

![](portal_files/figure-markdown_github/complete%20overlap-3.png)

``` r
o_s_w_h
```

![](portal_files/figure-markdown_github/complete%20overlap-4.png)

``` r
o_s_w_n_h
```

![](portal_files/figure-markdown_github/complete%20overlap-5.png)

``` r
overlapped_overlaps_p_weighted <- data.frame(
  sim = rep(overlapped_overlaps$sim, times = round(overlapped_overlaps$prod_n)),
  overlap_w = rep(overlapped_overlaps$overlap, times = round(overlapped_overlaps$prod_n))
)
o_s_w_p_h <- ggplot(data = overlapped_overlaps_p_weighted, aes(x = overlap_w, color = as.factor(sim))) +
    stat_ecdf() +
  theme_bw() +
  xlim(-.2, 1.2) +
  stat_ecdf(data = real_overlap_p_weighted, aes(x = weighted_overlap), color = "black")
  #facet_wrap(vars(sim), scales = "free_y")
o_s_h
```

![](portal_files/figure-markdown_github/complete%20overlap-6.png)

``` r
o_s_w_h
```

![](portal_files/figure-markdown_github/complete%20overlap-7.png)

``` r
o_s_w_n_h
```

![](portal_files/figure-markdown_github/complete%20overlap-8.png)

``` r
o_s_w_p_h
```

![](portal_files/figure-markdown_github/complete%20overlap-9.png)