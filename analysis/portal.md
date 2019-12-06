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
  geom_histogram(boundary = 0) +
  theme_bw() +
  xlim(-.2, 1.2)

real_overlap_weighted <- data.frame(
  weighted_overlap = rep(real_overlap$overlap, times = round(real_overlap$hm)))

r_o_w_h <- ggplot(data = real_overlap_weighted, aes(x = weighted_overlap)) +
  geom_histogram(boundary = 0) +
  theme_bw() +
  xlim(-.2, 1.2)

gridExtra::grid.arrange(grobs = list(r_o_h, r_o_w_h), nrow = 1)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](portal_files/figure-markdown_github/overlap%20real-1.png)

``` r
sad <- sample_community %>%
  group_by(species) %>%
  summarize(abund = dplyr::n(),
            meanwgt = mean(wgt)) %>%
  ungroup()

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
  geom_histogram(boundary = 0) +
  theme_bw() +
  xlim(-.2, 1.2) +
  facet_wrap(vars(sim), scales = "free_y")

overlapped_overlaps_weighted <- data.frame(
  sim = rep(overlapped_overlaps$sim, times = round(overlapped_overlaps$hm)),
  overlap_w = rep(overlapped_overlaps$overlap, times = round(overlapped_overlaps$hm))
)

o_s_w_h <- ggplot(data = overlapped_overlaps_weighted, aes(x = overlap_w)) +
    geom_histogram(boundary = 0) +
  theme_bw() +
  xlim(-.2, 1.2) +
  facet_wrap(vars(sim), scales = "free_y")
o_s_h
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 50 rows containing missing values (geom_bar).

![](portal_files/figure-markdown_github/complete%20overlap-3.png)

``` r
o_s_w_h
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 50 rows containing missing values (geom_bar).

![](portal_files/figure-markdown_github/complete%20overlap-4.png)
