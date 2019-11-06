KDEs, GMMs, and turns
================

``` r
isd_plots <- ggplot(data = filter(all_isds, sim %in% c(NA, 1, 10, 20, 30, 40, 50, 60, 70, 80, 90)), aes(x = wgt)) +
  geom_density() +
  theme_bw() +
  facet_grid(cols = vars(sim), rows = vars(source), scales = "free_y", drop = T) +
  xlim(0, 300) +
  ggtitle("ISDs")
isd_plots
```

    ## Warning: Removed 258 rows containing non-finite values (stat_density).

![](turns_etc_files/figure-markdown_github/isd%20plots-1.png)

``` r
kde_plots <- ggplot(data = filter(all_kdes, sim %in% c(NA, 1, 10, 20, 30, 40, 50, 60, 70, 80, 90)), aes(x = wgt, y = density)) +
  geom_point() +
  theme_bw() +
  facet_grid(cols = vars(sim), rows = vars(source), scales = "free_y", drop = T) +
  xlim(0, 300) +
  ggtitle("KDEs")
kde_plots
```

    ## Warning: Removed 1610 rows containing missing values (geom_point).

![](turns_etc_files/figure-markdown_github/kde%20plots-1.png)

``` r
gmm_plots <- ggplot(data = filter(all_gmms, sim %in% c(NA, 1, 10, 20, 30, 40, 50, 60, 70, 80, 90)), aes(x = wgt, y = density)) +
  geom_point() +
  theme_bw() +
  facet_grid(cols = vars(sim), rows = vars(source), scales = "free_y", drop = T) +
  xlim(0, 300) +
  ggtitle("GMMss")
gmm_plots
```

    ## Warning: Removed 1610 rows containing missing values (geom_point).

![](turns_etc_files/figure-markdown_github/gmm%20plots-1.png)

``` r
find_most_density <- function(density_vector, cutoff = .9) {
  
  density_df <- data.frame(initial_order = 1:length(density_vector),
                           density = density_vector)
  
  density_df <- density_df %>%
    arrange(desc(density)) %>%
    mutate(density_index = row_number())
  
  running_sum <- 0
  for(i in 1:nrow(density_df)) {
    running_sum <- running_sum + density_df$density[i]
    if(running_sum >= cutoff) {
      break
    }
  }
  
  density_df$in_most <- (density_df$density_index <= i)
  
  density_df <- density_df %>%
    arrange(initial_order)
  
  return(density_df$in_most)
}

all_gmms <- all_gmms %>%
  group_by(sim, source) %>%
  mutate(in_most = find_most_density(density, cutoff = .95)) %>%
  ungroup()

all_kdes <- all_kdes %>%
  group_by(sim, source) %>%
  mutate(in_most = find_most_density(density, cutoff = .95)) %>%
  ungroup()
```

``` r
kde_plots <- ggplot(data = filter(all_kdes, sim %in% c(NA, 1, 10, 20, 30, 40, 50, 60, 70, 80, 90)), aes(x = wgt, y = density, color = in_most)) +
  geom_point() +
  theme_bw() +
  facet_grid(cols = vars(sim), rows = vars(source), scales = "free_y", drop = T) +
  xlim(0, 300) +
  ggtitle("KDEs")+
  scale_color_viridis_d(end = .6)
kde_plots
```

    ## Warning: Removed 1610 rows containing missing values (geom_point).

![](turns_etc_files/figure-markdown_github/kde%20plots%202-1.png)

``` r
gmm_plots <- ggplot(data = filter(all_gmms, sim %in% c(NA, 1, 10, 20, 30, 40, 50, 60, 70, 80, 90)), aes(x = wgt, y = density, color = in_most)) +
  geom_point() +
  theme_bw() +
  facet_grid(cols = vars(sim), rows = vars(source), scales = "free_y", drop = T) +
  xlim(0, 300) +
  ggtitle("GMMs") +
  scale_color_viridis_d(end = .6)
gmm_plots
```

    ## Warning: Removed 1610 rows containing missing values (geom_point).

![](turns_etc_files/figure-markdown_github/gmm%20plots%202-1.png)
