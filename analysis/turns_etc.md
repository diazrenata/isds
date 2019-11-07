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
  mutate(in_most = find_most_density(density, cutoff = .9)) %>%
  ungroup()

all_kdes <- all_kdes %>%
  group_by(sim, source) %>%
  mutate(in_most = find_most_density(density, cutoff = .9)) %>%
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

``` r
all_kdes <- all_kdes %>%
  mutate(smoother = "kde") 
all_gmms <- all_gmms %>% 
  mutate(smoother = "gmm")

all <- bind_rows(all_kdes, all_gmms)


summarize_chunks <- function(in_most_df) {
  
  nchunks <- 0
  
  for(i in 2:nrow(in_most_df)) {
    if(all(in_most_df$in_most[i], in_most_df$in_most[i - 1] == FALSE)) {
      nchunks <- nchunks + 1
    }
  }
  
  min_in_most <- min(in_most_df[ which(in_most_df$in_most), "density"])
  max_in_most <- max(in_most_df[ which(in_most_df$in_most), "density"])
  
  edges_ratio <- max_in_most/min_in_most
  
  output <- data.frame(sim = in_most_df$sim[1], source = in_most_df$source[1], smoother = in_most_df$smoother[1], nchunks = nchunks, edges_ratio = edges_ratio, stringsAsFactors = F)
  
  return(output)
}

all_chunks <- list() 

for(i in 1:min(length(unique(all$sim)), 30)) {
  for(j in 1:length(unique(all$smoother))) {
    for(k in 1:length(unique(all$source))) {
      this_df <- filter(all, sim == unique(all$sim)[i],
                        smoother == unique(all$smoother)[j],
                        source == unique(all$source)[k])
      if(nrow(this_df) < 1) {
        next
      }
      
      all_chunks[[length(all_chunks) + 1]] <- summarize_chunks(this_df)
    }
  }
}

all_chunks <- bind_rows(all_chunks)

all_chunks <- all_chunks %>%
  arrange(desc(nchunks)) %>%
  mutate(chunk_rank = row_number()) %>%
  arrange(desc(edges_ratio)) %>%
  mutate(edges_rank = row_number())


all <- left_join(all, all_chunks, by = c("sim", "smoother", "source")) %>%
  mutate(nchunks = as.factor(nchunks))
```

``` r
nchunks_plot <- ggplot(data = filter(all, chunk_rank %% 10 == 1), aes(x = wgt, y = density, color = nchunks)) +
  geom_point(size = .5) +
  theme_bw() +
  scale_color_viridis_d(end = .8) +
  facet_wrap(vars(chunk_rank))
```

``` r
er_plot <- ggplot(data =filter(all, sim <= 25), aes(x = wgt, y = density, color = nchunks, shape = in_most)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis_d(option = "plasma", end = .8) +
  facet_wrap(vars(edges_rank), scales = "free")
er_plot
```

![](turns_etc_files/figure-markdown_github/by%20edge%20ratio%20plot-1.png)