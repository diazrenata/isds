Bootstrapping the mean
================

Demo with simple cases
----------------------

1.  All species are essentially the same; complete overlap
2.  Two clusters of species. Within clusters, species are essentially the same. The clusters do not overlap.
3.  All species are distinct and do not overlap.
4.  All species are different but overlap significantly.

We will assume all species' SBSDs are normal distributions, and that the standard deviation scales with the mean according to some coefficient. `.15` seems like a plausible coefficient to me.

![](bootstrap_means_files/figure-markdown_github/plot%20sad-1.png)

    ## V12 V13 V14 V11 
    ##   3  82 114   2

![](bootstrap_means_files/figure-markdown_github/plot%20ls%20outcomes-1.png)![](bootstrap_means_files/figure-markdown_github/plot%20ls%20outcomes-2.png)![](bootstrap_means_files/figure-markdown_github/plot%20ls%20outcomes-3.png)![](bootstrap_means_files/figure-markdown_github/plot%20ls%20outcomes-4.png)

``` r
resample_community <- function(community_df) {
  
  obs_means <- community_df %>%
    dplyr::group_by(species) %>%
    dplyr::summarize(meanwgt = mean(wgt),
                     nind = dplyr::n()) %>%
    dplyr::ungroup()
  
  obs_means_lown <- filter(obs_means, nind <= 50) %>%
    group_by(species) %>%
    mutate(meanwgt = rnorm(n= 1, mean = meanwgt, sd = .15 * meanwgt)) %>%
    ungroup()
  
  obs_means[ which(obs_means$nind <= 50), "meanwgt"] <- obs_means_lown$meanwgt
  
  new_abunds <- obs_means$nind * 50 / min(obs_means$nind)
  
  new_community <- sample_community(means = obs_means$meanwgt, abunds = new_abunds)
}
```

``` r
resampled_communities_raw <- lapply(ls_communities, FUN = function(a_community) 
  return(replicate(n = 100, expr = community_overlap(resample_community(a_community)), simplify = F)))

resampled_communities <- lapply(resampled_communities_raw, FUN = function(resamples)
  return(bind_rows(resamples, .id = "sim")))

resampled_communities <- bind_rows(resampled_communities, .id = "source") %>%
  mutate(sim_source = paste0(source, sim))

original_communities <- lapply(ls_communities, FUN = community_overlap)

original_communities <- bind_rows(original_communities, .id = "source") %>%
  mutate(sim = -99, sim_source =paste0(source, sim))

ls_new_means <- lapply(ls_communities, FUN = function(community_df) 
  return((community_df %>% group_by(species) %>% summarize(meanwgt = mean(wgt)) %>% ungroup())$meanwgt))
new_abunds <- ls_abunds * 50 / min(ls_abunds)

ls_up_error_communities <- lapply(ls_new_means, FUN = sample_community, abunds = new_abunds)

ls_up_overlap <- lapply(ls_up_error_communities, FUN = community_overlap) 

ls_up_overlap <- bind_rows(ls_up_overlap, .id = "source") %>%
  mutate(sim = -99, sim_source =paste0(source, sim))
```

``` r
ecdfs_plots <- ggplot(data = resampled_communities, aes(x = overlap, color = source, group = sim_source)) +
  stat_ecdf(alpha = .05) +
  theme_bw() +
  #facet_wrap(vars(source)) +
  scale_color_viridis_d(option = "magma", end = .7) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  stat_ecdf(data = original_communities, aes(x = overlap, group = sim_source), alpha = 1, linetype = 2)


ecdfs_plots
```

![](bootstrap_means_files/figure-markdown_github/plot%20ecdfs-1.png)

``` r
ecdfs_plots_facetted <- ecdfs_plots +
  facet_wrap(vars(source))

ecdfs_plots_facetted
```

![](bootstrap_means_files/figure-markdown_github/plot%20ecdfs-2.png)

``` r
hists_plots <- ggplot(data = resampled_communities, aes(x = overlap, color = source, fill = source, group = sim_source)) +
  geom_density(alpha = 0, size = .05) +
  theme_bw()+
  facet_wrap(vars(source), scales = "free_y") +
  scale_color_viridis_d(option = "magma", end = .7) +
  scale_fill_viridis_d(option = "magma", end = .7) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  geom_density(alpha = 0, data = original_communities, aes(x = overlap, group = sim_source), linetype = 2) +
  geom_density(alpha = 0, data = ls_up_overlap, aes(x = overlap, group = sim_source), linetype = 5)


hists_plots
```

![](bootstrap_means_files/figure-markdown_github/plot%20hists-1.png)

``` r
dice_density <- function(community_overlap_df, npieces = 10) {
  
  overlap_density <- density(community_overlap_df$overlap, from = 0, to = 1)
  
  overlap_density <- data.frame(
    val = overlap_density$x,
    density = overlap_density$y
  ) %>%
    mutate(density = density / sum(density))
  
  dice_points <- seq(0, 1, length.out = npieces + 1)
  
  diced_density <- data.frame(min_x = dice_points[1:length(dice_points) - 1],
                              max_x = dice_points[2:length(dice_points)], 
                              sum_density = 0,
                              mean_x = 0)
  
  for(i in 1:nrow(diced_density)) {
    
    this_section <- filter(overlap_density, val >= diced_density$min_x[i], val < diced_density$max_x[i])
    
    diced_density$sum_density[i] <- sum(this_section$density)
    diced_density$mean_x[i] <- sum(diced_density$min_x[i], diced_density$max_x[i]) / 2
    
  }
  
  return(diced_density)
  
}
```

``` r
diced_resamples <- lapply(resampled_communities_raw, FUN = function(list_of_community_df) return(lapply(list_of_community_df, FUN = function(community_df) return(dice_density((community_df))))))

diced_original <- lapply(ls_communities, FUN = function(community_df) return(dice_density(community_overlap(community_df))))

diced_up <- lapply(ls_up_error_communities, FUN = function(community_df) return(dice_density(community_overlap(community_df))))


diced_resamples <- lapply(diced_resamples, FUN = function(diced) return(bind_rows(diced, .id = "sim")))

diced_resamples <- bind_rows(diced_resamples, .id = "source")
diced_original <- bind_rows(diced_original, .id = "source") %>%
  mutate(sim = NA, ss = source)
diced_up <- bind_rows(diced_up, .id = "source") %>%
  mutate(sim = NA, ss = source)

diced_resamples$ss = paste0(diced_resamples$source, diced_resamples$sim)
```

``` r
diced_plot <- ggplot(data = diced_resamples, aes(x = mean_x, y = sum_density, color = source, group = ss)) +
  geom_line(size = .03) +
  theme_bw() +
  scale_color_viridis_d(end = .7, option = "magma") +
  geom_line(data = diced_original, linetype = 3, alpha = 1)+
  geom_line(data = diced_up, linetype = 5, alpha = .5) +
  facet_wrap(vars(source))

diced_plot
```

![](bootstrap_means_files/figure-markdown_github/diced%20plots-1.png)
