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
  in_most_df$chunk <- NA
  
  for(i in 1:nrow(in_most_df)) {
    if(i == 1) {
      if(in_most_df$in_most[i]) {
        nchunks <- nchunks + 1
      }
    } else if(all(in_most_df$in_most[i], in_most_df$in_most[i - 1] == FALSE)) {
      nchunks <- nchunks + 1
    }
    
    if(all(nchunks > 0, in_most_df$in_most[i])) {
    in_most_df$chunk[i] <- nchunks
    }
  }
  
  min_in_most <- min(in_most_df[ which(in_most_df$in_most), "density"])
  max_in_most <- max(in_most_df[ which(in_most_df$in_most), "density"])
  
  edges_ratio <- max_in_most/min_in_most
  

  in_most_evenness <- in_most_df %>%
    filter(in_most) %>%
    group_by(chunk) %>%
    summarize(chunk_sum = sum(density)) %>%
    ungroup()
  
  chunk_dom <- vegan::diversity(in_most_evenness$chunk_sum, index = "simpson")
  
  
  in_most_evenness_cutoff <- in_most_evenness %>%
    filter(chunk_sum / sum(in_most_evenness$chunk_sum) >= .1)
  
  cutoff_chunk_dom = vegan::diversity(in_most_evenness_cutoff$chunk_sum, index = "simpson")
  
  nchunks_cutoff <- nrow(in_most_evenness_cutoff)
  
  output <- data.frame(sim = in_most_df$sim[1], source = in_most_df$source[1], smoother = in_most_df$smoother[1], nchunks = nchunks, edges_ratio = edges_ratio, chunk_dominance = chunk_dom, cutoff_dom = cutoff_chunk_dom, nchunks_cutoff = nchunks_cutoff, stringsAsFactors = F)
  
  return(output)
}
# 
# add_chunk <- function(in_most_df) {
#   
#   nchunks <- 0
#   in_most_df$chunk <- NA
#   
#   for(i in 2:nrow(in_most_df)) {
#     if(all(in_most_df$in_most[i], in_most_df$in_most[i - 1] == FALSE)) {
#       nchunks <- nchunks + 1
#     }
#     
#     if(nchunks > 0) {
#     in_most_df$chunk[i] <- nchunks
#     }
#   }
#   return(in_most_df)
# }

all_chunks <- list() 

for(i in 1:min(30, length(unique(all$sim)))) {
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
  mutate(edges_rank = row_number()) %>%
  group_by(smoother, nchunks) %>%
  arrange(desc(edges_ratio)) %>%
  mutate(within_nchunks_e_r = row_number()) %>%
  arrange(desc(chunk_dominance)) %>%
  mutate(dominance_rank = row_number()) %>%
  ungroup() %>%
  group_by(smoother, nchunks_cutoff) %>%
  arrange(desc(cutoff_dom)) %>%
  mutate(cutoff_dominance_rank = row_number()) %>%
  ungroup()


all <- left_join(all, all_chunks, by = c("sim", "smoother", "source")) %>%
  mutate(nchunks = as.factor(nchunks),
         nchunks_cutoff = as.factor(nchunks_cutoff)) %>%
  group_by(sim, smoother, source) %>%
  mutate(p_sd = sd(density)) %>%
  ungroup()
```

``` r
nchunks_plot <- ggplot(data = filter(all, chunk_rank %% 10 == 1), aes(x = wgt, y = density, color = nchunks)) +
  geom_point(size = .5) +
  theme_bw() +
  scale_color_viridis_d(end = .8) +
  facet_wrap(vars(chunk_rank))
```

``` r
gmm_er_plot <- ggplot(data =filter(all, smoother == "gmm", as.numeric(nchunks) > 1, in_most), aes(x = wgt, y = density, color = chunk_dominance, alpha = in_most)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis_c(end = .8) +
  facet_grid(rows = vars(nchunks), cols = vars(dominance_rank), scales= "free") +
  ggtitle("GMMs")
gmm_er_plot
```

    ## Warning: Using alpha for a discrete variable is not advised.

![](turns_etc_files/figure-markdown_github/by%20edge%20ratio%20plot%20alpha%20no%20uniform-1.png)

``` r
gmm_er_plot_nou <- ggplot(data =filter(all, smoother == "gmm", source != "uniform", in_most), aes(x = wgt, y = density, color = chunk_dominance, alpha = in_most)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis_c(end = .8) +
  facet_grid(rows = vars(nchunks), cols = vars(dominance_rank), scales= "free") +
  ggtitle("GMMs")
gmm_er_plot_nou
```

    ## Warning: Using alpha for a discrete variable is not advised.

![](turns_etc_files/figure-markdown_github/by%20edge%20ratio%20plot%20alpha%20no%20uniform-2.png)

``` r
kde_er_plot <- ggplot(data =filter(all, smoother == "kde", as.numeric(nchunks) > 1,in_most), aes(x = wgt, y = density, color = chunk_dominance, alpha = in_most)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis_c(end = .8) +
  facet_grid(rows = vars(nchunks), cols = vars(dominance_rank), scales= "free_x") +
  ggtitle("KDEs")
kde_er_plot
```

    ## Warning: Using alpha for a discrete variable is not advised.

![](turns_etc_files/figure-markdown_github/by%20edge%20ratio%20plot%20alpha%20no%20uniform-3.png)

``` r
kde_er_plot_nou <- ggplot(data =filter(all, smoother == "kde", source != "uniform",in_most), aes(x = wgt, y = density, color = chunk_dominance, alpha = in_most)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis_c(end = .8) +
  facet_grid(rows = vars(nchunks), cols = vars(dominance_rank), scales= "free_x") +
  ggtitle("KDEs")
kde_er_plot_nou
```

    ## Warning: Using alpha for a discrete variable is not advised.

![](turns_etc_files/figure-markdown_github/by%20edge%20ratio%20plot%20alpha%20no%20uniform-4.png)

try removing low-value chunks

``` r
gmm_er_plot_co <- ggplot(data =filter(all, smoother == "gmm", source != "uniform"), aes(x = wgt, y = density, color = cutoff_dom, alpha = in_most)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis_c(end = .8) +
  facet_grid(rows = vars(nchunks_cutoff), cols = vars(cutoff_dominance_rank), scales= "free") +
  ggtitle("GMMs")
gmm_er_plot_co
```

    ## Warning: Using alpha for a discrete variable is not advised.

![](turns_etc_files/figure-markdown_github/remove%20lowval%20chunks-1.png)

``` r
kde_er_plot_co <- ggplot(data =filter(all, smoother == "kde", as.numeric(nchunks) > 1,in_most), aes(x = wgt, y = density, color = cutoff_dom, alpha = in_most)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis_c(end = .8) +
  facet_grid(rows = vars(nchunks_cutoff), cols = vars(cutoff_dominance_rank), scales= "free_x") +
  ggtitle("KDEs")
kde_er_plot_co
```

    ## Warning: Using alpha for a discrete variable is not advised.

![](turns_etc_files/figure-markdown_github/remove%20lowval%20chunks-2.png)
