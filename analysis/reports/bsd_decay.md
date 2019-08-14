BSD decay?
================

``` r
library(isds)
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
knitr::opts_chunk$set(echo = TRUE)

toyp <- get_toy_portal_data(years = 1990:2010)
```

    ## Loading in data version 1.90.0

``` r
ebsd <- toyp %>%
  group_by(species) %>%
  summarize(meanwgt = mean(wgt)) %>%
  ungroup() %>%
  mutate(logwgt = log(meanwgt))

set.seed(1977)

nsims = 100

uniform_log_bsd <- replicate(n = nsims, expr = draw_uniform_bsd(s = nrow(ebsd), min =  min(ebsd$logwgt), max =max(ebsd$logwgt)))

uniform_log_bsd <- as.data.frame(uniform_log_bsd) %>%
  tidyr::gather(key = "sim", value = "val") %>%
  dplyr::mutate(sim = as.integer(substr(sim, start = 2, stop = nchar(sim))),
                source = "uniform")

unimodal_log_bsd <- replicate(n = nsims, expr = draw_unimodal_bsd(ebsd$logwgt))
unimodal_log_bsd <- as.data.frame(unimodal_log_bsd) %>%
  tidyr::gather(key = "sim", value = "val") %>%
  dplyr::mutate(sim = as.integer(substr(sim, start = 2, stop = nchar(sim))),
                source = "unimodal")

mmodal_log_bsd_full <- replicate(n = nsims, expr = draw_multimodal_bsd(emp_vector = ebsd$logwgt, min_mode_gap = .5, min_sd_coeff = .2, max_sd_coeff = .2), simplify = F)

mmodal_log_bsd <- lapply(mmodal_log_bsd_full, FUN = function(X) return(X$bsd))
mmodal_log_bsd <- dplyr::bind_cols(mmodal_log_bsd) %>%
   tidyr::gather(key = "sim", value = "val") %>%
  dplyr::mutate(sim = as.integer(substr(sim, start = 2, stop = nchar(sim))),
                source = "mmodal")

empirical_log_bsd <- ebsd %>%
  select(logwgt) %>%
  mutate(sim = -99, source = "empirical") %>%
  rename(val = logwgt)

all_bsds <- bind_rows(empirical_log_bsd, mmodal_log_bsd, uniform_log_bsd, unimodal_log_bsd)
```

``` r
bsd_modes <- all_bsds %>%
  group_by(source, sim) %>%
  summarize(nb_modes = get_n_clumps(val),
            nb_modes_bic = Mclust(val, G = c(1:5))$G) %>%
  ungroup()
```

    ## Package 'mclust' version 5.4.5
    ## Type 'citation("mclust")' for citing this R package in publications.

``` r
modes_plot <- ggplot(data = bsd_modes, aes(x = nb_modes, color = source, fill = source)) +
  geom_histogram() +
  geom_point(data = bsd_modes[ which(bsd_modes$source == "empirical"), ], aes (x = nb_modes, y = nsims / 4), shape = 8, size = 3) + 
  facet_grid(source ~ . ) +
  theme_bw()


modes_plot
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](bsd_decay_files/figure-markdown_github/plot%20nb%20modes-1.png)

``` r
single_bsds <- lapply(unique(all_bsds$source), FUN = function(X)  return(filter(all_bsds, sim == sample.int(n = nsims, size = 1), source == X)))

single_bsds <- bind_rows(single_bsds) %>%
  left_join(bsd_modes, by = c("sim", "source")) %>%
  mutate(facet_name = paste0("source: ", source, "; ", nb_modes, " mode(s); ", nb_modes_bic, " mode(s) w BIC"))

single_bsd_density_plots <- ggplot(data = single_bsds, aes(x = val, color = source)) +
  geom_density() + 
  facet_wrap(facet_name ~ .) +
  theme_bw()

single_bsd_density_plots
```

![](bsd_decay_files/figure-markdown_github/single%20bsd%20plots-1.png)

``` r
single_bsd_histograms <- ggplot(data = single_bsds, aes(x = val, fill = source, color = source)) +
  geom_histogram() + 
  facet_wrap(facet_name ~ .) +
  theme_bw()

single_bsd_histograms
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](bsd_decay_files/figure-markdown_github/single%20bsd%20plots-2.png)

``` r
empirical_density_plot <- ggplot(data = filter(all_bsds, source == "empirical"), aes (x = val)) +
  geom_density() +
  ggtitle(paste0("Empirical BSD: ", filter(bsd_modes, source == "empirical")$nb_modes, " mode(s)")) +
  theme_bw()

empirical_count_plot <-  ggplot(data = filter(all_bsds, source == "empirical"), aes (x = val)) +
  geom_histogram() +
  ggtitle(paste0("Empirical BSD: ", filter(bsd_modes, source == "empirical")$nb_modes, " mode(s)")) +
  theme_bw()

gridExtra::grid.arrange(grobs = list(empirical_density_plot, empirical_count_plot), nrow = 1)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](bsd_decay_files/figure-markdown_github/single%20bsd%20plots-3.png)
