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

toyp <- get_toy_portal_data()
```

    ## Loading in data version 1.90.0

``` r
ebsd <- toyp %>%
  group_by(species) %>%
  summarize(meanwgt = mean(wgt)) %>%
  ungroup() %>%
  mutate(logwgt = log(meanwgt))

set.seed(1977)

uniform_log_bsd <- draw_uniform_bsd(s = nrow(ebsd), min = .75 * min(ebsd$logwgt), max = 1.1 *max(ebsd$logwgt))

unimodal_log_bsd <- draw_unimodal_bsd(ebsd$logwgt)

mmodal_log_bsd_full <- draw_multimodal_bsd(emp_vector = ebsd$logwgt,
                                       max_sd_coeff = .5)

mmodal_log_bsd <- mmodal_log_bsd_full$bsd


uniform_bsd <- draw_uniform_bsd(s = nrow(ebsd), min = .75 * min(ebsd$meanwgt), max = 1.1 * max(ebsd$meanwgt))
unimodal_bsd <- draw_unimodal_bsd(ebsd$meanwgt)
mmodal_bsd_full <- draw_multimodal_bsd(emp_vector = ebsd$meanwgt, min_mode_gap = 20, min_sd_coeff = .3, max_sd_coeff = 1)
mmodal_bsd <- mmodal_bsd_full$bsd
```

``` r
all_log_bsds <- data.frame(
  vals = c(mmodal_log_bsd, uniform_log_bsd, unimodal_log_bsd, ebsd$logwgt),
  source = c(rep("multimodal", 7),
             rep("uniform", 7), 
             rep("unimodal", 7),
             rep("empirical", 7))
)


all_log_bsd_plot <- ggplot(data = all_log_bsds, aes(x = vals, y = source, color = source)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none")

all_log_bsd_plot
```

![](bsd_decay_files/figure-markdown_github/visualize-1.png)

``` r
all_bsds <- data.frame(
  vals = c(mmodal_bsd, uniform_bsd, unimodal_bsd, ebsd$meanwgt),
  source = c(rep("multimodal", 7),
             rep("uniform", 7), 
             rep("unimodal", 7),
             rep("empirical", 7))
)


all_bsd_plot <- ggplot(data = all_bsds, aes(x = vals, y = source, color = source)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none")

all_bsd_plot
```

![](bsd_decay_files/figure-markdown_github/visualize-2.png)

``` r
raw_clumps <- all_bsds %>%
  group_by(source) %>%
  summarize(nbclumps = get_n_clumps(vals))
```

    ## Package 'mclust' version 5.4.5
    ## Type 'citation("mclust")' for citing this R package in publications.

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

``` r
log_clumps <- all_log_bsds %>%
  group_by(source) %>%
  summarize(nbclumps = get_n_clumps(vals))
```

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

``` r
log_clumps
```

    ## # A tibble: 4 x 2
    ##   source     nbclumps
    ##   <fct>         <int>
    ## 1 empirical         1
    ## 2 multimodal        3
    ## 3 uniform           1
    ## 4 unimodal          1

``` r
raw_clumps
```

    ## # A tibble: 4 x 2
    ##   source     nbclumps
    ##   <fct>         <int>
    ## 1 empirical         4
    ## 2 multimodal        3
    ## 3 uniform           2
    ## 4 unimodal          4

``` r
raw_ssq<- all_bsds %>%
  group_by(source) %>%
  summarize(ssq_prop = get_ssq_prop(vals))
```

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

``` r
log_ssq <- all_log_bsds %>%
  group_by(source) %>%
  summarize(ssq_prop = get_ssq_prop(vals))
```

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

    ## Warning in pastecs::turnpoints(density_estimates): value out of range in
    ## 'gammafn'

``` r
raw_ssq
```

    ## # A tibble: 4 x 2
    ##   source     ssq_prop
    ##   <fct>         <dbl>
    ## 1 empirical     0.995
    ## 2 multimodal    1.000
    ## 3 uniform       0.823
    ## 4 unimodal      0.989

``` r
log_ssq
```

    ## # A tibble: 4 x 2
    ##   source      ssq_prop
    ##   <fct>          <dbl>
    ## 1 empirical   0.      
    ## 2 multimodal  9.69e- 1
    ## 3 uniform     0.      
    ## 4 unimodal   -1.26e-16
