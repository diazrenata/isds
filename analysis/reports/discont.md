Exploring discontinuity analysis
================

``` r
library(drake)
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
knitr::opts_chunk$set(echo = TRUE)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake", "drake-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)
```

Get a sim.

``` r
toyp <- get_toy_portal_data()
```

    ## Loading in data version 1.90.0

``` r
ebsd <- toyp %>%
  group_by(species) %>%
  summarize(meanwgt = mean(wgt)) %>%
  ungroup() %>%
  mutate(logwgt = log(meanwgt))

bsdk <- ks::kde(ebsd$logwgt)

plot(bsdk)
```

![](discont_files/figure-markdown_github/get%20mean%20bsd-1.png)

``` r
bsdkp <- data.frame(
  eval = bsdk$eval.points,
  estimate = bsdk$estimate
)

bsdkp <- bsdkp %>%
  filter(eval > 0) %>%
  mutate(stdp = estimate / sum(estimate))

drawbsd <- function(){
  
  thisbsd <- sample(bsdkp$eval, size = 7, replace = T, prob = bsdkp$stdp)
  
  return(thisbsd)
}

sims <- replicate(n = 1000, expr = drawbsd(), simplify = T) %>%
  t()

find_gaps <- function(bsd) {
  
  bsd <- sort(bsd) 
  
  gaps <- vector(length = length(bsd) - 1)
  
  for(i in 1:(length(gaps))) {
    gaps[i] = bsd[i + 1] - bsd[i]
  }
  
  return(gaps)
}


sim_gaps <- apply(sims, MARGIN = 1, FUN = find_gaps) %>%
  t()

mean_gap <- apply(sim_gaps, MARGIN = 1, FUN = mean)
sd_gap <- apply(sim_gaps, MARGIN = 1, FUN = sd)

hist(mean_gap)
```

![](discont_files/figure-markdown_github/get%20mean%20bsd-2.png)

``` r
hist(sd_gap)
```

![](discont_files/figure-markdown_github/get%20mean%20bsd-3.png)

``` r
mean(sim_gaps)
```

    ## [1] 0.4785841

``` r
sd(sim_gaps)
```

    ## [1] 0.4207266

``` r
emp_gap <- find_gaps(ebsd$logwgt)
mean(emp_gap)
```

    ## [1] 0.4179499

``` r
sd(emp_gap)
```

    ## [1] 0.2564652

``` r
all_gaps <- as.vector(sim_gaps)

all_gaps <- as.data.frame(all_gaps)

emp_gap <- as.data.frame(emp_gap)

library(ggplot2)

gaphist <- ggplot(data = all_gaps, aes(x = all_gaps)) + 
  geom_freqpoly() +
  geom_point(data = emp_gap, aes(x = emp_gap, y = 2)) +
  geom_vline(xintercept = quantile(all_gaps$all_gaps, probs = c(0.5, 0.75, 0.95)))

gaphist
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](discont_files/figure-markdown_github/get%20mean%20bsd-4.png)
