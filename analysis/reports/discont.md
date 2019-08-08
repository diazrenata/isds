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

sims <- replicate(n = 500, expr = drawbsd(), simplify = T) %>%
  t()
```

Multimodal sims

``` r
find_gaps <- function(bsd) {
  
  bsd <- sort(bsd) 
  
  gaps <- vector(length = length(bsd) - 1)
  
  for(i in 1:(length(gaps))) {
    gaps[i] = bsd[i + 1] - bsd[i]
  }
  
  return(gaps)
}

nmodes <- sample(c(2,3,4), size = 1) 

modevals <- runif(n = nmodes,
                 min = min(ebsd$meanwgt) * .75, 
                 max = max(ebsd$meanwgt) * 1.1)

modegaps <- find_gaps(log(modevals))

while(min(modegaps) <= .75) {
  
modevals <- runif(n = nmodes,
                 min = min(ebsd$meanwgt) * .75, 
                 max = max(ebsd$meanwgt) * 1.1)

modegaps <- find_gaps(log(modevals))
}

sd_coeff <- runif(n = nmodes, min = 0.5, max = 2)

mode_p <- data.frame(
  val = seq(1, 1.25 * max(modevals), by = .1)
)

for(i in 1:nmodes) {
  mode_p[, i + 1] <- dnorm(mode_p$val, mean = modevals[i], sd = sd_coeff[i])
}

mode_p$sum <- rowSums(mode_p[ , 2:(nmodes + 1)])

mode_p$sum <- mode_p$sum / sum(mode_p$sum)

plot(mode_p$sum)
```

![](discont_files/figure-markdown_github/multimodal%20sims-1.png)
