Power law - truncated Pareto
================

From Thibault et al 2011:

"Using individual-level masses, we fitted the three major classes of frequency distributions to the data. We used the power law to characterize the monotonic decrease. Specifically, we fitted the truncated Pareto distribution to log10-transformed data using mle.truncpareto.m from White et al 2008, estimating the true minimum and maximum sizes using observed values (which overestimates the quality of the fit and is therefore conservative in this context). This produces an estimate of the frequency distribution of sizes comparable to traditional analyses of ISDs in trees (refs...) and the number density spectrum used in aquatic systems (sensu Andersen & Beyer 2006), with the data transformation affecting the exponent (but not the form) of the power function."

``` r
library(isds)
library(drake)

## Set up the cache and config
db <- DBI::dbConnect(RSQLite::SQLite(), here::here("drake", "drake-cache.sqlite"))
cache <- storr::storr_dbi("datatable", "keystable", db)

dat1 <- readd(dat1, cache = cache)

dat1 <- dat1 %>%
  dplyr::mutate(log10_size = log(wgt, base = 10))
```

Fit raw
-------

`vglm` cannot fit a truncated Pareto to either the raw or the log10-transformed data. Is it just too far from a powerlaw for the MLE to work?

A next attempt might be to find the best-fitting estimates by minimizing the KS D or maximizing R2, then evaluating whether that is significantly different via a KS test (or calculating the loglikelihood of the observed vector vs samples from the corresponding rtruncpareto).

I'm not sure if any of this is a good use of time, because these are visually not power-law.

Trying with code from Clauset et al
-----------------------------------

``` r
library(poweRlaw)

data("moby", package = "poweRlaw")

m_pl <- displ$new(moby)

m_pl
```

    ## Reference class object of class "displ" 
    ## Field "xmin": 
    ## [1] 1
    ## Field "pars": 
    ## NULL
    ## Field "no_pars": 
    ## [1] 1

``` r
est = estimate_xmin(m_pl)

m_pl$setXmin(est)

bs <- bootstrap(m_pl, no_of_sims = 100, threads = 2)
```

    ## Expected total run time for 100 sims, using 2 threads is 61.1 seconds.

``` r
plot(bs, trim = 0.1)
```

![](powerlaw_files/figure-markdown_github/clauset%20toy%20pl-1.png)

``` r
bs_p = bootstrap_p(m_pl, threads = 2)
```

    ## Expected total run time for 100 sims, using 2 threads is 67.4 seconds.

``` r
bs_p$p
```

    ## [1] 0.79

``` r
plot(bs_p)
```

![](powerlaw_files/figure-markdown_github/clauset%20toy%20pl-2.png)

``` r
d_pl <- displ$new(dat1$wgt)

d_bs <- bootstrap(d_pl, no_of_sims = 100, threads = 2)
```

    ## Expected total run time for 100 sims, using 2 threads is 37.8 seconds.

``` r
plot(d_bs)
```

![](powerlaw_files/figure-markdown_github/clauset%20pl%20on%20real%20dat-1.png)

``` r
d_p_bs <- bootstrap_p(d_pl, no_of_sims = 100, threads = 2)
```

    ## Expected total run time for 100 sims, using 2 threads is 30.8 seconds.

``` r
plot(d_p_bs)
```

![](powerlaw_files/figure-markdown_github/clauset%20pl%20on%20real%20dat-2.png)

``` r
d_p_bs$p
```

    ## [1] 0

``` r
est_d_pl <- estimate_xmin(d_pl)
d_pl$setXmin(est_d_pl)


plot(d_pl)
lines(d_pl)
```

![](powerlaw_files/figure-markdown_github/clauset%20pl%20on%20real%20dat-3.png)
