Major questions
================

Can we differentiate between an empirical BSD and uniform?
----------------------------------------------------------

![](main_files/figure-markdown_github/plot%20empirical%20and%20uniform%20BSDs-1.png)

    ## Package 'mclust' version 5.4.5
    ## Type 'citation("mclust")' for citing this R package in publications.

![](main_files/figure-markdown_github/fit%20gaussians%20to%20bsds-1.png)![](main_files/figure-markdown_github/fit%20gaussians%20to%20bsds-2.png)

    ## # A tibble: 2 x 3
    ##   source    AICc_nbclumps BIC_nbclumps
    ##   <chr>             <int>        <int>
    ## 1 empirical             1            1
    ## 2 uniform               1            1

Can we differentiate between an empirical ISD and uniform?
----------------------------------------------------------

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](main_files/figure-markdown_github/plot%20empirical%20and%20uniform%20ISDs-1.png)

![](main_files/figure-markdown_github/fit%20gaussians%20to%20isds-1.png)![](main_files/figure-markdown_github/fit%20gaussians%20to%20isds-2.png)

    ## # A tibble: 2 x 3
    ##   source    AICc_nbclumps BIC_nbclumps
    ##   <chr>             <int>        <int>
    ## 1 empirical             2            3
    ## 2 uniform               5            4

Can we differentiate between an empirical ISD and a minimally-constrained uniform?
----------------------------------------------------------------------------------

By "minimally-constrained uniform", I mean the following assumptions/steps:

-   Species' mean body sizes are drawn at random
-   Sd scales as .1 body size (generous)
-   Abundance is a logseries (parameter derives only from S and N and provisionally seems like a pretty good fit even compared to the feasible set) matched to species at random
-   "sorted" = if we sort the SAD decreasing with increasing body size

<!-- -->

    ##      source wgt
    ## 1 empirical   8
    ## 2 empirical   8
    ## 3 empirical   8
    ## 4 empirical   8
    ## 5 empirical   9
    ## 6 empirical   9

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](main_files/figure-markdown_github/minimally%20constrained%20uniform-1.png)

    ## # A tibble: 4 x 3
    ##   source              AICc_nbclumps BIC_nbclumps
    ##   <chr>                       <int>        <int>
    ## 1 empirical                       2            3
    ## 2 uniform                         5            4
    ## 3 uniform_constrained             2            2
    ## 4 uniform_sortedN                 2            2

OK, so the histograms "look" different but the number of turnpoints do not differ. The "looking" different here is the number of individuals per mode. We can calculate this as the number of individuals below/between each minimum.

    ## # A tibble: 11 x 3
    ##    source              mode   nind
    ##    <chr>               <chr> <dbl>
    ##  1 empirical           1        61
    ##  2 empirical           2       446
    ##  3 empirical           3        16
    ##  4 uniform             1        89
    ##  5 uniform             2       228
    ##  6 uniform             3       163
    ##  7 uniform             4        43
    ##  8 uniform_sortedN     1       155
    ##  9 uniform_sortedN     2       185
    ## 10 uniform_constrained 1        83
    ## 11 uniform_constrained 2       257
