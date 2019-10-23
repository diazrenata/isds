Major questions
================

Can we differentiate between an empirical ISD and uniform?
----------------------------------------------------------

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](just_isd_files/figure-markdown_github/plot%20empirical%20and%20uniform%20ISDs-1.png)

    ## Package 'mclust' version 5.4.5
    ## Type 'citation("mclust")' for citing this R package in publications.

![](just_isd_files/figure-markdown_github/fit%20gaussians%20to%20isds-1.png)![](just_isd_files/figure-markdown_github/fit%20gaussians%20to%20isds-2.png)

    ## # A tibble: 2 x 3
    ##   source    AICc_nbclumps BIC_nbclumps
    ##   <chr>             <int>        <int>
    ## 1 empirical             2            3
    ## 2 uniform               5            3

Can we differentiate between an empirical ISD and a minimally-constrained uniform?
----------------------------------------------------------------------------------

By "minimally-constrained uniform", I mean the following assumptions/steps:

-   Species' mean body sizes are drawn at random
-   Sd scales as .1 body size (generous)
-   Abundance is a logseries (parameter derives only from S and N and provisionally seems like a pretty good fit even compared to the feasible set) matched to species at random
-   "sorted" = if we sort the SAD decreasing with increasing body size

![](just_isd_files/figure-markdown_github/minimally%20constrained%20uniform-1.png)

    ## # A tibble: 4 x 3
    ##   source              AICc_nbclumps BIC_nbclumps
    ##   <chr>                       <int>        <int>
    ## 1 empirical                       2            3
    ## 2 uniform                         5            3
    ## 3 uniform_constrained             3            3
    ## 4 uniform_sortedN                 3            3

OK, so the histograms "look" different but the number of turnpoints do not differ. The "looking" different here is the number of individuals per mode. We can calculate this as the number of individuals below/between each minimum.

    ## # A tibble: 12 x 3
    ##    source              mode   nind
    ##    <chr>               <chr> <dbl>
    ##  1 empirical           1        61
    ##  2 empirical           2       446
    ##  3 empirical           3        16
    ##  4 uniform             1       148
    ##  5 uniform             2       198
    ##  6 uniform             3       177
    ##  7 uniform_sortedN     1       154
    ##  8 uniform_sortedN     2        81
    ##  9 uniform_sortedN     3         9
    ## 10 uniform_constrained 1       152
    ## 11 uniform_constrained 2        11
    ## 12 uniform_constrained 3        81

![](just_isd_files/figure-markdown_github/nind%20btwn%20minima-1.png)
