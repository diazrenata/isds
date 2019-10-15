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

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## # A tibble: 2 x 3
    ##   source    AICc_nbclumps BIC_nbclumps
    ##   <chr>             <int>        <int>
    ## 1 empirical             3            4
    ## 2 uniform               6            5

Can we differentiate between an empirical ISD and a minimally-constrained uniform?
----------------------------------------------------------------------------------

By "minimally-constrained uniform", I mean the following assumptions/steps:

-   Species' mean body sizes are drawn at random
-   Sd scales as .1 body size (generous)
-   Abundance is a logseries (parameter derives only from S and N and provisionally seems like a pretty good fit even compared to the feasible set) matched to species at random

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

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## Warning in pastecs::turnpoints(gmm_smooth): value out of range in 'gammafn'

    ## # A tibble: 4 x 3
    ##   source              AICc_nbclumps BIC_nbclumps
    ##   <chr>                       <int>        <int>
    ## 1 empirical                       3            4
    ## 2 uniform                         6            5
    ## 3 uniform_constrained             4            4
    ## 4 uniform_sortedN                 3            3

### what if we sort the SAD
