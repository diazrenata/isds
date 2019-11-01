New workflow
================

![](new_drake_files/figure-markdown_github/emp%20plot-1.png)

![](new_drake_files/figure-markdown_github/plot-1.png)

    ## Warning: Removed 6 rows containing missing values (geom_bar).

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](new_drake_files/figure-markdown_github/summarize%20sims-1.png)

### More closely on how these metrics work

-   Sd is familiar but not great, because if there are many modes with few individuals each, you get a low sd, similar to if you have a mode with a lot of individuals concentrated in a tight space.
-   Thus me gravitating towards the nb individuals/width of clump metric. This is basically "how pointy is the clump" and helps distinguish 4 pointy clumps from 4 loose clumps.

So, let's plot all our simulated ISDs in order of most to least average pointiness, and color code them by pointyness. A pointiness gut check.

![](new_drake_files/figure-markdown_github/sort%20by%20pointiness-1.png)

![](new_drake_files/figure-markdown_github/sort%20by%20prop%20pointiness-1.png)
