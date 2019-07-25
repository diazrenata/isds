Just plots
================
Renata Diaz
7/24/2019

``` r
cached(cache = cache)
```

    ## [1] "dat1"           "dat2"           "ids_dat1"       "ids_dat2"      
    ## [5] "ids_plots_dat1" "ids_plots_dat2" "sims_dat1"      "sims_dat2"

``` r
all_plot_names <- cached(cache = cache)[which(grepl(cached(cache = cache), pattern = "ids_plots"))]
                    
all_plots <- list()

for(i in 1:length(all_plot_names)) {
  all_plots[[i]] <- readd(all_plot_names[i], character_only = T, cache = cache)
}

for(i in 1:length(all_plots)) {
  
  dataset_name <- all_plots[[i]]$metadata$dat_name[1]
  
  # plot empirical
  
  print(all_plots[[i]]$id_plots[[ which(!all_plots[[i]]$metadata$sim)]])
  
  # plot fixed stdevs
  stdevs <- na.omit(unique(all_plots[[i]]$metadata$stdev))
  
  for(j in 1:length(stdevs)) {
    
    print(stdevs[j])
    
    these_indices <- which(all_plots[[i]]$metadata$stdev == stdevs[j])
    these_plots <- list()
    
    for(k in 1:length(these_indices)) {
      these_plots[[k]] <- all_plots[[i]]$id_plots[[these_indices[k]]]
    }
    
    gridExtra::grid.arrange(grobs = these_plots,
                            nrow = ceiling(k/3))
    
  }
  
  # plot stdev range
  
  stdev_ranges <- unique(all_plots[[i]]$metadata$stdev_range)
  stdev_ranges <- stdev_ranges[ which(grepl(stdev_ranges, pattern = "_"))]
  
  for(j in 1:length(stdev_ranges)) {
    print(stdev_ranges[j])
    these_indices <- which(all_plots[[i]]$metadata$stdev_range == stdev_ranges[j])
    these_plots <- list()
    
    for(k in 1:length(these_indices)) {
      these_plots[[k]] <- all_plots[[i]]$id_plots[[these_indices[k]]]
    }
    
    gridExtra::grid.arrange(grobs = these_plots,
                            nrow = ceiling(k/3))
  }
  
}
```

![](just_plots_files/figure-markdown_github/plot%20by%20dataset-1.png)

    ## [1] 0.05

![](just_plots_files/figure-markdown_github/plot%20by%20dataset-2.png)

    ## [1] 0.15

![](just_plots_files/figure-markdown_github/plot%20by%20dataset-3.png)

    ## [1] 0.25

![](just_plots_files/figure-markdown_github/plot%20by%20dataset-4.png)

    ## [1] 0.35

![](just_plots_files/figure-markdown_github/plot%20by%20dataset-5.png)

    ## [1] "0.01_0.04"

![](just_plots_files/figure-markdown_github/plot%20by%20dataset-6.png)![](just_plots_files/figure-markdown_github/plot%20by%20dataset-7.png)

    ## [1] 0.05

![](just_plots_files/figure-markdown_github/plot%20by%20dataset-8.png)

    ## [1] 0.15

![](just_plots_files/figure-markdown_github/plot%20by%20dataset-9.png)

    ## [1] 0.25

![](just_plots_files/figure-markdown_github/plot%20by%20dataset-10.png)

    ## [1] 0.35

![](just_plots_files/figure-markdown_github/plot%20by%20dataset-11.png)

    ## [1] "0.01_0.04"

![](just_plots_files/figure-markdown_github/plot%20by%20dataset-12.png)
