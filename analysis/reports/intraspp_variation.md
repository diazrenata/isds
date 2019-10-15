Intraspecific variation
================

``` r
library(replicatebecs)

download_raw_paper_data()
```

    ## [1] TRUE

``` r
process_raw_data()
```

    ## Loading in data version 1.136.0

    ## [1] TRUE

``` r
communities <- load_paper_data()
communities <- dplyr::bind_rows(communities, .id = "community")

unique_species <- communities %>%
  dplyr::select(community, individual_species_ids) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(individual_species_ids))


extract_species <- function(species_row, all_individuals) {
  this_species <- all_individuals %>%
    dplyr::filter(community == species_row[1],
                  individual_species_ids == species_row[2])
  return(this_species)
}

each_species <- apply(unique_species,MARGIN = 1, FUN = extract_species, all_individuals = communities)

test_normality <- function(species) {
  # species <- species %>%
  #   dplyr::filter(individual_sizes %in% 0:(mean(species$individual_sizes) * 3))
  # 
  
  if(nrow(species) < 3) {
    return(NA)
  }
  
  if(nrow(species) > 5000) {
    species <- species[ sample.int(n = nrow(species), size = 5000, replace = F), ]
  }
  
  if(min(species$individual_sizes) == max(species$individual_sizes)) {
    return(NA)
  }
  
  this_test <- shapiro.test(species$individual_sizes)
  
  return(this_test$p.value)
}

get_mean <- function(species) {
  return(mean(species$individual_sizes))
}
get_sd <- function(species){
  return(sd(species$individual_sizes))
}


each_species_p <- data.frame(
  species = unique_species$individual_species_ids,
  community = unique_species$community,
  p = vapply(each_species, test_normality, FUN.VALUE = .5),
  n = vapply(each_species, nrow, FUN.VALUE = 10),
  mean = vapply(each_species, get_mean, FUN.VALUE = 4),
  sd = vapply(each_species, get_sd, FUN.VALUE = .5)
) 
each_species_p <- each_species_p %>%
  dplyr::mutate(signif = (p >= 0.05),
                sd_scaled = sd / mean)
```

Generally failing a Shapiro-Wilk test for normality.

Plotting...

``` r
for(i in 1:length(each_species)){
  print(hist(each_species[[i]]$individual_sizes))
}
```

Plotting mean size v. sd

``` r
library(ggplot2)

size_sd_plot <- ggplot(data = each_species_p, aes(x = mean, y = sd)) + 
  geom_point() + 
  theme_bw()

size_sd_plot
```

    ## Warning: Removed 15 rows containing missing values (geom_point).

![](intraspp_variation_files/figure-markdown_github/size%20v%20sd%20plot-1.png)

``` r
size_sd_scaled_plot <- ggplot(data = each_species_p, aes(x = mean, y = sd_scaled)) + 
  geom_point() + 
  theme_bw()
size_sd_scaled_plot
```

    ## Warning: Removed 15 rows containing missing values (geom_point).

![](intraspp_variation_files/figure-markdown_github/size%20v%20sd%20plot-2.png)

``` r
mean(each_species_p$sd_scaled, na.rm = T)
```

    ## [1] 0.2402606

``` r
sd(each_species_p$sd_scaled, na.rm = T)
```

    ## [1] 0.115946
