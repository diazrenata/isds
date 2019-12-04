Species distributions overlap
================

``` r
many_sims <- read.csv(here::here("analysis", "sims_nounif.csv"))

ln_units <- .2

one_sim <- many_sims %>%
  filter(source == "constrained", time_chunk == "eighties", sim == 1) %>%
  mutate(species = as.factor(species),
         ln_mass = log(wgt), 
         size_class = ln_units * (floor(ln_mass/ln_units)),
         size_class_g = exp(size_class))

one_sim_counts <- one_sim %>%
  group_by(species, size_class, size_class_g) %>%
  summarize(nind = dplyr::n()) %>%
  ungroup() %>%
  group_by(species) %>%
  mutate(total_ind_species = sum(nind)) %>%
  ungroup()  %>%
  mutate(ind_proportional = nind / total_ind_species)
```

``` r
isd_plot <- ggplot(data = one_sim_counts, aes(x= size_class, y = nind)) +
  geom_col() +
  theme_bw()
isd_plot
```

![](species_overlap_files/figure-markdown_github/initial%20plots-1.png)

``` r
ssd_plot <- ggplot(data = one_sim_counts, aes(x = size_class, y = nind, fill = species)) +
  geom_col(alpha = .2, position = "dodge") +
  theme_bw() +
  scale_fill_viridis_d(end = .8)
ssd_plot
```

![](species_overlap_files/figure-markdown_github/initial%20plots-2.png)

``` r
two_species <- one_sim_counts %>%
  filter(species %in% c(1, 2))

two_species_plot <- ggplot(one_sim_counts, aes(x = size_class, y = ind_proportional, color = species)) +
  geom_point(data = two_species) +
  geom_line(data = two_species) +
  theme_bw() +
  scale_color_viridis_d(end = .8, option = "plasma")

two_species_plot
```

![](species_overlap_files/figure-markdown_github/species%20species%20doi-1.png)

``` r
all_species <- expand.grid(unique(as.numeric(one_sim_counts$species)), unique(as.numeric(one_sim_counts$species))) %>%
  rename(sp1 = Var1, sp2 = Var2) %>%
  filter(sp1 < sp2) %>%
  as.matrix()

expanded <- apply(all_species, MARGIN = 1, FUN = function(sp_vect, sim_counts) return(mutate(filter(sim_counts, as.numeric(species) %in% sp_vect), sp1 = sp_vect[1], sp2 = sp_vect[2])), sim_counts = one_sim_counts)

all_species <- bind_rows(expanded)

all_comb_plots <- ggplot(all_species, aes(x = size_class, y = ind_proportional, color = species)) +
  geom_point(data = all_species) +
  geom_line(data = all_species) +
  theme_bw() +
  scale_color_viridis_d(end = .8) + 
  facet_wrap(vars(sp1, sp2), scales = "free") +
  theme(strip.text = element_blank())

all_comb_plots
```

![](species_overlap_files/figure-markdown_github/all%20comb-1.png)

``` r
library(mclust)
```

    ## Package 'mclust' version 5.4.5
    ## Type 'citation("mclust")' for citing this R package in publications.

``` r
all_comb <- expand.grid(1:9, 1:9)  %>%
  filter(Var1 != Var2) %>%
  mutate(p = NA)

for(i in 1:nrow(all_comb)) {
  sp1 = all_comb[i, 1]
  sp2 = all_comb[i, 2]

reference_isd <- filter(one_sim, species == sp1)

reference_density <- densityMclust(reference_isd$wgt, modelNames = "V")

id <- data.frame(wgt = seq(0, floor(1.25 * max(one_sim$wgt)), by = .1),
                density = NA)

id$density <- predict(reference_density, newdata = id$wgt)

id$density <- id$density / sum(id$density) 

id$wgt = round(id$wgt, digits = 1)

sp2_p <- filter(one_sim, species == sp2) %>%
  mutate(wgt = round(wgt, digits = 1)) %>%
  left_join(id, by = "wgt")

illust <- ggplot(data = id, aes(x = wgt, y = density)) +
  geom_point() +
  geom_point(data = sp2_p, aes(x = wgt, y = mean(id$density)), color = "red", alpha = .1) +
  geom_point(x = mean(sp2_p$wgt), y = mean(sp2_p$density), color = "green") +
  theme_bw() +
  ggtitle(mean(sp2_p$density))

print(illust)

all_comb$p[i] <- mean(sp2_p$density)
}
```

![](species_overlap_files/figure-markdown_github/p%20given%20density-1.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-2.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-3.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-4.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-5.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-6.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-7.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-8.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-9.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-10.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-11.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-12.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-13.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-14.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-15.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-16.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-17.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-18.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-19.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-20.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-21.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-22.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-23.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-24.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-25.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-26.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-27.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-28.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-29.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-30.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-31.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-32.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-33.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-34.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-35.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-36.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-37.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-38.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-39.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-40.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-41.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-42.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-43.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-44.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-45.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-46.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-47.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-48.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-49.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-50.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-51.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-52.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-53.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-54.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-55.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-56.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-57.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-58.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-59.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-60.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-61.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-62.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-63.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-64.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-65.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-66.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-67.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-68.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-69.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-70.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-71.png)![](species_overlap_files/figure-markdown_github/p%20given%20density-72.png)

``` r
hist(all_comb$p)
```

![](species_overlap_files/figure-markdown_github/p%20given%20density-73.png)
