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
species_density <- lapply(as.list(1:length(unique(one_sim$species))),
                          FUN = function(species_i, sim_data) 
                            return(data.frame(
                              x = density(filter(sim_data, as.numeric(species) == species_i)$wgt, from = 0, to = 1.5 * max(sim_data$wgt), n = 8192)$x,
                              y = density(filter(sim_data, as.numeric(species) == species_i)$wgt, from = 0, to = 1.5 * max(sim_data$wgt), n =8192)$y,
                              species = species_i,
                              nind = nrow(filter(sim_data, as.numeric(species) == species_i))
                            )),
                          sim_data = one_sim)

species_density <- bind_rows(species_density)

species_density <- species_density %>%
  group_by(species) %>%
  mutate(total_d = sum(y)) %>%
  ungroup() %>%
  mutate(prop_d = y / total_d,
         prop_d_scaled = y * nind / total_d)

density_plots <- ggplot(data = species_density, aes(x = x, y = prop_d, color = species)) +
  geom_point() +
  facet_wrap(vars(species)) +
  theme_bw()
density_plots
```

![](species_overlap_files/figure-markdown_github/density%20plots-1.png)

``` r
density_plots_scaled <- ggplot(data = species_density, aes(x = x, y = prop_d_scaled, color = species)) +
  geom_point() +
  facet_wrap(vars(species)) +
  theme_bw()
density_plots_scaled
```

![](species_overlap_files/figure-markdown_github/density%20plots-2.png)

``` r
all_densities <- expand.grid(1:9, 1:9) %>% rename(sp1 = Var1, sp2 = Var2) %>%
  filter(sp1 < sp2) %>%
  as.matrix()

densities_intersections <- apply(all_densities, MARGIN = 1, FUN = function(spvect, density_dat)
  return(data.frame(
    x = filter(density_dat, species == spvect[1])$x,
    y = filter(density_dat, species == spvect[1])$prop_d_scaled *
      filter(density_dat, species == spvect[2])$prop_d_scaled,
    sp1 = spvect[1],
    sp2 = spvect[2]
  )),
  density_dat = species_density)
```

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

    ## Warning in data.frame(x = filter(density_dat, species == spvect[1])$x, y =
    ## filter(density_dat, : row names were found from a short variable and have
    ## been discarded

``` r
densities_intersections = bind_rows(densities_intersections)

intersection_plots <- ggplot(densities_intersections, aes(x = x, y = y)) +
  geom_point(data = densities_intersections) +
  geom_line(data = densities_intersections) +
  theme_bw() +
  facet_wrap(vars(sp1, sp2)) +
  theme(strip.text = element_blank())

intersection_plots
```

![](species_overlap_files/figure-markdown_github/density%20plots-3.png)
