Measuring overlap in SBSDs
================

SBSD = species body size distribution. The frequency/probability density of body sizes across all individuals of a species.

Rationale & background of using overlap
---------------------------------------

The ISD (the size distribution across all individuals of all species) is...slippery and often subject to interpretation.

Both Holling and ESS would predict polarized/bimodal distribution of overlap among all pairings of species. Species should either be quite similar or quite different, so high or low, but not intermediate, overlap.

I've imported the SBSD overlap metric from [Read et al 2018](https://onlinelibrary.wiley.com/doi/full/10.1111/ecog.03641):

1.  For all pairs of species in a community, construct the KDEs across the entire body size range and standardize so each KDE integrates to 1.
2.  Integrate the minimum of the two KDEs at each evaluation point.

In principle this varies from 0 to 1 for each pair of species.

Foreshadowing artefacts from rare species
-----------------------------------------

1.  Pointy KDEs
2.  Higher uncertainty wrt the true SBSD mean, sd, and indeed entire distribution.
3.  We may inherently want to downweight rare species, but *this is separate from* the above.

Demo with simple cases
----------------------

Let's leave rare species aside for the moment, and focus on how we expect the overlap distributions to come out under a few toy scenarios:

1.  All species are essentially the same; complete overlap
2.  Two clusters of species. Within clusters, species are essentially the same. The clusters do not overlap.
3.  All species are distinct and do not overlap.
4.  All species are different but overlap significantly.

For now, we will give all species an equal, reasonably large, number of individuals (50). This is wildly unrealistic for real communities.

We will assume all species' SBSDs are normal distributions, and that the standard deviation scales with the mean according to some coefficient. `.15` seems like a plausible coefficient to me.

![](edge_interior_files/figure-markdown_github/show%20plots-1.png)![](edge_interior_files/figure-markdown_github/show%20plots-2.png)![](edge_interior_files/figure-markdown_github/show%20plots-3.png)![](edge_interior_files/figure-markdown_github/show%20plots-4.png)

In these scenarios, the overlap metric behaves as expected. We have high overlap values when we have species we think should overlap fully, near 0 when there are either clumps that do not overlap or when there's no overlap between any species, and a spectrum of intermediate values when all the species overlap partially.

We would like a way to summarize the rightmost plots in a single metric. Let's try the proportion of values in the upper 30 and lower 20%.

``` r
overlap_dfs <- lapply(scenario_communities, FUN = community_overlap)

edge_proportion(overlap_dfs[[4]])
```

    ## [1] 0.6666667

Adding a logseries SAD
----------------------

Let's add this SAD, drawn from the METE logseries. We'll assign abundance to species at random. We can't construct a kde for a species with one individual, so we're coercing any species that gets 1 to have 2 individuals.

![](edge_interior_files/figure-markdown_github/plot%20sad-1.png)

    ## V13 V14 V12 V11 
    ##  62 113  23   2

![](edge_interior_files/figure-markdown_github/plot%20ls%20outcomes-1.png)![](edge_interior_files/figure-markdown_github/plot%20ls%20outcomes-2.png)![](edge_interior_files/figure-markdown_github/plot%20ls%20outcomes-3.png)![](edge_interior_files/figure-markdown_github/plot%20ls%20outcomes-4.png)

Just adding the SAD - and drawing all our samples from the *same distributions as above* - turns up unexpected overlap results. We get unexpectedly low values for the complete overlap scenario, because we are constructing kdes for low abundance species based on very few samples. Those samples may be off the "true" mean, and the resulting kde is much pointier than one constructed off of more samples. I think the complete\_overlap purple species illustrates being off the mean, and the complete\_overlap purple species illustrates the pointy kde. We also got some overlaps pushed downards in the partial\_overlap scenario, again because of artifiically pointy kdes.

Upsample with error propagation
-------------------------------

Here's how well we recovered the true means: (actually pretty well)

![](edge_interior_files/figure-markdown_github/show%20mean%20recovery-1.png)

And here's the outcomes:

![](edge_interior_files/figure-markdown_github/show%20error%20prop%20outcomes-1.png)![](edge_interior_files/figure-markdown_github/show%20error%20prop%20outcomes-2.png)![](edge_interior_files/figure-markdown_github/show%20error%20prop%20outcomes-3.png)![](edge_interior_files/figure-markdown_github/show%20error%20prop%20outcomes-4.png)

Many samples
------------

Now let's take the same pipeline as above, run it many times, and report the edge proportion.

For now we'll retain all the intermediate samples. This will be unwieldly eventually, but for now I suspect I'll want to look closely at some of them.

``` r
replicate_samples <- function(means_vect, abunds_vect, times = 100) {
  return(replicate(sample_community(means = means_vect, abunds = abunds_vect), n = times, simplify = F))
}

resample_community <- function(community_df) {
  summary_df <- community_df %>%
    group_by(species) %>%
    summarize(meanwgt = mean(wgt),
              abund = dplyr::n()) %>%
    ungroup()
  
  newabund <- ceiling(summary_df$abund * 50 / min(summary_df$abund))
  
  new_community <- sample_community(means = summary_df$meanwgt, abunds = newabund)
  return(new_community)
}

set.seed(5)

even_sims <- lapply(scenarios, FUN = replicate_samples, abunds_vect = abunds)

ls_sims <- lapply(scenarios, FUN = replicate_samples, abunds_vect = ls_abunds)

ls_upsampled_sims <- lapply(ls_sims, FUN = function(sims_list) return(lapply(sims_list, FUN = resample_community)))

extract_edge_prop <- function(sims_list) {
  return(data.frame(sim = 1:length(sims_list), edge_prop = vapply(sims_list, FUN = function(community_df) return(edge_proportion(community_overlap(community_df))), FUN.VALUE = 1)))
}

even_overlaps <- lapply(even_sims, FUN = extract_edge_prop)
ls_overlaps <- lapply(ls_sims, FUN = extract_edge_prop)
ls_up_overlaps <- lapply(ls_upsampled_sims, FUN = extract_edge_prop)

even_overlaps <- bind_rows(even_overlaps, .id = "source")
ls_overlaps <- bind_rows(ls_overlaps, .id = "source")
ls_up_overlaps <- bind_rows(ls_up_overlaps, .id = "source")

all_overlaps <- bind_rows(list(even = even_overlaps, ls = ls_overlaps, ls_up = ls_up_overlaps), .id = "sampling")

head(all_overlaps)
```

    ##   sampling           source sim edge_prop
    ## 1     even complete_overlap   1         1
    ## 2     even complete_overlap   2         1
    ## 3     even complete_overlap   3         1
    ## 4     even complete_overlap   4         1
    ## 5     even complete_overlap   5         1
    ## 6     even complete_overlap   6         1

``` r
extract_edge_prop_w <- function(sims_list, wcol = "prod_n") {
  return(data.frame(sim = 1:length(sims_list), edge_prop = vapply(sims_list, FUN = function(community_df) return(edge_proportion(community_overlap(community_df), weight_col = wcol)), FUN.VALUE = 1)))
}

even_overlaps_w <- lapply(even_sims, FUN = extract_edge_prop_w)
ls_overlaps_w <- lapply(ls_sims, FUN = extract_edge_prop_w)
ls_up_overlaps_w <- lapply(ls_upsampled_sims, FUN = extract_edge_prop_w)

even_overlaps_w <- bind_rows(even_overlaps_w, .id = "source")
ls_overlaps_w <- bind_rows(ls_overlaps_w, .id = "source")
ls_up_overlaps_w <- bind_rows(ls_up_overlaps_w, .id = "source")

all_overlaps_w <- bind_rows(list(even = even_overlaps_w, ls = ls_overlaps_w, ls_up = ls_up_overlaps_w), .id = "sampling")

head(all_overlaps_w)
```

    ##   sampling           source sim edge_prop
    ## 1     even complete_overlap   1         1
    ## 2     even complete_overlap   2         1
    ## 3     even complete_overlap   3         1
    ## 4     even complete_overlap   4         1
    ## 5     even complete_overlap   5         1
    ## 6     even complete_overlap   6         1

``` r
overlaps_plot <- ggplot(data = all_overlaps, aes(x = edge_prop, fill = source)) +
  geom_histogram(binwidth = .1, boundary = 0) +
  xlim(-.2, 1.2) +
  theme_bw() +
  facet_grid(rows = vars(sampling), cols = vars(source), scales = "free_y", switch = "y") + 
  scale_fill_viridis_d(option = "plasma", end = .8)
overlaps_plot
```

    ## Warning: Removed 12 rows containing missing values (geom_bar).

![](edge_interior_files/figure-markdown_github/show%20all%20overlaps-1.png)

``` r
overlaps_w_plot <- ggplot(data = all_overlaps_w, aes(x = edge_prop, fill = source)) +
  geom_histogram(binwidth = .1, boundary = 0) +
  xlim(-.2, 1.2) +
  theme_bw() +
  facet_grid(rows = vars(sampling), cols = vars(source), scales = "free_y", switch = "y") + 
  scale_fill_viridis_d(option = "plasma", end = .8)
overlaps_w_plot
```

    ## Warning: Removed 12 rows containing missing values (geom_bar).

![](edge_interior_files/figure-markdown_github/show%20all%20overlaps-2.png)

#### Overlaps density plots

This might give a better picture than the edge prop, in that it isn't as sensitive to where you put the cutoffs. But, potentially also computationally intensive and tough to make sense of.

``` r
extract_overlap <- function(sims_list) {
  overlaps <- lapply(sims_list, FUN = community_overlap)
  names(overlaps) <- 1:length(sims_list)
  overlaps <- bind_rows(overlaps, .id = "sim")
}

even_overlaps_r <- lapply(even_sims, FUN = extract_overlap)
ls_overlaps_r <- lapply(ls_sims, FUN = extract_overlap)
ls_up_overlaps_r <- lapply(ls_upsampled_sims, FUN = extract_overlap)

even_overlaps_r <- bind_rows(even_overlaps_r, .id = "source")
ls_overlaps_r <- bind_rows(ls_overlaps_r, .id = "source")
ls_up_overlaps_r <- bind_rows(ls_up_overlaps_r, .id = "source")

all_overlaps_r <- bind_rows(list(even = even_overlaps_r, ls = ls_overlaps_r, ls_up = ls_up_overlaps_r), .id = "sampling")

all_overlaps_r <- all_overlaps_r %>%
  mutate(sim_source_even = paste0(source, sampling, sim),
         prod_n = floor(prod_n/46))

raw_density_plot <- ggplot(data = all_overlaps_r, aes(x = overlap, y = stat(scaled), color = source, group = sim_source_even)) +
  geom_density(alpha = 0, size = .03, n = 16) +
  xlim(-.2, 1.2) +
  theme_bw() +
  facet_wrap(vars(sampling, source), scales = "free_y", strip.position = "top") + 
  scale_fill_viridis_d(option = "plasma", end = .8) +
  geom_vline(xintercept = c(.2, .7), color = "black")
raw_density_plot
```

![](edge_interior_files/figure-markdown_github/overlap%20density%20plots-1.png)

``` r
all_overlaps_scaled <- data.frame(
  sampling = rep(all_overlaps_r$sampling, times = all_overlaps_r$total_n),
  source = rep(all_overlaps_r$source, times = all_overlaps_r$total_n),
  sim_source_sampling = rep(all_overlaps_r$sim_source_even, times = all_overlaps_r$total_n),
  overlap = rep(all_overlaps_r$overlap, times = all_overlaps_r$total_n)
)

scaled_density_plot <-  ggplot(data = all_overlaps_scaled, aes(x = overlap, y = stat(scaled), color = source, group = sim_source_sampling)) +
  geom_density(alpha = 0, size = .03, n = 16) +
  xlim(-.2, 1.2) +
  theme_bw() +
  facet_wrap(vars(sampling, source), scales = "free_y", strip.position = "top") + 
  scale_fill_viridis_d(option = "plasma", end = .8) +
  geom_vline(xintercept = c(.2, .7), color = "black")
scaled_density_plot
```

![](edge_interior_files/figure-markdown_github/overlap%20density%20plots-2.png)
