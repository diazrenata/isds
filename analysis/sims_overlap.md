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

From sims
---------

![](sims_overlap_files/figure-markdown_github/show%20plots-1.png)![](sims_overlap_files/figure-markdown_github/show%20plots-2.png)![](sims_overlap_files/figure-markdown_github/show%20plots-3.png)![](sims_overlap_files/figure-markdown_github/show%20plots-4.png)![](sims_overlap_files/figure-markdown_github/show%20plots-5.png)![](sims_overlap_files/figure-markdown_github/show%20plots-6.png)![](sims_overlap_files/figure-markdown_github/show%20plots-7.png)![](sims_overlap_files/figure-markdown_github/show%20plots-8.png)![](sims_overlap_files/figure-markdown_github/show%20plots-9.png)

Upsample with error propagation
-------------------------------

Here's the outcomes:

![](sims_overlap_files/figure-markdown_github/show%20error%20prop%20outcomes-1.png)![](sims_overlap_files/figure-markdown_github/show%20error%20prop%20outcomes-2.png)![](sims_overlap_files/figure-markdown_github/show%20error%20prop%20outcomes-3.png)![](sims_overlap_files/figure-markdown_github/show%20error%20prop%20outcomes-4.png)![](sims_overlap_files/figure-markdown_github/show%20error%20prop%20outcomes-5.png)![](sims_overlap_files/figure-markdown_github/show%20error%20prop%20outcomes-6.png)![](sims_overlap_files/figure-markdown_github/show%20error%20prop%20outcomes-7.png)![](sims_overlap_files/figure-markdown_github/show%20error%20prop%20outcomes-8.png)![](sims_overlap_files/figure-markdown_github/show%20error%20prop%20outcomes-9.png)

<!-- ## Many samples -->
<!-- Now let's take the same pipeline as above, run it many times, and report the edge proportion. -->
<!-- For now we'll retain all the intermediate samples. This will be unwieldly eventually, but for now I suspect I'll want to look closely at some of them. -->
<!-- ```{r many samples} -->
<!-- replicate_samples <- function(means_vect, abunds_vect, times = 100) { -->
<!--   return(replicate(sample_community(means = means_vect, abunds = abunds_vect), n = times, simplify = F)) -->
<!-- } -->
<!-- resample_community <- function(community_df) { -->
<!--   summary_df <- community_df %>% -->
<!--     group_by(species) %>% -->
<!--     summarize(meanwgt = mean(wgt), -->
<!--               abund = dplyr::n()) %>% -->
<!--     ungroup() -->
<!--   newabund <- ceiling(summary_df$abund * 50 / min(summary_df$abund)) -->
<!--   new_community <- sample_community(means = summary_df$meanwgt, abunds = newabund) -->
<!--   return(new_community) -->
<!-- } -->
<!-- set.seed(5) -->
<!-- even_sims <- lapply(scenarios, FUN = replicate_samples, abunds_vect = abunds) -->
<!-- ls_sims <- lapply(scenarios, FUN = replicate_samples, abunds_vect = ls_abunds) -->
<!-- ls_upsampled_sims <- lapply(ls_sims, FUN = function(sims_list) return(lapply(sims_list, FUN = resample_community))) -->
<!-- extract_edge_prop <- function(sims_list) { -->
<!--   return(data.frame(sim = 1:length(sims_list), edge_prop = vapply(sims_list, FUN = function(community_df) return(edge_proportion(community_overlap(community_df))), FUN.VALUE = 1))) -->
<!-- } -->
<!-- even_overlaps <- lapply(even_sims, FUN = extract_edge_prop) -->
<!-- ls_overlaps <- lapply(ls_sims, FUN = extract_edge_prop) -->
<!-- ls_up_overlaps <- lapply(ls_upsampled_sims, FUN = extract_edge_prop) -->
<!-- even_overlaps <- bind_rows(even_overlaps, .id = "source") -->
<!-- ls_overlaps <- bind_rows(ls_overlaps, .id = "source") -->
<!-- ls_up_overlaps <- bind_rows(ls_up_overlaps, .id = "source") -->
<!-- all_overlaps <- bind_rows(list(even = even_overlaps, ls = ls_overlaps, ls_up = ls_up_overlaps), .id = "sampling") -->
<!-- head(all_overlaps) -->
<!-- extract_edge_prop_w <- function(sims_list, wcol = "prod_n") { -->
<!--   return(data.frame(sim = 1:length(sims_list), edge_prop = vapply(sims_list, FUN = function(community_df) return(edge_proportion(community_overlap(community_df), weight_col = wcol)), FUN.VALUE = 1))) -->
<!-- } -->
<!-- even_overlaps_w <- lapply(even_sims, FUN = extract_edge_prop_w) -->
<!-- ls_overlaps_w <- lapply(ls_sims, FUN = extract_edge_prop_w) -->
<!-- ls_up_overlaps_w <- lapply(ls_upsampled_sims, FUN = extract_edge_prop_w) -->
<!-- even_overlaps_w <- bind_rows(even_overlaps_w, .id = "source") -->
<!-- ls_overlaps_w <- bind_rows(ls_overlaps_w, .id = "source") -->
<!-- ls_up_overlaps_w <- bind_rows(ls_up_overlaps_w, .id = "source") -->
<!-- all_overlaps_w <- bind_rows(list(even = even_overlaps_w, ls = ls_overlaps_w, ls_up = ls_up_overlaps_w), .id = "sampling") -->
<!-- head(all_overlaps_w) -->
<!-- ``` -->
<!-- ```{r show all overlaps} -->
<!-- overlaps_plot <- ggplot(data = all_overlaps, aes(x = edge_prop, fill = source)) + -->
<!--   geom_histogram(binwidth = .1, boundary = 0) + -->
<!--   xlim(-.2, 1.2) + -->
<!--   theme_bw() + -->
<!--   facet_grid(rows = vars(sampling), cols = vars(source), scales = "free_y", switch = "y") +  -->
<!--   scale_fill_viridis_d(option = "plasma", end = .8) -->
<!-- overlaps_plot -->
<!-- overlaps_w_plot <- ggplot(data = all_overlaps_w, aes(x = edge_prop, fill = source)) + -->
<!--   geom_histogram(binwidth = .1, boundary = 0) + -->
<!--   xlim(-.2, 1.2) + -->
<!--   theme_bw() + -->
<!--   facet_grid(rows = vars(sampling), cols = vars(source), scales = "free_y", switch = "y") +  -->
<!--   scale_fill_viridis_d(option = "plasma", end = .8) -->
<!-- overlaps_w_plot -->
<!-- ``` -->
<!-- #### Overlaps density plots -->
<!-- This might give a better picture than the edge prop, in that it isn't as sensitive to where you put the cutoffs. But, potentially also computationally intensive and tough to make sense of.  -->
<!-- ```{r overlap density plots} -->
<!-- extract_overlap <- function(sims_list) { -->
<!--   overlaps <- lapply(sims_list, FUN = community_overlap) -->
<!--   names(overlaps) <- 1:length(sims_list) -->
<!--   overlaps <- bind_rows(overlaps, .id = "sim") -->
<!-- } -->
<!-- even_overlaps_r <- lapply(even_sims, FUN = extract_overlap) -->
<!-- ls_overlaps_r <- lapply(ls_sims, FUN = extract_overlap) -->
<!-- ls_up_overlaps_r <- lapply(ls_upsampled_sims, FUN = extract_overlap) -->
<!-- even_overlaps_r <- bind_rows(even_overlaps_r, .id = "source") -->
<!-- ls_overlaps_r <- bind_rows(ls_overlaps_r, .id = "source") -->
<!-- ls_up_overlaps_r <- bind_rows(ls_up_overlaps_r, .id = "source") -->
<!-- all_overlaps_r <- bind_rows(list(even = even_overlaps_r, ls = ls_overlaps_r, ls_up = ls_up_overlaps_r), .id = "sampling") -->
<!-- all_overlaps_r <- all_overlaps_r %>% -->
<!--   mutate(sim_source_even = paste0(source, sampling, sim), -->
<!--          prod_n = floor(prod_n/46)) -->
<!-- raw_density_plot <- ggplot(data = all_overlaps_r, aes(x = overlap, y = stat(scaled), color = source, group = sim_source_even)) + -->
<!--   geom_density(alpha = 0, size = .03, n = 16) + -->
<!--   xlim(-.2, 1.2) + -->
<!--   theme_bw() + -->
<!--   facet_wrap(vars(sampling, source), scales = "free_y", strip.position = "top") +  -->
<!--   scale_fill_viridis_d(option = "plasma", end = .8) + -->
<!--   geom_vline(xintercept = c(.2, .7), color = "black") -->
<!-- raw_density_plot -->
<!-- all_overlaps_scaled <- data.frame( -->
<!--   sampling = rep(all_overlaps_r$sampling, times = all_overlaps_r$total_n), -->
<!--   source = rep(all_overlaps_r$source, times = all_overlaps_r$total_n), -->
<!--   sim_source_sampling = rep(all_overlaps_r$sim_source_even, times = all_overlaps_r$total_n), -->
<!--   overlap = rep(all_overlaps_r$overlap, times = all_overlaps_r$total_n) -->
<!-- ) -->
<!-- scaled_density_plot <-  ggplot(data = all_overlaps_scaled, aes(x = overlap, y = stat(scaled), color = source, group = sim_source_sampling)) + -->
<!--   geom_density(alpha = 0, size = .03, n = 16) + -->
<!--   xlim(-.2, 1.2) + -->
<!--   theme_bw() + -->
<!--   facet_wrap(vars(sampling, source), scales = "free_y", strip.position = "top") +  -->
<!--   scale_fill_viridis_d(option = "plasma", end = .8) + -->
<!--   geom_vline(xintercept = c(.2, .7), color = "black") -->
<!-- scaled_density_plot -->
<!-- ``` -->
