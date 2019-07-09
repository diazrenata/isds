Effect of st.dev. on sim ISD
================
Renata Diaz
7/1/2019

Background
----------

I've generated sim/null model individual size distributions by generating communities with randomly selected (uniform) mean values for species sizes and logseries species abundance distributions. I have assigned *individual* sizes assuming that, for a species, the standard deviation of intraspecific size scales proportional to the species' mean size, and that the intraspecific sizes are normally distributed.

One could find the scaling coefficient by looking up rodent data. First, though, I'm trying a coarse range of scaling coefficients spanning what I think is the reasonable range (from, say, 5% to 20%). A coefficient of 0.05 would be a range of +/- 2 grams for an approximately 40 gram krat species, while a coefficient of .2 would be a range of +/- 8 grams for the same species. That seems pretty broad for a taxon with determinate growth!

Here, I'm plotting sample ISDs for sims with various standard deviation coefficients, to visualize how tuning the coefficient affects the general shape of the distribution.

``` r
all_id_plots <- readd(all_id_plots, cache = cache)

plots <- list() 

plots_df <- data.frame(row_index = 1:length(all_id_plots),
                       dat_name = NA,
                       sim_index = NA,
                       stdev = NA,
                       dat_type = NA)

for(i in 1:length(all_id_plots)) {
  plots[[i]] <- all_id_plots[[i]]$plot
  names(plots)[[i]] <- all_id_plots[[i]]$plot_title
  plots_df$dat_name[i] <- strsplit(names(plots)[[i]], split = " ")[[1]][1]
  plots_df$dat_type[i] <- strsplit(names(plots)[[i]], split = " ")[[1]][2]
  if(plots_df$dat_type[i] == "sim") {
    plots_df$sim_index[i] <- strsplit(names(plots)[[i]], split = " ")[[1]][3]
  plots_df$stdev[i] <- strsplit(names(plots)[[i]], split = " ")[[1]][6]
  }
}
```

``` r
gridExtra::grid.arrange(grobs = list(plots[[plots_df[ which(plots_df$dat_type == "empirical"), "row_index"]]]), nrow = ceiling(length(which(plots_df$dat_type == "empirical"))/2))
```

![](sim_stdev_report_files/figure-markdown_github/empirical%20plots-1.png)

``` r
# arrange sims as rows = sims, cols = stdevs

sims_plots_df <- dplyr::filter(plots_df, dat_type == "sim") %>%
  dplyr::arrange(dat_name, sim_index, stdev)
sims_plots <- list() 
for(i in 1:nrow(sims_plots_df)) {
  sims_plots[[i]] <- plots[[sims_plots_df$row_index[i]]]
}

gridExtra::grid.arrange(grobs = sims_plots,
                        nrow = length(unique(sims_plots_df$sim_index)))
```

![](sim_stdev_report_files/figure-markdown_github/sim%20plots-1.png)

``` r
result <- readd(result, cache = cache)

modes_result <- result %>%
  dplyr::filter(dat_type == "sim",
                threshold == 0.01) %>%
  dplyr::select(npeaks, stdev) %>%
  dplyr::group_by(stdev) %>%
  dplyr::summarise(mean_modes = mean(npeaks),
                   sd_modes = sd(npeaks)) %>%
  dplyr::ungroup()

empirical_modes <- result %>%
  dplyr::filter(dat_type == "emp") %>%
  dplyr::select(npeaks) %>%
  dplyr::distinct()

mode_plot <- ggplot(data = modes_result, aes(x = stdev, y = mean_modes)) + 
  ylim(0, max(modes_result$mean_modes) + 1) +
  geom_point(inherit.aes = TRUE) +
  geom_errorbar(aes(x = stdev, ymin = mean_modes - sd_modes, 
                    ymax = mean_modes + sd_modes)) +
  geom_hline(yintercept = empirical_modes$npeaks, color = "green", 
             linetype = "dashed") +
  theme_bw()

print(mode_plot)
```

    ## Warning: Removed 1 rows containing missing values (geom_errorbar).

![](sim_stdev_report_files/figure-markdown_github/nb%20modes%20plot-1.png)

``` r
gaps_result <- result %>%
  dplyr::filter(dat_type == "sim") %>%
  dplyr::select(ngaps, threshold, stdev) %>%
  dplyr::group_by(threshold, stdev) %>%
  dplyr::summarise(mean_gaps = mean(ngaps),
                   sd_gaps = sd(ngaps)) %>%
  dplyr::ungroup()

empirical_gaps <- result %>%
  dplyr::filter(dat_type == "emp") %>%
  dplyr::select(ngaps, threshold)

gaps_plot <- ggplot(data = gaps_result,
                    aes(x = threshold, 
                        y = mean_gaps,
                        color = as.factor(stdev))) + 
  geom_jitter(inherit.aes = TRUE, height = 0, width = 0.01) +
  geom_errorbar(aes(x = threshold, ymin = mean_gaps - sd_gaps,
                ymax = mean_gaps + sd_gaps, color = as.factor(stdev))) +
  geom_point(data = empirical_gaps,
             aes(x = threshold, y = ngaps), inherit.aes = FALSE,
             size = 3)+
  ylim(-0.5, max(empirical_gaps$ngaps, gaps_result$mean_gaps) + 1) +
  theme_bw()

print(gaps_plot)
```

    ## Warning: Removed 2 rows containing missing values (geom_errorbar).

![](sim_stdev_report_files/figure-markdown_github/gaps%20plot-1.png)
