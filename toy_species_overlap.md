Toy overlap
================

### Toy communities

``` r
set.seed(2019)
means <- c(40, 41, 100)
sds <- .1 * means

abund <- unlist(scads::sample_METE(s = 3, n = 200, nsamples = 1))

#abund <- sample(abund, size = 3, replace = F)

abund <- sort(abund, decreasing = T)

comm <- list() 

for(i in 1:3) {
  comm[[i]] <- data.frame(
    species = i,
    wgt = rnorm(mean = means[i], sd = sds[i], n = abund[i])
  )
}

comm <- bind_rows(comm)

comm_plot_all <- ggplot(data = comm, aes(x = wgt)) +
  geom_density() +
  theme_bw() +
  xlim(0, 200)

comm_plot_all
```

![](toy_species_overlap_files/figure-markdown_github/make%20toy-1.png)

``` r
comm_plot <- ggplot(data = comm, aes(x = wgt, fill = species, group = species)) +
  geom_density(alpha =  .4) +
  theme_bw() +
  xlim(0, 200)

comm_plot
```

![](toy_species_overlap_files/figure-markdown_github/make%20toy-2.png)

``` r
all_pairs <- expand.grid(1:3, 1:3) %>%
  as.data.frame() %>%
  filter(Var1 < Var2) %>%
  mutate(overlap = 0)


for(i in 1:nrow(all_pairs)) {
  sp1 = all_pairs[i, 1]
  sp2 = all_pairs[i, 2]
  
  sp1_density <- density(filter(comm, species == sp1)$wgt, from = 0, to = 200)$y
  
  sp1_density <- sp1_density / sum(sp1_density)
  
  sp2_density <- density(filter(comm, species == sp2)$wgt, from = 0, to = 200)$y
  sp2_density <- sp2_density / sum(sp2_density)
  
  min_density <- data.frame(
    sp1 = sp1_density,
    sp2 = sp2_density
  )
  
  min_density <- min_density %>%
    mutate(index = row_number()) %>%
    group_by(index) %>%
    mutate(min_den = min(sp1, sp2)) %>%
    ungroup()
  
  overlap <- sum(min_density$min_den)
  
  
  all_pairs$overlap[i] <- overlap
  
}
```

``` r
raw_overlap_hist <- ggplot(data = all_pairs, aes(x = overlap)) +
  geom_histogram() +
  theme_bw()
raw_overlap_hist
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](toy_species_overlap_files/figure-markdown_github/overlap%20hists-1.png)

``` r
abund_df <- data.frame(
  species = 1:3,
  abund = abund
)

all_pairs_w <- all_pairs %>%
 left_join(abund_df, by = c("Var1" = "species")) %>%
  rename(abund1 = abund) %>%
  left_join(abund_df, by = c("Var2" = "species")) %>%
  rename(abund2 = abund) %>%
  mutate(index = row_number()) %>%
  group_by(index) %>%
  mutate(hm_abund = psych::harmonic.mean(c(abund1, abund2)),
         total_abund = sum(abund1, abund2),
         prop_abund = sum(abund1, abund2) / sum(abund_df$abund)) %>%
  ungroup()

overlap_expanded <- list()
for(i in 1:nrow(all_pairs_w)) {
  overlap_expanded[[i]] <- rep(all_pairs_w$overlap[i], times = round(all_pairs_w$hm_abund[i]))
}

overlap_expanded <- data.frame(
  overlap = unlist(overlap_expanded))

hm_hist <- ggplot(data = overlap_expanded, aes(x= overlap)) +
  geom_histogram() +
  theme_bw()
hm_hist
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](toy_species_overlap_files/figure-markdown_github/overlap%20hists-2.png)
