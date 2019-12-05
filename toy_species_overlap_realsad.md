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

![](toy_species_overlap_realsad_files/figure-markdown_github/make%20toy-1.png)

``` r
comm_plot <- ggplot(data = comm, aes(x = wgt, fill = species, group = species)) +
  geom_density(alpha =  .4) +
  theme_bw() +
  xlim(0, 200)

comm_plot
```

![](toy_species_overlap_realsad_files/figure-markdown_github/make%20toy-2.png)

Scenarios:

#### Six species, two overlapped groups

``` r
set.seed(2)
means_62 <- c(20, 21, 22, 80, 81, 82) 
sds_62 <- .2 * means_62
#an_sad <- c(50, 50, 50, 50, 50, 50)
an_sad <- unlist(scads::sample_METE(s = 6, n = 200, nsamples = 1))

an_sad[ which(an_sad == 1)] <- 2

six_two <- sample_community(means_62, sds_62, an_sad)


six_two_plot_all <- ggplot(data = six_two, aes(x = wgt)) +
  geom_density() +
  theme_bw() +
  xlim(0, 200)

six_two_plot_all
```

![](toy_species_overlap_realsad_files/figure-markdown_github/six%20in%20two-1.png)

``` r
six_two_plot <- ggplot(data = six_two, aes(x = wgt, fill = species, group = species)) +
  geom_density(alpha =  .4) +
  theme_bw() +
  xlim(0, 200)

six_two_plot
```

![](toy_species_overlap_realsad_files/figure-markdown_github/six%20in%20two-2.png)

``` r
six_two_overlap <- get_overlap(six_two)

six_two_o <- ggplot(data = six_two_overlap, aes(x = overlap)) +
  geom_histogram(boundary = 0) + geom_vline(xintercept = c(0, 1), color = "red") + xlim(-.15, 1.15) +
  theme_bw()

six_two_o
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](toy_species_overlap_realsad_files/figure-markdown_github/six%20in%20two-3.png)

``` r
six_two_o_e <- list()
for(i in 1:nrow(six_two_overlap)) {
  six_two_o_e[[i]] <- rep(six_two_overlap$overlap[i], times = round(six_two_overlap$hm_abund[i]))
}
six_two_o_e <- data.frame(
  overlap = unlist(six_two_o_e))
six_two_hm <- ggplot(data = six_two_o_e, aes(x= overlap)) +
  geom_histogram(boundary = 0) + geom_vline(xintercept = c(0, 1), color = "red") + xlim(-.15, 1.15) +
  theme_bw()
six_two_hm
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](toy_species_overlap_realsad_files/figure-markdown_github/six%20in%20two-4.png)

#### Six nonoverlapping species

``` r
means_66 <- c(20,80, 130, 170, 250, 350) 
sds_66 <- .05 * means_66
six_six <- sample_community(means_66, sds_66, an_sad)


six_six_plot_all <- ggplot(data = six_six, aes(x = wgt)) +
  geom_density() +
  theme_bw() +
  xlim(0, 500)

six_six_plot_all
```

![](toy_species_overlap_realsad_files/figure-markdown_github/six%20in%20six-1.png)

``` r
six_six_plot <- ggplot(data = six_six, aes(x = wgt, fill = species, group = species)) +
  geom_density(alpha =  .4) +
  theme_bw() +
  xlim(0, 500)

six_six_plot
```

![](toy_species_overlap_realsad_files/figure-markdown_github/six%20in%20six-2.png)

``` r
six_six_overlap <- get_overlap(six_six)

six_six_o <- ggplot(data = six_six_overlap, aes(x = overlap)) +
  geom_histogram(boundary = 0) + geom_vline(xintercept = c(0, 1), color = "red") + xlim(-.15, 1.15) +
  theme_bw()

six_six_o
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](toy_species_overlap_realsad_files/figure-markdown_github/six%20in%20six-3.png)

``` r
six_six_o_e <- list()
for(i in 1:nrow(six_six_overlap)) {
  six_six_o_e[[i]] <- rep(six_six_overlap$overlap[i], times = round(six_six_overlap$hm_abund[i]))
}
six_six_o_e <- data.frame(
  overlap = unlist(six_six_o_e))
six_six_hm <- ggplot(data = six_six_o_e, aes(x= overlap)) +
  geom_histogram(boundary = 0) + geom_vline(xintercept = c(0, 1), color = "red") + xlim(-.15, 1.15) +
  theme_bw()
six_six_hm
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](toy_species_overlap_realsad_files/figure-markdown_github/six%20in%20six-4.png) \#\#\# Two groups of two overlapping with two that span the overlap

``` r
means_222 <- c(31, 32, 45, 60, 85, 86)
sds_222 <- .15 * means_222

two_two_two <- sample_community(means_222, sds_222, an_sad)

two_two_two_plot_all <- ggplot(data = two_two_two, aes(x = wgt)) +
  geom_density() +
  theme_bw() +
  xlim(0, 200)

two_two_two_plot_all
```

![](toy_species_overlap_realsad_files/figure-markdown_github/two%20in%20two%20plus%20two-1.png)

``` r
two_two_two_plot <- ggplot(data = two_two_two, aes(x = wgt, fill = species, group = species)) +
  geom_density(alpha =  .4) +
  theme_bw() +
  xlim(0, 200)

two_two_two_plot
```

![](toy_species_overlap_realsad_files/figure-markdown_github/two%20in%20two%20plus%20two-2.png)

``` r
two_two_two_overlap <- get_overlap(two_two_two)

two_two_two_o <- ggplot(data = two_two_two_overlap, aes(x = overlap)) +
  geom_histogram(boundary = 0) + geom_vline(xintercept = c(0, 1), color = "red") + xlim(-.15, 1.15) +
  theme_bw()

two_two_two_o
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](toy_species_overlap_realsad_files/figure-markdown_github/two%20in%20two%20plus%20two-3.png)

``` r
two_two_two_o_e <- list()
for(i in 1:nrow(two_two_two_overlap)) {
  two_two_two_o_e[[i]] <- rep(two_two_two_overlap$overlap[i], times = round(two_two_two_overlap$hm_abund[i]))
}
two_two_two_o_e <- data.frame(
  overlap = unlist(two_two_two_o_e))
two_two_hm <- ggplot(data = two_two_two_o_e, aes(x= overlap)) +
  geom_histogram(boundary = 0) + geom_vline(xintercept = c(0, 1), color = "red") + xlim(-.15, 1.15) +
  theme_bw()
two_two_hm
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](toy_species_overlap_realsad_files/figure-markdown_github/two%20in%20two%20plus%20two-4.png)

#### Six species, one group

``` r
means_61 <- c(45, 46, 47, 48, 49, 50)
sds_61 <- .15 * means_61

six_one <- sample_community(means_61, sds_61, an_sad)

six_one_plot_all <- ggplot(data = six_one, aes(x = wgt)) +
  geom_density() +
  theme_bw() +
  xlim(0, 200)

six_one_plot_all
```

![](toy_species_overlap_realsad_files/figure-markdown_github/six%20one-1.png)

``` r
six_one_plot <- ggplot(data = six_one, aes(x = wgt, fill = species, group = species)) +
  geom_density(alpha =  .4) +
  theme_bw() +
  xlim(0, 200)

six_one_plot
```

![](toy_species_overlap_realsad_files/figure-markdown_github/six%20one-2.png)

``` r
six_one_overlap <- get_overlap(six_one)

six_one_o <- ggplot(data = six_one_overlap, aes(x = overlap)) +
  geom_histogram(boundary = 0) + geom_vline(xintercept = c(0, 1), color = "red") + xlim(-.15, 1.15) +
  theme_bw()

six_one_o
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](toy_species_overlap_realsad_files/figure-markdown_github/six%20one-3.png)

``` r
six_one_o_e <- list()
for(i in 1:nrow(six_one_overlap)) {
  six_one_o_e[[i]] <- rep(six_one_overlap$overlap[i], times = round(six_one_overlap$hm_abund[i]))
}
six_one_o_e <- data.frame(
  overlap = unlist(six_one_o_e))
six_one_hm <- ggplot(data = six_one_o_e, aes(x= overlap)) +
  geom_histogram(boundary = 0) + geom_vline(xintercept = c(0, 1), color = "red") + xlim(-.15, 1.15) +
  theme_bw()
six_one_hm
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](toy_species_overlap_realsad_files/figure-markdown_github/six%20one-4.png)

#### Six overlapping

``` r
means_6 <- c(30, 40, 50, 60, 70, 80)
sds_6 <- .15 * means_6

six_ov <- sample_community(means_6, sds_6, an_sad)

six_ov_plot_all <- ggplot(data = six_ov, aes(x = wgt)) +
  geom_density() +
  theme_bw() +
  xlim(0, 200)

six_ov_plot_all
```

![](toy_species_overlap_realsad_files/figure-markdown_github/six%20overlap-1.png)

``` r
six_ov_plot <- ggplot(data = six_ov, aes(x = wgt, fill = species, group = species)) +
  geom_density(alpha =  .4) +
  theme_bw() +
  xlim(0, 200)

six_ov_plot
```

![](toy_species_overlap_realsad_files/figure-markdown_github/six%20overlap-2.png)

``` r
six_ov_overlap <- get_overlap(six_ov)

six_ov_o <- ggplot(data = six_ov_overlap, aes(x = overlap)) +
  geom_histogram(boundary = 0) + geom_vline(xintercept = c(0, 1), color = "red") + xlim(-.15, 1.15) +
  theme_bw()

six_ov_o
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](toy_species_overlap_realsad_files/figure-markdown_github/six%20overlap-3.png)

``` r
six_ov_o_e <- list()
for(i in 1:nrow(six_ov_overlap)) {
  six_ov_o_e[[i]] <- rep(six_ov_overlap$overlap[i], times = round(six_ov_overlap$hm_abund[i]))
}
six_ov_o_e <- data.frame(
  overlap = unlist(six_ov_o_e))
six_ov_hm <- ggplot(data = six_ov_o_e, aes(x= overlap)) +
  geom_histogram(boundary = 0) + geom_vline(xintercept = c(0, 1), color = "red") + xlim(-.15, 1.15) +
  theme_bw()
six_ov_hm
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](toy_species_overlap_realsad_files/figure-markdown_github/six%20overlap-4.png)
