fit_gmm <- function(isd, max_g = 9) {

  this_gmm <- mclust::densityMclust(isd$wgt, G = 1:max_g, modelNames = "V")

  return(this_gmm)
}

integrate_gmm <- function(gmm) {

  density_range <- range(gmm$data)

  id <- data.frame(wgt = seq(floor(.5 * density_range[1]), floor(1.25 * density_range[2]), by = 1),
                   density = NA)

  id$density <- predict(gmm, id$wgt)

  id$density <- id$density / sum(id$density)


  return(id)

}


integrate_gmm2 <- function(gmm) {

  density_range <- range(gmm$data)

  id <- data.frame(ratio = seq(floor(.5 * density_range[1]), floor(1.25 * density_range[2]), by = .01),
                   density = NA)

  id$density <- predict(gmm, id$ratio)

  id$density <- id$density / sum(id$density)

  #id<- dplyr::filter(id, density >= .01 * max(id$density))

  return(id)

}

count_peaks <- function(id) {

  npeaks <- sum(pastecs::turnpoints(id$density)$peaks)

  return(npeaks)
}

get_peaks <- function(id) {

  peaks <- id$ratio[ which(pastecs::turnpoints(id$density)$peaks)]

  return(peaks)

}

apply_cutoff <- function(id, cutoff = 1, nind = NULL) {

  if(!is.null(nind)) {
    cutoff <- cutoff/nind
  }

  if(all(id$density >= cutoff)) {

    drops <- data.frame(start_wgt = NA, last_wgt = NA, cutoff = cutoff * nind, drop_index = NA)
    return(drops)

  }

  min_above_cutoff <- min(which(id$density >= cutoff))
  max_above_cutoff <- max(which(id$density >= cutoff))

  drops <- list()

  for(i in min_above_cutoff:max_above_cutoff) {

    if(id$density[i] >= cutoff) {
      next
    } else {
      if(length(drops) == 0) {
        drops[[1]] <- data.frame(
          start_wgt = id$wgt[i],
          last_wgt = id$wgt[i]
        )
      } else {
        if(drops[[ length(drops)]]$last_wgt[1] == id$wgt[i - 1]) {
          drops[[ length(drops)]]$last_wgt[1] <- id$wgt[i]
        } else {
          drops[[length(drops) + 1]] <- data.frame(
            start_wgt = id$wgt[i],
            last_wgt = id$wgt[i]
          )
        }
      }
    }
  }

  if(length(drops) == 0) {

    drops <- data.frame(start_wgt = NA, last_wgt = NA, cutoff = cutoff * nind, drop_index = NA)
    return(drops)

  }

  names(drops) <- 1:length(drops)

  drops <- dplyr::bind_rows(drops, .id = "drop_index")

  drops$drop_index <- as.numeric(drops$drop_index)

  drops$cutoff <- cutoff * nind

  return(drops)

}

summarize_drops <- function(drops) {

  if(!is.na(drops$start_wgt[1])) {
    ndrops <- max(drops$drop_index)
  mean_drop_width <- mean(drops$last_wgt - drops$start_wgt)
  } else {
    ndrops <- 0
    mean_drop_width <- NA
  }

  drop_summary = data.frame(ndrops = ndrops, mean_drop_width = mean_drop_width, cutoff = drops$cutoff[1])

  return(drop_summary)
}

sample_uniform_isd <- function(isd) {
  min <- .8 * min(isd[,2])
  max <- 1.2 * max(isd[,2])

  uniform_isd <- data.frame(species = NA,
                            wgt = runif(n = nrow(isd), min = min, max = max))

  return(uniform_isd)
}

sample_constrained_isd <- function(isd, size_relationship = "none") {
  min <- .8*min(isd[,2])
  max <- 1.2*max(isd[,2])
  nspp <- length(unique(isd[,1]))
  sd <- .25

  this_sad <- data.frame(N = (scads::sample_METE(s = nspp, n = nrow(isd), nsamples = 1)[,1]))

  this_sad$meanwgt <- runif(min = min, max = max, n = nspp)

  if(size_relationship == "negative") {
    this_sad$meanwgt <- sort(this_sad$meanwgt, decreasing = T)
  }

  this_sad$sd <- .25 * this_sad$meanwgt
  this_sad$species <- 1:nspp

  this_isd <- apply(as.matrix(this_sad), MARGIN = 1, FUN = function(this_row) return(data.frame(species = this_row[4], wgt = rnorm(n = this_row[1], mean = this_row[2], sd = this_row[3]), row.names = NULL)))

  this_isd <- dplyr::bind_rows(this_isd)

  this_isd$wgt[ which(this_isd$wgt <= 0)] <- min

  return(this_isd)
}


drops_wrapper <- function(isd, cutoffs) {
  this_gmm <- fit_gmm(isd, max_g = 4)
  this_id <- integrate_gmm(this_gmm)

  drops <- lapply(cutoffs, FUN = apply_cutoff, id = this_id, nind = 4199)
  drops_summaries <- lapply(drops, summarize_drops)

  drops_summaries <- bind_rows(drops_summaries)
  drops_summaries$npeaks <- count_peaks(this_id)

  return(drops_summaries)

}


sample_ratio <- function(isd, nsamples) {

  random_comparisons <- replicate(nsamples, expr = sort(sample.int(n = nrow(isd), size = 2, replace = F)))  %>%
    t() %>%
    as.data.frame() %>%
    distinct()

  colnames(random_comparisons) = c("rowindex", "comparisonindex")

  isd_comparisons <- isd %>%
    mutate(rowindex = row_number()) %>%
    right_join(random_comparisons, by = "rowindex")

  pairing <- isd %>%
    mutate(comparisonindex = row_number()) %>%
    rename(comparisonwgt = wgt) %>%
    select(-species)

  isd_comparisons <- left_join(isd_comparisons, pairing, by = "comparisonindex")

  isd_comparisons <- isd_comparisons %>%
    mutate(ratio1 = wgt/comparisonwgt,
           ratio2 = comparisonwgt/wgt,
           difference = abs(comparisonwgt - wgt)) %>%
    mutate(ratio = ifelse(ratio1 > ratio2, ratio1, ratio2)) %>%
    select(-ratio1, -ratio2)

  return(isd_comparisons)
}

integrate_kde <- function(isd, band = 10) {
 this_kde <- ks::kde(isd$wgt, H = band)

 density_range <- range(this_kde$x)

 id <- data.frame(wgt = seq(floor(.5 * density_range[1]), floor(1.25 * density_range[2]), by = 1),
                  density = NA)

 id$density <- predict(this_kde, x = id$wgt)

 id$density <- id$density / sum(id$density)

 #id<- dplyr::filter(id, density >= .01 * max(id$density))

 return(id)


}