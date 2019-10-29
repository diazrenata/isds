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

  this_isd <- apply(as.matrix(this_sad), MARGIN = 1, FUN = function(this_row) return(data.frame(species = this_row[4], wgt = rnorm(n = this_row[1], mean = this_row[2], sd = this_row[3]))))

  this_isd <- dplyr::bind_rows(this_isd)

  this_isd$wgt[ which(this_isd$wgt <= 0)] <- min

  return(this_isd)
}

get_clumps <- function(isd, max_n_clumps = NULL) {

  if(is.null(max_n_clumps)) {
    max_n_clumps <- length(unique(isd$species))
  }

  library(mclust)

  this_gmm <- densityMclust(isd[,2], G = 1:max_n_clumps, modelNames = "V")

  id <- data.frame(wgt = seq(0.2, (max(isd[,2]) * 2), by = 0.2))

  id$p <- predict(this_gmm, newdata = id$wgt)

  d_turns <- pastecs::turnpoints(id$p)


  id$is_minimum <- d_turns$pits
  id$is_maximum <- d_turns$peaks

  nclumps <- sum(id$is_maximum)

  clumps <- list()

  for(i in 1:nclumps) {
    if(i == 1) {
      if(nclumps == 1) {
        clumps[[i]] <- c(i, -50, max(id$wgt) * 10)
      } else {
        clumps[[i]] <- c(i, -50, id$wgt[which(id$is_minimum)[1]])
      }
    } else if(i == nclumps) {
      clumps[[i]] <- c(i, id$wgt[which(id$is_minimum)[nclumps - 1]], max(id$wgt)*10)
    } else {
      clumps[[i]] <- c(i, id$wgt[which(id$is_minimum)[i - 1]],
                       id$wgt[which(id$is_minimum)[i]])
    }
  }

  isd$clump <- NA

  for(i in 1:nrow(isd)) {
    for(j in 1:nclumps) {
      if(dplyr::between(isd$wgt[i], clumps[[j]][2], clumps[[j]][3])) {
        isd$clump[i] <- clumps[[j]][1]
      }
    }
  }

  if(!("source" %in% colnames(isd))) {
    isd$source <- "empirical"
    isd$sim <- -99
  }
  return(isd)
}

sim_wrapper <- function(isd, type) {

  if(type == "uniform") {

    sampled <- sample_uniform_isd(isd)

  } else if(type == "constrained") {

    sampled <- sample_constrained_isd(isd, size_relationship = "none")

  } else if(type == "constrained_density") {

    sampled <- sample_constrained_isd(isd, size_relationship = "negative")

  } else if (type == "none") {
    return(isd)
  } else {
    return(NA)
  }

  sampled$source = type

  clumps <- get_clumps(sampled, max_n_clumps = length(unique(isd$species)))

  return(clumps)
}

repeat_sims <- function(ntimes, isd, type) {
  sims <- replicate(n = ntimes, expr = sim_wrapper(isd, type = type), simplify = F)
  names(sims) <- 1:ntimes
  sims <- bind_rows(sims, .id = "sim")
  sims$sim <- as.numeric(sims$sim)
  return(sims)
}

summarize_clumps <- function(isd) {

  isd <- isd %>%
    mutate(clump = as.integer(clump)) %>%
    group_by(source, sim, clump) %>%
    summarize(nind = n(),
              clump_min = min(wgt),
              clump_max = max(wgt),
              clump_sd = sd(wgt)) %>%
    ungroup() %>%
    mutate(clump_width = clump_max - clump_min) %>%
    mutate(ind_width = nind / clump_width) %>%
    group_by(source, sim) %>%
    summarize(nclumps = max(clump),
              mean_ind_width = mean(ind_width),
              mean_sd = mean(clump_sd))%>%
    ungroup()

  return(isd)

}
