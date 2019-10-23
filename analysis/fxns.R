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

get_clumps <- function(isd, max_n_clumps) {

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
      clumps[[i]] <- c(i, -50, id$wgt[which(id$is_minimum)[1]])
    } else if(i == nclumps) {
      clumps[[i]] <- c(i, id$wgt[which(id$is_minimum)[nclumps - 1]], max(id$wgt)*2)
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

  isd$clump <- as.factor(isd$clump)
  return(isd)
}

sim_wrapper <- function(isd, type) {

  if(type == "uniform") {

    sampled <- sample_uniform_isd(isd)

  } else if(type == "constrained") {

    sampled <- sample_constrained_isd(isd, size_relationship = "none")

  } else if(type == "constrained_density") {

    sampled <- sample_constrained_isd(isd, size_relationship = "negative")

  } else {
    return(NA)
  }

  clumps <- get_clumps(sampled, max_n_clumps = length(unique(isd$species)))

  return(clumps)
}
