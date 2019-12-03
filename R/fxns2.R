#' Fit GMM to isd
#'
#' @param isd df with wgt
#' @param max_g max nb gaussians
#'
#' @return Mclust GMM
#' @export
#' @importFrom mclust densityMclust mclustBIC
fit_gmm <- function(isd, max_g = 9) {

  this_gmm <- mclust::densityMclust(isd$wgt, G = 1:max_g, modelNames = "V")

  return(this_gmm)
}

#' Integrate GMM
#'
#' @param gmm Mclust gmm
#'
#' @return df of all wgts, density
#' @export
#'
integrate_gmm <- function(gmm) {

  density_range <- range(gmm$data)

  id <- data.frame(wgt = seq(floor(.5 * density_range[1]), floor(1.25 * density_range[2]), by = 1),
                   density = NA)

  id$density <- predict(gmm, id$wgt)

  id$density <- id$density / sum(id$density)


  return(id)

}



#' Count peaks in id
#'
#' @param id df with density
#'
#' @return nb peaks (turnpoints)
#' @export
#' @importFrom pastecs turnpoints
count_peaks <- function(id) {

  npeaks <- sum(pastecs::turnpoints(id$density)$peaks)

  return(npeaks)
}

#' Sample uniform isd
#'
#' @param isd isd to base it on
#'
#' @return isd drawn from uniform
#' @export
#'
sample_uniform_isd <- function(isd) {
  min <- .8 * min(isd[,2])
  max <- 1.2 * max(isd[,2])

  uniform_isd <- data.frame(species = NA,
                            wgt = runif(n = nrow(isd), min = min, max = max))

  return(uniform_isd)
}

#' Sample constrained isd
#'
#' @param isd isd to base on
#' @param size_relationship "none" or "negative
#'
#' @return isd
#' @export
#'
#' @importFrom scads sample_METE
#' @importFrom dplyr bind_rows
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
