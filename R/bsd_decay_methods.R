#' Draw a BSD from uniform
#'
#' (Really a wrapper for `sample`)
#'
#' @param s Number of species
#' @param min Min size
#' @param max Max size
#'
#' @return vector of length s, species mean body sizes
#' @export
#'
draw_uniform_bsd <- function(s, min, max) {
  means_vector <- runif(s, min, max)
  return(means_vector)
}

#' Draw a BSD from unimodal distribution
#'
#' Unimodal distribution based on kde of empirical vector
#' After Allen 1999
#'
#' @param emp_vector Vector of mean body sizes to base kde on
#'
#' @return Vector of body sizes drawn from a smooth probability dist based on kde based on emp_vector
#' @export
#' @importFrom dplyr filter mutate
draw_unimodal_bsd <- function(emp_vector) {

  bsdk <- ks::kde(emp_vector)

  bsdkp <- data.frame(
    eval = bsdk$eval.points,
    estimate = bsdk$estimate
  )

  bsdkp <- bsdkp %>%
    dplyr::filter(eval > 0) %>%
    dplyr::mutate(stdp = estimate / sum(estimate))

  thisbsd <- sample(bsdkp$eval, size = length(emp_vector), replace = T, prob = bsdkp$stdp)

  return(thisbsd)

}


#' Find gaps in bsd
#'
#' @param bsd vector of species means
#'
#' @return vector of length length(bsd) - 1, gaps between means
#' @export
#'
find_gaps <- function(bsd) {

  bsd <- sort(bsd)

  gaps <- vector(length = length(bsd) - 1)

  for(i in 1:(length(gaps))) {
    gaps[i] = bsd[i + 1] - bsd[i]
  }

  return(gaps)
}


#' Draw multimodal BSD
#'
#' @param emp_vector base vector
#' @param min_buffer_coeff defaults to .75
#' @param max_buffer_coeff defaults to 1.1
#' @param min_mode_gap defaults to .5 - makes sense if vector is on log scale
#' @param min_sd_coeff defaults to .1 - makes sense if on log scale
#' @param max_sd_coeff defaults to 1
#' @param return_full return nmodes and mean_p (TRUE) or just the vector?
#'
#' @return list of vector of mean_sizes length S drawn from multimodal distribution; nmodes; mode_p distribution
#' @export
#'
draw_multimodal_bsd <- function(emp_vector,
                                min_buffer_coeff = .75,
                                max_buffer_coeff = 1.1,
                                min_mode_gap = .5,
                                min_sd_coeff = .1,
                                max_sd_coeff = 1,
                                return_full = TRUE) {

  nmodes <- sample(c(2,3,4), size = 1)

  modevals <- runif(n = nmodes,
                    min =emp_vector * min_buffer_coeff,
                    max = emp_vector * max_buffer_coeff)

  modegaps <- find_gaps(modevals)

  while(min(modegaps) <= min_mode_gap) {

    modevals <- runif(n = nmodes,
                      min =emp_vector * min_buffer_coeff,
                      max = emp_vector * max_buffer_coeff)

    modegaps <- find_gaps(modevals)
  }

  sd_coeff <- runif(n = nmodes, min = min_sd_coeff, max = max_sd_coeff)

  mode_p <- data.frame(
    val = seq(0, 5 * max(modevals), by = .01)
  )

  for(i in 1:nmodes) {
    mode_p[, i + 1] <- dnorm(mode_p$val, mean = modevals[i], sd = sd_coeff[i])
  }

  mode_p$sum <- rowSums(mode_p[ , 2:(nmodes + 1)])

  mode_p$sum <- mode_p$sum / sum(mode_p$sum)

  mean_sizes <- sample(mode_p$val, size = length(emp_vector), replace = T, prob = mode_p$sum)

  if(return_full) {
  return(list(bsd = mean_sizes,
              nmodes = nmodes,
              mode_p = mode_p))

  } else {
    return(mean_sizes)
  }
}

#' Get nb clumps
#'
#' @param mean_size_vector bsd
#'
#' @return nb clumps identified as nb Gs in GMM
#' @export
#'
#' @importFrom pastecs turnpoints
#'
get_n_clumps <- function(mean_size_vector) {
  library(mclust)
  clumps <- densityMclust(mean_size_vector, G = 1:4)
  interval_size = 0.01
  min_size = 0
  max_size = max(mean_size_vector) * 5
  density_evalpoints <- seq(min_size, max_size, by = interval_size)

  density_estimates <- predict(clumps, newdata = density_evalpoints)

  density_modes <- pastecs::turnpoints(density_estimates)

  nbclumps <- sum(density_modes$peaks)


  return(nbclumps)
}


#' Get between/total ssq
#'
#' @param mean_size_vector mean size vector
#'
#' @return btwn/total ssq for kmeans
#' @export
#'
get_ssq_prop <- function(mean_size_vector, nbclumps = NULL) {
  if(is.null(nbclumps)) {
    nbclumps <- get_n_clumps(mean_size_vector)
  }

  km <- kmeans(mean_size_vector, nbclumps)

  btwn <- km$betweenss
  tot <- km$totss

  prop <- btwn/tot

  return(prop)

}
