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

  library(mclust)

  bsdgmm <- densityMclust(emp_vector, G = 1, modelNames = "V")

  bsdkp <- data.frame(
    eval = seq(0, 2* (max(emp_vector)), by = .1),
    estimate = predict(bsdgmm, newdata = seq(0, 2* (max(emp_vector)), by = .1))
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
#' @return vector of length length(bsd) - 1, ratios of means
#' @export
#'
find_gaps_scaled <- function(bsd) {

  bsd <- sort(bsd)

  gaps <- vector(length = length(bsd) - 1)

  for(i in 1:(length(gaps))) {
    gaps[i] = bsd[i + 1] / bsd[i]
  }

  return(gaps)
}


#' Draw multimodal BSD
#'
#' @param emp_vector base vector
#' @param min_buffer_coeff defaults to 1
#' @param max_buffer_coeff defaults to 1
#' @param min_mode_gap_coeff defaults to 1.5 (larger mode must be at leas 150 percent of smaller mode)
#' @param min_sd_coeff defaults to .2
#' @param max_sd_coeff defaults to .2
#' @param return_full return nmodes and mean_p (TRUE) or just the vector?
#'
#' @return list of vector of mean_sizes length S drawn from multimodal distribution; nmodes; mode_p distribution
#' @export
#'
draw_multimodal_bsd <- function(emp_vector,
                                min_buffer_coeff = 1,
                                max_buffer_coeff = 1,
                                min_mode_gap_coeff = 1.5,
                                min_sd_coeff = .1,
                                max_sd_coeff = 1,
                                return_full = TRUE) {

  nmodes <- sample(c(2,3,4), size = 1)

  modevals = vector(length = nmodes)

  for(i in 1:length(modevals)) {

    this_min = max(min(emp_vector), min_mode_gap_coeff * modevals[i - 1])

    max_coeff = min_mode_gap_coeff ^ (nmodes - i)

    this_max = max(emp_vector) / max_coeff

    modevals[i] = runif(n = 1, min = this_min, max = this_max)
  }

  # modevals <- runif(n = nmodes,
  #                   min =emp_vector * min_buffer_coeff,
  #                   max = emp_vector * max_buffer_coeff)
  #
  # modegaps <- find_gaps_scaled(modevals)
  #
  # while(min(modegaps) <= min_mode_gap_coeff) {
  #
  #   modevals <- runif(n = nmodes,
  #                     min =emp_vector * min_buffer_coeff,
  #                     max = emp_vector * max_buffer_coeff)
  #
  #   modegaps <- find_gaps_scaled(modevals)
  # }

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
#' @param max_nb_clumps max nb clumps to try
#' @return nb clumps identified as nb Gs in GMM
#' @export
#'
#' @importFrom pastecs turnpoints
get_n_clumps <- function(mean_size_vector, max_nb_clumps = 4) {
  library(mclust)

  clump_aicc <- vector(length = max_nb_clumps)

  for(i in 1:max_nb_clumps){
      this_aicc <- try(AICc(Mclust(mean_size_vector, G =  i, modelNames = "V")), silent = T)
      clump_aicc[i] <- ifelse(is.numeric(this_aicc), this_aicc, NA)
  }


  nbclumps <- ifelse(any(!is.na(clump_aicc)), which(clump_aicc == min(clump_aicc, na.rm = T)), NA)

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


#' AICc (from LDATS)
#' Directly copied from weecology/LDATS because installing LDATS is weird on Travis.
#' @param object model
#'
#' @return AICc
#' @export
#'
AICc <- function (object)
{
  aic <- AIC(object)
  ll <- logLik(object)
  np <- attr(ll, "df")
  no <- attr(ll, "nobs")
  aic + (2 * np^2 + 2 * np)/(no - np - 1)
}

#' Make BSD
#' From community dat with species and wgt columns
#' @param community df with species and wgt columns per individual
#' @return df with species, meanwgt, logwgt columns
#' @export
#' @importFrom dplyr group_by summarize ungroup mutate
#'
make_bsd <- function (community)
{
  bsd <- community %>%
    dplyr::group_by(species) %>%
    dplyr::summarize(meanwgt = mean(wgt)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(logwgt = log(meanwgt))

  return(bsd)
}
