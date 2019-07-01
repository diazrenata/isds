#' Find gaps to a given threshold
#'
#' Cuts integrated density off at a given probability and finds gaps in the remaining distribution. (Gaps defined as non-sequential starting values, as would occur if the intervening values got filtered out when the distribution was cut off).
#'
#' @param threshold cutoff probability for gaps
#' @param integrated_density result of get_integrated_density
#'
#' @return integrated_density with columns for `is_gap_start`, `is_gap_end`
#' @export
#' @importFrom dplyr lag lead filter left_join
#' @importFrom tidyr replace_na
find_gaps <- function(threshold, integrated_density) {
  first_peak <- min(integrated_density[
    which(integrated_density$start_is_peak), "start"])

  within_peaks <- dplyr::filter(integrated_density, start >= first_peak,
                                by_max >= threshold) %>%
    dplyr::mutate(start_diff =
                    (start - dplyr::lag(start, n = 1, default = NA)) > 0.000000001) %>%
    dplyr::mutate(is_gap_start = dplyr::lead(start_diff, n = 1, default = F),
                  is_gap_end = start_diff) %>%
    tidyr::replace_na(replace = list(start_diff = FALSE,
                                     is_gap_start = FALSE,
                                     is_gap_end = FALSE))

  integrated_density <- dplyr::left_join(integrated_density,
                                         within_peaks,
                                         by = colnames(integrated_density)) %>%
    tidyr::replace_na(replace = list(start_diff = FALSE,
                                     is_gap_start = FALSE,
                                     is_gap_end = FALSE)) %>%
    dplyr::mutate(threshold = threshold)

  return(integrated_density)

}

#' Count gaps
#'
#' @param integrated_density result of find_gaps(integrated_density)
#'
#' @return number of gaps
#' @export
count_gaps <- function(integrated_density) {
  return(length(which(integrated_density$is_gap_start)))
}

#' Count peaks
#'
#' @param integrated_density result of find_gaps(integrated_density).
#'
#' @return number of peaks
count_peaks <- function(integrated_density) {
 return(length(which(integrated_density$start_is_peak)))
}

#' Get threshold
#'
#' Retrieve the threshold probability used in find_gaps. For collecting results.
#'
#' @param integrated_density result of find_gaps(integrated_density)
#'
#' @return threshold value
#' @export
get_threshold <- function(integrated_density) {
  return(integrated_density$threshold[1])
}

#' Get type
#' For collecting results.
#' @param integrated_density result of find_gaps(integrated_density)
#'
#' @return "emp" or "sim"
#' @export
get_type <- function(integrated_density) {
  return(integrated_density$type[1])
}

#' Get dataset name
#' For collecting results.
#' @param integrated_density result of find_gaps(integrated_density)
#'
#' @return Name of dataset used in integrated_density/sims
#' @export dat_name

get_datname <- function(integrated_density) {
  return(integrated_density$dat_name[1])
}

#' Retrieve standard deviation
#'
#' Retrieve sd used to create ISD for sims. For collecting results.
#'
#' @param integrated_density result of find_gaps(integrated_density)
#'
#' @return sd
#' @export
get_stdev <- function(integrated_density) {
  return(integrated_density$stdev[1])
}

#' Collect results
#'
#' @param integrated_density Result of find_gaps
#'
#' @return data frame 1 row long with summary of thresholding exercise.
#' @export
#'
get_result <- function(integrated_density) {
  result <- data.frame(ngaps = count_gaps(integrated_density),
                       npeaks = count_peaks(integrated_density),
                       threshold = get_threshold(integrated_density),
                       dat_type = get_type(integrated_density),
                       datname = get_datname(integrated_density),
                       stdev = get_stdev(integrated_density))
  return(result)
}
