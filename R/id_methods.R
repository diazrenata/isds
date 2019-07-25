#' Make results table
#'
#' @param id_list  list ofi ntegrated densities
#'
#' @return df of rsults
#' @export
#'
#' @importFrom dplyr bind_rows
make_results <- function(id_list) {

  results <- lapply(id_list, make_metadata)
  results <- dplyr::bind_rows(results)

  results$npeaks <- vapply(id_list, count_peaks, FUN.VALUE = 2)
  results$mean_p <- vapply(id_list, get_mean_p, FUN.VALUE = .5)
  results$width <- vapply(id_list, get_width, FUN.VALUE = 100)

  return(results)
}

#' Get metadata from pars list
#'
#' @param id_list list ofi ntegrated density, pars
#'
#' @return metadata df
#' @export
make_metadata <- function(id_list) {
  this_metadata <- id_list$pars
  this_metadata$stdev_range <- ifelse(is.na(this_metadata$stdev_range), "no range", paste(this_metadata$stdev_range[1], this_metadata$stdev_range[2], sep = "_" ))
  this_metadata$stdev_range <- as.character(this_metadata$stdev_range[1])
  if(is.null(this_metadata$stdev)) {
    this_metadata$stdev <- NA
  }
  this_metadata$dat_name <- as.character(this_metadata$dat_name)
  this_metadata$stdev_range <- as.character(this_metadata$stdev_range)
  return(this_metadata)
}
#' Count peaks
#'
#' @param integrated_density_list list
#'
#' @return number of peaks
#' @export
count_peaks <- function(integrated_density_list) {
  return(length(which(integrated_density_list$integrated_density$start_is_peak)))
}

#' Get mean ratio or stdev of ratio modes
#'
#' Get mean or stdev of ratio of modes
#'
#' @param integrated_density integrated_density
#' @param what "mean" or "stdev"
#'
#' @return mean or stdev
#' @export
#' @importFrom dplyr filter select
get_mode_ratio <- function(integrated_density, what = "mean") {
  if(sum(integrated_density$start_is_peak) <= 1) {
    return(NA)
  }
  modes <- integrated_density %>%
    dplyr::filter(start_is_peak) %>%
    dplyr::select(start)

  ratios = vector(length = nrow(modes) - 1,
                  mode = "numeric")
  for(i in 1:length(ratios)) {
    ratios[i] <- modes$start[i + 1] - modes$start[i]
  }
  if(what == "mean") {
    return(mean(ratios))
  }
  if(what == "stdev") {
    return(sd(ratios))
  }
}

#' Get mean probability density
#'
#' @param integrated_density integrated_density
#'
#' @return mean
#' @export
get_mean_p <- function(integrated_density) {
  if(is.list(integrated_density)) {
  integrated_density <- integrated_density$integrated_density
  }
  beginning <- min(which(integrated_density$by_max >= (.001)))
  end <- max(which(integrated_density$by_max >= (.001)))

  integrated_density <- integrated_density[ beginning:end, ]

  return(mean(integrated_density$by_max))
}


#' Get width of size spectrum
#'
#' @param integrated_density integrated_density
#'
#' @return width
#' @export
get_width <- function(integrated_density) {
  if(is.list(integrated_density)) {
    integrated_density <- integrated_density$integrated_density
  }
  beginning <- min(which(integrated_density$by_max >= (.001)))
  end <- max(which(integrated_density$by_max >= (.001)))

  return(end - beginning)
}



#' Get elevation change
#' @param integrated_density integrated_density
#'
#' @return scaled e change
#' @export
#' @importFrom dplyr filter select
get_scaled_e_change <- function(integrated_density) {
  if(sum(integrated_density$start_is_peak) <= 1) {
    return(NA)
  }
  peaks <- integrated_density %>%
    dplyr::filter(start_is_peak) %>%
    dplyr::select(by_max)

  peaks

  if(nrow(peaks) == 1) {
    return(NA)
  }

  min_e_change <- sum(peaks$by_max[1], peaks$by_max[nrow(peaks)])
  for(i in 2:nrow(peaks)) {
    min_e_change <- sum(min_e_change, peaks$by_max[i] - peaks$by_max[i-1])
  }

  max_e_change <- sum(peaks$by_max) * 2

  turns <- integrated_density %>%
    dplyr::filter(start_is_turnpoint)

  true_e_change <- sum(peaks$by_max[1], peaks$by_max[nrow(peaks)])

  for(i in 2:nrow(turns)) {
    true_e_change = true_e_change + abs(turns$by_max[i] - turns$by_max[i -1])
  }
  min_e_change
  max_e_change
  true_e_change

  scaled_e_change <- (true_e_change - min_e_change) / (max_e_change - min_e_change)

  return(scaled_e_change)
}
