#' Sample a community
#'
#' @param means Vector of species means
#' @param sds Vector of standard deviations
#' @param abunds Vector of abundanaces
#' @param sd_coefficient if sds is not specified, sds = sd_coefficient * means
#'
#' @return data frame of simulated community
#' @export
#' @importFrom dplyr bind_rows
#'
sample_community <- function(means, sds = NULL, abunds, sd_coefficient = .15) {

  if(is.null(sds)) {
    sds <- sd_coefficient * means
  }

  comm <- list()

  for(i in 1:length(means)) {
    comm[[i]] <- data.frame(
      species = i,
      wgt = rnorm(mean = means[i], sd = sds[i], n = abunds[i])
    )
  }

  comm <- dplyr::bind_rows(comm)

  return(comm)
}

#' Plot an isd
#'
#' @param community_df df with wgt
#'
#' @return plot of isd
#' @export
#' @importFrom ggplot2 ggplot geom_density theme_bw xlim
plot_isd <- function(community_df) {

  xlims <- c(0, 1.5 * max(community_df$wgt))

  isd_plot <- ggplot(data = community_df, aes(x = wgt)) +
    geom_density() +
    theme_bw() +
    xlim(xlims)

  return(isd_plot)

}

#' Plot species body size distributions
#'
#' @param community_df df with species, wgt
#'
#' @return sbsd plot
#' @export
#' @importFrom ggplot2 ggplot geom_density theme_bw scale_color_viridis_d scale_fill_viridis_d xlim theme
plot_sbsds <- function(community_df) {

  community_df$species <- as.factor(community_df$species)
  xlims <- c(0, 1.5 * max(community_df$wgt))

  sbsd_plot <- ggplot(data = community_df, aes(x = wgt, group = species, color = species, fill = species)) +
    geom_density(alpha = .3) +
    theme_bw() +
    scale_color_viridis_d(option = "plasma", end = .8) +
    scale_fill_viridis_d(option = "plasma", end = .8)+
    xlim(xlims) +
    theme(legend.position = "none")

  return(sbsd_plot)

}

#' Plot histogram of overlap values
#'
#' @param overlap_df df with overlap, optionally a column to weight by
#' @param weighting_col optional name of column to weight by
#'
#' @return histogram
#' @export
#' @importFrom ggplot2 ggplot geom_histogram xlim geom_vline theme_bw
plot_overlaps <- function(overlap_df, weighting_col = NULL) {

  if(!is.null(weighting_col)) {
    overlap_df <- data.frame(
      overlap = rep(overlap_df$overlap, times = overlap_df[, weighting_col])
    )
  }

  overlap_plot <- ggplot(data = overlap_df, aes(x = overlap)) +
    geom_histogram(boundary = 0, closed = "left", binwidth = .1) +
    xlim(-.2, 1.2) +
    geom_vline(xintercept = c(0, 1), color = "red") +
    theme_bw()

}
