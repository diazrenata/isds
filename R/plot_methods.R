#' Plot integrated density
#'
#' @param integrated_density int d dataframe
#' @param threshold_lines vector of values to put lines at to mark thresholds
#' @param plot_title title for plot
#'
#' @return plot of integrated density with maxima and minima marked
#' @importFrom ggplot2 ggplot geom_point aes theme_bw geom_hline labs
plot_integrated_density <- function(integrated_density, threshold_lines = NULL, plot_title = NULL) {

  integrated_plot <- ggplot2::ggplot(data = integrated_density, ggplot2::aes(x = start, y = by_max)) +
    ggplot2::geom_point(inherit.aes = TRUE, size = .1) +
    ggplot2::geom_point(data = integrated_density[ which(integrated_density$start_is_peak), c("start", "by_max")], inherit.aes = TRUE, color = "green", size = 2) +
    ggplot2::geom_point(data = integrated_density[ which(integrated_density$start_is_pit), c("start", "by_max")], inherit.aes = TRUE, color = "blue", size = 2) +
    ggplot2::theme_bw()

  if(!is.null(threshold_lines)) {
    integrated_plot <- integrated_plot +
      ggplot2::geom_hline(yintercept = threshold_lines, color = "red")
  }

  if(!is.null(plot_title)) {
    integrated_plot <- integrated_plot +
      ggplot2::labs(title = paste0("Prob. dens: ", plot_title))
  }

  id_plot_output <- list(plot = integrated_plot,
                         plot_title = plot_title)

  return(id_plot_output)
}


#' Make summary plots
#'
#' @param summary_dataframe result
#' @param dat_name dat name for this set of plots
#'
#' @return histogram of n peaks, dotplot of npeaks:ngaps by threshold
#' @export
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot theme_bw aes geom_bar geom_point labs facet_wrap geom_jitter geom_abline
plot_isd_methods_summary <- function(summary_dataframe, dat_name) {

  summary_dataframe <- dplyr::filter(summary_dataframe, datname == dat_name)

  baseplot <- ggplot2::ggplot(data = summary_dataframe) +
    ggplot2::theme_bw()

  peaks_plot <- baseplot +
    ggplot2::geom_bar(data = dplyr::filter(summary_dataframe, dat_type == "sim"), ggplot2::aes(x = as.factor(npeaks)), position = ggplot2::position_dodge2()) +
    ggplot2::geom_point(data = dplyr::filter(summary_dataframe, dat_type == "emp"),
                        ggplot2::aes(x = as.factor(npeaks), y = 0.5), color = "green", size= 5) +
    ggplot2::labs(x = "Number of peaks", y = "Count", title = paste0("Number of peaks (all) - ", dat_name)) +
    ggplot2::facet_wrap(. ~ threshold)

  ngaps_plot <- baseplot +
    ggplot2::geom_jitter(ggplot2::aes(x = npeaks, y = ngaps, color = dat_type), width = .2, height = 0) +
    ggplot2::xlim(-0, max(summary_dataframe$npeaks) + .5) +
    ggplot2::ylim(0, max(summary_dataframe$npeaks) + 1) +
    ggplot2::labs(x = "Number of peaks", y = "Number of gaps", title = paste0("Number of gaps - ", dat_name)) +
    ggplot2::geom_abline(slope = 1, intercept = -1) +
    ggplot2::facet_wrap(. ~ threshold)

  ngaps_by_t_plot <- baseplot +
    ggplot2::geom_jitter(ggplot2::aes(x = threshold, y = ngaps), width = .2, height = 0) +
    ggplot2::labs(x = "Threshold", y = "Number of gaps", title = paste0("Number of gaps - ", dat_name)) +
    ggplot2::facet_wrap(. ~ stdev)

  summary_plots <- list(peaks_plot = peaks_plot,
                        ngaps_plot = ngaps_plot,
                        ngaps_by_t_plot = ngaps_by_t_plot,
                        dat_name = dat_name)

  return(summary_plots)
}
