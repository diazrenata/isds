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
      ggplot2::labs(title = paste0("Integrated probability density: ", plot_title))
  }

  return(integrated_plot)
}
#
# # Plot ISD methods summary variables
#
# plot_isd_methods_summary <- function(summary_dataframe) {
#
#   baseplot <- ggplot2::ggplot(data = summary_dataframe) +
#     ggplot2::theme_bw()
#
#   if(any(summary_dataframe$dat_type == "emp")) {
#     baseplot <- baseplot +
#       ggplot2::aes(color = dat_type)
# }
#   peaks_plot <- baseplot +
#     ggplot2::geom_bar(ggplot2::aes(x = as.factor(npeaks))) +
#     ggplot2::labs(x = "Number of peaks", y = "Count", title = "Number of peaks (all)")
#
#   ngaps_plot <- baseplot +
#     ggplot2::geom_jitter(ggplot2::aes(x = nmodes, y = ngaps), width = .2, height = 0) +
#     ggplot2::xlim(-.5, max(summary_dataframe$nmodes) + .5) +
#     ggplot2::ylim(0, max(summary_dataframe$nmodes) + 1) +
#     ggplot2::labs(x = "Number of peaks", y = "Number of gaps", title = "Number of gaps") +
#     ggplot2::geom_abline(slope = 1, intercept = -1) +
#     ggplot2::facet_grid(. ~ threshold)
#
#   summary_plots <- list(peaks_plot = peaks_plot,
#                         ngaps_plot = ngaps_plot)
#
#   return(summary_plots)
# }
