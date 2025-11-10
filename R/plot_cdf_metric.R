#' plot_cdf_metric
#' genera gráfico CDF comparando grupo A vs grupo B
#' @export
plot_cdf_metric <- function(cdf_df, metric_name) {

  library(ggplot2)
  library(ggthemes)

  p <- ggplot(cdf_df, aes(x=p)) +
    geom_line(aes(y=cdf_A, color="grupo A"), size=1.2) +
    geom_line(aes(y=cdf_B, color="grupo B"), size=1.2, linetype="dashed") +
    labs(title = paste0("CDF — ", metric_name),
         x = "quantil",
         y = "CDF") +
    scale_color_manual(values=c("grupo A"="#0072B2","grupo B"="#D55E00")) +
    theme_economist() +
    theme(legend.title = element_blank())

  return(p)
}
