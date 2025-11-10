#' ECDF ponderada (segura a NA)
#' @keywords internal
cdf_weighted <- function(y, w = NULL, t_grid = NULL) {
  inp <- .validate_inputs(y, w, "y")
  y <- inp$y; w <- inp$w
  ok <- is.finite(y) & is.finite(w)
  y <- y[ok]; w <- w[ok]

  W <- sum(w)
  ord <- order(y)
  y <- y[ord]; w <- w[ord]
  cum_w <- cumsum(w)

  if (is.null(t_grid)) t_grid <- sort(unique(y))

  Fvals <- vapply(t_grid, function(t) {
    idx <- max(which(y <= t))
    if (!is.finite(idx) || idx <= 0) return(0)
    cum_w[idx] / W
  }, numeric(1))
  data.frame(t = t_grid, F = Fvals)
}

#' Diferencia de CDF ponderadas (A - B)
#' @export
cdf_diff <- function(yA, wA = NULL, yB, wB = NULL, t_grid = NULL) {
  A <- cdf_weighted(yA, wA, t_grid)
  B <- cdf_weighted(yB, wB, if (is.null(t_grid)) A$t else t_grid)
  if (!all(A$t == B$t)) {
    t_all <- sort(unique(c(A$t, B$t)))
    A <- data.frame(t = t_all, F = stats::approx(A$t, A$F, xout = t_all, rule = 2)$y)
    B <- data.frame(t = t_all, F = stats::approx(B$t, B$F, xout = t_all, rule = 2)$y)
  }
  data.frame(t = A$t, F_A = A$F, F_B = B$F, diff = A$F - B$F)
}

#' Grafico CDF ponderadas
#' @export
plot_cdf <- function(df_cdf, group_levels, title_prefix = "CDF ponderada") {

  nameA <- group_levels[1]
  nameB <- group_levels[2]

  title <- paste0(title_prefix, ": ", nameA, " vs ", nameB)

  ggplot2::ggplot(df_cdf, ggplot2::aes(x = t)) +
    ggplot2::geom_line(ggplot2::aes(y = F_A), linewidth = 1) +
    ggplot2::geom_line(ggplot2::aes(y = F_B), linetype = "dashed", linewidth = 1) +
    ggplot2::labs(
      x = "t",
      y = "F(t)",
      title = title,
      subtitle = paste0("Línea continua: ", nameA, " | Línea discontinua: ", nameB)
    ) +
    ggthemes::theme_economist() +
    ggplot2::theme(legend.position = "none")
}
