#' GLC ponderada (Generalized Lorenz Curve)
#' @keywords internal
glc_weighted <- function(y, w = NULL, p_grid = seq(0.01, 1, by = 0.01)) {
  inp <- .validate_inputs(y, w, "y")
  y <- inp$y; w <- inp$w
  ok <- is.finite(y) & is.finite(w)
  y <- y[ok]; w <- w[ok]

  ord <- order(y)
  y <- y[ord]; w <- w[ord]
  W <- sum(w)
  cum_w <- cumsum(w) / W
  cum_yw <- cumsum(y * w)
  mu <- sum(y * w) / W
  f <- stats::approxfun(x = cum_w, y = cum_yw / W, yleft = 0, yright = mu, method = "linear")
  glc_vals <- f(p_grid)
  data.frame(p = p_grid, GLC = glc_vals, mu = mu)
}

#' Diferencia de GLC (A - B)
#' @export
glc_diff <- function(yA, wA = NULL, yB, wB = NULL, p_grid = seq(0.01, 1, by = 0.01)) {
  A <- glc_weighted(yA, wA, p_grid)
  B <- glc_weighted(yB, wB, p_grid)
  stopifnot(all(A$p == B$p))
  data.frame(p = A$p, GLC_A = A$GLC, GLC_B = B$GLC, diff = A$GLC - B$GLC)
}

#' Gráfico GLC
#' @export
plot_glc <- function(df_glc, group_levels, title_prefix = "Curvas Generalizadas de Lorenz (GLC)") {

  nameA <- group_levels[1]
  nameB <- group_levels[2]

  title <- paste0(title_prefix, ": ", nameA, " vs ", nameB)

  ggplot2::ggplot(df_glc, ggplot2::aes(x = p)) +
    ggplot2::geom_line(ggplot2::aes(y = GLC_A), linewidth = 1) +
    ggplot2::geom_line(ggplot2::aes(y = GLC_B), linetype = "dashed", linewidth = 1) +
    ggplot2::labs(
      x = "p (cuantil de población)",
      y = "GLC(p)",
      title = title,
      subtitle = paste0("Línea continua: ", nameA, " | Línea discontinua: ", nameB)
    ) +
    ggthemes::theme_economist() +
    ggplot2::theme(legend.position = "none")
}
