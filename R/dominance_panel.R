#' dominance_panel_df:
#' Calcula CDF, GLC y Araar para múltiples métricas y guarda 3 PNG por métrica.
#' - A/B se toman dinámicamente de los niveles de `group_col`.
#' - Si `group_col` tiene >2 niveles, puedes fijar cuáles comparar con `compare_levels`.
#' - No imprime tabla. Devuelve `invisible(NULL)`.
#'
#' @param data data.frame
#' @param group_col nombre de la columna de grupos (factor/char/num)
#' @param value_cols vector de nombres de columnas (métricas)
#' @param weight_col (opcional) nombre columna de pesos
#' @param compare_levels (opcional) vector de longitud 2 con los niveles a comparar
#' @param B bootstrap para Araar
#' @param conf confianza para Araar
#' @param out_dir carpeta de salida para PNGs
#' @export
dominance_panel_df <- function(data,
                               group_col,
                               value_cols,
                               weight_col = NULL,
                               compare_levels = NULL,
                               B = 1000,
                               conf = 0.95,
                               out_dir = "/Users/guillermoperalta/Desktop/plots") {

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  g_raw <- data[[group_col]]
  levs_all <- levels(as.factor(g_raw))
  if (length(levs_all) < 2) stop("`group_col` debe tener al menos 2 niveles")

  # elegir niveles A/B
  if (!is.null(compare_levels)) {
    if (length(compare_levels) != 2) stop("`compare_levels` debe tener longitud 2")
    if (!all(compare_levels %in% levs_all)) stop("`compare_levels` no están en los niveles de group_col")
    levs <- compare_levels
  } else {
    if (length(levs_all) > 2) {
      message("group_col tiene más de 2 niveles; usando los dos primeros: ",
              paste(levs_all[1:2], collapse = " vs "))
    }
    levs <- levs_all[1:2]
  }

  lvlA <- levs[1]; lvlB <- levs[2]
  g <- as.character(g_raw)  # para indexar seguro

  for (metric in value_cols) {
    # limpiar NA y tipar
    y <- as.numeric(data[[metric]])
    w <- if (is.null(weight_col)) rep(1, length(y)) else as.numeric(data[[weight_col]])

    ok <- is.finite(y) & is.finite(w) & !is.na(g)
    y <- y[ok]; w <- w[ok]; gk <- g[ok]

    yA <- y[gk == lvlA]; wA <- w[gk == lvlA]
    yB <- y[gk == lvlB]; wB <- w[gk == lvlB]

    # CDF
    cdf_df <- dominanceR::cdf_diff(yA, wA, yB, wB)
    p1 <- dominanceR::plot_cdf(cdf_df, group_levels = levs, title_prefix = "CDF ponderada")
    ggplot2::ggsave(file.path(out_dir, paste0("cdf_", metric, ".png")),
                    p1, width = 7, height = 5, dpi = 300)

    # GLC
    glc_df <- dominanceR::glc_diff(yA, wA, yB, wB)
    p2 <- dominanceR::plot_glc(glc_df, group_levels = levs, title_prefix = "Curvas Generalizadas de Lorenz (GLC)")
    ggplot2::ggsave(file.path(out_dir, paste0("glc_", metric, ".png")),
                    p2, width = 7, height = 5, dpi = 300)

    # Araar
    arr <- dominanceR::araar_test(yA, wA, yB, wB, B = B, conf = conf)
    p3 <- dominanceR::plot_araar(arr, group_levels = levs, title_prefix = "Test de Araar")
    ggplot2::ggsave(file.path(out_dir, paste0("araar_", metric, ".png")),
                    p3, width = 7, height = 5, dpi = 300)
  }

  invisible(NULL)
}
