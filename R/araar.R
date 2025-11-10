#' Bandas bootstrap para diferencia GLC (Araar)
#' @export
araar_ci <- function(yA, wA = NULL, yB, wB = NULL,
                     p_grid = seq(0.05, 0.95, by = 0.01),
                     B = 1000, conf = 0.95, seed = 123, progress = TRUE) {
  inpA <- .validate_inputs(yA, wA, "yA")
  inpB <- .validate_inputs(yB, wB, "yB")
  yA <- inpA$y; wA <- .norm_weights(inpA$w)
  yB <- inpB$y; wB <- .norm_weights(inpB$w)

  base <- glc_diff(yA, wA, yB, wB, p_grid)

  M <- matrix(NA_real_, nrow = B, ncol = length(p_grid))
  if (!is.null(seed)) set.seed(seed)
  for (b in seq_len(B)) {
    idxA <- sample.int(length(yA), replace = TRUE, prob = wA)
    idxB <- sample.int(length(yB), replace = TRUE, prob = wB)
    db <- glc_diff(yA[idxA], wA[idxA], yB[idxB], wB[idxB], p_grid)
    M[b, ] <- db$diff
    if (progress && (b %% max(50, B %/% 20) == 0)) message("bootstrap ", b, "/", B)
  }
  lo <- (1 - conf) / 2; hi <- 1 - lo
  LB <- apply(M, 2, stats::quantile, probs = lo, names = FALSE, type = 7)
  UB <- apply(M, 2, stats::quantile, probs = hi, names = FALSE, type = 7)
  list(p = base$p, diff = base$diff, LB = LB, UB = UB, conf = conf)
}

#' Test de Araar (Dominancia SD2 por GLC)
#' @export
araar_test <- function(yA, wA = NULL, yB, wB = NULL,
                       p_grid = seq(0.05, 0.95, by = 0.01),
                       B = 1000, conf = 0.95) {
  bands <- araar_ci(yA, wA, yB, wB, p_grid, B, conf, progress = FALSE)
  LB <- bands$LB; UB <- bands$UB
  domA <- all(LB > 0)
  domB <- all(UB < 0)
  decision <- if (domA) "A_dominates_B_SD2" else if (domB) "B_dominates_A_SD2" else "no_dominance"
  crossings <- which(diff(sign(bands$diff)) != 0L)
  list(decision = decision,
       p = bands$p, diff = bands$diff, LB = LB, UB = UB, conf = bands$conf,
       crossings_p = bands$p[crossings],
       share_LB_pos = mean(LB > 0),
       share_UB_neg = mean(UB < 0))
}

#' Gráfico Araar dinámico (A vs B con colores)
#' @export
plot_araar <- function(araar_result, group_levels, title_prefix = "Test de Araar") {
  nameA <- group_levels[1]; nameB <- group_levels[2]
  title <- paste0(title_prefix, ": ", nameA, " vs ", nameB)

  df <- data.frame(
    p = araar_result$p,
    diff = araar_result$diff,
    LB = araar_result$LB,
    UB = araar_result$UB
  )
  df$grupo_signo <- ifelse(df$diff >= 0, nameA, nameB)

  ggplot2::ggplot(df, ggplot2::aes(x = p, y = diff)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = LB, ymax = UB), fill = "grey70", alpha = 0.2) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_line(ggplot2::aes(color = grupo_signo), linewidth = 1.2) +
    ggplot2::scale_color_manual(values = c(nameA = "#0072B2", nameB = "#D55E00")) +
    ggplot2::labs(
      x = "p (cuantil de población)",
      y = paste0("GLC_", nameA, " - GLC_", nameB),
      title = title,
      subtitle = paste0("Decisión: ", araar_result$decision,
                        " | Conf: ", format(araar_result$conf))
    ) +
    ggthemes::theme_economist() +
    ggplot2::theme(legend.title = ggplot2::element_blank())
}
