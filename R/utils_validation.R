.validate_inputs <- function(y, w = NULL, name = "y") {
  if (!is.numeric(y) || any(!is.finite(y))) stop(name, " debe ser numérico y finito.")
  if (any(y < 0)) stop(name, " debe ser no negativo.")
  if (is.null(w)) w <- rep(1, length(y))
  if (!is.numeric(w) || length(w) != length(y) || any(!is.finite(w)) || any(w <= 0)) {
    stop("Pesos inválidos: mismo largo que y, positivos y finitos.")
  }
  list(y = as.numeric(y), w = as.numeric(w))
}
.norm_weights <- function(w) w / sum(w)
.make_grid <- function(grid, lower, upper, by_default = 0.01) {
  if (is.null(grid)) seq(lower, upper, by = by_default) else grid
}
`%||%` <- function(a, b) if (is.null(a)) b else a
