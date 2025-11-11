# dominanceR

Paquete para R que proporciona una caja de herramientas para análisis de dominancia estocástica (SD1 / SD2) mediante CDF, Curvas Generalizadas de Lorenz y el Test de Araar con bandas bootstrap. Diseñado para investigación económica aplicada y reproducible.

---

## Instalación

Desde GitHub:

```r
install.packages("devtools")  # si no está instalado
devtools::install_github("guillermo1205ad/dominanceR")
library(dominanceR)
```

---

## Cómo fijar la carpeta donde se guardan los gráficos

Por defecto los gráficos se guardan en una carpeta llamada `plots` dentro del **working directory actual** (el que devuelve `getwd()` en R).

Ejemplo:

```r
setwd("~/Desktop/proyecto_paper")
```

Esto hará que los gráficos queden en:

```
~/Desktop/proyecto_paper/plots/
```

cada máquina es distinta → tú decides la carpeta donde estás trabajando mediante `setwd()`.

---

## Ejemplo mínimo de uso

```r
library(dominanceR)

df <- data.frame(
  beneficiario = rep(c("control","tratado"), each=200),
  var_ventas_norm = c(rnorm(200, 0, 1), rnorm(200, 0.35, 1))
)

dominance_panel_df(
  data = df,
  group_col = "beneficiario",
  value_cols = c("var_ventas_norm"),
  B = 200
)
```

Esto creará 3 gráficos en la carpeta `plots`.

---

## Qué hace cada gráfico?

| método | interpretación |
|---|---|
| `CDF`  | SD1 (dominancia estocástica 1) |
| `GLC`  | SD2 vía Lorenz generalizada |
| `Araar` | prueba formal bootstrap |

---

## Issues

https://github.com/guillermo1205ad/dominanceR/issues
