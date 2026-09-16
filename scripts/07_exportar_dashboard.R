# =============================================================================
# 07_exportar_dashboard.R
# Lee el panel histórico consolidado y genera docs/data.json para el dashboard.
#
# Estructura del JSON exportado:
# {
#   "actualizado": "YYYY-MM-DD",
#   "aglomerado": 33,
#   "aglomerado_nombre": "Gran Río Cuarto",
#   "ultimo_periodo": "YYYY-TX",
#   "panel": [ { ANO4, TRIMESTRE, periodo, quiebre_metodologico, ... } ],
#   "ultimo": { ... indicadores del último trimestre disponible ... },
#   "cobertura": {
#     "desde": "YYYY-TX",
#     "hasta": "YYYY-TX",
#     "n_periodos": N,
#     "n_con_pobreza": N,
#     "n_con_mercado_trabajo": N,
#     "n_con_ingresos": N
#   },
#   "changelog": [ ... ],
#   "rupturas_conocidas": [ ... ]
# }
#
# El objeto "ultimo" facilita que el dashboard muestre KPI cards del trimestre
# más reciente sin parsear todo el panel.
# =============================================================================

library(dplyr)
library(jsonlite)

DIR_PANEL      <- "data/panel"
DIR_METODOLOGIA <- "metodologia"
DIR_DOCS       <- "docs"
dir.create(DIR_DOCS, showWarnings = FALSE, recursive = TRUE)

# ── Cargar panel ──────────────────────────────────────────────────────────────
archivo_panel <- file.path(DIR_PANEL, "panel_historico.rds")
if (!file.exists(archivo_panel)) {
  stop("No se encontró panel_historico.rds. Ejecutar 06_panel_historico.R primero.")
}
panel <- readRDS(archivo_panel)

if (nrow(panel) == 0) {
  stop("El panel histórico está vacío. Verificar que existan indicadores en data/processed/.")
}

# ── Último período con datos (al menos un indicador no-NA) ────────────────────
# Excluye columnas de metadata para buscar el último período "real"
cols_indicadores <- setdiff(names(panel),
                            c("ANO4", "TRIMESTRE", "periodo",
                              "quiebre_metodologico"))

# Para cada fila, contar cuántos indicadores tienen valor
n_indicadores_por_fila <- apply(panel[, cols_indicadores, drop = FALSE], 1,
                                 function(r) sum(!is.na(r)))

# Último período con al menos un indicador
ultimo_idx <- max(which(n_indicadores_por_fila > 0))
ultimo_row <- panel[ultimo_idx, ]
ultimo_periodo_str <- ultimo_row$periodo[1]

# ── Serializar NA: jsonlite los convierte a null ───────────────────────────────
# Asegurar que el panel no tenga factores (jsonlite los serializa como enteros)
panel <- panel %>% mutate(across(where(is.factor), as.character))

# Redondear columnas numéricas de tasas a 1 decimal, ingresos a 0
cols_tasa    <- grep("^tasa_|^pct_", names(panel), value = TRUE)
cols_ingreso <- grep("^ingreso_|^ipcf_|^itf_", names(panel), value = TRUE)
cols_n       <- grep("^n_|^poblacion_", names(panel), value = TRUE)

for (col in cols_tasa)
  panel[[col]] <- round(as.numeric(panel[[col]]), 1)
for (col in cols_ingreso)
  panel[[col]] <- round(as.numeric(panel[[col]]), 0)
for (col in cols_n)
  panel[[col]] <- as.integer(panel[[col]])

# ── Bloque "ultimo" ───────────────────────────────────────────────────────────
ultimo <- as.list(ultimo_row)
ultimo <- lapply(ultimo, function(x) if (length(x) == 1) x[[1]] else x)

# ── Cobertura ─────────────────────────────────────────────────────────────────
cobertura <- list(
  desde                = panel$periodo[1],
  hasta                = panel$periodo[nrow(panel)],
  n_periodos           = nrow(panel),
  n_con_pobreza        = if ("tasa_pobreza" %in% names(panel))
    sum(!is.na(panel$tasa_pobreza)) else 0L,
  n_con_mercado_trabajo = if ("tasa_actividad" %in% names(panel))
    sum(!is.na(panel$tasa_actividad)) else 0L,
  n_con_ingresos       = if ("ingreso_medio_ocup_ppal" %in% names(panel))
    sum(!is.na(panel$ingreso_medio_ocup_ppal)) else 0L
)

# ── Changelog ─────────────────────────────────────────────────────────────────
changelog <- list()
archivo_changelog <- file.path(DIR_METODOLOGIA, "changelog.csv")
if (file.exists(archivo_changelog)) {
  tryCatch({
    cl <- read.csv(archivo_changelog, stringsAsFactors = FALSE)
    changelog <- unname(split(cl, seq_len(nrow(cl))))
  }, error = function(e) {
    warning(sprintf("No se pudo leer changelog.csv: %s", e$message))
  })
}

# ── Rupturas conocidas ────────────────────────────────────────────────────────
rupturas <- list()
archivo_rupturas <- file.path(DIR_METODOLOGIA, "rupturas_conocidas.csv")
if (file.exists(archivo_rupturas)) {
  tryCatch({
    rk <- read.csv(archivo_rupturas, stringsAsFactors = FALSE)
    rupturas <- unname(split(rk, seq_len(nrow(rk))))
  }, error = function(e) {
    warning(sprintf("No se pudo leer rupturas_conocidas.csv: %s", e$message))
  })
}

# ── Armar y exportar JSON ──────────────────────────────────────────────────────
exportar <- list(
  actualizado        = format(Sys.Date(), "%Y-%m-%d"),
  aglomerado         = 33L,
  aglomerado_nombre  = "Gran Río Cuarto",
  ultimo_periodo     = ultimo_periodo_str,
  panel              = panel,
  ultimo             = ultimo,
  cobertura          = cobertura,
  changelog          = changelog,
  rupturas_conocidas = rupturas
)

archivo_json <- file.path(DIR_DOCS, "data.json")
write_json(exportar, archivo_json,
           pretty = TRUE,
           auto_unbox = TRUE,
           na = "null",
           digits = NA)   # NA = usa los valores ya redondeados

message(sprintf("data.json exportado: %s (%s)",
                archivo_json,
                format(file.size(archivo_json), big.mark = ".", decimal.mark = ",")))
message(sprintf("Último período en el panel: %s", ultimo_periodo_str))
message(sprintf("Total de períodos: %d", nrow(panel)))
