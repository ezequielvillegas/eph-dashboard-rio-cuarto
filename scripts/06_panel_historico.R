# =============================================================================
# 06_panel_historico.R
# Consolida todos los archivos de indicadores procesados en un único panel
# cronológico, listo para exportar al dashboard.
#
# Fuentes (todas opcionales si el período no fue procesado aún):
#   data/processed/indicadores_YYYY_TX.rds     → pobreza + indigencia
#   data/processed/mercado_trabajo_YYYY_TX.rds → tasas de empleo/desempleo
#   data/processed/ingresos_YYYY_TX.rds        → ingresos medios/medianos
#
# Salidas:
#   data/panel/panel_historico.rds
#   data/panel/panel_historico.csv
#
# Columnas del panel:
#   ANO4, TRIMESTRE, periodo (YYYY-TX), quiebre_metodologico
#   + todas las columnas de indicadores disponibles (NA donde no haya datos)
#
# Nota: quiebre_metodologico == TRUE para trimestres anteriores a 2016-T2
#       (rediseño muestral post-Censo 2010, metodología INDEC).
# =============================================================================

library(dplyr)

DIR_PROCESSED <- "data/processed"
DIR_PANEL     <- "data/panel"
dir.create(DIR_PANEL, showWarnings = FALSE, recursive = TRUE)

# ── Función auxiliar: lista y carga todos los archivos de un patrón ──────────
cargar_archivos <- function(patron) {
  archivos <- list.files(DIR_PROCESSED, pattern = patron, full.names = TRUE)
  if (length(archivos) == 0) return(NULL)

  lista <- lapply(archivos, function(f) {
    tryCatch(readRDS(f),
             error = function(e) {
               warning(sprintf("No se pudo leer %s: %s", basename(f), e$message))
               NULL
             })
  })
  lista <- Filter(Negate(is.null), lista)
  if (length(lista) == 0) return(NULL)
  bind_rows(lista)
}

# ── Cargar los tres bloques de indicadores ────────────────────────────────────
indicadores_pob <- cargar_archivos("^indicadores_\\d{4}_T\\d\\.rds$")
indicadores_mt  <- cargar_archivos("^mercado_trabajo_\\d{4}_T\\d\\.rds$")
indicadores_ing <- cargar_archivos("^ingresos_\\d{4}_T\\d\\.rds$")

# ── Determinar el universo de períodos ─────────────────────────────────────────
# Usa la unión de períodos disponibles en cualquiera de las tres fuentes.
periodos <- bind_rows(
  if (!is.null(indicadores_pob)) indicadores_pob[, c("ANO4", "TRIMESTRE")],
  if (!is.null(indicadores_mt))  indicadores_mt[,  c("ANO4", "TRIMESTRE")],
  if (!is.null(indicadores_ing)) indicadores_ing[, c("ANO4", "TRIMESTRE")]
) %>%
  distinct(ANO4, TRIMESTRE) %>%
  arrange(ANO4, TRIMESTRE)

if (nrow(periodos) == 0) {
  message("No se encontraron indicadores procesados en data/processed/. El panel histórico no se actualiza.")
  quit(save = "no", status = 0)
}

# ── Merge progresivo por ANO4 + TRIMESTRE ─────────────────────────────────────
panel <- periodos

if (!is.null(indicadores_pob)) {
  panel <- left_join(panel, indicadores_pob, by = c("ANO4", "TRIMESTRE"))
}

if (!is.null(indicadores_mt)) {
  # Evitar duplicar columnas ya presentes (tasa_desocupacion puede aparecer en ambas)
  cols_mt <- setdiff(names(indicadores_mt), c("ANO4", "TRIMESTRE", names(panel)))
  if (length(cols_mt) > 0) {
    panel <- left_join(panel, indicadores_mt[, c("ANO4", "TRIMESTRE", cols_mt)],
                       by = c("ANO4", "TRIMESTRE"))
  } else {
    # Si tasa_desocupacion ya existe, priorizar la de mercado_trabajo (más completa)
    cols_mt_all <- setdiff(names(indicadores_mt), c("ANO4", "TRIMESTRE"))
    panel <- left_join(panel, indicadores_mt, by = c("ANO4", "TRIMESTRE"),
                       suffix = c("", "_mt"))
    # Para columnas duplicadas, usar la versión _mt si la base está NA
    dup_cols <- grep("_mt$", names(panel), value = TRUE)
    for (col_mt in dup_cols) {
      col_base <- sub("_mt$", "", col_mt)
      if (col_base %in% names(panel)) {
        panel[[col_base]] <- ifelse(is.na(panel[[col_base]]),
                                    panel[[col_mt]], panel[[col_base]])
      }
      panel[[col_mt]] <- NULL
    }
  }
}

if (!is.null(indicadores_ing)) {
  cols_ing <- setdiff(names(indicadores_ing), c("ANO4", "TRIMESTRE", names(panel)))
  if (length(cols_ing) > 0) {
    panel <- left_join(panel, indicadores_ing[, c("ANO4", "TRIMESTRE", cols_ing)],
                       by = c("ANO4", "TRIMESTRE"))
  }
}

# ── Agregar metadatos de período ───────────────────────────────────────────────
panel <- panel %>%
  mutate(
    periodo              = sprintf("%d-T%d", ANO4, TRIMESTRE),
    quiebre_metodologico = (ANO4 < 2016) | (ANO4 == 2016 & TRIMESTRE == 1)
  ) %>%
  arrange(ANO4, TRIMESTRE)

# Mover periodo y quiebre al frente para legibilidad
panel <- panel %>%
  select(ANO4, TRIMESTRE, periodo, quiebre_metodologico, everything())

# ── Consistencia mínima del panel ─────────────────────────────────────────────
n_periodos <- nrow(panel)
n_con_pobreza <- if ("tasa_pobreza" %in% names(panel))
  sum(!is.na(panel$tasa_pobreza)) else 0L
n_con_mercado <- if ("tasa_actividad" %in% names(panel))
  sum(!is.na(panel$tasa_actividad)) else 0L
n_con_ingresos <- if ("ingreso_medio_ocup_ppal" %in% names(panel))
  sum(!is.na(panel$ingreso_medio_ocup_ppal)) else 0L

message(sprintf(
  "Panel histórico: %d períodos | %d con pobreza | %d con mercado trabajo | %d con ingresos",
  n_periodos, n_con_pobreza, n_con_mercado, n_con_ingresos
))

# ── Guardar ───────────────────────────────────────────────────────────────────
saveRDS(panel, file.path(DIR_PANEL, "panel_historico.rds"))
write.csv(panel, file.path(DIR_PANEL, "panel_historico.csv"), row.names = FALSE, na = "")

message(sprintf("Panel guardado: %s", file.path(DIR_PANEL, "panel_historico.rds")))
message(sprintf("Panel CSV guardado: %s", file.path(DIR_PANEL, "panel_historico.csv")))
