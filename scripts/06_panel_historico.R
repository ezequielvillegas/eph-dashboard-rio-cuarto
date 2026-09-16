# =============================================================================
# 06_panel_historico.R
# Acumula los indicadores de cada trimestre en un panel historico unico.
#
# SUPUESTO A VALIDAR: este script asume que tu 05_indicadores.R (existente)
# guarda, para el trimestre que acaba de procesar, un archivo
#   data/processed/indicadores_<ANIO>_T<TRIM>.rds
# con (al menos) las columnas ANO4, TRIMESTRE y los indicadores calculados
# para el aglomerado 33. Si tu script usa otro nombre/ruta, ajusta
# `DIR_PROCESADOS` y el patron de `list.files` de abajo.
# =============================================================================

library(dplyr)

DIR_PROCESADOS <- "data/processed"
ARCHIVO_PANEL_RDS <- "data/panel/panel_historico.rds"
ARCHIVO_PANEL_CSV <- "data/panel/panel_historico.csv"

dir.create("data/panel", showWarnings = FALSE, recursive = TRUE)

archivos <- list.files(
  DIR_PROCESADOS, pattern = "^indicadores_\\d{4}_T\\d\\.rds$", full.names = TRUE
)

if (length(archivos) == 0) {
  stop("No se encontraron archivos de indicadores en data/processed/. ",
       "Verifica que 05_indicadores.R ya haya corrido para al menos un trimestre.")
}

panel <- lapply(archivos, readRDS) %>%
  bind_rows() %>%
  distinct(ANO4, TRIMESTRE, .keep_all = TRUE) %>%
  arrange(ANO4, TRIMESTRE) %>%
  mutate(
    # TRUE para todo periodo anterior al rediseno muestral de 2016-T2:
    # separa visualmente las series no comparables en el dashboard
    quiebre_metodologico = (ANO4 < 2016) | (ANO4 == 2016 & TRIMESTRE < 2),
    periodo = sprintf("%d-T%d", ANO4, TRIMESTRE)
  )

saveRDS(panel, ARCHIVO_PANEL_RDS)
write.csv(panel, ARCHIVO_PANEL_CSV, row.names = FALSE)

message(sprintf(
  "Panel historico actualizado: %d periodos (%s a %s)",
  nrow(panel), min(panel$periodo), max(panel$periodo)
))
