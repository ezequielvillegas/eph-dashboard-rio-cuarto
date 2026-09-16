# =============================================================================
# 03_merge.R
# Para cada trimestre nuevo en data/raw/ (que todavia no tenga su
# contraparte en data/interim/), filtra aglomerado 33 (Rio Cuarto) y
# mergea individual + hogar por CODUSU + NRO_HOGAR.
#
# Convenciones del proyecto:
#   - AGLOMERADO == 33 identifica Rio Cuarto
#   - la clave de merge es CODUSU + NRO_HOGAR
#   - el merge se valida contra duplicados de esa clave antes de guardar
#   - el resultado se guarda a disco (saveRDS) para que 04_pobreza.R lo
#     pueda leer de forma independiente, sin depender de que este script
#     haya corrido en la misma sesion de R
# =============================================================================

library(dplyr)

DIR_RAW <- "data/raw"
DIR_INTERIM <- "data/interim"
dir.create(DIR_INTERIM, showWarnings = FALSE, recursive = TRUE)

# --- Identifica que trimestres ya estan descargados como par completo -------
archivos_individual <- list.files(DIR_RAW, pattern = "^individual_\\d{4}_T\\d\\.rds$")
periodos_disponibles <- gsub("^individual_|\\.rds$", "", archivos_individual)

for (periodo in periodos_disponibles) {

  archivo_salida <- file.path(DIR_INTERIM, sprintf("merged_%s.rds", periodo))
  if (file.exists(archivo_salida)) next  # ya procesado en una corrida anterior

  archivo_individual <- file.path(DIR_RAW, sprintf("individual_%s.rds", periodo))
  archivo_hogar <- file.path(DIR_RAW, sprintf("hogar_%s.rds", periodo))

  if (!file.exists(archivo_hogar)) {
    message(sprintf("  [aviso] falta la base hogar de %s, se salta este trimestre.", periodo))
    next
  }

  message(sprintf("Procesando %s...", periodo))

  individual <- readRDS(archivo_individual)
  hogar <- readRDS(archivo_hogar)

  # --- Filtro geografico: solo Rio Cuarto -----------------------------------
  individual_rc <- individual %>% filter(AGLOMERADO == 33)
  hogar_rc <- hogar %>% filter(AGLOMERADO == 33)

  if (nrow(individual_rc) == 0) {
    message(sprintf("  [aviso] %s no tiene casos de aglomerado 33, se salta.", periodo))
    next
  }

  # --- Limpieza de claves de merge -------------------------------------------
  individual_rc <- individual_rc %>%
    mutate(CODUSU = as.character(CODUSU), NRO_HOGAR = as.character(NRO_HOGAR))
  hogar_rc <- hogar_rc %>%
    mutate(CODUSU = as.character(CODUSU), NRO_HOGAR = as.character(NRO_HOGAR))

  # --- Validacion de duplicados en la base hogar antes de mergear ------------
  duplicados_hogar <- hogar_rc %>%
    count(CODUSU, NRO_HOGAR) %>%
    filter(n > 1)

  if (nrow(duplicados_hogar) > 0) {
    warning(sprintf(
      "%s: %d combinaciones CODUSU+NRO_HOGAR duplicadas en la base hogar. Revisar antes de confiar en el merge.",
      periodo, nrow(duplicados_hogar)
    ))
  }

  # --- Merge: cada persona (individual) con los datos de su hogar -----------
  # Se evitan columnas repetidas (ANO4, TRIMESTRE, AGLOMERADO, etc. estan en
  # ambas bases): se descartan de `hogar_rc` antes del join.
  columnas_compartidas <- intersect(names(individual_rc), names(hogar_rc))
  columnas_compartidas <- setdiff(columnas_compartidas, c("CODUSU", "NRO_HOGAR"))

  hogar_para_merge <- hogar_rc %>% select(-any_of(columnas_compartidas))

  base_mergeada <- individual_rc %>%
    left_join(hogar_para_merge, by = c("CODUSU", "NRO_HOGAR"))

  if (nrow(base_mergeada) != nrow(individual_rc)) {
    warning(sprintf(
      "%s: el merge cambio la cantidad de filas (de %d a %d). Puede indicar duplicados en hogar.",
      periodo, nrow(individual_rc), nrow(base_mergeada)
    ))
  }

  saveRDS(base_mergeada, archivo_salida)
  message(sprintf("  guardado: %s (%d personas)", archivo_salida, nrow(base_mergeada)))
}
