# =============================================================================
# 05_indicadores.R
# Calcula, para cada trimestre con pobreza ya estimada, los indicadores
# que alimentan el panel historico y el dashboard.
#
# Fuente de la variable `situacion` (pobre/indigente/no_pobre): la agrega
# eph::calculate_poverty() en 04_pobreza.R. Puede ser NA cuando PONDIH == 0
# (hogar sin ingreso declarado) - se excluye con na.rm en todos los calculos.
#
# Convenciones del proyecto:
#   - PONDI  se usa para indicadores a nivel persona (desocupacion)
#   - PONDIH se usa para indicadores a nivel hogar / pobreza
#   - na.rm = TRUE en todos los calculos
# =============================================================================

library(dplyr)

DIR_INTERIM <- "data/interim"
DIR_PROCESSED <- "data/processed"
dir.create(DIR_PROCESSED, showWarnings = FALSE, recursive = TRUE)

archivos_pobreza <- list.files(DIR_INTERIM, pattern = "^pobreza_\\d{4}_T\\d\\.rds$")

calcular_tasa_ponderada <- function(condicion, peso) {
  # % ponderado de casos que cumplen `condicion`, ignorando NA en cualquiera
  # de los dos vectores (equivalente a na.rm = TRUE aplicado a la razon).
  valido <- !is.na(condicion) & !is.na(peso)
  if (sum(peso[valido]) == 0) return(NA_real_)
  100 * sum(peso[valido & condicion], na.rm = TRUE) / sum(peso[valido], na.rm = TRUE)
}

for (archivo in archivos_pobreza) {
  periodo <- gsub("^pobreza_|\\.rds$", "", archivo)
  # periodo tiene forma "2023_T4" -> separar en ANO4 / TRIMESTRE
  partes <- strsplit(periodo, "_T")[[1]]
  anio <- as.integer(partes[1])
  trimestre <- as.integer(partes[2])

  archivo_salida <- file.path(DIR_PROCESSED, sprintf("indicadores_%d_T%d.rds", anio, trimestre))
  if (file.exists(archivo_salida)) next  # ya calculado

  base <- readRDS(file.path(DIR_INTERIM, archivo))

  # --- Pobreza e indigencia (nivel persona, ponderador PONDIH) ---------------
  es_pobre_o_indigente <- base$situacion %in% c("pobre", "indigente")
  es_indigente <- base$situacion == "indigente"

  tasa_pobreza <- calcular_tasa_ponderada(es_pobre_o_indigente, base$PONDIH)
  tasa_indigencia <- calcular_tasa_ponderada(es_indigente, base$PONDIH)

  # --- Desocupacion (nivel persona, ponderador PONDI) ------------------------
  # ESTADO: 1 = ocupado, 2 = desocupado (PEA = ocupados + desocupados)
  pea <- base$ESTADO %in% c(1, 2)
  desocupado <- base$ESTADO == 2
  tasa_desocupacion <- calcular_tasa_ponderada(
    condicion = ifelse(pea, desocupado, NA),
    peso = base$PONDI
  )

  # --- Menores de 18 en hogares pobres (nivel persona, ponderador PONDIH) ----
  es_menor <- base$CH06 < 18 & base$CH06 >= 0  # CH06 puede tener codigos negativos para "no aplica"
  pct_menores_en_hogares_pobres <- calcular_tasa_ponderada(
    condicion = ifelse(es_menor, es_pobre_o_indigente, NA),
    peso = base$PONDIH
  )

  # --- Hogares con menores que son pobres (nivel hogar, ponderador PONDIH) --
  hogares <- base %>%
    group_by(CODUSU, NRO_HOGAR) %>%
    summarise(
      tiene_menores = any(CH06 < 18 & CH06 >= 0, na.rm = TRUE),
      situacion_hogar = dplyr::first(situacion),
      PONDIH = dplyr::first(PONDIH),
      .groups = "drop"
    )

  pobre_hogar <- hogares$situacion_hogar %in% c("pobre", "indigente")
  pct_hogares_con_menores_pobres <- calcular_tasa_ponderada(
    condicion = ifelse(hogares$tiene_menores, pobre_hogar, NA),
    peso = hogares$PONDIH
  )

  indicadores <- data.frame(
    ANO4 = anio,
    TRIMESTRE = trimestre,
    tasa_pobreza = round(tasa_pobreza, 1),
    tasa_indigencia = round(tasa_indigencia, 1),
    tasa_desocupacion = round(tasa_desocupacion, 1),
    pct_menores_en_hogares_pobres = round(pct_menores_en_hogares_pobres, 1),
    pct_hogares_con_menores_pobres = round(pct_hogares_con_menores_pobres, 1)
  )

  saveRDS(indicadores, archivo_salida)
  message(sprintf("Indicadores calculados para %d-T%d -> %s", anio, trimestre, archivo_salida))
}
