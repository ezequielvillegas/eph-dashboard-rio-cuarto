# =============================================================================
# 05b_mercado_trabajo.R
# Calcula indicadores del mercado de trabajo para cada trimestre disponible
# en data/interim/ (archivos merged o con pobreza calculada).
#
# Indicadores producidos:
#   — tasa_actividad           : Activos / Pob. referencia (%)
#   — tasa_empleo              : Ocupados / Pob. referencia (%)
#   — tasa_desocupacion        : Desocupados / PEA (%)
#   — tasa_subocupacion        : Subocupados / PEA (%)
#   — tasa_informalidad        : Asalariados sin desc. jubilatorio / Asalariados (%)
#   — n_activos, n_ocupados, n_desocupados, n_inactivos, n_pob_referencia
#     (totales expandidos con PONDI)
#   — n_casos_muestra          : filas de la base sin ponderar (control calidad)
#   — poblacion_expandida      : sum(PONDI[ESTADO %in% 1:3])
#
# DEFINICIONES (metodología INDEC):
#   Población de referencia: ESTADO %in% 1:3 (activos + inactivos >= 10 años)
#   PEA                    : ESTADO %in% 1:2 (ocupados + desocupados)
#   Subocupado horario     : ESTADO==1 AND horas_total < 35 AND PP03G==1
#   Informal (asalariado)  : CAT_OCUP==3 AND PP07H==2
#
# Ponderador: PONDI (individual).
# na.rm = TRUE en todos los cálculos.
# Valores especiales (9, 99, 999, -9) excluidos en las variables que los usan.
# =============================================================================

library(dplyr)

DIR_INTERIM   <- "data/interim"
DIR_PROCESSED <- "data/processed"
dir.create(DIR_PROCESSED, showWarnings = FALSE, recursive = TRUE)

# ── Función central de tasa ponderada ────────────────────────────────────────
tasa_pond <- function(condicion, peso, na_cond = TRUE) {
  # Calcula % ponderado de casos que cumplen `condicion`.
  # Si na_cond = TRUE, los NA en condicion se excluyen del denominador.
  if (na_cond) {
    valido <- !is.na(condicion) & !is.na(peso) & peso > 0
  } else {
    valido <- !is.na(peso) & peso > 0
  }
  denom <- sum(peso[valido], na.rm = TRUE)
  if (denom == 0) return(NA_real_)
  100 * sum(peso[valido & !is.na(condicion) & condicion], na.rm = TRUE) / denom
}

total_pond <- function(condicion, peso) {
  valido <- !is.na(condicion) & !is.na(peso) & peso > 0 & condicion
  sum(peso[valido], na.rm = TRUE)
}

# ── Detectar archivos fuente disponibles ─────────────────────────────────────
# Prefiere archivos con pobreza calculada (pobreza_*) porque ya tienen PONDIH
# mergeado. Si no existen, usa merged_*.
archivos_pobreza <- list.files(DIR_INTERIM, pattern = "^pobreza_\\d{4}_T\\d\\.rds$")
archivos_merged  <- list.files(DIR_INTERIM, pattern = "^merged_\\d{4}_T\\d\\.rds$")

# Construye un mapa periodo -> archivo
fuentes <- data.frame(
  periodo  = gsub("^pobreza_|\\.rds$", "", archivos_pobreza),
  archivo  = file.path(DIR_INTERIM, archivos_pobreza),
  stringsAsFactors = FALSE
)
# Agrega merged que no tengan pobreza
periodos_merged <- gsub("^merged_|\\.rds$", "", archivos_merged)
faltantes <- periodos_merged[!periodos_merged %in% fuentes$periodo]
if (length(faltantes) > 0) {
  fuentes <- rbind(fuentes, data.frame(
    periodo = faltantes,
    archivo = file.path(DIR_INTERIM, paste0("merged_", faltantes, ".rds")),
    stringsAsFactors = FALSE
  ))
}

# ── Procesa cada período ──────────────────────────────────────────────────────
for (i in seq_len(nrow(fuentes))) {
  periodo <- fuentes$periodo[i]
  partes  <- strsplit(periodo, "_T")[[1]]
  anio    <- as.integer(partes[1])
  trimestre <- as.integer(partes[2])

  archivo_salida <- file.path(DIR_PROCESSED,
                              sprintf("mercado_trabajo_%d_T%d.rds", anio, trimestre))
  if (file.exists(archivo_salida)) next

  base <- tryCatch(readRDS(fuentes$archivo[i]),
                   error = function(e) { warning(sprintf("%s: no se pudo leer (%s)", periodo, e$message)); NULL })
  if (is.null(base)) next

  # -- Validar variables mínimas requeridas ------------------------------------
  vars_req <- c("ESTADO", "PONDI")
  faltantes_req <- setdiff(vars_req, names(base))
  if (length(faltantes_req) > 0) {
    warning(sprintf("%s: faltan variables requeridas (%s). Se salta.",
                    periodo, paste(faltantes_req, collapse = ", ")))
    next
  }

  # -- Limpieza de ESTADO: solo 1, 2, 3, 4 son válidos -----------------------
  base <- base %>%
    mutate(ESTADO = ifelse(ESTADO %in% 1:4, ESTADO, NA_integer_))

  # -- Universo ----------------------------------------------------------------
  pob_ref   <- base$ESTADO %in% 1:3   # población de referencia (>=10 años)
  pea       <- base$ESTADO %in% 1:2   # activos
  ocupados  <- base$ESTADO == 1
  desocupados <- base$ESTADO == 2
  inactivos <- base$ESTADO == 3

  # -- Tasas principales -------------------------------------------------------
  tasa_actividad   <- tasa_pond(pea[pob_ref],      base$PONDI[pob_ref])
  tasa_empleo      <- tasa_pond(ocupados[pob_ref],  base$PONDI[pob_ref])
  tasa_desocupacion <- tasa_pond(desocupados[pea],  base$PONDI[pea])

  # -- Subocupación horaria (ESTADO==1 AND horas<35 AND quiere más horas) ------
  # Variables de horas: intenta PP3E_TOT; si no existe, construye desde PP3E_1+PP3E_2
  if ("PP3E_TOT" %in% names(base)) {
    horas <- suppressWarnings(as.numeric(base$PP3E_TOT))
  } else if (all(c("PP3E_1", "PP3E_2") %in% names(base))) {
    horas <- suppressWarnings(
      as.numeric(base$PP3E_1) + as.numeric(base$PP3E_2)
    )
  } else {
    horas <- NA_real_
  }

  # PP03G: 1=quiere más horas, 2=no; 9=NS/NR → excluir
  pp03g <- if ("PP03G" %in% names(base)) {
    ifelse(base$PP03G %in% c(1, 2), base$PP03G, NA_integer_)
  } else NA_integer_

  subocupado <- ocupados &
    !is.na(horas) & horas < 35 &
    !is.na(pp03g) & pp03g == 1

  tasa_subocupacion <- if (any(!is.na(horas))) {
    tasa_pond(subocupado[pea], base$PONDI[pea])
  } else {
    warning(sprintf("%s: no se encontró variable de horas para subocupación.", periodo))
    NA_real_
  }

  # -- Informalidad (asalariados sin descuento jubilatorio) --------------------
  # CAT_OCUP: 1=Patrón, 2=Cta propia, 3=Asalariado, 4=Trab.familiar s/rem
  # PP07H:    1=Sí le descuentan jubilación, 2=No; 9=NS/NR → excluir
  tasa_informalidad <- NA_real_
  if (all(c("CAT_OCUP", "PP07H") %in% names(base))) {
    asalariados <- ocupados & !is.na(base$CAT_OCUP) & base$CAT_OCUP == 3
    pp07h_limpio <- ifelse(base$PP07H %in% 1:2, base$PP07H, NA_integer_)
    informales   <- asalariados & !is.na(pp07h_limpio) & pp07h_limpio == 2
    tasa_informalidad <- tasa_pond(informales[asalariados], base$PONDI[asalariados])
  }

  # -- Totales expandidos ------------------------------------------------------
  n_activos       <- round(total_pond(pea,        base$PONDI))
  n_ocupados      <- round(total_pond(ocupados,   base$PONDI))
  n_desocupados   <- round(total_pond(desocupados, base$PONDI))
  n_inactivos     <- round(total_pond(inactivos,  base$PONDI))
  n_pob_ref       <- round(sum(base$PONDI[pob_ref & !is.na(base$PONDI)], na.rm = TRUE))

  # -- Control de calidad -------------------------------------------------------
  n_casos_muestra    <- sum(pob_ref, na.rm = TRUE)
  poblacion_expandida <- n_pob_ref

  # -- Consistencia interna -----------------------------------------------------
  if (!is.na(tasa_actividad) && (tasa_actividad < 0 || tasa_actividad > 100))
    warning(sprintf("%s: tasa_actividad fuera de rango (%.1f)", periodo, tasa_actividad))
  if (!is.na(tasa_desocupacion) && (tasa_desocupacion < 0 || tasa_desocupacion > 100))
    warning(sprintf("%s: tasa_desocupacion fuera de rango (%.1f)", periodo, tasa_desocupacion))
  if (!is.na(n_desocupados) && !is.na(n_activos) && n_desocupados > n_activos)
    warning(sprintf("%s: n_desocupados (%d) > n_activos (%d) — revisar.", periodo, n_desocupados, n_activos))

  indicadores <- data.frame(
    ANO4              = anio,
    TRIMESTRE         = trimestre,
    tasa_actividad    = round(tasa_actividad, 1),
    tasa_empleo       = round(tasa_empleo, 1),
    tasa_desocupacion = round(tasa_desocupacion, 1),
    tasa_subocupacion = round(tasa_subocupacion, 1),
    tasa_informalidad = round(tasa_informalidad, 1),
    n_activos         = n_activos,
    n_ocupados        = n_ocupados,
    n_desocupados     = n_desocupados,
    n_inactivos       = n_inactivos,
    n_pob_referencia  = n_pob_ref,
    n_casos_muestra   = n_casos_muestra,
    poblacion_expandida = poblacion_expandida
  )

  saveRDS(indicadores, archivo_salida)
  message(sprintf("Mercado trabajo calculado para %s -> %s", periodo, archivo_salida))
}
