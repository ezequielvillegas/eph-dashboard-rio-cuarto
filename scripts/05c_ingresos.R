# =============================================================================
# 05c_ingresos.R
# Calcula indicadores de ingresos para cada trimestre disponible
# en data/interim/ (archivos merged o con pobreza calculada).
#
# Indicadores producidos:
#   — ingreso_medio_ocup_ppal   : Promedio ponderado de P21 (ocupación principal)
#   — ingreso_mediano_ocup_ppal : Mediana ponderada de P21
#   — ingreso_medio_individual  : Promedio ponderado de TOT_P12 (todos los ingresos)
#   — ingreso_mediano_individual: Mediana ponderada de TOT_P12
#   — ipcf_medio                : Promedio ponderado de IPCF (ingreso per cápita familiar)
#   — ipcf_mediano              : Mediana ponderada de IPCF
#   — itf_medio                 : Promedio ponderado de ITF (ingreso total familiar)
#   — itf_mediano               : Mediana ponderada de ITF
#   — pct_sin_ingreso           : % de ocupados sin ningún ingreso declarado
#   — n_casos_ingreso_ocup      : casos con P21 > 0 (control de calidad)
#   — n_casos_ingreso_ind       : casos con TOT_P12 > 0
#
# DEFINICIONES (metodología INDEC):
#   P21       : ingreso de la ocupación principal (PONDIIO como ponderador)
#   TOT_P12   : suma de todos los ingresos individuales (PONDII como ponderador)
#   ITF       : ingreso total del hogar (PONDIH como ponderador, nivel hogar)
#   IPCF      : ingreso per cápita familiar = ITF / miembros del hogar
#               (se usa PONDIH; disponible en base individual como variable ya calculada)
#
# Valores especiales: -9, 0 en ingresos indican "sin ingreso declarado".
#   Para medias/medianas: se excluyen los menores o iguales a 0 (solo ingresos positivos).
#   Para pct_sin_ingreso: se computan como sin ingreso.
#
# Nota metodológica: PONDIIO solo tiene valor > 0 para el asalariado/cta propia
#   con ingreso declarado; PONDII cubre todos los perceptores individuales.
#   Para ITF/IPCF, el análisis es a nivel hogar (un registro por hogar).
#   Como la base interim es individual, se toma CH03==1 (jefe/a) o se deduplica
#   por CODUSU+NRO_HOGAR para evitar duplicar hogares.
# =============================================================================

library(dplyr)

DIR_INTERIM   <- "data/interim"
DIR_PROCESSED <- "data/processed"
dir.create(DIR_PROCESSED, showWarnings = FALSE, recursive = TRUE)

# ── Mediana ponderada (algoritmo interpolación lineal) ────────────────────────
mediana_ponderada <- function(x, w) {
  # x: valores numéricos, w: pesos positivos. NA en cualquiera → excluir.
  valido <- !is.na(x) & !is.na(w) & w > 0 & x > 0
  x <- x[valido]; w <- w[valido]
  if (length(x) == 0) return(NA_real_)
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cum_w <- cumsum(w)
  total_w <- sum(w)
  mitad <- total_w / 2
  idx <- which(cum_w >= mitad)[1]
  if (idx == 1) return(x[1])
  # Interpolación lineal entre el punto anterior y el actual
  x_prev <- x[idx - 1]; x_cur <- x[idx]
  w_prev <- cum_w[idx - 1]; w_cur <- cum_w[idx]
  if (w_cur == w_prev) return(x_cur)
  x_prev + (mitad - w_prev) / (w_cur - w_prev) * (x_cur - x_prev)
}

# ── Media ponderada simple ────────────────────────────────────────────────────
media_ponderada <- function(x, w, solo_positivos = TRUE) {
  if (solo_positivos) {
    valido <- !is.na(x) & !is.na(w) & w > 0 & x > 0
  } else {
    valido <- !is.na(x) & !is.na(w) & w > 0
  }
  denom <- sum(w[valido], na.rm = TRUE)
  if (denom == 0) return(NA_real_)
  sum(x[valido] * w[valido], na.rm = TRUE) / denom
}

# ── Detectar archivos fuente disponibles ─────────────────────────────────────
archivos_pobreza <- list.files(DIR_INTERIM, pattern = "^pobreza_\\d{4}_T\\d\\.rds$")
archivos_merged  <- list.files(DIR_INTERIM, pattern = "^merged_\\d{4}_T\\d\\.rds$")

fuentes <- data.frame(
  periodo = gsub("^pobreza_|\\.rds$", "", archivos_pobreza),
  archivo = file.path(DIR_INTERIM, archivos_pobreza),
  stringsAsFactors = FALSE
)
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
  periodo  <- fuentes$periodo[i]
  partes   <- strsplit(periodo, "_T")[[1]]
  anio     <- as.integer(partes[1])
  trimestre <- as.integer(partes[2])

  archivo_salida <- file.path(DIR_PROCESSED,
                              sprintf("ingresos_%d_T%d.rds", anio, trimestre))
  if (file.exists(archivo_salida)) next

  base <- tryCatch(readRDS(fuentes$archivo[i]),
                   error = function(e) {
                     warning(sprintf("%s: no se pudo leer (%s)", periodo, e$message))
                     NULL
                   })
  if (is.null(base)) next

  # -- Variables de ingreso individual (P21, TOT_P12) -------------------------
  # Convertir a numérico; valores -9 o negativos → NA
  limpiar_ingreso <- function(v) {
    v <- suppressWarnings(as.numeric(v))
    v[!is.na(v) & v < 0] <- NA_real_
    v
  }

  # P21: ingreso ocupación principal
  ingreso_ocup_ppal <- ingreso_medio_ocup <- ingreso_mediano_ocup <- NA_real_
  pct_sin_ingreso_ocup <- NA_real_
  n_casos_ingreso_ocup <- NA_integer_

  if ("P21" %in% names(base)) {
    p21 <- limpiar_ingreso(base$P21)

    # Ponderador: PONDIIO si existe y tiene variación, si no PONDI
    if ("PONDIIO" %in% names(base)) {
      w_ocup <- suppressWarnings(as.numeric(base$PONDIIO))
      w_ocup[is.na(w_ocup) | w_ocup < 0] <- NA_real_
      # Si PONDIIO = 0 pero p21 > 0, usar PONDI como fallback
      if ("PONDI" %in% names(base)) {
        w_pondi <- suppressWarnings(as.numeric(base$PONDI))
        w_pondi[is.na(w_pondi) | w_pondi < 0] <- NA_real_
        w_ocup <- ifelse(!is.na(p21) & p21 > 0 & (is.na(w_ocup) | w_ocup == 0),
                         w_pondi, w_ocup)
      }
    } else if ("PONDI" %in% names(base)) {
      w_ocup <- suppressWarnings(as.numeric(base$PONDI))
      w_ocup[is.na(w_ocup) | w_ocup < 0] <- NA_real_
    } else {
      w_ocup <- rep(1, nrow(base))
    }

    # Solo ocupados (ESTADO == 1) para P21
    ocupados_mask <- !is.na(base$ESTADO) & base$ESTADO == 1
    p21_ocup <- ifelse(ocupados_mask, p21, NA_real_)
    w_ocup_ocu <- ifelse(ocupados_mask, w_ocup, NA_real_)

    ingreso_medio_ocup    <- media_ponderada(p21_ocup, w_ocup_ocu)
    ingreso_mediano_ocup  <- mediana_ponderada(p21_ocup, w_ocup_ocu)
    n_casos_ingreso_ocup  <- sum(!is.na(p21_ocup) & p21_ocup > 0, na.rm = TRUE)

    # % sin ingreso declarado entre ocupados
    total_ocup <- sum(ocupados_mask & !is.na(w_ocup_ocu) & w_ocup_ocu > 0, na.rm = TRUE)
    sin_ingreso_ocup <- sum(ocupados_mask & (is.na(p21) | p21 == 0) &
                              !is.na(w_ocup) & w_ocup > 0, na.rm = TRUE)
    denom_ocup <- sum(w_ocup_ocu[ocupados_mask & !is.na(w_ocup_ocu) & w_ocup_ocu > 0],
                      na.rm = TRUE)
    denom_sin  <- sum(w_ocup[ocupados_mask & (is.na(p21) | p21 == 0) &
                               !is.na(w_ocup) & w_ocup > 0], na.rm = TRUE)
    pct_sin_ingreso_ocup <- if (!is.na(denom_ocup) && denom_ocup > 0)
      round(100 * denom_sin / denom_ocup, 1) else NA_real_
  }

  # TOT_P12: total ingresos individuales
  ingreso_medio_ind <- ingreso_mediano_ind <- NA_real_
  n_casos_ingreso_ind <- NA_integer_

  if ("TOT_P12" %in% names(base)) {
    tot_p12 <- limpiar_ingreso(base$TOT_P12)

    if ("PONDII" %in% names(base)) {
      w_ind <- suppressWarnings(as.numeric(base$PONDII))
      w_ind[is.na(w_ind) | w_ind < 0] <- NA_real_
      # Fallback a PONDI si PONDII == 0
      if ("PONDI" %in% names(base)) {
        w_pondi <- suppressWarnings(as.numeric(base$PONDI))
        w_pondi[is.na(w_pondi) | w_pondi < 0] <- NA_real_
        w_ind <- ifelse(!is.na(tot_p12) & tot_p12 > 0 & (is.na(w_ind) | w_ind == 0),
                        w_pondi, w_ind)
      }
    } else if ("PONDI" %in% names(base)) {
      w_ind <- suppressWarnings(as.numeric(base$PONDI))
      w_ind[is.na(w_ind) | w_ind < 0] <- NA_real_
    } else {
      w_ind <- rep(1, nrow(base))
    }

    ingreso_medio_ind   <- media_ponderada(tot_p12, w_ind)
    ingreso_mediano_ind <- mediana_ponderada(tot_p12, w_ind)
    n_casos_ingreso_ind <- sum(!is.na(tot_p12) & tot_p12 > 0, na.rm = TRUE)
  }

  # -- Variables de ingreso de hogar (ITF, IPCF) -------------------------------
  # Para no duplicar, se toma un registro por hogar.
  # Prioridad: CH03==1 (jefe/a). Si no existe CH03, deduplica por CODUSU+NRO_HOGAR.
  itf_medio <- itf_mediano <- ipcf_medio <- ipcf_mediano <- NA_real_

  vars_hogar <- c("ITF", "IPCF")
  tiene_hogar <- any(vars_hogar %in% names(base))

  if (tiene_hogar && all(c("CODUSU", "NRO_HOGAR") %in% names(base))) {
    # Construir base hogar
    if ("CH03" %in% names(base)) {
      base_hogar <- base %>%
        filter(!is.na(CH03) & CH03 == 1)
    } else {
      # Toma el primer registro por hogar
      base_hogar <- base %>%
        group_by(CODUSU, NRO_HOGAR) %>%
        slice(1) %>%
        ungroup()
    }

    # Ponderador de hogar
    if ("PONDIH" %in% names(base_hogar)) {
      w_hogar <- suppressWarnings(as.numeric(base_hogar$PONDIH))
      w_hogar[is.na(w_hogar) | w_hogar < 0] <- NA_real_
    } else {
      w_hogar <- rep(1, nrow(base_hogar))
      warning(sprintf("%s: PONDIH no encontrado, usando peso 1 para indicadores de hogar.", periodo))
    }

    # ITF
    if ("ITF" %in% names(base_hogar)) {
      itf <- limpiar_ingreso(base_hogar$ITF)
      itf_medio   <- media_ponderada(itf, w_hogar)
      itf_mediano <- mediana_ponderada(itf, w_hogar)
    }

    # IPCF
    if ("IPCF" %in% names(base_hogar)) {
      ipcf <- limpiar_ingreso(base_hogar$IPCF)
      ipcf_medio   <- media_ponderada(ipcf, w_hogar)
      ipcf_mediano <- mediana_ponderada(ipcf, w_hogar)
    }
  } else if (tiene_hogar) {
    warning(sprintf("%s: CODUSU/NRO_HOGAR no encontrados; no se calculan indicadores de hogar.", periodo))
  }

  # -- Consistencia interna ----------------------------------------------------
  if (!is.na(ingreso_medio_ocup) && ingreso_medio_ocup <= 0)
    warning(sprintf("%s: ingreso_medio_ocup_ppal <= 0 (%.0f)", periodo, ingreso_medio_ocup))
  if (!is.na(ingreso_mediano_ocup) && ingreso_mediano_ocup > ingreso_medio_ocup * 3)
    warning(sprintf("%s: mediana muy superior a media en ocup_ppal — revisar outliers.", periodo))

  # -- Armado del data.frame de salida -----------------------------------------
  indicadores <- data.frame(
    ANO4                      = anio,
    TRIMESTRE                 = trimestre,
    ingreso_medio_ocup_ppal   = round(ingreso_medio_ocup,    0),
    ingreso_mediano_ocup_ppal = round(ingreso_mediano_ocup,  0),
    ingreso_medio_individual  = round(ingreso_medio_ind,     0),
    ingreso_mediano_individual = round(ingreso_mediano_ind,  0),
    ipcf_medio                = round(ipcf_medio,            0),
    ipcf_mediano              = round(ipcf_mediano,          0),
    itf_medio                 = round(itf_medio,             0),
    itf_mediano               = round(itf_mediano,           0),
    pct_sin_ingreso           = round(pct_sin_ingreso_ocup,  1),
    n_casos_ingreso_ocup      = n_casos_ingreso_ocup,
    n_casos_ingreso_ind       = n_casos_ingreso_ind
  )

  saveRDS(indicadores, archivo_salida)
  message(sprintf("Ingresos calculados para %s -> %s", periodo, archivo_salida))
}
