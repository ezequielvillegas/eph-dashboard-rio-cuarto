# =============================================================================
# 00b_control_metodologico.R
# Control de cambios metodologicos entre trimestres consecutivos de la EPH.
#
# Enfoque: en vez de intentar leer los PDF de "diseno de registro" de INDEC
# (el sitio es dinamico y el formato de esos documentos cambia; scrapearlos
# de forma automatica es fragil), se compara directamente la ESTRUCTURA de
# los microdatos ya descargados:
#   1) diferencias en el conjunto de variables (columnas) entre un trimestre
#      y el anterior
#   2) diferencias en los valores que toman variables categoricas clave
#      (ESTADO, CAT_OCUP, NIVEL_ED, CH04) - una aparicion/desaparicion de
#      categorias suele delatar un cambio de cuestionario o de codificacion
#
# Cualquier diferencia se registra en metodologia/changelog.csv con fecha,
# de forma que el panel historico y el dashboard puedan mostrar de donde
# viene cada quiebre, y quien use el tablero sepa que dos trimestres no son
# directamente comparables sin revisar el detalle.
#
# Esto NO reemplaza la lectura manual de los informes tecnicos de INDEC
# ante cambios grandes (como el rediseno muestral del 2do trimestre 2016):
# esos quiebres estructurales conviene dejarlos harcodeados y documentados
# (ver tabla RUPTURAS_CONOCIDAS mas abajo), porque no son detectables solo
# mirando nombres de columnas.
# =============================================================================

library(dplyr)

DIR_RAW <- "data/raw"
ARCHIVO_CHANGELOG <- "metodologia/changelog.csv"
dir.create("metodologia", showWarnings = FALSE)

# --- Rupturas metodologicas conocidas (mantener a mano, no autodetectables) --
RUPTURAS_CONOCIDAS <- data.frame(
  periodo = c("2016-T2"),
  descripcion = c(paste(
    "Rediseno muestral de la EPH: nuevo marco muestral posterior al Censo",
    "2010, nuevos factores de expansion (PONDERA/PONDIH). Series previas",
    "a este trimestre no son directamente comparables sin ajuste."
  ))
)

VARIABLES_CLAVE <- c("ESTADO", "CAT_OCUP", "NIVEL_ED", "CH04")

listar_bases <- function(tipo) {
  archivos <- list.files(DIR_RAW, pattern = paste0("^", tipo, "_.*\\.rds$"))
  # Extrae anio y trimestre del nombre de archivo para poder ordenar
  info <- data.frame(archivo = archivos, stringsAsFactors = FALSE) %>%
    mutate(
      anio = as.integer(sub(paste0(tipo, "_(\\d{4})_T(\\d).rds"), "\\1", archivo)),
      trimestre = as.integer(sub(paste0(tipo, "_(\\d{4})_T(\\d).rds"), "\\2", archivo))
    ) %>%
    arrange(anio, trimestre)
  info
}

comparar_consecutivos <- function(tipo) {
  info <- listar_bases(tipo)
  if (nrow(info) < 2) return(invisible(NULL))

  registros <- list()

  for (i in 2:nrow(info)) {
    actual <- readRDS(file.path(DIR_RAW, info$archivo[i]))
    previo  <- readRDS(file.path(DIR_RAW, info$archivo[i - 1]))

    periodo_actual <- sprintf("%d-T%d", info$anio[i], info$trimestre[i])
    periodo_previo <- sprintf("%d-T%d", info$anio[i - 1], info$trimestre[i - 1])

    # 1) diferencias de columnas
    cols_nuevas <- setdiff(names(actual), names(previo))
    cols_perdidas <- setdiff(names(previo), names(actual))

    if (length(cols_nuevas) > 0 || length(cols_perdidas) > 0) {
      registros[[length(registros) + 1]] <- data.frame(
        tipo_base = tipo, periodo = periodo_actual, comparado_contra = periodo_previo,
        tipo_cambio = "columnas",
        detalle = paste(
          if (length(cols_nuevas) > 0) paste("nuevas:", paste(cols_nuevas, collapse = ", ")) else NULL,
          if (length(cols_perdidas) > 0) paste("perdidas:", paste(cols_perdidas, collapse = ", ")) else NULL,
          sep = " | "
        ),
        fecha_deteccion = as.character(Sys.time())
      )
    }

    # 2) diferencias en categorias de variables clave
    for (v in VARIABLES_CLAVE) {
      if (v %in% names(actual) && v %in% names(previo)) {
        val_actual <- sort(unique(na.omit(actual[[v]])))
        val_previo <- sort(unique(na.omit(previo[[v]])))
        if (!identical(val_actual, val_previo)) {
          registros[[length(registros) + 1]] <- data.frame(
            tipo_base = tipo, periodo = periodo_actual, comparado_contra = periodo_previo,
            tipo_cambio = paste0("categorias_", v),
            detalle = sprintf(
              "antes: {%s} | ahora: {%s}",
              paste(val_previo, collapse = ","), paste(val_actual, collapse = ",")
            ),
            fecha_deteccion = as.character(Sys.time())
          )
        }
      }
    }
  }

  if (length(registros) > 0) dplyr::bind_rows(registros) else NULL
}

nuevos_hallazgos <- dplyr::bind_rows(
  comparar_consecutivos("individual"),
  comparar_consecutivos("hogar")
)

if (!is.null(nuevos_hallazgos) && nrow(nuevos_hallazgos) > 0) {
  if (file.exists(ARCHIVO_CHANGELOG)) {
    previo <- read.csv(ARCHIVO_CHANGELOG, stringsAsFactors = FALSE)
    # evita duplicar filas ya registradas en corridas anteriores
    combinado <- dplyr::bind_rows(previo, nuevos_hallazgos) %>% distinct()
  } else {
    combinado <- nuevos_hallazgos
  }
  write.csv(combinado, ARCHIVO_CHANGELOG, row.names = FALSE)
  message(sprintf(
    "Control metodologico: %d cambio(s) nuevo(s) detectado(s). Ver %s",
    nrow(nuevos_hallazgos), ARCHIVO_CHANGELOG
  ))
} else {
  message("Control metodologico: sin cambios estructurales respecto del trimestre anterior.")
}

# Las rupturas conocidas siempre se guardan (no dependen de la deteccion automatica)
write.csv(RUPTURAS_CONOCIDAS, "metodologia/rupturas_conocidas.csv", row.names = FALSE)
