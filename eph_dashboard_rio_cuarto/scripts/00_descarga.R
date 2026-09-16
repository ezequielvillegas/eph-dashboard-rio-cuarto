# =============================================================================
# 00_descarga.R
# Descarga automatica de microdatos EPH (INDEC) via el paquete `eph`.
# Descarga bases hogar e individual de los ultimos N anios, todos los
# trimestres disponibles, y las guarda en data/raw/ si todavia no existen.
#
# Idempotente: si un trimestre ya fue descargado, no lo vuelve a bajar.
# Tolerante a trimestres que todavia no fueron publicados por INDEC
# (los salta con un aviso, no corta la ejecucion).
# =============================================================================

if (!requireNamespace("eph", quietly = TRUE)) {
  install.packages("eph", repos = "https://cloud.r-project.org")
}
library(eph)
library(dplyr)

# --- Configuracion ----------------------------------------------------------
ANIOS_HACIA_ATRAS <- 10
ANIO_ACTUAL <- as.integer(format(Sys.Date(), "%Y"))
ANIOS <- (ANIO_ACTUAL - ANIOS_HACIA_ATRAS):ANIO_ACTUAL
TRIMESTRES <- 1:4

DIR_RAW <- "data/raw"
dir.create(DIR_RAW, showWarnings = FALSE, recursive = TRUE)

log_descarga <- data.frame(
  anio = integer(), trimestre = integer(), tipo = character(),
  estado = character(), fecha_corrida = character()
)

descargar_base <- function(anio, trimestre, tipo) {
  nombre_archivo <- file.path(
    DIR_RAW, sprintf("%s_%d_T%d.rds", tipo, anio, trimestre)
  )

  if (file.exists(nombre_archivo)) {
    return(list(estado = "ya_existia"))
  }

  resultado <- tryCatch({
    base <- get_microdata(year = anio, period = trimestre, type = tipo)
    saveRDS(base, nombre_archivo)
    list(estado = "descargada")
  }, error = function(e) {
    # Trimestre todavia no publicado, o problema puntual de red/servidor.
    # No se corta el pipeline: se reintenta en la proxima corrida.
    message(sprintf(
      "  [aviso] %s %d T%d no disponible todavia (%s)",
      tipo, anio, trimestre, conditionMessage(e)
    ))
    list(estado = "no_disponible")
  })

  resultado
}

for (anio in ANIOS) {
  for (trimestre in TRIMESTRES) {
    for (tipo in c("individual", "hogar")) {
      message(sprintf("Chequeando %s %d T%d...", tipo, anio, trimestre))
      r <- descargar_base(anio, trimestre, tipo)
      log_descarga <- rbind(log_descarga, data.frame(
        anio = anio, trimestre = trimestre, tipo = tipo,
        estado = r$estado, fecha_corrida = as.character(Sys.time())
      ))
    }
  }
}

write.csv(
  log_descarga,
  file.path(DIR_RAW, "log_descargas.csv"),
  row.names = FALSE
)

n_nuevas <- sum(log_descarga$estado == "descargada")
message(sprintf("Descarga finalizada. Bases nuevas esta corrida: %d", n_nuevas))

# Guarda un flag simple para que el orquestador sepa si hay trabajo nuevo
writeLines(as.character(n_nuevas > 0), "data/raw/.hay_datos_nuevos")
