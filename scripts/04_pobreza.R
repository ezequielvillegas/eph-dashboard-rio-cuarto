# =============================================================================
# 04_pobreza.R
# Calcula pobreza e indigencia siguiendo la metodologia de linea de INDEC
# (2016), usando las funciones ya validadas del paquete `eph`:
#   - get_poverty_lines(regional = TRUE): descarga las canastas basicas
#     alimentaria (CBA) y total (CBT) regionales oficiales, directo de
#     la fuente publicada por INDEC en los comunicados de pobreza.
#     Esto reemplaza la necesidad de tipear valores de CBA/CBT a mano.
#   - calculate_poverty(): aplica la formula oficial (ingreso total
#     familiar vs. canasta ajustada por adulto equivalente del hogar) y
#     agrega la clasificacion pobre / indigente / no_pobre por persona.
#
# IMPORTANTE (ver conventions.md del proyecto): aun descargando la canasta
# de forma automatica, el resultado de calculate_poverty() es una
# APROXIMACION a la metodologia oficial (el propio paquete lo aclara: "no
# es un producto oficial de INDEC"). Antes de publicar una tasa de pobreza
# en un informe institucional, conviene contrastar el numero contra el
# ultimo comunicado de prensa de INDEC para el mismo periodo/region.
# =============================================================================

if (!requireNamespace("eph", quietly = TRUE)) {
  install.packages("eph", repos = "https://cloud.r-project.org")
}
library(eph)
library(dplyr)

DIR_INTERIM <- "data/interim"

# --- Canasta regional: se descarga fresca en cada corrida -------------------
canastas_regionales <- tryCatch(
  get_poverty_lines(regional = TRUE),
  error = function(e) {
    stop("No se pudo descargar la canasta regional (get_poverty_lines). ",
         "Revisar conexion / disponibilidad del servicio. Detalle: ", conditionMessage(e))
  }
)
saveRDS(canastas_regionales, file.path(DIR_INTERIM, "canastas_regionales.rds"))

# --- Procesa cada trimestre mergeado que todavia no tenga su version --------
# --- con pobreza calculada ---------------------------------------------------
archivos_merged <- list.files(DIR_INTERIM, pattern = "^merged_\\d{4}_T\\d\\.rds$")

for (archivo in archivos_merged) {
  periodo <- gsub("^merged_|\\.rds$", "", archivo)
  archivo_salida <- file.path(DIR_INTERIM, sprintf("pobreza_%s.rds", periodo))

  if (file.exists(archivo_salida)) next  # ya procesado

  base <- readRDS(file.path(DIR_INTERIM, archivo))

  # calculate_poverty necesita, como minimo: ANO4, TRIMESTRE, REGION,
  # CODUSU, NRO_HOGAR, CH04, CH06, ITF, PONDIH (variables oficiales EPH,
  # ninguna fue inventada: son las que exige la documentacion del paquete).
  variables_requeridas <- c("ANO4", "TRIMESTRE", "REGION", "CODUSU", "NRO_HOGAR", "CH04", "CH06", "ITF", "PONDIH")
  faltantes <- setdiff(variables_requeridas, names(base))
  if (length(faltantes) > 0) {
    warning(sprintf(
      "%s: faltan variables requeridas para calcular pobreza (%s). Se salta este trimestre.",
      periodo, paste(faltantes, collapse = ", ")
    ))
    next
  }

  base_pobreza <- tryCatch(
    calculate_poverty(base = base, basket = canastas_regionales, print_summary = FALSE),
    error = function(e) {
      warning(sprintf("%s: calculate_poverty fallo (%s). Se salta.", periodo, conditionMessage(e)))
      NULL
    }
  )

  if (is.null(base_pobreza)) next

  saveRDS(base_pobreza, archivo_salida)
  message(sprintf("Pobreza calculada para %s -> %s", periodo, archivo_salida))
}
