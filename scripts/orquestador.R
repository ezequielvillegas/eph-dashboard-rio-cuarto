# =============================================================================
# orquestador.R
# Corre todo el pipeline EPH en orden. Este es el unico script que llama
# GitHub Actions.
# =============================================================================

mensaje_paso <- function(n, nombre) {
  message(sprintf("\n== Paso %s: %s ==", n, nombre))
}

mensaje_paso("00", "Descarga de microdatos nuevos")
source("scripts/00_descarga.R")

mensaje_paso("00b", "Control metodologico")
source("scripts/00b_control_metodologico.R")

mensaje_paso("03", "Merge hogar-individual (filtro Rio Cuarto)")
source("scripts/03_merge.R")

mensaje_paso("04", "Calculo de pobreza (metodologia oficial via paquete eph)")
source("scripts/04_pobreza.R")

mensaje_paso("05", "Indicadores (pobreza, indigencia, desocupacion, menores)")
source("scripts/05_indicadores.R")

mensaje_paso("06", "Actualizacion del panel historico")
source("scripts/06_panel_historico.R")

mensaje_paso("07", "Exportacion para el dashboard")
source("scripts/07_exportar_dashboard.R")

message("\nPipeline completo.")
