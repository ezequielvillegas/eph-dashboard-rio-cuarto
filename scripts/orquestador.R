# =============================================================================
# orquestador.R
# Punto de entrada del pipeline EPH – aglomerado 33 (Gran Río Cuarto).
# Ejecutar con: Rscript scripts/orquestador.R
#
# Orden de ejecución:
#   00_descarga.R              → descarga bases individuales y de hogar
#   00b_control_metodologico.R → detecta cambios de estructura entre trimestres
#   03_merge.R                 → filtra agl.33 y mergea individuo+hogar
#   04_pobreza.R               → calcula pobreza e indigencia con canasta regional
#   05_indicadores.R           → indicadores de pobreza por hogar/persona
#   05b_mercado_trabajo.R      → tasas de actividad, empleo, desocupación, etc.
#   05c_ingresos.R             → ingresos medios/medianos (P21, TOT_P12, IPCF, ITF)
#   06_panel_historico.R       → consolida todos los indicadores en un panel único
#   07_exportar_dashboard.R    → genera docs/data.json para el dashboard web
# =============================================================================

cat("=== Pipeline EPH – Gran Río Cuarto ===\n")
cat(sprintf("Inicio: %s\n\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

# ── Helper: ejecutar un script y reportar duración ────────────────────────────
ejecutar_script <- function(nombre) {
  ruta <- file.path("scripts", nombre)
  if (!file.exists(ruta)) {
    warning(sprintf("Script no encontrado: %s — se omite.", ruta))
    return(invisible(FALSE))
  }
  cat(sprintf("--- %s ---\n", nombre))
  t0 <- proc.time()["elapsed"]
  tryCatch(
    source(ruta, echo = FALSE),
    error = function(e) {
      cat(sprintf("ERROR en %s: %s\n", nombre, conditionMessage(e)))
      stop(sprintf("Pipeline abortado en %s", nombre), call. = FALSE)
    }
  )
  elapsed <- round(proc.time()["elapsed"] - t0, 1)
  cat(sprintf("    OK (%.1f s)\n\n", elapsed))
  invisible(TRUE)
}

# ── Secuencia de scripts ───────────────────────────────────────────────────────
ejecutar_script("00_descarga.R")
ejecutar_script("00b_control_metodologico.R")
ejecutar_script("03_merge.R")
ejecutar_script("04_pobreza.R")
ejecutar_script("05_indicadores.R")
ejecutar_script("05b_mercado_trabajo.R")
ejecutar_script("05c_ingresos.R")
ejecutar_script("06_panel_historico.R")
ejecutar_script("07_exportar_dashboard.R")

cat("=== Pipeline completado ===\n")
cat(sprintf("Fin: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
