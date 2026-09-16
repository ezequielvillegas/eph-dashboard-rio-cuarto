# =============================================================================
# 07_exportar_dashboard.R
# Exporta el panel historico + el changelog metodologico a un unico JSON
# que consume el dashboard HTML estatico (docs/index.html).
# Al vivir en docs/, GitHub Pages lo sirve y redespliega solo con cada push.
# =============================================================================

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite", repos = "https://cloud.r-project.org")
}
library(jsonlite)

panel <- readRDS("data/panel/panel_historico.rds")

changelog <- if (file.exists("metodologia/changelog.csv")) {
  read.csv("metodologia/changelog.csv", stringsAsFactors = FALSE)
} else {
  data.frame()
}

rupturas <- if (file.exists("metodologia/rupturas_conocidas.csv")) {
  read.csv("metodologia/rupturas_conocidas.csv", stringsAsFactors = FALSE)
} else {
  data.frame()
}

salida <- list(
  actualizado = as.character(Sys.time()),
  panel = panel,
  changelog = changelog,
  rupturas_conocidas = rupturas
)

dir.create("docs", showWarnings = FALSE)
write_json(salida, "docs/data.json", dataframe = "rows", auto_unbox = TRUE, na = "null")

message("docs/data.json generado.")
