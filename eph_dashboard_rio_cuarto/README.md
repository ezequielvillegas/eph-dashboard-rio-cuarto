# EPH Río Cuarto — pipeline automatizado

Descarga, controla, procesa y publica indicadores de la Encuesta Permanente
de Hogares (INDEC) para el aglomerado 33 (Río Cuarto). Corre solo cada
trimestre vía GitHub Actions.

## Qué hace cada pieza

| Archivo | Función |
|---|---|
| `scripts/00_descarga.R` | Descarga con `eph::get_microdata()` todas las bases hogar/individual de los últimos 10 años que todavía no estén en `data/raw/`. Idempotente y tolerante a trimestres aún no publicados. |
| `scripts/00b_control_metodologico.R` | Compara columnas y categorías clave entre trimestres consecutivos; deja registro en `metodologia/changelog.csv`. La ruptura conocida de 2016-T2 (rediseño muestral) queda documentada aparte, en `metodologia/rupturas_conocidas.csv`. |
| `scripts/03_merge.R` | Filtra `AGLOMERADO == 33`, valida duplicados de `CODUSU + NRO_HOGAR` y mergea individual + hogar por trimestre. |
| `scripts/04_pobreza.R` | Descarga la canasta básica regional oficial (`eph::get_poverty_lines(regional = TRUE)`) y aplica la metodología de pobreza/indigencia (`eph::calculate_poverty()`), que agrega la columna `situacion` (`pobre` / `indigente` / `no_pobre`). |
| `scripts/05_indicadores.R` | Calcula, por trimestre: tasa de pobreza, tasa de indigencia, tasa de desocupación, % de menores de 18 en hogares pobres y % de hogares con menores que son pobres. Guarda `data/processed/indicadores_<año>_T<trimestre>.rds`. |
| `scripts/06_panel_historico.R` | Acumula todos los trimestres en una serie única, marcando `quiebre_metodologico = TRUE` para todo lo anterior a 2016-T2. |
| `scripts/07_exportar_dashboard.R` | Vuelca panel + changelog + rupturas a `docs/data.json`. |
| `scripts/orquestador.R` | Corre todo en orden. Es lo único que ejecuta GitHub Actions. |
| `.github/workflows/actualizacion_eph.yml` | Corre el orquestador los días 5 de febrero/mayo/agosto/noviembre (margen sobre el calendario habitual de publicación de INDEC) y commitea los resultados. También se puede disparar a mano. |
| `docs/index.html` | Dashboard autocontenido (ECharts) que lee `docs/data.json`: selector de indicador, línea de tiempo con sombreado para el período pre-2016-T2, y tabla de cambios metodológicos detectados. |

## Sobre los indicadores calculados

Son una aproximación construida sobre la metodología oficial de pobreza de
INDEC (2016), implementada por el paquete `eph` (no es un producto oficial
de INDEC — así lo aclara el propio paquete). Antes de publicar una cifra en
un informe institucional, conviene contrastarla contra el último
comunicado de prensa de INDEC para el mismo período. La tasa de
desocupación usa `ESTADO` (1 = ocupado, 2 = desocupado) ponderado por
`PONDI`; la pobreza usa `PONDIH`, siguiendo la convención habitual de
persona/PONDI, hogar/PONDIH.

## Puesta en marcha (GitHub Actions)

1. Creá un repositorio en GitHub (puede ser privado) y subí esta carpeta
   completa tal cual está.
2. En **Settings → Pages**, elegí "Deploy from a branch", rama `main`,
   carpeta `/docs`. Ahí queda publicado el dashboard en una URL tipo
   `https://tu-usuario.github.io/tu-repo/`.
3. El workflow ya está configurado para correr solo los días 5 de
   febrero, mayo, agosto y noviembre. También podés dispararlo a mano
   desde la pestaña **Actions → Actualizacion trimestral EPH Río
   Cuarto → Run workflow** — conviene hacerlo una vez apenas subas el
   repo, para validar que la primera corrida completa funciona antes de
   esperar al próximo trimestre real.
4. No hace falta ningún secret ni credencial: los datos de INDEC son
   públicos y el permiso de escritura para commitear (`contents: write`)
   ya está declarado en el workflow.

## Sobre el control metodológico

`00b_control_metodologico.R` no lee los PDF de INDEC (el sitio
institucional es dinámico y frágil de scrapear); en cambio compara
directamente la estructura de los datos descargados. Esto detecta bien
cambios de variables o de categorías, pero **no reemplaza una revisión
manual** ante anuncios grandes de rediseño. Si en el futuro INDEC anuncia
otro rediseño muestral, se agrega una fila a mano en la tabla
`RUPTURAS_CONOCIDAS` dentro de ese mismo script.

## data.json de ejemplo

`docs/data.json` viene con datos ilustrativos para que puedas abrir
`docs/index.html` en el navegador y ver cómo luce el dashboard antes de
la primera corrida real. Se sobreescribe automáticamente en cada
ejecución del pipeline.

## Primera corrida local (opcional, para probar antes de subir a GitHub)

```r
install.packages(c("eph", "dplyr", "jsonlite"))
setwd("eph_dashboard_rio_cuarto")
source("scripts/orquestador.R")
```

La primera corrida va a tardar bastante (descarga ~10 años x 4 trimestres
x 2 bases). Las corridas siguientes son rápidas porque no vuelve a bajar
lo que ya está en `data/raw/`.
