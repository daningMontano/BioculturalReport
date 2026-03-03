# =============================================================================
# setup.R — Bootstrap del entorno Fundación Pachamama Dashboard
# Ejecutar UNA sola vez antes de lanzar la app:
#   source("setup.R")
#
# R recomendado: 4.4.x  (compatible desde 4.2.0)
# =============================================================================

cat("╔══════════════════════════════════════════════════════╗\n")
cat("║   Fundación Pachamama — Setup del Entorno            ║\n")
cat("║   Actuamos Ahora · Buen Vivir · Amazonía             ║\n")
cat("╚══════════════════════════════════════════════════════╝\n\n")

# ── 1. Verificar versión mínima de R ─────────────────────────────────────────
r_version <- as.numeric(paste0(R.Version()$major, ".",
                                sub("\\..*", "", R.Version()$minor)))
if (r_version < 4.2) {
  stop("Se requiere R >= 4.2.0. Versión actual: ", R.Version()$version.string,
       "\nDescarga en: https://cloud.r-project.org")
}
cat("✓ R", R.Version()$version.string, "\n")

# ── 2. Instalar renv si no está ───────────────────────────────────────────────
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("→ Instalando renv...\n")
  install.packages("renv", repos = "https://cloud.r-project.org")
}
cat("✓ renv", as.character(packageVersion("renv")), "\n")

# ── 3. Restaurar el entorno desde renv.lock ───────────────────────────────────
cat("\n→ Restaurando paquetes desde renv.lock (puede tardar ~3-5 min la primera vez)...\n\n")
renv::restore(prompt = FALSE)

# ── 4. Verificar paquetes críticos ───────────────────────────────────────────
paquetes_criticos <- c("shiny", "bslib", "bsicons", "dplyr", "ggplot2",
                        "plotly", "leaflet", "DT", "glue", "purrr")

cat("\n── Verificación de paquetes ──────────────────────────────\n")
ok <- TRUE
for (pkg in paquetes_criticos) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  ✓ %-20s %s\n", pkg, as.character(packageVersion(pkg))))
  } else {
    cat(sprintf("  ✗ %-20s NO INSTALADO\n", pkg))
    ok <- FALSE
  }
}

# ── 5. Verificar dependencia del sistema: GDAL/GEOS para {sf} ────────────────
cat("\n── Verificación de {sf} (datos espaciales) ───────────────\n")
if (requireNamespace("sf", quietly = TRUE)) {
  cat("  ✓ sf", as.character(packageVersion("sf")), "\n")
  tryCatch({
    sf::sf_extSoftVersion()
    cat("  ✓ GDAL / GEOS / PROJ: disponibles\n")
  }, error = function(e) {
    cat("  ⚠ sf instalado pero sin librerías del sistema (GDAL/GEOS).\n")
    cat("    Instala con:\n")
    cat("      Ubuntu/Debian: sudo apt-get install libgdal-dev libgeos-dev libproj-dev\n")
    cat("      macOS:         brew install gdal geos proj\n")
    cat("      Windows:       usar instalador precompilado de CRAN (ya incluye libs)\n")
  })
} else {
  cat("  ⚠ {sf} no disponible.\n")
}

# ── 6. Resultado final ────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════════\n")
if (ok) {
  cat("✅ Entorno listo. Para lanzar la app ejecuta:\n\n")
  cat("   shiny::runApp(port = 3838, launch.browser = TRUE)\n\n")
} else {
  cat("⚠ Algunos paquetes no se instalaron.\n")
  cat("  Ejecuta renv::restore() manualmente o revisa el log.\n\n")
}
cat("══════════════════════════════════════════════════════\n")
